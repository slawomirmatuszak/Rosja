library(data.table)
#############################################################################################################
# ustawiamy daty

daty <- seq(as.Date("2020-05-02"), as.Date("2020-05-04"), by=1)
#daty <- format(daty, "%d/%m/%Y")
daty <- as.character(daty)
daty <- gsub("/", "-", daty)

#pobieramy dane
linki <- paste0("https://raw.githubusercontent.com/k0ka/covid19-russia-data/master/data/", daty, ".csv")
dest <- paste0("E:/R/Rosja/dane/", daty, ".csv")

# usuwa błąd
oldw <- getOption("warn")
options(warn = -1)

# kod działa
for(i in seq_along(linki)){
  tryCatch(download.file(linki[i], dest[i], mode="wb"),
           error = function(e) print(paste('did not work out')))
}

###########################################################################################################
#łączymy pliki csv
# read file path
all_paths <-
  list.files(path = "./dane/",
             pattern = "*.csv",
             full.names = TRUE)
# read file content
all_content <-
  all_paths %>%
  lapply(read.table,
         header = TRUE,
         sep = ",",
         encoding = "UTF-8")
# read file name
all_filenames <- all_paths %>%
  basename() %>%
  as.list()
# combine file content list and file name list
all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)
# unlist all lists and change column name
Rosja.git <- rbindlist(all_lists, fill = T)
# change column name

rm(all_content, all_filenames, all_lists, all_paths)

###########################################################################################################
library(tidyverse)
library(lubridate)

load("obwody.Rda")
obwody <- obw.gotowe %>%
  rename(obwod.pl=20) %>%
  select(Lat, Long_, Region.Id, terytorium, ludnosc,iso,obwod.pl, okreg.federalny )

Rosja.git <- Rosja.git %>%
  rename(data=6)%>%
  mutate(data=gsub(".csv","", data),
         data=ymd(data))%>%
  left_join(obwody, by="Region.Id")%>%
  mutate(ludnosc = if_else(Region.Name=="Всего", 146748590, ludnosc),
         obwod.pl = if_else(Region.Name=="Всего", paste("Rosja"), obwod.pl),
         okreg.federalny = if_else(Region.Name=="Всего", paste("Rosja"), okreg.federalny))%>%
  group_by(Region.Name)%>%
  mutate(zach.dzienne = c(0,diff(Sick)))%>%
  mutate(zgony.dzienne = c(0,diff(Die)))%>%
  mutate(wyleczeni.dzienne = c(0,diff(Healed)))%>%
  mutate(obwod=as.factor(obwod.pl))%>%
  mutate(zach.100 = zach.dzienne*100000/ludnosc)%>%
  mutate(zach.100.cum = Sick*100000/ludnosc)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  ungroup()

save(Rosja.git, file="rosja.git.Rda")

##########################################################################################################
# wykresy dla Rosji

glowne.zarazone.100 <- Rosja.git %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  arrange(desc(srednia))%>%
  filter(obwod.pl!="Rosja")%>%
  head(11)%>%
  select(12)%>%
  pull()

kolejnosc <- Rosja.git %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  filter(obwod.pl!="Rosja")%>%
  arrange(desc(srednia))%>%
  head(12)%>%
  select(obwod.pl)

a <- Rosja.git %>%
  filter(obwod.pl %in% glowne.zarazone.100, Sick>0)

a$obwod.pl <- ordered(a$obwod.pl, levels = kolejnosc$obwod.pl)

#wykres z datą na osi x
ggplot(a)+
  geom_path(aes(x=data, y=srednia), color="blue", size=2)+
  facet_wrap(~obwod.pl, ncol = 4)+
  theme_bw()


##############################################################################################################
# porównanie RU do Białorusi
# wykres z id na osi x
a <- Rosja.git %>%
  filter(obwod.pl %in% glowne.zarazone.100)%>%
  filter(srednia>0.1)%>%
  group_by(obwod.pl)%>%
  mutate(id=row_number())%>%
  select(obwod.pl, srednia, id, data)

load("bialorus.Rda")
a <- a %>%
  bind_rows(bialorus)%>%
  mutate(by=if_else(obwod.pl=="Białoruś", paste("tak"), paste("nie")))

kolejnosc <- a %>%
  filter(data==max(data)-1)%>%
  arrange(desc(srednia))%>%
  select(obwod.pl)

a$obwod.pl <- ordered(a$obwod.pl, levels = kolejnosc$obwod.pl)

linia1 <- a %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()

linia2 <- a %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%  ungroup()%>%
  select(id)%>%
  pull()

data.by <- a %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%  
  ungroup()%>% 
  select(data)%>%
  pull()

png("obowdy.4.maja.png", units="in", width=10, height=6, res=300)
ggplot(a)+
  geom_path( aes(x=id, y=srednia, color=by),size=2, show.legend = F)+
  facet_wrap(~obwod.pl, ncol = 4)+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  labs(x= "ilość dni od przekroczenia 0,1 zarażenia na 100 tys. mieszkańców", 
       y= "dzienna ilość zakażeń",
       #linetype="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców na Białorusi i 11 najbardziej dotkniętych jednostkach RF",
       subtitle = "Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców",
       caption = "Źródło: CSSE at Johns Hopkins University, Rospotrebnadzor, liczba ludności za Wikipedią")+
  scale_color_manual(values = c("tak"="orange4", "nie"="blue"))+
  #guides(color=FALSE)+
  scale_linetype_manual(name = c("", " "), values = c("longdash", "dotted"), labels = c(paste("poziom przyrostu zarażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y") ), "ilość dni od przekroczenia poziomu 0,1 zarażenia \nna 100 tys. mieszkancow na Białorusi")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top",
        plot.caption = element_text( size = 8))
dev.off()