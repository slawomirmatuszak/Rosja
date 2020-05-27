###########################################################################################
# pobór danych o ludności we Włoszech
library(rvest)

url <- "https://pl.wikipedia.org/wiki/Regiony_W%C5%82och"

h <- read_html(url)

tab <- h %>% html_nodes("table")
tab <- tab[[2]] %>% html_table(fill=T)

names(tab) <- c("nazwa.pl", "nazwa", "stolica", "population", "area")

lud <- gsub("[0-9]", "", tab$area)
lud <- lud[1]

bolzano <- tibble("nazwa.pl"="Południowy Tyrol", "nazwa"="P.A. Bolzano", "stolica"="Bolzano", "population"=515714, "area"=7400)
trydent <- tibble("nazwa.pl"="Trydent", "nazwa" = "P.A. Trento", "stolica"= "Trento", "population"=536237, "area"=6206)

regiony.IT <- tab %>%
  mutate(population = gsub(lud, "", population),
         population = as.numeric(population),
         area = gsub(lud, "", area),
         area = as.numeric(area))%>%
  mutate(nazwa = case_when(nazwa=="Valle d’Aosta / Vallée d'Aoste" ~ "Valle d'Aosta",
                           nazwa=="Friuli-Venezia Giulia" ~ "Friuli Venezia Giulia",
                           TRUE~nazwa))%>%
  filter(nazwa!="Trentino-Alto Adige / Südtirol")%>%
  bind_rows(trydent, bolzano)
save(regiony.IT, file = "regiony.IT.Rda")

###########################################################################################
# dane o Covid we Włoszech
library(tidyverse)
library(scales)
library(lubridate)

ITA <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

load("regiony.IT.Rda")

ITA <- ITA %>%
  select(1,3,4,5,6,14,15,16)

oldnames <- names(ITA)
newnames <- c("data", "kod", "nazwa", "lat", "long", "recovered", "dead", "total.cases")

ITA <- ITA %>%
  rename_at(vars(all_of(oldnames)), ~ newnames)%>%
  left_join(regiony.IT, by="nazwa")%>%
  group_by(nazwa)%>%
  mutate(zach.dzienne = c(0,diff(total.cases)))%>%
  mutate(zgony.dzienne = c(0,diff(dead)))%>%
  mutate(wyleczeni.dzienne = c(0,diff(recovered)))%>%
  mutate(zach.100 = zach.dzienne*100000/population)%>%
  mutate(zach.100.cum = total.cases*1e5/population)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))%>%
  separate(data, c("data", "godzina"), sep = " ", remove = T)%>%
  mutate(data=ymd(data))%>%
  select(-godzina)

save(ITA, file = "ITA.Rda")

test<-ITA %>%
  filter(data==max(data))%>%
  ungroup()%>%
  select(nazwa.pl, total.cases, population, zach.100.cum)%>%
  arrange(desc(zach.100.cum))

#wykres dla Włoch
ggplot(ITA)+
  geom_path( aes(x=data, y=srednia),size=2)+
  facet_wrap(~nazwa.pl, ncol = 5)+
  theme_bw()

a <- ITA

  ggplot(a, aes(x=data, y=zach.100))+
   geom_point(size=2, color="blue", alpha = 0.7)+
   geom_smooth(size = 2, color="red3", se=F,span=0.4)+
   facet_wrap(~nazwa.pl, ncol = 5, scales = "free")+
   theme_bw()
##################################################################################################################################
# wykres porównujący prowincje z różnych krajów

reg.max <-  ITA %>%
  filter(srednia==max(srednia, na.rm = T))%>%
  filter(population>1e6)%>%
  ungroup()%>%
  arrange(desc(srednia))%>%
  head(4)%>%
  select(nazwa)%>%
  pull()

ITA.wykres <- ITA %>%
  ungroup()%>%
  filter(nazwa %in% reg.max)%>%
  filter(srednia>1)%>%
  group_by(nazwa.pl)%>%
  mutate(id=row_number())%>%
  select(nazwa.pl, srednia, id, data, zach.100)%>%
  rename(obwod.pl=nazwa.pl)%>%
  mutate(data=ymd(data))

save(ITA.wykres, file="ITA.wykres.Rda")

#################################################################################################################
#dodajemy Włochy
load("ITA.wykres.Rda")

#dodajemy Białoruś
load("bialorus.Rda")

#dodajemy Niemcy
load("DE.wykres.Rda")

# dodajemy rosję  
load("rosja.git.Rda")

#dodajemy Hiszpanię
load("ES.wykres.Rda")

glowne.zarazone.100 <- Rosja.git %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  arrange(desc(srednia))%>%
  filter(obwod.pl!="Rosja")%>%
  head(3)%>%
  select(12)%>%
  pull()

RU <- Rosja.git %>%
  filter(obwod.pl %in% glowne.zarazone.100)%>%
  filter(srednia>1)%>%
  group_by(obwod.pl)%>%
  mutate(id=row_number())%>%
  select(obwod.pl, srednia, id, data, zach.100)%>%
  bind_rows(ITA.wykres)%>%
  bind_rows(ES.wykres)%>%
  bind_rows(bialorus)%>%
  bind_rows(DE.wykres)%>%
  mutate(panstwo = case_when(obwod.pl=="Bawaria" ~ "Niemcy",
                             obwod.pl=="Badenia-Wirtembergia" ~ "Niemcy",
                             obwod.pl=="Hamburg" ~ "Niemcy",
                             obwod.pl=="Białoruś" ~ "Białoruś",
                             obwod.pl=="Miasto Moskwa" ~ "Rosja",
                             obwod.pl=="Obwód kałuski" ~ "Rosja",
                             obwod.pl=="Obwód moskiewski" ~ "Rosja",
                             obwod.pl=="Lombardia" ~ "Włochy",
                             obwod.pl=="Emilia-Romania" ~ "Włochy",
                             obwod.pl=="Piemont" ~ "Włochy",
                             obwod.pl=="Liguria" ~ "Włochy",
                             TRUE ~ "Hiszpania"))

obostrzenia.IT.dodatkowe <- data.frame("obwod.pl"=c("Piemont","Liguria"), "obostrzenia" = c(1, 1))
obostrzenia.IT <- RU %>%
  group_by(obwod.pl)%>%
  filter(data=="2020-03-09")%>%
  filter(panstwo=="Włochy")%>%
  select(obwod.pl, id)%>%
  rename(obostrzenia=id)%>%
  bind_rows(obostrzenia.IT.dodatkowe)

obostrzenia.DE <- RU %>%
  group_by(obwod.pl)%>%
  filter(data=="2020-03-22")%>%
  filter(panstwo=="Niemcy")%>%
  select(obwod.pl, id)%>%
  rename(obostrzenia=id)

obostrzenia.ES <- RU %>%
  group_by(obwod.pl)%>%
  filter(data=="2020-03-14")%>%
  filter(panstwo=="Hiszpania")%>%
  select(obwod.pl, id)%>%
  rename(obostrzenia=id)

obostrzenia <- bind_rows(obostrzenia.DE, obostrzenia.ES, obostrzenia.IT)

RU <- left_join(RU, obostrzenia, by="obwod.pl")



kolejnosc <- RU %>%
  filter(srednia==max(srednia))%>%
  arrange(desc(srednia))%>%
  select(obwod.pl)

RU$obwod.pl <- ordered(RU$obwod.pl, levels = kolejnosc$obwod.pl)

linia1 <- RU %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()

linia2 <- RU %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%  ungroup()%>%
  select(id)%>%
  pull()

data.by <- RU %>%
  filter(obwod.pl=="Białoruś")%>%
  filter(id==max(id))%>%  
  ungroup()%>% 
  select(data)%>%
  pull()

png("E:/R/Covid-19/wykresy/5.panstw.20maja.png", units="in", width=13, height=10, res=300)
ggplot(RU)+
  geom_path( aes(x=id, y=srednia, color=panstwo),size=2)+
  facet_wrap(~obwod.pl, ncol = 5)+
  labs(color="", y="dzienne nowe zakażenia na 100 tys. mieszkańców", x="ilość dni od przekroczenia 1 zakażenia na 100 tys. mieszkańców",
       title = "Dzienny przyrost* nowych zakażeń na 100 tys. mieszkańców",
       #subtitle = "Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców",
       caption = "*Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców.                                                  Źródło: CSSE at Johns Hopkins University, Rospotrebnadzor, RKI, Protezione Civile, Ministerio de Sanidad, liczba ludności za Wikipedią")+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = obostrzenia, linetype="c"),color= "red4", show.legend = F)+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  scale_linetype_manual(name = c("", " ", "c"), values = c("longdash", "dotted", "solid"), labels = c(paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y") ), "ilość dni od przekroczenia poziomu 1 zakażenia \nna 100 tys. mieszkancow na Białorusi", "moment wprowadzenia lockdownu"))+
  scale_color_manual(values = c("Białoruś"="goldenrod4", "Rosja"="blue", "Niemcy"="darkgreen", "Włochy"="firebrick3", "Hiszpania"="slateblue4"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top",
        plot.caption = element_text( size = 8))
dev.off()


# Włochy i Dagestan -------------------------------------------------------
library(tidyverse)
library(gridExtra)
#dodajemy Włochy
load("ITA.Rda")
# dodajemy rosję  
load("rosja.git.Rda")

a <- Rosja.git %>%
  rename(nazwa.pl=obwod.pl)%>%
  mutate(dead.cum = Die*1e5/ludnosc)%>%
  select(data, nazwa.pl, srednia, zach.100.cum, dead.cum, Die)%>%
  rename(dead=Die)%>%
  filter(nazwa.pl=="Republika Dagestanu")

ITA.Dag <- ITA %>%
  ungroup()%>%
  filter(nazwa.pl %in% c("Dolina Aosty", "Piemont", "Lombardia", "Trydent", "Wenecja Euganejska", "Emilia-Romania", "Friuli-Wenecja Julijska", "Liguria", "Toskania"))%>%
  mutate(dead.cum = dead*1e5/population)%>%
  select(data, nazwa.pl, srednia, zach.100.cum, dead, dead.cum)%>%
  bind_rows(a)

ggplot(ITA.Dag)+
  geom_path( aes(x=data, y=srednia),size=2, color="orange")+
  facet_wrap(~nazwa.pl, ncol = 5)+
  labs(x="", y="", title = "Ilość dziennych nowych zakażeń na 100 tys. mieszkańców")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))-> p1

ggplot(ITA.Dag)+
  geom_path( aes(x=data, y=zach.100.cum),size=2, color="orange")+
  facet_wrap(~nazwa.pl, ncol = 5)+
  labs(x="", y="", title = "Skumulowana ilość zakażeń na 100 tys. mieszkańców")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))-> p2

ggplot(ITA.Dag)+
  geom_path( aes(x=data, y=dead),size=2, color="red")+
  facet_wrap(~nazwa.pl, ncol = 5)+
  labs(x="", y="", title = "Skumulowana ilość zgonów z powodu Covid-19")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))-> p3

ggplot(ITA.Dag)+
  geom_path( aes(x=data, y=dead.cum),size=2, color="red")+
  facet_wrap(~nazwa.pl, ncol = 5)+
  labs(x="", y="", title = "Skumulowana ilość zgonów na 100 tys. mieszkańców z powodu Covid-19")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))-> p4

png("E:/R/Covid-19/wykresy/dagestan.24maja.png", units="in", width=16, height=10, res=300)
grid.arrange(p1, p3, p2, p4, ncol=2)
dev.off()
