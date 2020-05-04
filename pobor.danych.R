library(tidyverse)
library(scales)
library(lubridate)

url <- read_csv(file="https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv")

confirmed <- url %>%
  pivot_longer(cols=c(12:length(names(url))), names_to = "data", values_to = "zarazeni")%>%
  mutate(data=mdy(data))%>%
  select(-c(1:6))%>%
  unite(col="id", c(data, Province_State), sep="_", remove = F)
  
url <- read_csv(file="https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_RU.csv")

death <- url %>%
  pivot_longer(cols=c(12:length(names(url))), names_to = "data", values_to = "zgony")%>%
  mutate(data=mdy(data))%>%
  select(-c(1:6))%>%
  unite(col="id", c(data, Province_State), sep="_", remove = F)%>%
  select(id, zgony)

rosja <- left_join(confirmed, death, by="id") %>%
  select(-c(1,6))%>%
  group_by(Province_State)%>%
  mutate(zach.dzienne = c(0,diff(zarazeni)))%>%
  mutate(zgony.dzienne = c(0,diff(zgony)))%>%
  select(-Country_Region)%>%
  rename(obwod=Province_State)%>%
  ungroup()%>%
  mutate(obwod=as.factor(obwod))

load("obwody.Rda")

rosja <- obw.gotowe%>%
  rename(obwod=Province_State)%>%
  left_join(rosja, by="obwod")%>%
  rename(obwod.pl=17)%>%
  group_by(obwod.pl)%>%
  mutate(zach.100 = zach.dzienne*100000/ludnosc)%>%
  mutate(zach.100.cum = zarazeni*100000/ludnosc)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))

save(rosja, file="rosja.Rda")

# trzeba będzie poprawić Krym z polskimi nazwami
test <- rosja%>%
  filter(is.na(obwod.pl))

###############################################################################################################################


glowne.zarazone <- rosja %>%
  filter(data==max(data))%>%
  filter(ludnosc>1e6)%>%
  arrange(desc(zarazeni))%>%
  head(11)%>%
  select(17)%>%
  pull()

kolejnosc <- rosja %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  arrange(desc(zarazeni))%>%
  head(11)%>%
  select(obwod.pl)

a <- rosja %>%
  filter(obwod.pl %in% glowne.zarazone, zarazeni>0)

a$obwod.pl <- ordered(a$obwod.pl, levels = kolejnosc$obwod.pl)

ggplot(a)+
  geom_col(aes(x=data, y=zarazeni), fill="blue", color="grey")+
  facet_wrap(~obwod.pl, ncol = 4, scales = "free_y")+
  theme_bw()

ggplot(a)+
  geom_col(aes(x=data, y=zach.dzienne), fill="blue", color="grey")+
  facet_wrap(~obwod.pl, ncol=4, scales = "free_y")+
  theme_bw()

ggplot(a)+
  geom_col(aes(x=data, y=zgony), fill="red3", color="grey")+
  facet_wrap(~obwod.pl, ncol=5)+
  theme_bw()

#################################
#średnia kroczaca
glowne.zarazone.100 <- rosja %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  arrange(desc(srednia))%>%
  head(11)%>%
  select(17)%>%
  pull()

kolejnosc <- rosja %>%
  filter(ludnosc>1e6)%>%
  filter(data==max(data))%>%
  arrange(desc(srednia))%>%
  head(11)%>%
  select(obwod.pl)

a <- rosja %>%
  filter(obwod.pl %in% glowne.zarazone.100, zarazeni>0)

a$obwod.pl <- ordered(a$obwod.pl, levels = kolejnosc$obwod.pl)

#wykres z datą na osi x
ggplot(a)+
  geom_path(aes(x=data, y=srednia), color="blue", size=2)+
  facet_wrap(~obwod.pl, ncol = 4)+
  theme_bw()

# wykres z id na osi x
a <- rosja %>%
  filter(obwod.pl %in% glowne.zarazone.100)%>%
  filter(srednia>0.1)%>%
  mutate(id=row_number())%>%
  select(obwod.pl, srednia, id, data)

load("bialorus.Rda")
a <- a %>%
  bind_rows(bialorus)%>%
  mutate(by=if_else(obwod.pl=="Białoruś", paste("tak"), paste("nie")))

kolejnosc <- a %>%
  filter(data==max(data))%>%
  arrange(desc(srednia))%>%
  select(obwod.pl)
 
a$obwod.pl <- ordered(a$obwod.pl, levels = kolejnosc$obwod.pl)

linia1 <- a %>%
  filter(obwod.pl=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(srednia)%>%
  pull()

linia2 <- a %>%
  filter(obwod.pl=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(id)%>%
  pull()

data.by <- a %>%
  filter(obwod.pl=="Białoruś", id==max(id))%>%
  ungroup()%>%
  select(data)%>%
  pull()

png("obowdy.png", units="in", width=10, height=9, res=300)
ggplot(a)+
  geom_path(aes(x=id, y=srednia, color=by), size=2, show.legend = F)+
  facet_wrap(~obwod.pl, ncol = 3)+
  scale_color_manual(values = c("tak"="orange4", "nie"="blue"))+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  geom_vline(xintercept = linia2, color = "firebrick3", linetype = "dashed")+
  labs(x= "ilość dni od przekroczenia 0,1 zarażenia na 100 tys. mieszkańców", 
       y= "dzienny przyrost", 
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców na Białorusi i 11 najbardziej dotkniętych jednostkach RF",
       subtitle = "średnia krocząca z 7 dni",
       caption = "Źródło: CSSE at Johns Hopkins University, Rospotrebnadzor, liczba ludności za Wikipedią")+
  scale_linetype_manual(name = c("", " "), values = c("longdash", "dotted"), labels = c(paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y") ), "ilość dni od przekroczenia poziomu 0,1 zakażenia \nna 100 tys. mieszkancow na Białorusi"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top",
        plot.caption = element_text( size = 8))
dev.off()

# to samo - wariant z legendą h i vline
png("obowdy2.png", units="in", width=10, height=6, res=300)
  ggplot(a)+
  geom_path( aes(x=id, y=srednia, color=by),size=2, show.legend = F)+
  facet_wrap(~obwod.pl, ncol = 4)+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  labs(x= "ilość dni od przekroczenia 0,1 zarażenia na 100 tys. mieszkańców", 
       y= "dzienna ilość zakażeń",
       #linetype="",
       title = "Liczba dziennych zakażeń na 100 tys. mieszkańców na Białorusi i w ośmiu najbardziej dotkniętych jednostkach RF",
       subtitle = "Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców",
       caption = "Źródło: CSSE at Johns Hopkins University, Rospotrebnadzor, liczba ludności za Wikipedią")+
  scale_color_manual(values = c("tak"="orange4", "nie"="blue"))+
  #guides(color=FALSE)+
  scale_linetype_manual(name = c("", " "), values = c("longdash", "dotted"), labels = c(paste("poziom przyrostu zarażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y") ), "ilość dni od przekroczenia poziomu 0,1 zarażenia \nna 100 tys. mieszkancow na Białorusi")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top",
        plot.caption = element_text( size = 8))
dev.off()

###################################################################################################################################
#mapy
#kod ponizej jest jeszcze do  poprawek. Mapy  działają, trzeba znaleźć jakieś ID. 
  
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


RU <- ne_states(country = "russia", returnclass = "sf")%>%
  filter(adm1_code != "RUS+99?")%>%
  filter(iso_3166_2 != "UA-40")%>%
  filter(iso_3166_2 != "UA-43")%>%
  rename(iso=iso_3166_2)
RU[74,5] <- "RU-MOS"
RU[75,5] <- "RU-MOW"



# link o geometrii mapy rosji https://stackoverflow.com/questions/37566620/globe-shaped-map-of-russia

# informacja o różnego rodzaju kodach terytorialnych w RU. Na razie chyba niepotrzebna
regiony <- read_csv(file="https://raw.githubusercontent.com/hflabs/region/master/region.csv")


#testujemy mapę
a <- filter(rosja, data==max(data), id2!="Crimea", id2!="Sevastopol")%>%
  mutate(zach.100 = zarazeni*100000/ludnosc)

RU <- left_join(RU, a, by="iso")

ggplot(data = RU) +
  geom_sf(aes(fill=zach.dzienne)) +
  coord_sf(xlim = c(20, 180), expand = F) +
  scale_fill_gradient(low = "#FFFFCC", high = "#FF3300")+
  #geom_label(data=b, aes(x=Long, y=Lat), label=sprintf("%0.1f",c$zach.100), size=3) +
  labs(fill="liczba \nprzypadków", x="", y="",
       title = "Liczba zakażeń SARS-CoV-2 na 100 tys. mieszkańców",
       #subtitle = paste( "stan na", format(as.Date(UA1$data), "%d/%m/%Y")),
       caption = "Źródło: Ministerstwo Zdrowia Ukrainy, Center for Systems Science and Engineering at Johns Hopkins University, Bank Światowy") +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank(), 
        legend.direction = "horizontal", legend.position=c(0.15,0.9), plot.caption = element_text(size = 8),
        plot.background = element_rect(colour = "grey", size = 0.5), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

##############################################################################################################################################
#próbujemy shp map Rosji
library(maptools)
library(rgeos)
library(rgdal)

shp1 <- readOGR("./mapa", layer = "admin_level_4")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "int_ref")

ggplot() + 
  geom_map(data=a, aes(map_id=iso, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  xlim(15,190) +
  ylim(40,83) +
  coord_map("azequalarea") +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków SARS-CoV-2  na 100 tys. mieszkańców",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$liczba, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

ggplot() + 
  geom_map(data=a, aes(map_id=iso, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  xlim(15,190) +
  ylim(40,83) +
  coord_map(project='azequalarea') +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków SARS-CoV-2  na 100 tys. mieszkańców",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$liczba, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

# mapa tylko europejskiej części
shp1 <- readOGR("./mapa", layer = "a")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "int_ref")


ggplot() + 
  geom_sf(data=europe, fill="grey97")+
  geom_map(data=a, aes(map_id=iso, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  #xlim(15,190) +
  #ylim(40,70) +
  #coord_map(project='mercator') +
  coord_sf(xlim = c(15,90), ylim = c(40,70), expand = F) +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków SARS-CoV-2  na 100 tys. mieszkańców",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$liczba, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

europe <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data=europe) + 
  geom_sf(data=europe)

####################################################################################################################################################

# mapa tylko europejskiej części
shp1 <- readOGR("./mapa", layer = "Azja")
#zmieniamy format danych
shp1f <- fortify(shp1, region = "int_ref")


ggplot() + 
  geom_sf(data=europe, fill="grey97")+
  geom_map(data=a, aes(map_id=iso, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  #xlim(15,190) +
  #ylim(40,70) +
  #coord_map(project='mercator') +
  coord_sf(xlim = c(50,190), ylim = c(40,90), expand = F, datum = st_crs(4326)) +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków SARS-CoV-2  na 100 tys. mieszkańców",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$liczba, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))

europe <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() + 
  geom_map(data=a, aes(map_id=iso, fill=zach.100), map=shp1f) + 
  geom_path(data = shp1f, aes(x=long, y=lat, group=group), colour="grey", size=0.5) + 
  xlim(15,190) +
  ylim(40,83) +
  coord_map(project='azequalarea') +
  scale_fill_gradient(low = "white", high = "orange") +
  labs(fill= "liczba \nzarażeń", title = "Liczba potwierdzonych przypadków SARS-CoV-2  na 100 tys. mieszkańców",
       #subtitle =  paste0( "stan na ", format(as.Date(a$data2), "%d/%m/%Y"), ", godz. 9.00"),
       caption = "Źródło - Ministerstwo Zdrowia Ukrainy") +
  #geom_label(data=a, aes(x=long, y=lat), label=a$liczba, size=3) +
  theme_bw()+
  theme(axis.ticks = element_blank(), panel.border = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = c(0.1, 0.2),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.background = element_rect(colour = "grey", size = 0.5), 
        plot.caption = element_text(size = 8))
