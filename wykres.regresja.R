library(tidyverse)
library(scales)
library(lubridate)

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

png("5.panstw.8maja.scatterplot.png", units="in", width=15, height=10, res=300)
ggplot(RU, aes(x=id, y=zach.100))+
  geom_point(aes(size="dzienne nowe zakażenia"), color="blue", alpha=0.4) +
  #scale_color_manual(values = c("nowe zakażenia"="blue4", "średnia"="red4" ))+
  geom_path( aes(x=id, y=srednia, color=panstwo),size=2)+
  #geom_smooth(aes(color="średnia"), size=1.5, se=F, span=0.4)+
  coord_cartesian(ylim = c(0,max(RU$zach.100)))+
  facet_wrap(~obwod.pl, ncol = 5)+
  labs(color="",size="", y="dzienne nowe zakażenia na 100 tys. mieszkańców", x="ilość dni od przekroczenia 1 zakażenia na 100 tys. mieszkańców",
       title = "Dzienny przyrost* nowych zakażeń na 100 tys. mieszkańców",
       #subtitle = "Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców",
       caption = "*Średnia krocząca z 7 dni. Jednostki terytorialne powyżej 1 mln mieszkańców.                                                  Źródło: CSSE at Johns Hopkins University, Rospotrebnadzor, RKI, Protezione Civile, Ministerio de Sanidad, liczba ludności za Wikipedią")+
  geom_hline(aes(yintercept = linia1, linetype=""), color="red4")+
  geom_vline(aes(xintercept = obostrzenia, linetype="c"),color= "red4", show.legend = F)+
  geom_vline(aes(xintercept = linia2, linetype=" "),color= "red4", show.legend = F)+
  scale_linetype_manual(name = c("", " ", "c"), values = c("longdash", "dotted", "solid"), labels = c(paste("poziom przyrostu zakażeń na Białorusi\nstan na ",format(data.by,"%d %B %Y") ), "ilość dni od przekroczenia poziomu 1 zakażenia \nna 100 tys. mieszkancow na Białorusi", "moment wprowadzenia lockdownu"))+
  scale_color_manual(values = c("nowe zakażenia"="blue4","Białoruś"="goldenrod4", "Rosja"="blue", "Niemcy"="darkgreen", "Włochy"="firebrick3", "Hiszpania"="slateblue4"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = "top",
        plot.caption = element_text( size = 8))
dev.off()
