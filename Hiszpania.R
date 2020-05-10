library(tidyverse)
library(readxl)
# infomracje o regionach

regiony.ES <- read_xlsx("hiszpania.regiony.xlsx")

################################################################################################################################
#pobór danych o hiszpanii
library(tidyverse)
ES <- read_csv("https://raw.githubusercontent.com/Secuoyas-Experience/covid-19-es/master/datos-comunidades-csv/covid-19-ES-CCAA-DatosCasos.csv")

ES <- ES %>%
  select(1, 4:9, 12,13)%>%
  rename(data=1, iso=2, kod=3, nazwa=4, cases=7, zmarli=8, recovered=9)%>%
  left_join(regiony.ES, by="kod")%>%
  arrange(data)%>%
  group_by(nazwa)%>%
  mutate(zach.dzienne = c(0,diff(cases)))%>%
  mutate(zgony.dzienne = c(0,diff(zmarli)))%>%
  mutate(wyleczeni.dzienne = c(0,diff(recovered)))%>%
  mutate(zach.100 = zach.dzienne*100000/population)%>%
  mutate(srednia= zoo::rollmean(zach.100, k=7, fill=NA, align="right"))

#wykres dla hiszpanii
ggplot(ES)+
  geom_path( aes(x=data, y=srednia),size=2)+
  facet_wrap(~nazwa.pl, ncol = 5)+
  geom_hline(yintercept = 10, color="blue")+
  theme_bw()


reg.max.ES <-  ES %>%
  filter(population>1e6)%>%
  filter(srednia==max(srednia, na.rm = T))%>%
  ungroup()%>%
  arrange(desc(srednia))%>%
  head(4)%>%
  select(nazwa.pl)%>%
  pull()

ES.wykres <- ES %>%
  ungroup()%>%
  filter(nazwa.pl %in% reg.max.ES)%>%
  filter(srednia>1)%>%
  group_by(nazwa.pl)%>%
  mutate(id=row_number())%>%
  select(nazwa.pl, srednia, id, data, zach.100)%>%
  rename(obwod.pl=nazwa.pl)

save(ES.wykres, file="ES.wykres.RDA")
