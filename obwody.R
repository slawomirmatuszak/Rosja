library(tidyverse)
library(rvest)
library(readxl)

#obwody po angielsku
url <- "https://en.wikipedia.org/wiki/ISO_3166-2:RU"

h <- read_html(url)

tab <- h %>% html_nodes("table")
tab <- tab[[1]] %>% html_table(fill=T)
obwody_en <- tab[-1,]
names(obwody_en)[1] <- "iso"
names(obwody_en)[3] <- "subdivision.var.name"

# obwody po polsku
url <- "https://pl.wikipedia.org/wiki/ISO_3166-2:RU"

h <- read_html(url)

tab <- h %>% html_nodes("table")
tab <- tab[[1]] %>% html_table(fill=T)
obwody_pl <- tab%>%
  rename(iso=1)

# obwody po rosyjsku

url <- "https://ru.wikipedia.org/wiki/ISO_3166-2:RU"

h <- read_html(url)

tab <- h %>% html_nodes("table")
tab1 <- tab[[2]] %>% html_table(fill=T)
tab2 <- tab[[3]] %>% html_table(fill=T)
tab3 <- tab[[4]] %>% html_table(fill=T)
tab4 <- tab[[5]] %>% html_table(fill=T)
tab5 <- tab[[6]] %>% html_table(fill=T)
tab6 <- tab[[7]] %>% html_table(fill=T)
obwody_ru <- rbind(tab1, tab2, tab3, tab4, tab5, tab6)%>%
  rename(iso=1)

obwody <- merge(obwody_en, obwody_pl, by="iso")
obwody <- merge(obwody, obwody_ru, by="iso")
obwody <- as_tibble(obwody)

obwody <- obwody%>%
  rename(obwod=7)%>%
  mutate(id=obwod)%>%
  mutate(id=gsub("область", "", id),
         id=gsub("край", "", id),
         id=gsub("Республика", "", id),
         id=gsub("автономный округ", "", id),
         id=gsub(" ", "", id))

rm("a","h", "obwody_en" ,"obwody_pl", "obwody_ru", "tab", "tab1","tab2","tab3","tab4","tab5","tab6","url" )

##############################################

# dane o ludności i terytorium
url <- "https://ru.wikipedia.org/wiki/%D0%A1%D1%83%D0%B1%D1%8A%D0%B5%D0%BA%D1%82%D1%8B_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B9%D1%81%D0%BA%D0%BE%D0%B9_%D0%A4%D0%B5%D0%B4%D0%B5%D1%80%D0%B0%D1%86%D0%B8%D0%B8"

h <- read_html(url)

tab <- h %>% html_nodes("table")

obwody.pop <- tab[[5]] %>% html_table(fill=T)%>%
  select(c(2,5,6,7,9))

names(obwody.pop) <- c("obwod", "terytorium", "ludnosc", "stolica","kod.OKATO")

obwody.pop <- as_tibble(obwody.pop)

#ogromny problem z usunięciem spacji. W końcu udało się znaleźć rozwiązanie


obwody.pop <- obwody.pop%>%
  filter(stolica!="")%>%
  mutate(ludnosc=gsub("↗","", ludnosc),
         ludnosc=gsub("↘","", ludnosc))

lud <- gsub("[0-9]", "", obwody.pop$ludnosc)
lud <- lud[1]
obwody.pop <- obwody.pop%>%
  mutate(ludnosc=gsub(lud, "", ludnosc),
         ludnosc=as.numeric(ludnosc))%>%
  mutate(obwod = gsub("\\[.*?\\]", "", obwod))%>%
  mutate(kod.OKATO = gsub("\\[.*?\\]", "", kod.OKATO))

obwody.pop <- obwody.pop %>%
  mutate(id=obwod)%>%
  mutate(id=gsub("область", "", id),
         id=gsub("край", "", id),
         id=gsub("Республика", "", id),
         id=gsub("АО", "", id),
         id=gsub(" ", "", id))%>%
  filter(obwod!="Российская Федерация")

obwody.pop[15,6] <- "Саха"

# test dla porówania nazw w obu tabelach

test <- usefun::outersect(a$id, obwody.pop$id)
#poprawiamy nazwy w obwodach
test2 <- intersect(test, obwody$id)

obwody<-obwody%>%
  mutate(id=gsub("Еврейскаяавтономная", "Еврейская", id),
         id=gsub("Кабардино-Балкария", "Кабардино-Балкарская", id),
         id=gsub("Карачаево-Черкесия", "Карачаево-Черкесская", id),
         id=gsub("Кемеровская", "Кемеровская-Кузбасс", id),
         id=gsub("Удмуртия", "Удмуртская", id),
         id=gsub("Чечня", "Чеченская", id),
         id=gsub("Чувашия", "Чувашская", id),
         id=gsub("Якутия", "Саха", id),
         )

obw.calosc<- left_join(obwody.pop, obwody, by="id")%>%
  rename(angielska=8)%>%
  mutate(angielska=if_else(id=="Крым", paste("Crimea"), angielska))%>%
  mutate(angielska=if_else(id=="Севастополь", paste("Sevastopol"), angielska))%>%
  mutate(id2=angielska)%>%
  mutate(id2=gsub("oblast'", "", id2),
         id2=gsub("avtonomnaya", "", id2),
         id2=gsub("avtonomnyy", "", id2),
         id2=gsub("okrug", "", id2),
         id2=gsub("kray", "", id2),
         id2=gsub("Respublika", "", id2),
         id2=gsub("'", "", id2),
         id2=gsub(",", "", id2),
         id2=gsub("(local variant is Yugra)", "", id2, fixed = T),
         id2=gsub("(local variant is Tuva)", "", id2, fixed = T),
         id2=gsub("(local variant is Jakutija)", "", id2, fixed = T),
         id2=gsub(" ", "", id2),
         id2=gsub("(localvariantisAlaniya[SevernayaOsetiyaAlaniya])", "", id2, fixed = T))%>%
  rename(id2.obwody=id2)

# nazwy obwodów z github

obw.git <- read_csv(file="https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv")

obw.git <- obw.git%>%
  select(7,9,10)%>%
  unique() %>%
  mutate(id2=Province_State)%>%
  mutate(id2=gsub("oblast", "", id2),
         id2=gsub("kray", "", id2),
         id2=gsub("Republic", "", id2),
         id2=gsub("republic", "", id2),
         id2=gsub("of", "", id2),
         id2=gsub("AO", "", id2),
         id2=gsub("Autonomous", "", id2),
         id2=gsub("autonomous", "", id2),
         id2=gsub(" ", "", id2))%>%
  mutate(id2=if_else(Province_State=="Moscow oblast", paste("Moscow2"), id2))

#etap ręczny w excelu
lacznik <- readxl::read_xlsx("obwody.xlsx")%>%
  rename(id2=id2.git)%>%
  select(-1)%>%
  mutate(okreg.federalny = case_when(is.na(federal) ~ "Rosja",
                                     federal=="Central" ~"Centralny",
                                     federal=="Ural" ~"Uralski",
                                     federal=="Northwestern" ~"Północno-Zachodni",
                                     federal=="Volga" ~"Nadwołżański",
                                     federal=="Far Eastern" ~"Dalekowschodni",
                                     federal=="Siberian" ~"Syberyjski",
                                     federal=="Southern" ~"Południowy",
                                     federal=="North Caucasian" ~"Północnokaukaski"))

obw.gotowe <- left_join(obw.git, lacznik, by="id2")%>%
  left_join(obw.calosc, by="id2.obwody")

save(obw.gotowe, file="./obwody.Rda")
