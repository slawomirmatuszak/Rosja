library(data.table)
library(tidyverse)
#############################################################################################################
# ustawiamy daty

daty <- seq(as.Date(Sys.Date()-1), Sys.Date(), by=1)
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
  mutate(obwod.pl=ifelse(Region.Name=="Республика Крым", paste("Krym"), obwod.pl))%>%
  mutate(obwod.pl=ifelse(Region.Name=="Севастополь", paste("Sewastopol"), obwod.pl))%>%
  ungroup()

save(Rosja.git, file="rosja.git.Rda")
