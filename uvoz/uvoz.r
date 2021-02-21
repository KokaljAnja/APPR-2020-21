# 2. faza: Uvoz podatkov

library(readr)
library(tidyr)
library(dplyr)
library(rvest)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


#UVOŽENE TABELE

#Tip lastništva in vrsta stavbe slovencev po regijah, letno
tabela1 <- read_csv2("podatki/Tabela_1.csv", skip = 1, 
                     col_names = c("regija", "Tip lastništva", "vrsta stavbe", "leto", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

#Stopnja preobremenjenosti s stanovanjskimi stroški glede na starost in spol, Slovenija, letno
tabela5 <- read_csv2("podatki/Tabela_5.csv", skip = 1, 
                     col_names = c("stopnja_preobremenjenosti", "starost", "Spol", "leto", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

#Podatki o število slovencev po regijah
url <- "podatki/PrebivalciPoRegijah.htm"
stran <- read_html(url)
tabela <- stran %>% html_nodes(xpath = "//table") %>% .[[1]] %>% html_table(fill = TRUE)
colnames(tabela) <- c("regija", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")


#PREČIŠČENE TABELE

#Tip lastništva slovencev po regijah, letno (iz tabele 1)
tabelaI <- tabela1 %>%
  filter(regija != "SLOVENIJA", `Tip lastništva` != "Tip lastništva -SKUPAJ",
         `vrsta stavbe` != "1 Stanovanja v stanovanjskih stavbah", 
         `vrsta stavbe` !="1.1 Stanovanja v  enostanovanjskih stavbah",
         `vrsta stavbe` != "1.2 Stanovanja v dvostanovanjskih stavbah", 
         `vrsta stavbe` != "1.3 Stanovanja v tro- ali več stanovanjskih stavbah", 
         `vrsta stavbe` != "2 Stanovanja v nestanovanjskih stavbah") %>%
  select(-`vrsta stavbe`)

#Vrsta stanovanj slovencev po regijah letno (iz tabele 1)
tabelaII <- tabela1 %>%
  filter(regija != "SLOVENIJA", `Tip lastništva` != "Stanovanja z drugimi tipi lastništva",
         `Tip lastništva` != "Najeta stanovanja",
         `Tip lastništva` != "Lastniška stanovanja",
         `vrsta stavbe` !="1 Stanovanja v stanovanjskih stavbah",
         `vrsta stavbe` != "2 Stanovanja v nestanovanjskih stavbah",
         `vrsta stavbe` != "Vrsta stavbe - SKUPAJ") %>%
  select(-`Tip lastništva`) %>% 
  group_by(regija, `vrsta stavbe`) %>%
  summarise(povprecje=sum(`Število prebivalcev`)/3)

#Ševilo prebivalcev po regijah (iz tabele, url)
#tabelaIII <- tabela %>%
#  slice(4:15) %>%
#  select(-3, -4, -5, -7, -8, -10) %>%
#  gather(-regija, key = leto, value = stevilo) %>% group_by(regija) %>% 
#  summarise(povprecje=sum(`stevilo`)/3)

#Stopnja preobremenjenosti glede na starost in spol, letno (iz tabele 5)
tabelaIV <- tabela5 %>%
  select(-stopnja_preobremenjenosti) %>% mutate(leto = as.integer(leto))









#Še ne uporabljene tabele
tabela2 <- read_csv2("podatki/Tabela_2.csv", skip = 1, 
                     col_names = c("leto", "primanklaji", "Tip lastništva", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabela3 <- read_csv2("podatki/Tabela_3.csv", skip = 1, 
                     col_names = c("tip gospodinjstva", "leto", "primanklaji", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabela4 <- read_csv2("podatki/Tabela_4.csv", skip = 1, 
                     col_names = c("regija", "leto", "primanklaji", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

#Še ne urejene tabele
tabela6 <- read_csv2("podatki/Tabela_6.csv", skip = 1, 
                     col_names = c("Regija", "Leto", "Površina(km2)", "Število prebivalcev", "Število moških", "Število žensk", "Gostota naseljenosti"),
                     locale=locale(encoding="Windows-1250"))

tabela7 <- read_csv2("podatki/Tabela_7.csv", skip = 1, 
                     col_names = c("Regija", "Leto", "Število oseb v prenaseljenih stanovanjih"),
                     locale=locale(encoding="Windows-1250"))

tabelaV <- inner_join(tabela6, tabela7, by=c("Regija", "Leto")) %>% 
  select(-`Število moških`, -`Število žensk`)




















# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% pivot_longer(`1`:`4`, names_to="velikost.druzine", values_to="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

