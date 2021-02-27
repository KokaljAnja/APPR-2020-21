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
tabela5 <- read_csv2("podatki/tabela_5.csv", skip = 1, 
                     col_names = c("stopnja_preobremenjenosti", "starost", "Spol", "leto", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

#Podatki o število slovencev po regijah
url <- "podatki/PrebivalciPoRegijah.htm"
stran <- read_html(url)
tabela <- stran %>% html_nodes(xpath = "//table") %>% .[[1]] %>% html_table(fill = TRUE)
colnames(tabela) <- c("regija", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

#Dokaj neuporabna tabela (uporabila sem jo samo za število prebivalcev, kar pa je že prejšna tabela)
tabela6 <- read_csv2("podatki/Tabela_6.csv", skip = 1, 
                     col_names = c("Regija", "Leto", "Površina", "Število prebivalcev", "Število moških", "Število žensk", "Gostota"),
                     locale=locale(encoding="Windows-1250"))

#Število prenaseljenih oseb po regijah, letno
tabela7 <- read_csv2("podatki/Tabela_7.csv", skip = 1, 
                     col_names = c("Regija", "Leto", "Število oseb v prenaseljenih stanovanjih"),
                     locale=locale(encoding="Windows-1250"))

#Stopnja stanovanjske prikrajšanosti glede na regije, letno
tabela4 <- read_csv2("podatki/Tabela_4.csv", skip = 1, 
                     col_names = c("regija", "leto", "Primanjkljaji", "Delež gospodinjstev"),
                     locale=locale(encoding="Windows-1250"))

#Samoocena splošnega zadovoljstva z življenjem glede na stanovanjsko prikrajšanost, po spolu, letno
tabela8 <- read_csv2("podatki/Tabela_8.csv", skip = 1, 
                     col_names = c("Spol", "Ocena", "Leto", "Število"),
                     locale=locale(encoding="Windows-1250"))


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
tabelaIII <- tabela %>%
  slice(4:15) %>%
  select(-3, -4, -5, -7, -8, -10) %>%
  pivot_longer(c(-regija), names_to="leto", values_to="stevilo",
               names_transform=c(leto=parse_number),
               values_transform=c(stevilo=parse_number)) %>%
  group_by(regija) %>% summarise(povprecje=mean(stevilo)*1000)

#Stopnja preobremenjenosti glede na starost in spol, letno (iz tabele 5)
tabelaIV <- tabela5 %>%
  select(-stopnja_preobremenjenosti)

#Število prenaseljenih stanovanj po letih
tabela6 <- tabela6 %>%
  select(-Površina, -`Število moških`, -`Število žensk`, -`Gostota`) %>%
  group_by(Leto) %>%
  summarise(skupaj=sum(`Število prebivalcev`))

tabela7 <- tabela7 %>%
  group_by(Leto) %>%
  summarise(skupaj=sum(`Število oseb v prenaseljenih stanovanjih`))

tabelaV <- full_join(tabela6, tabela7, by = "Leto")

#Stopnja stanovanjske prikrajšanosti glede na regije, letno(iz tabele 4)
tabelaVI <- tabela4 %>% filter(regija !="SLOVENIJA",
                               leto !="2010",
                               leto !="2012",
                               leto !="2013",
                               leto !="2014",
                               leto !="2016",
                               leto !="2017",
                               leto !="2018")

#Samoocena splošnega zadovoljstva z življenjem glede na stanovanjsko prikrajšanost, po spolu, letno(iz tabele 8)
tabelaVII <- tabela8 %>% filter(Ocena != "Neznano (%)",
                                Ocena != "Povprečje")

