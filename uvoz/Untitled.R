library(readr)
library(tidyr)
library(dplyr)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

tabela1 <- read_csv2("podatki/Tabela_1.csv", skip = 1, 
                     col_names = c("regija", "tip lastništva", "vrsta stavbe", "leto", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabela2 <- read_csv2("podatki/Tabela_2.csv", skip = 1, 
                     col_names = c("leto", "primanklaji", "tip lastništva", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabela3 <- read_csv2("podatki/Tabela_3.csv", skip = 1, 
                     col_names = c("tip gospodinjstva", "leto", "primanklaji", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabela4 <- read_csv2("podatki/Tabela_4.csv", skip = 1, 
                     col_names = c("regija", "leto", "primanklaji", "Število prebivalcev"),
                     locale=locale(encoding="Windows-1250"))

tabelaI <- tabela1 %>%
  filter(regija != "SLOVENIJA", `tip lastništva` != "Tip lastništva -SKUPAJ",
         `vrsta stavbe` != "1 Stanovanja v stanovanjskih stavbah", 
         `vrsta stavbe` !="1.1 Stanovanja v  enostanovanjskih stavbah",
         `vrsta stavbe` != "1.2 Stanovanja v dvostanovanjskih stavbah", 
         `vrsta stavbe` != "1.3 Stanovanja v tro- ali več stanovanjskih stavbah", 
         `vrsta stavbe` != "2 Stanovanja v nestanovanjskih stavbah") %>%
  select(-`vrsta stavbe`)

