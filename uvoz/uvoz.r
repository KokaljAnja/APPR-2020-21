# 2. faza: Uvoz podatkov

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
