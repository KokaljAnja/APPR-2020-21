# 3. faza: Vizualizacija podatkov

require(dplyr)
library(ggplot2)
require(tmap)


#1.graf

povprecje.lastnistva.po.regijah <- tabelaI %>% group_by(regija, `Tip lastništva`) %>%
  summarise(povprecje=sum(`Število prebivalcev`)/3)

graf_povprecja_po_regijah <- ggplot(data=povprecje.lastnistva.po.regijah, aes(x=regija, y=povprecje, fill=`Tip lastništva`)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  labs(x = "Regija", y = "Povprečno število", title = "Povprečno število tip lastništva na leto \nv posameznih statističnih regijah") +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG")


#2.graf in #3.graf

Vrste_st <- split(tabelaII, tabelaII$`vrsta stavbe`)
Eno_st <- Vrste_st[[1]] %>% select(-`vrsta stavbe`)
Dvo_st <- Vrste_st[[2]] %>% select(-`vrsta stavbe`)
Tro_Vec_st <- Vrste_st[[3]] %>% select(-`vrsta stavbe`) 
Eno_dvo_st <- rbind(Eno_st, Dvo_st) %>% group_by(regija) %>% summarise(st.pr=sum(povprecje))

#zemljevid1 <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_SVN_shp.zip", "gadm36_SVN_1")

#4.graf

slices <- c(1358319, 520386) 
lbls <- c("Stanovanja v eno ali \ndvostanovanjskih stavbah", "Stanovanja v tro- ali več \nstanovanjskih stavbah")
png(file = "pie.jpg")
torta <- pie3D(slices, labels=lbls, explode=0.1, 
               main="Razmerje med eno ali dvo stanovanjsko stavbo \nter tro ali več stanovanjsko stavbo", 
               col=c("darkgoldenrod3", "cadetblue"))
dev.off


#5.graf

graf_st.preobremenjenosti <- ggplot(data=tabelaIV, aes(x=leto, y=`Število prebivalcev`, col=Spol)) +
                                      geom_line(size=1) + facet_grid(~starost) +
  theme_bw() +
  scale_color_manual(values=c("darkgoldenrod3", "cadetblue")) +
  labs(x = "Leto", y = "Število prebivalcev", title = "Število prebivalcev preobremenjenih s stanovanjskimi stroški \nglede na starost, spol in leto")


#6.graf

graf_prenaseljenosti <- ggplot(tabelaV, aes(x=`Število oseb v prenaseljenih stanovanjih`, y=`Gostota naseljenosti`, color=Regija, size=`Število prebivalcev`)) +
  geom_point()





















# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")

#Če zemljevid nima nastavljene projekcije, jo ročno določimo
#proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")

levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
