# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(tmap)

povprecje.lastnistva.po.regijah <- tabelaI %>% group_by(regija, `tip lastništva`) %>%
  summarise(povprecje=sum(`Število prebivalcev`)/3)

graf_povprecja_po_regijah <- ggplot(povprecje.lastnistva.po.regijah, aes(x=regija, y=povprecje, fill=factor(`tip lastništva`))) +
  geom_bar(position = 'dodge')



# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
# Če zemljevid nima nastavljene projekcije, jo ročno določimo
#proj4string(zemljevid) <- CRS("+proj=utm +zone=10+datum=WGS84")
#
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
