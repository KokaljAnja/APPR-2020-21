# 3. faza: Vizualizacija podatkov

require(dplyr)
library(ggplot2)
require(tmap)
library(cowplot)
library(plotrix)


#1.graf (TIP LASTNIŠTVA)

povprecje.lastnistva.po.regijah <- tabelaI %>% group_by(regija, `Tip lastništva`) %>%
  summarise(povprecje=(sum(`Število prebivalcev`)/3)/1000)

graf_povprecja_po_regijah <- ggplot(data=povprecje.lastnistva.po.regijah, aes(x=regija, y=povprecje, fill=`Tip lastništva`)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  labs(x = "Regija", y = "Povprečno število(/1000)", title = "Povprečno število tip lastništva na leto \nv posameznih statističnih regijah") +
  theme_dark() +
  scale_fill_brewer(palette = "BrBG")


#2.graf in #3.graf (VRSTA STAVBE)

Vrste_st <- split(tabelaII, tabelaII$`vrsta stavbe`)
Eno_st <- Vrste_st[[1]] %>% select(-`vrsta stavbe`)
Dvo_st <- Vrste_st[[2]] %>% select(-`vrsta stavbe`)
Tro_Vec_st <- Vrste_st[[3]] %>% select(-`vrsta stavbe`) 
Eno_dvo_st <- rbind(Eno_st, Dvo_st) %>% group_by(regija) %>% summarise(povprecje=sum(povprecje))

stanovanje <- left_join(Tro_Vec_st, tabelaIII, by = c("regija"))
stanovanje$povprecje.x <- as.numeric(stanovanje$povprecje.x)
stanovanje$povprecje.y <- as.numeric(stanovanje$povprecje.y)
stanovanje <- transform(stanovanje, delez = round((povprecje.x/povprecje.y)*100, 2)) %>% select(-povprecje.x, -povprecje.y)

hisa <- left_join(Eno_dvo_st, tabelaIII, by = c("regija"))
hisa$povprecje.x <- as.numeric(hisa$povprecje.x)
hisa$povprecje.y <- as.numeric(hisa$povprecje.y)
hisa <- transform(hisa, delez = round((povprecje.x/povprecje.y)*100, 2)) %>% select(-povprecje.x, -povprecje.y)

zemljevid <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", encoding = "UTF-8")
zemljevid$NAME_1 <- c("Gorenjska", "Goriška","Jugovzhodna", "Koroška", "Primorsko-notranjska", "Obalno-kraška", 
                      "Osrednjeslovenska", "Podravska", "Pomurska", "Savinjska", "Posavska", "Zasavska")
zemljevid <- fortify(zemljevid)

graf2 <- ggplot() + geom_polygon(data=left_join(zemljevid, stanovanje, by=c("NAME_1"="regija")),
                                                aes(x=long, y=lat, group=group, fill=delez)) +
  geom_line() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Delež (%)")) +
  ggtitle("Povprečno število ljudi, ki živi v tro- ali \nvečstanovanjskih objektih") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "cadetblue",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill") +
  theme_dark()

graf3 <- ggplot() + geom_polygon(data=left_join(zemljevid, hisa, by=c("NAME_1"="regija")),
                                 aes(x=long, y=lat, group=group, fill=delez)) +
  geom_line() + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(fill=guide_colorbar(title="Delež (%)")) +
  ggtitle("Povprečno število ljudi, ki živi v eno- ali \ndvostanovanjskih objektih") +
  labs(x = " ") +
  labs(y = " ") +
  scale_fill_gradient(low = "white", high = "darkgoldenrod3",
                      space = "Lab", na.value = "#e0e0d1", guide = "black",
                      aesthetics = "fill") +
  theme_dark()


#4.graf (PRIMERJAVA-VRSTA STAVBE)

slices <- c(1358319, 520386) 
lbls <- c("Stanovanja v eno ali \ndvostanovanjskih stavbah", "Stanovanja v tro- ali več \nstanovanjskih stavbah")
png(file = "pie.jpg")
torta <- pie3D(slices, labels=lbls, explode=0.1, 
               main="Razmerje med eno ali dvo stanovanjsko stavbo \nter tro ali več stanovanjsko stavbo", 
               col=c("darkgoldenrod3", "cadetblue"))
dev.off


#5.graf (STOPNJA PREOBREMENJENOSTI)

graf_st.preobremenjenosti <- ggplot(data=tabelaIV, aes(x=leto, y=`Število prebivalcev`, col=Spol)) +
                                      geom_line(size=1) + facet_grid(~starost) +
  theme_bw() +
  scale_color_manual(values=c("darkgoldenrod3", "cadetblue")) +
  labs(x = "Leto", y = "Število prebivalcev", title = "Število prebivalcev preobremenjenih s stanovanjskimi stroški \nglede na starost, spol in leto") +
  scale_x_continuous(breaks=seq(2010, 2020, 4))