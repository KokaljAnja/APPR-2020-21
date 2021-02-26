# 4. faza: Analiza podatkov

#PRENASELJENOST STANOVANJ

graf7 <- ggplot(tabelaV, aes(x=skupaj.x, y=skupaj.y)) +
  geom_point(col="black") +
  geom_smooth(method="lm", formula=y ~ x, col="cadetblue") + 
  labs(x = "Število prebivalcev", y = "Število oseb v prenaseljenih stanovanjih", 
       title = "Število oseb v prenaseljenih stanovanjih glede na število prebivalcev v \nistem letu") +
  theme_bw()














podatki <- obcine %>% transmute(obcina, povrsina, gostota,
                                gostota.naselij=naselja/povrsina) %>%
  left_join(povprecja, by="obcina")
row.names(podatki) <- podatki$obcina
podatki$obcina <- NULL

# Število skupin
n <- 5
skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
