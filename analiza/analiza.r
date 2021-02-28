# 4. faza: Analiza podatkov

#PRENASELJENOST STANOVANJ

graf7 <- ggplot(tabelaV, aes(x=skupaj.x, y=skupaj.y)) +
  geom_point(col="black") +
  geom_smooth(method="lm", formula=y ~ x, col="cadetblue") + 
  labs(x = "Število prebivalcev", y = "Število oseb v prenaseljenih stanovanjih", 
       title = "Število oseb v prenaseljenih stanovanjih glede na število \nprebivalcev v istem letu") +
  theme_bw()

