library(shiny)

shinyServer(function(input, output) {
  
  output$prvi_stolpec <- renderPlot({
    graf.primankljaja <- ggplot(tabelaVI %>%
                            filter(regija == input$prvi_stolpec)) +
      aes(x=leto, y=`Delež gospodinjstev`, fill=Primanjkljaji) +
      coord_flip() +
      labs( x='Leto', y = 'Delež gospodinjstev') + 
      geom_col(position="dodge") +
      theme_dark() +
      scale_fill_brewer(palette = "BrBG") +
      scale_x_continuous(breaks = seq(2011, 2019, by=4))
    print(graf.primankljaja)
  })
  
  output$drugi_stolpec <- renderPlot({
    graf.ocene <- ggplot(tabelaVII %>%
                                  filter(Ocena == input$drugi_stolpec)) +
      aes(x=Leto, y=Število, fill=Spol) +
      labs( x='Leto', y = 'Število ocenjenih') +
      coord_flip() +
      geom_col(position="dodge") +
      scale_fill_manual(values=c('darkgoldenrod3', 'cadetblue')) +
      scale_x_continuous(breaks = seq(2012, 2019, by=1))
    print(graf.ocene)
  })
})
