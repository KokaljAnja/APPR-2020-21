library(shiny)

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Primanjkljaj",
             titlePanel("Delež gospodinjstev glede na element prikrajšanosti"),
             sidebarPanel(
               selectInput(inputId = "prvi_stolpec",
                           label = "Izberi regijo",
                           choices = unique(tabelaVI$regija))),
             mainPanel(plotOutput("prvi_stolpec"))),
    
    tabPanel("Samoocena",
             titlePanel("Samoocena splošnega zadovoljstva z življenjem"),
             sidebarPanel(
               selectInput(inputId = "drugi_stolpec",
                           label = "Izberi oceno",
                           choices = unique(tabelaVII$Ocena))),
             mainPanel(plotOutput("drugi_stolpec")))
             
  )             
))
