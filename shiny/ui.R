library(shiny)

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Dijaki",
             titlePanel("Število dijakov v posamezni vrsti izobraževanja"),
             sidebarPanel(
               selectInput(inputId = "prvi_stolpec",
                           label = "Izberi vrsto izobraževanja",
                           choices = unique(tabelaVI$regija))),
             mainPanel(plotOutput("prvi_stolpec"))),
    
    tabPanel("Dijaki",
             titlePanel("Število dijakov v posamezni vrsti izobraževanja"),
             sidebarPanel(
               selectInput(inputId = "drugi_stolpec",
                           label = "Izberi vrsto izobraževanja",
                           choices = unique(tabelaVII$Ocena))),
             mainPanel(plotOutput("drugi_stolpec")))
             
  )             
))
