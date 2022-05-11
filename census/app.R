library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
stany <- readRDS("data/stany.rds")

ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Tworzenie mapy demograficznej na podstawie 
               informacji ze spisu powszechnego w USA w 2010 r."),
      
      selectInput("var", 
                  label = "Wybierz zmienną do wyświetlenia",
                  choices = c("Procent Bialych",
                              "Procent Czarnoskorych",
                              "Procent Latynosow", 
                              "Procent Azjatow"),
                  selected = "Procent Bialych"),
      
      sliderInput("zakres", 
                  label = "Wybrany zakres:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      plotOutput("mapa")
      )
  )
)

server <- function(input, output) {
  
  output$mapa <- renderPlot({
    data <- switch(input$var, 
                   "Procent Bialych" = stany$white,
                   "Procent Czarnoskorych" = stany$black,
                   "Procent Latynosow" = stany$hispanic,
                   "Procent Azjatow" = stany$asian)
    
    kolor <- switch(input$var, 
                    "Procent Bialych" = "darkgreen",
                    "Procent Czarnoskorych" = "black",
                    "Procent Latynosow" = "darkorange",
                    "Procent Azjatow" = "darkviolet")
    
    tytul <- switch(input$var, 
                    "Procent Bialych" = "% białych",
                    "Procent Czarnoskorych" = "% czarnoskórych",
                    "Procent Latynosow" = "% latynosów",
                    "Procent Azjatow" = "% azjatów")
    
    
    percent_map(var = data, color = kolor, legend.title = tytul,min=input$zakres[1],max=input$zakres[2])
  })
  
}



shinyApp(ui, server)