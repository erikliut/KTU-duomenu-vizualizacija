#4uzd
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)

duom <- read_csv("C:/Users/eriliu/Desktop/Svarbus failai/Univeras/6 semestras/Programavimas duomenu tvarkymui ir vizualizavimui/2 lab/KTU-duomenu-vizualizacija/laboratorinis/data/lab_sodra.csv")
duom <- duom[duom$ecoActCode == "452000",]

web <- fluidPage(
  titlePanel("Krovinio gabenimo agentu ir ekspeditoriu veikla"),
  sidebarLayout(
    sidebarPanel(
      selectInput("imoniukodai", label = "Iveskite imones koda", choices = unique(duom$code)),
    ),
    mainPanel(
      plotOutput("atlyginimai"),
      tableOutput("table")
    )
  ))

server <- function(input, output, session){
  dmn <- reactive({
    req(input$imoniukodai)
    df <- duom %>% filter(code %in% input$imoniukodai)
  })
  
  output$atlyginimai <- renderPlot({
    g <- ggplot(dmn(), aes(x = month, y = avgWage, group = name, color = name))
    g + geom_line()
  })
  
  output$table <- renderTable(dmn())
}

shinyApp(web, server)
