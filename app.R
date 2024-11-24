library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)
#library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)
library(reactable)
library(readr)
library(lmtest)
library(car)

source("dados.R", encoding = "UTF-8")
# Carregar as partes das abas
source("ui_aba1.R", encoding = "UTF-8")
source("server_aba1.R", encoding = "UTF-8")  
source("ui_aba2.R", encoding = "UTF-8") 
source("server_aba2.R", encoding = "UTF-8") 

# UI principal
ui <- fluidPage(
  titlePanel("Dashboard com Múltiplas Abas"),
  tabsetPanel(
    tabPanel("Aba 1", ui_aba1),  
    tabPanel("Aba 2", ui_aba2) 
  )
)

# Server principal
server <- function(input, output, session) {
  server_aba1(input, output, session)
  server_aba2(input, output, session)
}

# Executa o app
shinyApp(ui = ui, server = server)