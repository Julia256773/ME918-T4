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
#library(car)
library(maps)
library(rsconnect)
library(RColorBrewer)

source("dados.R", encoding = "UTF-8")

source("ui_aba1.R", encoding = "UTF-8")
source("server_aba1.R", encoding = "UTF-8")  
source("ui_aba2.R", encoding = "UTF-8") 
source("server_aba2.R", encoding = "UTF-8")
source("ui_aba3.R", encoding = "UTF-8") 
source("server_aba3.R", encoding = "UTF-8")



# UI principal
ui <- fluidPage(
  titlePanel("Análise de Filmes com Dados do IMDb"),
  tabsetPanel(
    tabPanel("Visão Geral", ui_aba1),  
    tabPanel("Regressão", ui_aba2),
    tabPanel("ANOVA", ui_aba3)
  )
)

# Server principal
server <- function(input, output, session) {
  server_aba1(input, output, session)
  server_aba2(input, output, session)
  server_aba3(input, output, session)
}

# Executa o app
shinyApp(ui = ui, server = server)