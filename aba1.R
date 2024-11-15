
library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)

dados = read_csv("dados.csv")
colnames(dados) = c("nome", "classificacao", "pais", "ano", "lucro", "custo", "vitorias", "indicacoes", "nota", "total_votos", "nota_categoria")

ui <- fluidPage(

    titlePanel("Graficozinhos sobre filmes muito bons"),
    
    fluidRow(
      column(3, radioButtons(
                          inputId = "radio",
                          label = "Radio",
                          choices = c("Todos","América do Norte","América do Sul/Central", "Europa", "Ásia"))
      ),
      column(3, selectInput(
                          inputId = "pais",
                          label = "País",
                          choices = unique(dados$pais))
      ),
      column(3, dateRangeInput(
                          inputId = "ano",
                          label = "Ano",
                          start = min(dados$ano), end = max(dados$ano))
      ),
      column(3, selectInput(
                          inputId = "variavel",
                          label = "Variáveis",
                          choices = colnames(dados[,5:10]))
      )
    ), #acaba primeira linha
    fluidRow(
      column(4, plotlyOutput(outputId = "histograma")),
      column(4, plotlyOutput(outputId = "serie_temporal")),
      column(4, plotlyOutput(outputId = "barra")
    ), #acaba segunda linha
    fluidRow(
      column(8, plotlyOutput(outputId = "mapa")),
      column(4, plotlyOutput(outputId = "barra_deitado"))
    ), #acaba terceira inha
    fluidRow(
      column(12, dataTableOutput(outputId = "tabela")))
    )
)#acaba o ui



server <- function(input, output, session) {
  output$histograma = renderPlotly({
    dados %>% 
      filter(pais == input$pais) %>%
      ggplot()+
      geom_histogram(aes(x = .data[[input$variavel]]))+
      labs(y = "")+
      theme_bw()+
      labs(title = paste("Distribuição de", input$variavel, "dos filmes"))
    })
  
  output$serie_temporal = renderPlotly({
    dados %>% 
      filter(pais == input$pais) %>%
      group_by(ano) %>% 
      summarise(lucro = sum(lucro),
                custo = sum(custo),
                vitorias = sum(vitorias),
                indicacoes = sum(indicacoes),
                nota= mean(nota),
                total_votos = sum(total_votos)) %>% 
      ggplot()+
      geom_line(aes(x=ano, y= .data[[input$variavel]]))+
      theme_bw()+
      labs(title = ifelse(input$variavel=="nota", 
                          "Média de nota por ano", 
                          paste("Total de", input$variavel, "por ano")))
    })
  
  output$mapa = renderPlotly({
    iris %>% 
      ggplot(aes(x = Sepal.Length, y=Sepal.Width, fill=Species))+
      geom_point()+
      labs(title = "Mapa da Vane")
    })
  
  output$barra_deitado = renderPlotly({
    dados %>% 
      group_by(pais) %>% 
      summarise(lucro = sum(lucro),
                custo = sum(custo),
                vitorias = sum(vitorias),
                indicacoes = sum(indicacoes),
                nota= mean(nota),
                total_votos = sum(total_votos)) %>%
      ggplot()+
      geom_bar(
        aes(x = fct_reorder(pais, .data[[input$variavel]], .desc = FALSE), 
            y = .data[[input$variavel]]),
        stat = "identity")+
      labs(x = "País", y = input$variavel)+
      coord_flip()+
      theme_bw()
    })
  
  output$tabela = renderDataTable({
    dados %>% 
      filter(pais == input$pais) %>% 
      select(nome, lucro, custo, vitorias, indicacoes, nota, total_votos)
  },
    options = list(scrollX = TRUE)  # rolagem horizontal
  )
  
  output$barra =  renderPlotly({
    dados %>% 
      filter(pais == input$pais) %>%
      group_by(classificacao) %>% 
      summarise(n = n()) %>% 
      ggplot()+
      geom_bar(aes(x = classificacao, y=n), stat="identity")+
      theme_bw()+
      labs(title = "Quantidade de filmes por classificação")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
