ui_aba1 <- fluidPage(
  
  titlePanel("Análises de filmes com dados do IMDb"),
  
  fluidRow(
    column(3, radioButtons(
      inputId = "continente",
      label = "Continente",
      choices = c("Todos", unique(dados$Continente))) 
    ),
    column(3, selectInput(
      inputId = "pais",
      label = "Pais",
      choices = unique(dados$Pais))
    ),
    column(3, sliderInput(
      inputId = "ano",
      label = "Ano",
      min = min(dados$Ano), max = max(dados$Ano), step=1, value = 2000, animate=TRUE)
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
      column(12, dataTableOutput(outputId = "tabela_naousada")))
  )
)#acaba o ui
