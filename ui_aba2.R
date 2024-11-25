ui_aba2 <- fluidPage(
  fluidRow(
    column(6,selectInput(inputId = "variavel",
                         label = "Variáveis",
                         choices = colnames(dados[, 5:8])))
  ),
  
  fluidRow(
    column(7, #Gráfico de dispersão
           plotlyOutput(outputId = "grafico")),  
    column(5, # Tabela da regressão
           reactableOutput(outputId = "tabela")),
    column(5, # Texto tabela regressão
           textOutput(outputId = "texto1")),
    column(5, # Tabela testes de residuos
           reactableOutput(outputId = "testes"))
  ),
  
  fluidRow(
    column(4, # Gráfico Histograma
           plotlyOutput(outputId = "residuos")),
    column(4, # Gráfico de Homocedasticidade
           plotlyOutput(outputId = "residuos2")),
    column(4, # Gráfico de QQ-Plot
           plotlyOutput(outputId = "residuos3"))
  )
)
