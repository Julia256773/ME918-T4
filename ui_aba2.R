ui_aba2 <- fluidPage(
  titlePanel("Análises inferenciais de filmes com dados do IMDb"),
  titlePanel("Análise Inferencial 1"),
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
  ),
  titlePanel("Análise Inferencial 2"),
  fluidRow(
    column(3,selectInput(inputId = "X",
                         label = "Escolha a variável de agrupamento:",
                         choices = c("Classificacao", "Nota_categoria")),
           selectInput(
             inputId = "choices",
             label = "Selecione as variáveis que entrarão no modelo da ANOVA:",
             choices = c("Indicacoes", "Lucro", "Ano", "Vitorias", "Custo", "Pais", "Classificacao"),
             multiple = TRUE
           )),
    column(9, fluidRow(
      column(6,  #Boxplto
             plotOutput(outputId = "boxplot")),
      column(6,  #Anova
             tableOutput(outputId = "anova"))
    ))
  )
)
