ui_aba3 <- fluidPage(
  fluidRow(
    column(6, selectInput(inputId = "X",
                          label = "Escolha a variável de agrupamento:",
                          choices = c("Classificacao", "Nota_categoria"))),
    column(6, selectInput(
      inputId = "choices",
      label = "Selecione as variáveis que entrarão no modelo da ANOVA:",
      choices = c("Indicacoes", "Lucro", "Ano", "Vitorias", "Custo", "Pais", "Classificacao"),
      multiple = TRUE
    ))),
  fluidRow(
    column(6,  #Boxplot
           plotOutput(outputId = "boxplot")),
    column(6,  #Anova
           tableOutput(outputId = "anova"))
  )
)
