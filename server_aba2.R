server_aba2 <- function(input, output, session) {
  # Gráfico de dispersão
  output$grafico <- renderPlotly({
    ggplot(dados, aes_string(x = "Nota", y = input$variavel)) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "red", se = F) +  # Linha de regressão
      theme_bw() + 
      labs(y = "Nota Média", x = input$variavel)  # Rótulos dos eixos
  })
  
  # Tabela da regressão
  output$tabela <- renderReactable({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    coef <- round(summary(modelo)$coefficients,5)
    
    reactable(coef,
              columns = list(
                Estimate = colDef(name = "Coeficiente"),
                `Std. Error` = colDef(name = "Erro Padrão"),
                `t value` = colDef(name = "Valor t"),
                `Pr(>|t|)` = colDef(name = "p-valor")
              ),
              defaultColDef = colDef(
                align = "right",
                style = list(fontSize = "12px")
              ),
              pagination = TRUE,  # Habilita paginação
              showSortable = TRUE,  # Habilita ordenação
              highlight = TRUE,  # Destaca a linha ao passar o mouse
              compact = FALSE  # Tabela mais compacta
    )
  })
  
  # Gráfico Histograma
  output$residuos <- renderPlotly({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    residuos <- modelo$residuals
    p <- ggplot(data = data.frame(residuos), aes(x = residuos)) + 
      geom_histogram(binwidth = 5, fill = "darkcyan", color = "black", alpha = 0.7) +
      labs(x = "Resíduos", y = "Frequência") +
      theme_bw()
    ggplotly(p)
  })
  
  # Gráfico de Homocedasticidade
  output$residuos2 <- renderPlotly({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    residuos <- modelo$residuals
    valores_ajustados <- modelo$fitted.values
    p <- ggplot(data = data.frame(valores_ajustados, residuos), aes(x = valores_ajustados, y = residuos)) +
      geom_point(color = "black") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Valores Ajustados", y = "Resíduos") +
      theme_bw()
    
    ggplotly(p)
  })
  
  # Gráfico de QQ-Plot
  output$residuos3 <- renderPlotly({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    residuos <- modelo$residuals
    
    qq <- ggplot(data = data.frame(residuos), aes(sample = residuos)) +
      stat_qq() +
      stat_qq_line(color = "red") +  # Adiciona a linha de referência
      labs(x = "Quantis Teóricos", y = "Quantis Observados") +
      theme_bw()
    
    ggplotly(qq)
  })
  
  # Tabela testes de residuos
  output$testes <- renderReactable({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    residuos <- modelo$residuals
    shapiro <- shapiro.test(residuos)
    bp <- bptest(modelo)
    
    resultados <- data.frame(
      Teste = c("Shapiro-Wilk", "Breusch-Pagan"),
      Estatistica = c(round(shapiro$statistic,4), round(bp$statistic,4)),
      P_valor = c(round(shapiro$p.value,4), round(bp$p.value,4)))
    
    reactable(resultados, 
              columns = list(
                Teste = colDef(name = "Teste de Hipótese"),
                Estatistica = colDef(name = "Estatística"),
                P_valor = colDef(name = "P-valor")
              ),
              searchable = FALSE,
              compact = FALSE)
  })
  # Texto tabela regressão
  output$texto1 <- renderText({
    modelo <- lm(as.formula(paste("Nota", "~", input$variavel)), data = dados)
    p_valor <- round(summary(modelo)$coefficients,5)[2,4]
    
    ifelse(p_valor < 0.05, 
           paste0("Com base no p-valor obtido ", p_valor, ",rejeitamos a hipótese nula de que o 
     coeficiente da regressão é igual a zero, 
     indicando que a variável explicativa tem um 
      efeito significativo sobre a variável dependente."),
           paste0("Com base no p-valor obtido ", p_valor, ",não rejeitamos a hipótese nula de que o 
     coeficiente da regressão é igual a zero, 
     indicando que a variável explicativa não tem um 
      efeito significativo sobre a variável dependente."))
  })
  
  output$boxplot = renderPlot({
    dados %>% 
      ggplot(aes_string(x = "Nota", fill = input$X)) + 
      geom_boxplot() + 
      theme_bw()
  })
  
  output$anova = renderTable({
    if (length(input$choices) > 0) {
      variaveis = paste(input$choices, collapse = " + ")
      formula = as.formula(paste("Nota ~", variaveis))
      
      anova = aov(formula, data = dados)
      resumo = summary(anova)
      
      tabela_anova = as.data.frame(resumo[[1]])  
      tabela_anova$Termos = rownames(tabela_anova)  
      rownames(tabela_anova) = NULL  
      
      
      tabela_anova = tabela_anova[, c("Termos", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
      colnames(tabela_anova) = c("Termos", "GL", "Soma Quadrados", "Média Quadrados", "F", "P-valor")
      
      tabela_anova } else { "Nenhuma variável selecionada para o modelo ANOVA."}
  })
  
}