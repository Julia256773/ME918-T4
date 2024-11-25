server_aba3 <- function(input, output, session) {
  output$boxplot = renderPlot({
    dados %>% 
      ggplot(aes_string(x = "Nota", y = input$X, fill=input$X)) + 
      geom_boxplot() + 
      theme_bw()+
      scale_fill_manual(values = brewer.pal(n = length(unique(dados[[input$X]])), "Blues"))+
      theme(legend.position = "none")
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