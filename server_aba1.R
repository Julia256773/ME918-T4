
server_aba1 = function(input, output, session) {
  
  dados_filtrado = reactive({ 
    if(input$continente == "Todos"){dados}
    if(input$continente != "Todos"){dados %>% filter(Continente == input$continente)}
  })
  observeEvent(dados_filtrado(), {
    choices = unique(dados_filtrado()$Pais)
    updateSelectInput(inputId = "pais", choices = choices)
  })
  
  output$histograma = renderPlotly({
    dados %>% 
      filter(Pais == input$pais) %>%
      ggplot()+
      geom_histogram(aes(x = .data[[input$variavel]]))+
      labs(y = "")+
      theme_bw()+
      labs(title = paste("Distribuição de", input$variavel, "dos filmes"))
  })
  
  output$serie_temporal = renderPlotly({
    dados %>% 
      filter(Pais == input$pais) %>%
      group_by(Ano) %>% 
      summarise(Lucro = sum(Lucro),
                Custo = sum(Custo),
                Vitorias = sum(Vitorias),
                Indicacoes = sum(Indicacoes),
                Nota= mean(Nota),
                Votos = sum(Votos)) %>% 
      ggplot()+
      geom_line(aes(x=Ano, y= .data[[input$variavel]]))+
      theme_bw()+
      labs(title = ifelse(input$variavel=="Nota", 
                          "Média de nota por ano", 
                          paste("Total de", input$variavel, "por ano")))
  })
  
  output$mapa = renderPlotly({
    if(input$continente == "Todos"){filtro = dados}
    if(input$continente != "Todos"){filtro = dados %>% filter(Continente == input$continente)}
    filtro %>% 
      group_by(Pais, region) %>% 
      summarise(Lucro = sum(Lucro),
                Custo = sum(Custo),
                Vitorias = sum(Vitorias),
                Indicacoes = sum(Indicacoes),
                Nota= mean(Nota),
                .groups = "drop") %>% 
      merge(world, by = "region", all.x=TRUE) %>% 
      arrange(group, order) %>% 
      ggplot(aes(x = long, y = lat, group = group, fill = .data[[input$variavel]])) +
      geom_polygon(color = "black", size = 0.2) +
      scale_fill_viridis_c(option = "plasma", name = input$variavel) +
      theme_bw() +
      labs(title = "Mapa Temático por Continente",
           fill = "",
           x = "",
           y = "") +
      theme(axis.text = element_blank(),  
            axis.ticks = element_blank(),
            panel.grid = element_blank()) 
  })
  
  output$barra_deitado = renderPlotly({
    dados %>% 
      group_by(Pais) %>% 
      summarise(Lucro = sum(Lucro),
                Custo = sum(Custo),
                Vitorias = sum(Vitorias),
                Indicacoes = sum(Indicacoes),
                Nota= mean(Nota),
                Votos = sum(Votos)) %>%
      ggplot()+
      geom_bar(
        aes(x = fct_reorder(Pais, .data[[input$variavel]], .desc = FALSE), 
            y = .data[[input$variavel]]),
        stat = "identity")+
      labs(x = "País", y = input$variavel)+
      coord_flip()+
      theme_bw()
  })
  
  output$tabela_naousada = renderDataTable({
    dados %>% 
      filter(Pais == input$pais) %>% 
      select(Nome, Lucro, Custo, Vitorias, Indicacoes, Nota, Votos)
  },
  options = list(scrollX = TRUE)  # rolagem horizontal
  )
  
  output$barra =  renderPlotly({
    dados %>% 
      filter(Pais == input$pais) %>%
      group_by(Classificacao) %>% 
      summarise(n = n()) %>% 
      ggplot()+
      geom_bar(aes(x = Classificacao, y=n), stat="identity")+
      theme_bw()+
      labs(title = "Quantidade de filmes por classificação")
  })
}