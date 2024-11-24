
dados = read_csv("dados.csv", show_col_types = FALSE)

colnames(dados) = c("Nome", "Classificacao", "Pais", "Ano", "Lucro", "Custo", 
                    "Vitorias", "Indicacoes", "Nota", "Votos", "Nota_categoria")


dados = mutate(dados, Continente = case_when(
  Pais %in% c("Alemanha", "França", "Dinamarca", "Irlanda", 
              "Itália", "Polônia", "Reino Unido", "Suécia") ~ "Europa",
  Pais %in% c("Argentina", "Brasil") ~ "América do Sul",
  Pais %in% c("Austrália", "Nova Zelândia") ~ "Oceania",
  Pais %in% c("Canadá", "EUA", "México") ~ "América do Norte",
  Pais %in% c("Coreia do Sul", "Índia", "Irã", "Japão", "Líbano", "Turquia") ~ "Ásia",
  TRUE ~ "Desconhecido"
))

