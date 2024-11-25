
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

dados <- mutate(dados, region = case_when(
  Pais == "Alemanha" ~ "Germany",
  Pais == "França" ~ "France",
  Pais == "Dinamarca" ~ "Denmark",
  Pais == "Irlanda" ~ "Ireland",
  Pais == "Itália" ~ "Italy",
  Pais == "Polônia" ~ "Poland",
  Pais == "Reino Unido" ~ "United Kingdom",
  Pais == "Suécia" ~ "Sweden",
  Pais == "Argentina" ~ "Argentina",
  Pais == "Brasil" ~ "Brazil",
  Pais == "Austrália" ~ "Australia",
  Pais == "Nova Zelândia" ~ "New Zealand",
  Pais == "Canadá" ~ "Canada",
  Pais == "EUA" ~ "USA",
  Pais == "México" ~ "Mexico",
  Pais == "Coreia do Sul" ~ "South Korea",
  Pais == "Índia" ~ "India",
  Pais == "Irã" ~ "Iran",
  Pais == "Japão" ~ "Japan",
  Pais == "Líbano" ~ "Lebanon",
  Pais == "Turquia" ~ "Turkey",
  TRUE ~ "Unknown"
))

world <- map_data("world")
dados_mapa <- merge(world, dados, by = "region", all.x = TRUE)

