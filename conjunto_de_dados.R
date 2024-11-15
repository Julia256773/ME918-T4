
########### ME714 ############
#### CÓDIGO DO TRABALHO FINAL #####
######### GRUPO: João Pedro de Campos Formigari RA: 236144
####Nathan Augusto Elias RA: 236258
#####Rita Braga Soares da Silva RA: 251627




library(tidyverse)
library(patchwork)
library(lmtest)
library(corrplot)


 
#Limpando os dados
dados <- read.csv("filmes.csv")
indicacao <- read.csv("vitorias_indicacoes.csv",sep = ";")
dados_ruim <- read.csv("piores_filmes.csv")

dados$nome[170] <- "Os Suspeitos "
indicacao$nome[93] <- "Os Suspeitos "
dados$nome <- str_trim(dados$nome, "left")

dados <- dados %>% left_join(indicacao, by="nome")

colnames(dados_ruim) <- c("nome","país","ano","class","custo","lucro","vitórias",
                          "indicações","nota_media","total_votos")

dados_n <- dados[,-9] %>%  group_by(nome, class, país, ano, lucro,
                                    custo, vitórias,indicações) %>% 
  summarise(nota_media = sum(per_votos*nota/100),total_votos = sum(votos)) %>% ungroup()

dados_n <- bind_rows(dados_n, dados_ruim)
dados_n <- dados_n %>% 
  mutate(país = if_else(país == 'Estados Unidos da América', 'EUA', país), 
         país = if_else(país == 'Alemanha Ocidental', 'Alemanha', país),
         class = if_else(class == "(Banned)", "Banido", class),
         class = if_else(class == "Approved", "10", class),
         class = if_else(class == "Not Rated", "Não Classificado", class),
         class = if_else(class == "PG", "Livre",class),
         class = if_else(class == "R", "Livre",class),
         class = if_else(class == "PG-13", "14",class))
 
 
dados_n <- dados_n %>%
  mutate(filme_categoria = if_else(nota_media < 5, "Piores", "Melhores"))
 


#Análise Descritiva

 
# Filtrar para "filme ruim"
dados_filme_ruim <- dados_n %>% filter(filme_categoria == "Piores")

# Filtrar para "filme bom"
dados_filme_bom <- dados_n %>% filter(filme_categoria == "Melhores")
 

 
summary(dados_n[5:10])
 


 
dados_n %>%
  ggplot(aes(x = total_votos / 1000, fill = filme_categoria)) +
  geom_histogram(color = "white", bins = 80, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_continuous(n.breaks = 20) +
  labs(x = "Total de Votos (Milhares)", y = "Frequência") +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  theme_classic() +
  facet_wrap(~ filme_categoria, ncol = 1)
 


 
dados_n %>%
  ggplot(aes(x = filme_categoria, y = total_votos / 1000, fill = filme_categoria)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1, show.legend = FALSE) +
  labs(x = "Categoria", y = "Total de Votos (Milhares)") +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  theme_classic()
 

# Histograma para filme RUIM e filme BOM
 
# Criar o histograma para "filme ruim"
histograma_filme_ruim <- ggplot(dados_filme_ruim, aes(x = nota_media)) +
  geom_histogram(fill = "indianred", color = "white", bins = 14) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Nota Média", y = "Frequência", title = "Piores Filmes") +
  theme_classic()

# Criar o histograma para "filme bom"
histograma_filme_bom <- ggplot(dados_filme_bom, aes(x = nota_media)) +
  geom_histogram(fill = "darkcyan", color = "white", bins = 14) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Nota Média", y = "Frequência", title = "Melhores Filmes") +
  theme_classic()

# Combinar os dois gráficos em uma única imagem
combined_plot <- histograma_filme_ruim + histograma_filme_bom +
  plot_layout(ncol = 1)

# Mostrar o gráfico combinado
print(combined_plot)
 


 
### Histograma pra o número de vitórias


histograma_ruim_vitoria <- ggplot(dados_filme_ruim, aes(x = vitórias)) +
  geom_histogram(fill = "indianred", color = "white", bins = 20) +
  labs(x = "Número de Prêmios", y = "Frequência", title = "Piores Filmes") +
  theme_classic()

# Criar o histograma para "filme bom"
histograma_bom_vitoria <- ggplot(dados_filme_bom, aes(x = vitórias)) +
  geom_histogram(fill = "darkcyan", color = "white", bins = 20) +
  labs(x = "Número de Prêmios", y = "Frequência", title = "Melhores Filmes") +
  theme_classic()

# Combinar os dois gráficos em uma única imagem
combined_plot2 <- histograma_ruim_vitoria / histograma_bom_vitoria

# Mostrar o gráfico combinado
print(combined_plot2)
 

 
### Histograma pra o número de indicações


histograma_ruim_indicacao <- ggplot(dados_filme_ruim, aes(x = indicações)) +
  geom_histogram(fill = "indianred", color = "white", bins = 20) +
  labs(x = "Número de Indicações", y = "Frequência", title = "Piores Filmes") +
  theme_classic()

# Criar o histograma para "filme bom"
histograma_bom_indicacao <- ggplot(dados_filme_bom, aes(x = indicações)) +
  geom_histogram(fill = "darkcyan", color = "white", bins = 20) +
  labs(x = "Número de Indicações", y = "Frequência", title = "Melhores Filmes") +
  theme_classic()

# Combinar os dois gráficos em uma única imagem
combined_plot3 <- histograma_ruim_indicacao / histograma_bom_indicacao

# Mostrar o gráfico combinado
print(combined_plot3)
 


 
# Criar o boxplot para vitórias por categoria
ggplot(dados_n, aes(x = filme_categoria, y = vitórias, fill = filme_categoria)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1, show.legend = FALSE) +
  labs(x = "Categoria", y = "Número de Premiações/Vitórias")+
  theme_classic() +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan"))
 


 
# Calcular a média de vitórias por categoria
media_vitorias <- dados_n %>%
  group_by(filme_categoria) %>%
  summarise(media_vitorias = mean(vitórias, na.rm = TRUE))

# Mostrar a média de vitórias por categoria
print(media_vitorias)
 


 
### Anos
dados_n_anos <- dados_n %>%
  mutate(decade = case_when(
    ano >= 1920 & ano < 1930 ~ "1920-1930",
    ano >= 1930 & ano < 1940 ~ "1930-1940",
    ano >= 1940 & ano < 1950 ~ "1940-1950",
    ano >= 1950 & ano < 1960 ~ "1950-1960",
    ano >= 1960 & ano < 1970 ~ "1960-1970",
    ano >= 1970 & ano < 1980 ~ "1970-1980",
    ano >= 1980 & ano < 1990 ~ "1980-1990",
    ano >= 1990 & ano < 2000 ~ "1990-2000",
    ano >= 2000 & ano < 2010 ~ "2000-2010",
    ano >= 2010 & ano < 2020 ~ "2010-2020",
    ano >= 2020 & ano <= 2024 ~ "2020-2024",
    TRUE ~ NA_character_
  ))

# Remover linhas com NA na coluna decade
dados_n_anos <- dados_n_anos %>% filter(!is.na(decade))

# Agrupar os dados e calcular a contagem de filmes por categoria e por intervalo personalizado
dados_agrupados <- dados_n_anos %>%
  group_by(decade, filme_categoria) %>%
  summarise(contagem = n()) %>%
  ungroup()

# Criar o gráfico de barras empilhadas
ggplot(dados_agrupados, aes(x = decade, y = contagem, fill = filme_categoria)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  labs(x = "Anos", y = "Número de Filmes", fill = "Categoria") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 


 
#### País
# Agrupar os dados e calcular a contagem de filmes por país e categoria
dados_agrupados2 <- dados_n %>%
  group_by(país, filme_categoria) %>%
  summarise(contagem = n()) %>%
  ungroup()

# Calcular a contagem total de filmes por país para ordenar
dados_ordenados <- dados_agrupados2 %>%
  group_by(país) %>%
  summarise(total = sum(contagem)) %>%
  ungroup() %>%
  arrange(desc(total))

# Reordenar o fator país com base na contagem total
dados_agrupados2 <- dados_agrupados2 %>%
  mutate(país = factor(país, levels = dados_ordenados$país))

# Criar o gráfico de barras horizontais empilhadas em ordem decrescente
ggplot(dados_agrupados2, aes(x = país, y = contagem, fill = filme_categoria)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  labs(x = "País", y = "Contagem de Filmes",  fill = "Categoria") +
  theme_classic() +
  coord_flip()
 

 
dados_agrupados3 <- dados_n %>%
  group_by(class, filme_categoria) %>%
  summarise(contagem = n()) %>%
  ungroup()

# Reordenar o fator país com base na contagem total
dados_agrupados3 <- dados_agrupados3 %>%
  mutate(class = factor(class, levels = c("18", "16", "14", "12", "10", "Livre", "Não Classificado", "Banido")))

ggplot(dados_agrupados3, aes(x = class, y = contagem, fill = filme_categoria)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  labs(x = "Classificação Indicativa", y = "Contagem de Filmes", fill = "Categoria") +
  theme_classic() +
  coord_flip()
 
 
dados_n %>% 
  ggplot(aes(x = custo, y = lucro,  colour = filme_categoria)) +
  geom_point()+
  scale_colour_manual(values = c("Piores" = "indianred", "Melhores" = "darkcyan")) +
  labs(x = "Custo", y = "Receita", colour = "Categoria") +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgreen")
 

################ ######################## ##############
################# Inferencial #######################

# Correlação entre variáveis contínuas


# Selecionar as variáveis contínuas usando base R
dados_continuas <- dados_n[, c("lucro", "custo", "vitórias", "indicações", "nota_media", "total_votos")]

# Calcular a correlação entre as variáveis contínuas
cor(dados_continuas)

# Calcular a matriz de correlação
cor_matrix <- cor(dados_continuas)

# Visualizar a matriz de correlação com corrplot
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         diag = FALSE, title = "Matriz de Correlação", mar = c(0,0,1,0))

####### Teste Qui-Quadrado ###########


# Teste Qui-Quadrado para associação entre filme_categoria e país
chisq_test_pais <- chisq.test(table(dados_n$país, dados_n$filme_categoria))
print(chisq_test_pais)

# Teste Qui-Quadrado para associação entre filme_categoria e class
chisq_test_class <- chisq.test(table(dados_n$class, dados_n$filme_categoria))
print(chisq_test_class)


################ Lucro ##############

# Calcular os quartis da variável custo
quartis_lucro <- quantile(dados_n$lucro, probs = c(0, 0.25, 0.75, 1), na.rm = TRUE)

# Verificar os quartis
print(quartis_lucro)

# Criar uma nova coluna de categoria para custo
dados_n_lucro <- dados_n %>%
  mutate(categoria_lucro = cut(lucro,
                               breaks = quartis_lucro,
                               labels = c("baixo", "médio", "alto"),
                               include.lowest = TRUE))

# Verificar o dataframe atualizado
head(dados_n_lucro)

# Verificar a distribuição das categorias de custo
table(dados_n_lucro$categoria_lucro)



# Criar uma tabela de contingência entre categoria_custo e filme_categoria
tabela_lucro_filme <- table(dados_n_lucro$categoria_lucro, dados_n_lucro$filme_categoria)

# Verificar a tabela de contingência
print(tabela_lucro_filme)

# Realizar o teste Qui-Quadrado de Independência
chisq_test_lucro_filme <- chisq.test(tabela_lucro_filme)

# Verificar as frequências esperadas
print(chisq_test_lucro_filme$expected)

# Resultado do Teste Qui-Quadrado
print(chisq_test_lucro_filme)


#######################################################
##################  MODELAGEM  #######################

# Transformar a variável resposta em fator
dados_n$filme_categoria <- as.factor(dados_n$filme_categoria)

# Dividir os dados em treinamento (70%) e teste (30%)
set.seed(251627)
train_indices <- sample(seq_len(nrow(dados_n)), size = 0.7 * nrow(dados_n))
train_data <- dados_n[train_indices, ]
test_data <- dados_n[-train_indices, ]

# Ajustar o modelo logístico
modelo_logistico <- glm(filme_categoria ~ indicações + ano + lucro, 
                        data = train_data, family = binomial(link="logit"))


## AusÛncia de outliers/ pontos de alavancagem

plot(modelo_logistico, which = 5)

## AusÛncia de multicolinearidade

library(car)
library(xtable)
as.table(vif(modelo_logistico)) %>% xtable()

### Multicolinearidade: VIF > 10


## RelaþÒo linear entre cada VI contÝnua e o logito da VD


### InteraþÒo entre a VI contÝnua e o seu log nÒo significativa (Box-Tidwell)

intlog <- train_data$lucro * log(train_data$lucro)

train_data$intlog <- intlog

modint <- glm(filme_categoria ~ indicações + ano + lucro + intlog, 
              data = train_data, family = binomial(link="logit"))

summary(modint)

# Modelo com todos os parametros
modelo_completo <- glm(filme_categoria ~ indicações + lucro + ano +
                         vitórias + custo + país + class, data = train_data, family = binomial(link="logit"))


## Analise do modelo

## Overall effects

Anova(modelo_logistico, type = 'II', test = "Wald") %>% xtable()


## Efeitos especificos

summary(modelo_logistico)


## ObtenþÒo das raizes de chance com IC 95% (usando log-likelihood)

exp(cbind(OR = coef(modelo_logistico), confint(modelo_logistico)))


## ObtenþÒo das raz§es de chance com IC 95% (usando erro padrÒo = SPSS)

exp(cbind(OR = coef(modelo_logistico), confint.default(modelo_logistico)))


# Comparacao de modelos
## AIC e BIC
AIC(modelo_logistico, modelo_completo)
BIC(modelo_logistico, modelo_completo)


anova(modelo_completo, modelo_logistico, test="Chisq")

lrtest(modelo_logistico, modelo_completo) %>% xtable()


# Fazer previsões nos dados de teste
previsoes <- predict(modelo_logistico, newdata = test_data, type = "response")

# Converter previsões para classes (0 ou 1) usando um limiar de 0.5
classe_prevista <- ifelse(previsoes > 0.5, 1, 0)

# Calcular acurácia do modelo
acuracia <- mean(classe_prevista == test_data$filme_categoria)
print(paste("Acurácia do modelo:", acuracia))

# Matriz de confusão
table(test_data$filme_categoria, classe_prevista) %>% xtable()

# Calcular outras métricas de avaliação (sensibilidade, especificidade, etc.)
library(caret)

confusionMatrix(as.factor(classe_prevista), as.factor(test_data$filme_categoria)) 



