
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introdução

Este repositório contém arquivos que conjuntamente produzem um dashboard
através do pacote shiny. O Dashboard contém informações, visualizações e
testes relacionados à informações de filmes lançados entre os anos
1921 - 2024. Dentre as análises há uma aba para análise descritiva e
exploratória, onde pode-se selecionar o continente e pais do filme,
assim como o ano em que foi lançado e , em seguida, a variável a qual se
deseja analisar (indicações a premiações, nota do filme pelo IMDB,
número de votos que resultaram a essa nota e número de prêmios ganhos).
Há também duas abas adicionais com o objetivo de analisar variáveis
influentes já nota do IMBD. Isto é, foram feitas duas análises:
Regressão de nota com outras variáveis (que pode ser escolhida pelo
usuário), para analisar o impacto de uma variável numérica na nota final
dos filmes); e ANOVa de nota com outra variável, para analisar a
diferença de nota entre categorias de variáveis categóricas.

# Conjunto de dados

O conjunto de dados utilizado foi obtido através de webscrapping do site
<https://www.imdb.com/>. O mesmo foi feito por Nathan Augusto Elias
utilizando a ferramenta Webscrapper. Os dados foram coletados em
30/04/2024.

# Arquivos

Ha seguir a uma explicação dos arquivos presentes nesse repositório e
suas respectivas funções:

## Arquivos Principais:

- `app.R`: arquivo que compila os demais e de onde é gerado o Dashboard.
  Nele contém importações e chamadas de pacotes usados na construção do
  dash; a função source() para leitura de arquivos de apoio (com
  codificação para UTF-8).
- `ui_aba1.R`: Contém estrutura do “user interface” para a aba Visão
  Geral: layout, controles de entrada e controles de saída.
- `ui_aba2.R`: Contém estrutura do “user interface” para a aba
  Regressão: layout, controles de entrada e controles de saída.
- `ui_aba3.R`: Contém estrutura do “user interface” para a aba ANOVA:
  layout, controles de entrada e controles de saída.
- `server_aba1.R`: Contém as lógicas das interações com gráficos e
  tabelas da aba 1.
- `server_aba2.R`: Contém as lógicas das interações com gráficos e
  tabelas da aba 2.
- `server_aba3.R`: Contém as lógicas das interações com gráficos e
  tabelas da aba 3.

## Arquivos suporte:

- `dados.csv`: Conjunto de dados iniciais utilizados no dash
- `dados.R`: Nova construção de dados a partir de dados.csv para
  inclusão das informações de continente e outras manipulações
  necessárias.
- Pasta `rsconnsct`: Conexão a partir do pacote rsconnect com o site
  onde o Dashboard pode ser acessado.

# Como acessar o dash

Você pode acessar o Dashboard de duas formas:

1.  Baixando os arquivos desde repositório como um projeto no RStudio e
    rodar o arquivo app.R. não se esqueça de entrar pelo encoding UTF-8
    e , caso não tenha instalado, é necessário instalar os pacotes
    utilizados (listados no início de app.R)

2.  Acessando o link <https://juliavidoto.shinyapps.io/trab4/>.

Para atualizar o dash toda vez que fizer uma alteração no console do R
precisa rodar deployApp() e ele automaticamente atualiza no site
indicado.
