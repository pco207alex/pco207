######################################################
#Alexandre Costa / Jesuel
#Email:d2023102963@unifei.edu.br
#      jesuelsd@unifei.edu.br
#PCO207/2024
######################################################

# Carregar bibliotecas
library(tidyverse)
library(janitor)
library(palmerpenguins)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(ggcorrplot)
library(GGally)
library(readr)
library(lubridate)
library(corrplot)
library(reshape2)
library(factoextra)

# Definir os arquivos CSV
students_files <- c(
  "UFRN_2018.csv",
  "UFRN_2019.csv",
  "UFRN_2020.csv", 
  "UFRN_2021.csv",
  "UFRN_2022.csv",
  "UFRN_2023.csv",
  "UFRN_2024.csv"
)

# Função para ler e padronizar os dados
ler_e_padronizar <- function(file_path, colunas_padrao) {
  dados <- read_delim(
    file_path,
    delim = ";",
    col_names = TRUE,
    show_col_types = FALSE,
    locale = locale(encoding = "latin1")
  )
  
  # Padronizar nomes das colunas
  dados <- janitor::clean_names(dados)
  
  # Verificar as colunas disponíveis
  colunas_disponiveis <- intersect(colunas_padrao, names(dados))
  
  # Selecionar somente as colunas disponíveis
  dados <- dados %>% select(all_of(colunas_disponiveis))
  
  # Remover linhas com valores ausentes
  dados <- na.omit(dados)
  
  return(dados)
}

# Definir a ordem padrão das colunas
colunas_padrao <- c(
  "matricula", "nome_discente", "sexo", "ano_ingresso", "periodo_ingresso", 
  "forma_ingresso", "tipo_discente", "status", "sigla_nivel_ensino", 
  "nivel_ensino", "id_curso", "nome_curso", "modalidade_educacao", 
  "id_unidade", "nome_unidade", "id_unidade_gestora", "nome_unidade_gestora"
)

# Ler e combinar os arquivos
dados_combinados <- map_df(students_files, ~ler_e_padronizar(.x, colunas_padrao))

# Exibir valores únicos de "status" e "forma_ingresso", se disponíveis
if ("status" %in% names(dados_combinados)) {
  status <- unique(dados_combinados$status)
  print(status)
} else {
  message("A coluna 'status' não está presente nos dados combinados.")
}

if ("forma_ingresso" %in% names(dados_combinados)) {
  ingresso <- unique(dados_combinados$forma_ingresso)
  print(ingresso)
} else {
  message("A coluna 'forma_ingresso' não está presente nos dados combinados.")
}

##################################### a partir daqui começa o PCA

# Carregar as bibliotecas para FAMD
library(FactoMineR)
library(factoextra)

# Certifique-se de que as variáveis qualitativas estão no formato fator
dados_combinados$sexo <- as.factor(dados_combinados$sexo)
dados_combinados$ano_ingresso <- as.factor(dados_combinados$ano_ingresso)
dados_combinados$forma_ingresso <- as.factor(dados_combinados$forma_ingresso)
dados_combinados$status <- as.factor(dados_combinados$status)
dados_combinados$nivel_ensino <- as.factor(dados_combinados$nivel_ensino)

# Selecione as variáveis para o FAMD (mista)
dados_famd <- dados_combinados %>% select(sexo, ano_ingresso, forma_ingresso, status, nivel_ensino)

# Check the dimensions of your data
dim(dados_famd)

# Example of setting dimnames correctly
#dimnames(dados_famd) <- list(row_names, col_names)

# Aplicar FAMD
famd_result <- FAMD(dados_famd, graph = FALSE)

# Visualizar o resumo do FAMD
#summary(famd_result)

# Gráficos:
# Gráfico dos indivíduos
fviz_famd_ind(famd_result, geom = "point", ggtheme = theme_minimal())

# Gráfico das variáveis
fviz_famd_var(famd_result, ggtheme = theme_minimal())
#head(dados_famd)

# Gráfico com os indivíduos coloridos pela variável "status" (exemplo)
dados_combinados$status <- as.factor(dados_combinados$status)
#fviz_famd_ind(famd_result, geom = "point", col.ind = dados_combinados$status, palette = "Set1", addEllipses = TRUE, legend.title = "Status")
fviz_famd_ind(famd_result, geom = "point", habillage = dados_combinados$status, palette = "Set1", addEllipses = TRUE, legend.title = "Status")
length(dados_combinados$status)

nrow(famd_result$ind$coord)

#Dimensões e Variância:
#  
#  As dimensões (Dim.1, Dim.2, etc.) representam os eixos principais que explicam a maior parte da variância nos dados.
#A tabela de eigenvalues mostra a variância explicada por cada dimensão. Dimensões com maior variância explicam mais da estrutura dos dados.
#Indivíduos:
#   
#   Cada ponto no gráfico representa um indivíduo (ou observação) do seu conjunto de dados.
# A proximidade entre pontos indica similaridade. Indivíduos próximos compartilham características semelhantes.
# A cor dos pontos (definida pelo parâmetro habillage) pode representar diferentes grupos ou categorias, como o status dos indivíduos.
# Elipses:
#   
#   As elipses adicionadas ao gráfico representam a dispersão dos grupos. Elas ajudam a visualizar a variabilidade dentro de cada grupo.
# Grupos com elipses que se sobrepõem indicam que os indivíduos desses grupos têm características semelhantes.
# Contribuição das Variáveis:
#   
#   As variáveis contínuas e categóricas contribuem para a formação das dimensões. A contribuição de cada variável pode ser analisada para entender quais variáveis são mais importantes para cada dimensão.
# Variáveis com alta contribuição em uma dimensão específica têm um impacto significativo na separação dos indivíduos ao longo dessa dimensão.
# Cos2 e Contribuição:
#   
#   O valor de cos2 indica a qualidade da representação dos indivíduos nas dimensões. Valores altos de cos2 significam que o indivíduo está bem representado pela dimensão.
# A contribuição (ctr) mostra a importância de cada indivíduo na formação da dimensão. Indivíduos com alta contribuição influenciam fortemente a dimensão.
# Aqui está um exemplo de como interpretar um gráfico FAMD:
#   
#   Dimensões: Se Dim.1 e Dim.2 explicam uma porcentagem significativa da variância, concentre-se nesses eixos para a interpretação principal.
# Indivíduos: Observe a distribuição dos pontos. Indivíduos do mesmo grupo (mesma cor) devem estar próximos se forem semelhantes.
# Elipses: Verifique se as elipses dos grupos se sobrepõem ou estão separadas. Grupos bem separados indicam diferenças claras entre eles.
# Variáveis: Analise quais variáveis contribuem mais para cada dimensão para entender quais características estão dirigindo a separação dos indivíduos.
# 
################################################### fim do PCA


# Visualizar a variância explicada
p0 <- fviz_eig(famd_result, addlabels = TRUE, ylim = c(0, 50))
print(p0)


# Visualizar as variáveis
p1 = fviz_pca_var(famd_result, 
                  col.var = "black", 
                  axes = c(1, 2),
                  select.var = list(contrib = 6), 
                  title = "1a PCA de var")
print(p1)


# Visualizar os indivíduos
p2 = fviz_pca_ind(famd_result, 
                  col.var = "blue", 
                  axes = c(1, 2),
                  title = "1a PCA de ind")
print(p2)

p2 = fviz_famd(famd_result, 
               col.ind = "blue", 
               axes = c(1, 2),
               title = "1a PCA de ind")
print(p2)


# Visualizar o biplot para FAMD
p3 = fviz_famd(famd_result, 
               axes = c(1, 2), 
               title = "1a FAMD biplot")
print(p3)
#inspecionar o objeto para teste
# Verifique a estrutura do objeto famd_result
str(famd_result)

#mapa de fatores do indi
fviz_famd_ind(famd_result,col.ind = "cos2",
              gradient.cols = c("blue", "orange", "red"),
              repel = TRUE)



