######################################################
#Alexandre Costa / Jesuel
#Email:d2023102963@unifei.edu.br
#      jesuelsd@unifei.edu.br
#PCO207/2024
######################################################

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
library(FactoMineR)
library(factoextra)
library(vcd)
library(mda)

data(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

PCA(X, scale.unit = TRUE, ncp = 5, graph = TRUE)

res.pca <- PCA(decathlon2.active, graph = FALSE)

print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


var <- get_pca_var(res.pca)
var


data(wine)
wine_data <- wine[,c(1,2,13,22,24,28,30)]
head(wine_data)

str(wine_data)



# Ler o arquivo CSV
students_files <- c(
  "UFRN_2018.csv")#,
  "UFRN_2019.csv",
  "UFRN_2020.csv", 
  "UFRN_2021.csv",
  "UFRN_2022.csv",
  "UFRN_2023.csv",
  "UFRN_2024.csv"
)

dados <- read_csv(students_files, id = "file")
#Erro: Files must have consistent column names:  


ler_e_padronizar <- function(file_path, colunas_padrao) {
  dados <- read_delim(file_path, delim = ";", col_names = TRUE, show_col_types = FALSE)
  dados <- dados %>% select(all_of(colunas_padrao))
  return(dados)
  dados <- dados |> na.omit()
}

# Suprimir mensagens de especificação de colunas
#── Column specification ────────────────────────────────────────────────
#Delimiter: ";"
#chr (11): nome_discente, sexo, forma_ingresso, tipo_discente, status...
#dbl  (6): matricula, ano_ingresso, periodo_ingresso, id_curso, id_un...
#
#ℹ Use `spec()` to retrieve the full column specification for this data.
#ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#Rows: 13264 Columns: 18  


# Listar os arquivos e definir a ordem padrão das colunas
arquivos <- list.files(pattern = "*.csv", full.names = TRUE)
#*.CSV
#UFRN_2024.csv

colunas_padrao <- c("matricula", "nome_discente", "sexo", "ano_ingresso", "periodo_ingresso", "forma_ingresso", "tipo_discente", "status", "sigla_nivel_ensino", "nivel_ensino", "id_curso", "nome_curso", "modalidade_educacao", "id_unidade", "nome_unidade", "id_unidade_gestora", "nome_unidade_gestora")

# Ler e combinar os arquivos
dados_combinados <- arquivos %>%
  map_df(~ler_e_padronizar(.x, colunas_padrao))

dados_filtrados1 <- dados_combinados[,c(1, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14)]

head(dados_filtrados1)

dados_ano_ingresso <- dados_combinados %>%
#  count(ano_ingresso) %>%
  count(forma_ingresso)

dados2 <- dados_filtrados1 %>%
  mutate(
    Sexo = ifelse(sexo == "M", 1, 0),
    FI_SiSU = ifelse(forma_ingresso == "SiSU", 1, 0),
    FI_PRO_SEL = ifelse(forma_ingresso == "PROCESSO SELETIVO", 1, 0),
    FI_AL_ESP_POS = ifelse(forma_ingresso == "ALUNO ESPECIAL POS-GRADUACAO", 1, 0),
    #    FI_NA = ifelse(forma_ingresso == "NAO INFORMADO", 1, 0),
    FI_RVR = ifelse(forma_ingresso == "REOCUPAÇÃO DE VAGAS RESIDUAIS", 1, 0),
    FI_VEST = ifelse(forma_ingresso == "VESTIBULAR", 1, 0),
    FI_RE_SEG_CI = ifelse(forma_ingresso == "REINGRESSO SEGUNDO CICLO", 1, 0),
    FI_SEL_POS = ifelse(forma_ingresso == "SELEÇÃO DE PÓS-GRADUAÇÃO", 1, 0),
    #    FI_AL_MOB_INT = ifelse(forma_ingresso == "ALUNO EM MOBILIDADE INTERNACIONAL", 1, 0),
    #    FI_AL_MOB_NAC = ifelse(forma_ingresso == "ALUNO EM MOBILIDADE NACIONAL", 1, 0),
    #    FI_TRAS_COM = ifelse(forma_ingresso == "TRANSF. COMPULSORIA", 1, 0),
    #    FI_COO_INT = ifelse(forma_ingresso == "COOPERAÇÃO INTERNACIONAL", 1, 0),
    #    FI_MOB_INT = ifelse(forma_ingresso == "MOBILIDADE INTERNACIONAL", 1, 0),
    #    FI_NEI = ifelse(forma_ingresso == "NEI", 1, 0),
    #    FI_AL_ESP = ifelse(forma_ingresso == "ALUNO ESPECIAL" , 1, 0),
    #    FI_EST_COMP = ifelse(forma_ingresso == "ESTUDOS COMPLEMENTARES" , 1, 0),
    #    FI_JUD = ifelse(forma_ingresso == "JUDICIAL", 1, 0),
    #    FI_PER_SED = ifelse(forma_ingresso == "PERMUTA DE SEDE", 1, 0),
    #    FI_REI_AUT = ifelse(forma_ingresso == "REINGRESSO AUTOMATICO", 1, 0),
    #    FI_CON_PEC = ifelse(forma_ingresso == "CONVENIO PEC-G", 1, 0),
    FI_PES = ifelse(forma_ingresso == "PES", 1, 0),
    #    FI_TAL_MET = ifelse(forma_ingresso == "TALENTO METRÓPOLE", 1, 0),
    #    FI_TRANSF = ifelse(forma_ingresso == "TRANSFERÊNCIA", 1, 0),
    #    FI_MUD_CUR = ifelse(forma_ingresso == "MUDANÇA DE CURSO", 1, 0),
    FI_PRO_SEL_ESP = ifelse(forma_ingresso == "PROCESSO SELETIVO ESPECÍFICO", 1, 0),
    #    FI_ORD_JUD = ifelse(forma_ingresso == "ORDEM JUDICIAL", 1, 0),
    FI_PRO_SEL_EXT_TEC = ifelse(forma_ingresso == "PROCESSO SELETIVO EXTERNO TÉCNICO", 1, 0),
    #    FI_SORT = ifelse(forma_ingresso == "SORTEIO", 1, 0),
    FI_PROM = ifelse(forma_ingresso == "PROMOVER", 1, 0),
    #    FI_SISU2 = ifelse(forma_ingresso == "SiSU 2a Edição", 1, 0),
    FI_PRO_SEL_EXT_TEC_INT = ifelse(forma_ingresso == "PROCESSO SELETIVO EXTERNO TÉCNICO INTEGRADO", 1, 0),
    FI_EAD_REM_GER = ifelse(forma_ingresso == "EAD REMANESCENTES GERAL", 1, 0),
    FI_EAD_REM_ADM = ifelse(forma_ingresso == "EAD REMANESCENTES ADM", 1, 0),
    #    FI_THE_MUS = ifelse(forma_ingresso == "THE_MUSICA", 1, 0),
    #    FI_DAN_REM = ifelse(forma_ingresso == "DANÇA REMANESCENTES", 1, 0),
    #    FI_THE_DAN = ifelse(forma_ingresso == "THE_DANCA", 1, 0),
    #    FI_PRON = ifelse(forma_ingresso == "PRONERA", 1, 0),
    #    FI_REM_GES_COO = ifelse(forma_ingresso == "REMANESCENTES GESTAO COOPERATIVAS", 1, 0),
    #    FI_TRA_VOL = ifelse(forma_ingresso == "TRANSFERÊNCIA VOLUNTÁRIA" , 1, 0),
    #    FI_REI_ESP = ifelse(forma_ingresso == "REINGRESSO ESPECÍFICO", 1, 0),
    TIPO = ifelse(tipo_discente == "REGULAR", 1, 0),
#    TD_ESP = ifelse(tipo_discente == "ESPECIAL", 1, 0),
    ST_CANCELADO = ifelse(status == "CANCELADO" , 1, 0),
    ST_CONCLUIDO = ifelse(status == "CONCLUÍDO" , 1, 0),
    ST_FORMADO = ifelse(status == "FORMADO" , 1, 0),
    ST_ATIVO = ifelse(status == "ATIVO" , 1, 0),
    ST_DEFENDIDO = ifelse(status == "DEFENDIDO" , 1, 0),
    ST_AT_FORMANDO = ifelse(status == "ATIVO - FORMANDO" , 1, 0),
    ST_TRANCADO = ifelse(status == "TRANCADO" , 1, 0),
    ST_CADASTRADO = ifelse(status == "CADASTRADO" , 1, 0)
  )

dados_filtrados2 <- dados2 [,c(3,4,9,10, 12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)]


str(dados_filtrados2)

# Check for missing values
sum(is.na(dados_filtrados2))

# Remove rows with missing values
dados_filtrados2 <- na.omit(dados_filtrados2)

# Check for infinite values
sum(is.infinite(dados_filtrados2))

# Replace infinite values with NA
dados_filtrados2[is.infinite(dados_filtrados2)] <- NA

# Impute the newly introduced NAs
dados_filtrados2 <- imputePCA(dados_filtrados2)$completeObs

#Standardize Your Data: Sometimes, scaling your data can help in resolving such issues. You can use the scale() function to standardize your dataset.
dados_filtrados2 <- scale(dados_filtrados2)

dados2a <- FAMD(dados_filtrados2, graph=TRUE)

dados2a

dados_combinados_classificacao <- dados_combinados |>
  mutate(across(where(~ !is.numeric(.)) & !starts_with("ocorrencia_classificacao"), ~ as.numeric(factor(.)))) |>
  select(-c(ocorrencia_latitude, ocorrencia_longitude, codigo_ocorrencia,
            codigo_ocorrencia1, codigo_ocorrencia2, codigo_ocorrencia3, codigo_ocorrencia4))



str(dados_combinados)

wine_famd <- FAMD(dados_combinados,
                  graph=FALSE)
wine_famd

fviz_famd_ind(wine_famd,col.ind = "cos2",
              gradient.cols = c("blue", "orange", "red"),
              repel = TRUE)


status <- unique(dados_combinados$status)
print(status)

ingresso <- unique(dados_combinados$forma_ingresso)
print(ingresso)



#top 10 formas de ingresso
top_ingressos <- dados_combinados %>%
  count(forma_ingresso) %>%
  top_n(15, n) %>%
  arrange(desc(n))






dados_num <- dados_combinados %>%
  select_if(is.numeric) %>%
  select('ano_ingresso', 'periodo_ingresso', 'id_unidade_gestora', 'id_unidade', 'id_curso')


#matrix 1
matrix <- cor(dados_num, use = 'complete.obs')

# Definir a diagonal principal como NA
diag(matrix) <- 0

# Ajustar o tamanho da janela gráfica
windows(width = 16, height = 9) # Para Windows

#Gráfico 1
corrplot(matrix, method = 'circle', type = 'lower', tl.cex = 0.9,title = "Matriz de Correlação", addCoef.col = 'black') +
  geom_point() +
  theme_minimal() + 
  theme(legend.position="bottom")




d <- dados2 |>
  select(where(is.numeric))


#matrix 2
matrix2 <- cor(d, use = 'complete.obs')

# Definir a diagonal principal como NA
diag(matrix2) <- 0

# Ajustar o tamanho da janela gráfica
windows(width = 30, height = 20) # Para Windows

#na.omit()



#na.label Label to be used for rendering NA cells. Default is '?'. If ’square’, then the cell
#is rendered as a square with the na.label.col color.




#Dados3
# tipo 1 = Regular / 0 = Especial
dados_status <- dados2 %>%
  filter(!is.na(status))

dados3 <- dados_status %>%
  group_by(ano_ingresso, status) %>%
  summarise(matricula = n(), .groups = 'drop')

dados3$status_num <- as.numeric(as.factor(dados3$status))
correlacao <- cor(dados3$ano_ingresso, dados3$status_num)

dados_correlacao <- dados3 %>%
  group_by(ano_ingresso, status) %>%
  summarise(correlacao = cor(as.numeric(ano_ingresso), as.numeric(as.factor(status)), use = "complete.obs"), .groups = 'drop')



#Dados4
# Converter 'ano_ingresso' e 'modalidade_educacao' para numérico
dados_modalidade <- dados2 %>%
  filter(!is.na(modalidade_educacao)) %>%
  mutate(
    ano_ingresso_num = as.numeric(ano_ingresso),
    modalidade_educacao_num = as.numeric(as.factor(modalidade_educacao))
  )

# Calcular a correlação para cada combinação de 'ano_ingresso' e 'modalidade_educacao'
dados_correlacao <- dados_modalidade %>%
  group_by(ano_ingresso, modalidade_educacao) %>%
  summarise(
    correlacao = ifelse(sd(ano_ingresso_num) == 0 | sd(modalidade_educacao_num) == 0, NA, cor(ano_ingresso_num, modalidade_educacao_num, use = "complete.obs")),
    .groups = 'drop'
  ) 

# Calcular a correlação entre 'ano_ingresso_num' e 'modalidade_educacao_num' 
correlacao <- cor(dados_modalidade$ano_ingresso_num, dados_modalidade$modalidade_educacao_num, use = "complete.obs")  


# Criar um novo dataframe com os valores de correlação
dados_correlacao <- data.frame(
  ano_ingresso = unique(dados_modalidade$ano_ingresso),
  modalidade_educacao = unique(dados_modalidade$modalidade_educacao),
  correlacao = correlacao
)


dados4 <- dados_modalidade %>%
  group_by(ano_ingresso, modalidade_educacao) %>%
  summarise(contagem = n(), .groups = 'drop')




