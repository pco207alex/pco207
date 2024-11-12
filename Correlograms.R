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


# Ler o arquivo CSV
students_files <- c(
        "UFRN_2018.csv",
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

colunas_padrao <- c("matricula", "nome_discente", "sexo", "ano_ingresso", "periodo_ingresso", "forma_ingresso", "tipo_discente", "status", "sigla_nivel_ensino", "nivel_ensino", "id_curso", "nome_curso", "modalidade_educacao", "id_unidade", "nome_unidade", "id_unidade_gestora", "nome_unidade_gestora")

# Ler e combinar os arquivos
dados_combinados <- arquivos %>%
  map_df(~ler_e_padronizar(.x, colunas_padrao))

status <- unique(dados_combinados$status)
print(status)

ingresso <- unique(dados_combinados$forma_ingresso)
print(ingresso)



#top 10 formas de ingresso
top_ingressos <- dados_combinados %>%
  count(forma_ingresso) %>%
  top_n(15, n) %>%
  arrange(desc(n))




dados2 <- dados_combinados %>%
#  count(matricula) %>%
  mutate(
    Sex_Fem = ifelse(sexo == "F", 1, 0),
    Sex_Mas = ifelse(sexo == "M", 1, 0),
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
    TD_REG = ifelse(tipo_discente == "REGULAR", 1, 0),
    TD_ESP = ifelse(tipo_discente == "ESPECIAL", 1, 0),
    ST_CANCELADO = ifelse(status == "CANCELADO" , 1, 0),
    ST_CONCLUIDO = ifelse(status == "CONCLUÍDO" , 1, 0),
    ST_FORMADO = ifelse(status == "FORMADO" , 1, 0),
    ST_ATIVO = ifelse(status == "ATIVO" , 1, 0),
    ST_DEFENDIDO = ifelse(status == "DEFENDIDO" , 1, 0),
    ST_AT_FORMANDO = ifelse(status == "ATIVO - FORMANDO" , 1, 0),
    ST_TRANCADO = ifelse(status == "TRANCADO" , 1, 0),
    ST_CADASTRADO = ifelse(status == "CADASTRADO" , 1, 0)
#    FI_ = ifelse(sexo == , 1, 0),
#    FI_ = ifelse(sexo == , 1, 0),
#    FI_ = ifelse(sexo == , 1, 0),
#    FI_ = ifelse(sexo == , 1, 0),
)

dados_num <- dados_combinados %>%
  select_if(is.numeric) %>%
  select('ano_ingresso', 'periodo_ingresso', 'id_unidade_gestora', 'id_unidade', 'id_curso')


#matrix 1
matrix <- cor(dados_num, use = 'complete.obs')

# Definir a diagonal principal como NA
diag(matrix) <- 0

# Ajustar o tamanho da janela gráfica
windows(width = 16, height = 9) # Para Windows

#Gráfico
corrplot(matrix, method = 'circle', type = 'lower', tl.cex = 0.9,title = "Matriz de Correlação") +
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

#Gráfico 2
corrplot(matrix2, method = 'circle', type = 'lower', title = "Matriz de Correlação 2", na.label = ".") +
  geom_point() +
  theme_minimal() + 
  theme(legend.position="bottom")

#na.label Label to be used for rendering NA cells. Default is '?'. If ’square’, then the cell
#is rendered as a square with the na.label.col color.




#Dados3
# tipo 1 = Regular / 0 = Especial
dados_status <- dados2 %>%
  filter(!is.na(status))

dados3 <- dados_status %>%
  group_by(ano_ingresso, status) %>%
  summarise(matricula = n(), .groups = 'drop')


#matrix 3
# Status dos alunos por ano
ggplot(dados3, aes(x = ano_ingresso, y = status, fill = matricula)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(dados3$matricula), space = "Lab", name="Correlação") +
#  theme_minimal() +
  labs(title = "Mapa de Calor das Correlações", x = "Ano", y = "Status") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
#  scale_x_discrete(guide = guide_axis(check.overlap = false))




#Dados4
dados_modalidade <- dados2 %>%
  filter(!is.na(modalidade_educacao))

dados4 <- dados_modalidade %>%
  group_by(ano_ingresso, modalidade_educacao) %>%
  summarise(matricula = n(), .groups = 'drop')


#matrix 4
#Modalidade Educacao dos alunos por ano
ggplot(dados4, aes(x = ano_ingresso, y = modalidade_educacao, fill = matricula)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(dados3$matricula), space = "Lab", name="Correlação") +
  #  theme_minimal() +
  labs(title = "Mapa de Calor das Correlações", x = "Ano", y = "Modalidade") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
#  scale_x_discrete(guide = guide_axis(check.overlap = false))


#Grafico 5
# Gerar o gráfico de correlograma
ggpairs(dados_num) +
  theme_minimal() +
  labs(title = "Correlograma dos Dados Combinados")


