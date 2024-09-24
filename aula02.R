######################################################
#Alexandre Costa
#Email:d2023102963@unifei.edu.br
#PCO207/2024
######################################################

library(tidyverse)
library(janitor)
library(palmerpenguins)
library(ggthemes)


x <- read_csv("data\\file.csv")

x <- read_csv(file.path("data","file.csv"))

students <- read_csv("https://pos.it/r4ds-students-csv",
                     na = c("N/A",""))


students <- students |> rename(student_id = 'Student ID',
                          full_name = 'Full Name')


students <- rename(students, student_id = 'Student ID',
                             full_name = 'Full Name')
                                                         

w <- x |> f(y,z) |> g(t)
w <- f(x,y,z)
r <- g(w,t)


students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan))


students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )


students |> write_csv(teste.csv)

x <- read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2)



penguins
view(penguins)
glimpse(penguins)


penguins |> map_dfc(\(x) sum(is.na(x)))


p <- penguins |> 
     ggplot(aes(x = flipper_length_mm,
                y = body_mass_g,
                color = species)) +
     geom_point() +
     theme_bw() +
     theme(legend.position = "bottom")
print(p)


# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)

# Ler o arquivo CSV
dados <- read.csv("UFRN_2021_2024.csv", sep = ";")
#dados <- read.csv("UFRN_2021_2024.csv", sep = ";", fileEncoding = "UTF-8")


# Filtrar dados por ano
dados <- dados %>%
  filter(ano_ingresso %in% "2021")

# Contar o número de alunos por ano de ingresso
dados_ano_ingresso <- dados %>%
  count(ano_ingresso)

# Preparar os dados para o gráfico de pizza
dados_genero_ano <- dados %>%
  count(ano_ingresso, sexo) %>%
  group_by(ano_ingresso) %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(ano_ingresso, " - ", sexo, " (", n, ")"))

# Contar o número de alunos por curso e selecionar os 10 maiores
top_cursos <- dados %>%
  count(nome_curso) %>%
  top_n(10, n) %>%
  arrange(desc(n))

# Contar o número de alunos por unidade e selecionar as 10 maiores
top_unidades <- dados %>%
  count(nome_unidade) %>%
  top_n(10, n) %>%
  arrange(desc(n))

# Filtrar os dados para incluir apenas os 10 maiores cursos
dados_filtrados <- dados %>%
  filter(nome_curso %in% top_cursos$nome_curso)

# Filtrar os dados para incluir apenas as 10 maiores unidades
dados_filtrados_unidade <- dados %>%
  filter(nome_unidade %in% top_unidades$nome_unidade)

# Recontar o número de alunos por unidade nos dados filtrados
#dados_filtrados <- dados_filtrados %>%
#  count(nome_unidade)

# Recontar o número de alunos por unidade nos dados filtrados
#dados_filtrados_unidade <- dados_filtrados %>%
#  count(nome_unidade, sort = TRUE)

# Gráfico de barras para distribuição de gênero
ggplot(dados, aes(x = sexo)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição de Gênero  2021 - 2024", x = "Gênero", y = "Contagem") +
  theme_minimal()



# Criar o gráfico de pizza
ggplot(dados_genero, aes(x = "", y = perc, fill = label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribuição de Gênero 2021 - 2024", x = NULL, y = NULL, fill = "Gênero") +
  theme_void() +
  theme(legend.position = "right")


# Criar o gráfico de pizza separado por ano de ingresso
ggplot(dados_genero_ano, aes(x = "", y = perc, fill = label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribuição de Gênero por Ano de Ingresso", x = NULL, y = NULL, fill = "Gênero") +
  theme_void() +
  theme(legend.position = "right") +
  facet_wrap(~ ano_ingresso)


# Gráfico de barras para ano de ingresso
ggplot(dados, aes(x = ano_ingresso)) +
  geom_bar(fill = "lightgreen") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Ano de Ingresso dos Alunos", x = "Ano de Ingresso", y = "Contagem") +
  theme_minimal()





# Criar o gráfico de barras para ano de ingresso com estilo profissional
ggplot(dados_ano_ingresso, aes(x = factor(ano_ingresso), y = n, fill = factor(ano_ingresso))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribuição de Alunos por Ano de Ingresso",
       x = "Ano de Ingresso",
       y = "Número de Alunos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  ) +
  scale_fill_brewer(palette = "Set3")



# Criar o gráfico de dispersão usando o nome do curso
#ggplot(dados_filtrados, aes(x = ano_ingresso, y = nome_curso, color = sexo)) +
#  geom_point(alpha = 0.7, size = 4) +
#  scale_color_brewer(palette = "Set1") +
#  labs(title = "Distribuição de Alunos pelos 10 Maiores Cursos por Ano de Ingresso",
#       subtitle = "Dados de 2021 a 2024",
#       x = "Ano de Ingresso",
##       y = "Nome do Curso",
#       color = "Gênero") +
#  theme_minimal() +
#  theme(
##    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "darkblue"),
#    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "darkblue"),
#    axis.title.x = element_text(size = 14, face = "bold", color = "darkblue"),
#    axis.title.y = element_text(size = 14, face = "bold", color = "darkblue"),
#    axis.text.x = element_text(size = 12, color = "darkblue"),
#    axis.text.y = element_text(size = 12, color = "darkblue"),
#    legend.title = element_text(size = 14, face = "bold", color = "darkblue"),
#    legend.text = element_text(size = 12, color = "darkblue"),
#    panel.grid.major = element_line(color = "grey80"),
#    panel.grid.minor = element_blank()
#  ) +
#  guides(color = guide_legend(override.aes = list(size = 5)))



# Gráfico de barras para status dos alunos
ggplot(dados, aes(x = status)) +
  geom_bar(fill = "coral") +
#  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Status dos Alunos", x = "Status", y = "Contagem") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black", size = 3.5) +
    theme_minimal()



# Gráfico de barras para distribuição por curso
ggplot(dados, aes(x = nome_curso)) +
  geom_bar(fill = "purple") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição de Alunos por Curso", x = "Curso", y = "Contagem")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Criar o gráfico de barras para os 10 maiores cursos
ggplot(dados_filtrados, aes(x = reorder(nome_curso, -table(nome_curso)[nome_curso]))) +
  geom_bar(fill = "purple") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  labs(title = "Distribuição de Alunos pelos 10 Maiores Cursos", x = "Curso", y = "Contagem", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, color = "black")




# Gráfico de barras para distribuição por unidade
ggplot(dados_filtrados_unidade, aes(x = nome_unidade)) +
  geom_bar(fill = "orange") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuição de Alunos por Unidade", x = "Unidade", y = "Contagem") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# com erro
# Criar o gráfico de barras para as 10 maiores unidades
ggplot(dados, aes(x = reorder(nome_unidade, -n), y = n, fill = nome_unidade)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Distribuição de Alunos pelas 10 Maiores Unidades",
       x = "Nome da Unidade",
       y = "Número de Alunos",
       fill = "Unidade") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "darkblue"),
    axis.title.x = element_text(size = 14, face = "bold", color = "darkblue"),
    axis.title.y = element_text(size = 14, face = "bold", color = "darkblue"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = "darkblue"),
    axis.text.y = element_text(size = 12, color = "darkblue"),
    legend.title = element_text(size = 14, face = "bold", color = "darkblue"),
    legend.text = element_text(size = 12, color = "darkblue"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3")
