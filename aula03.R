library(nycflights13)
library(tidyverse)
library(plyr)
library(pryr)

m <- flights |>
      filter(dest == "IAH") |> 
      group_by(year, month, day) |> 
      summarize(
        arr_delay = mean(arr_delay, na.rm = TRUE)
      )

?flights

flights

#p <- m |> ggplot(aes(x = 1:nrow(m), y = arr_delay)) +
p <- m |> ggplot(aes(x = month, y = arr_delay)) +
     geom_line() + 
     theme_minimal()
print(p)




m <- flights |>
  arrange(desc(dep_delay))


m <-  flights |> 
      mutate(
        gain = dep_delay - arr_delay,
        speed = distance / air_time * 60,
        .before = 1
      )



m <-  flights |> 
      mutate(
        gain = dep_delay - arr_delay,
        hours = air_time / 60,
        gain_per_hour = gain / hours,
        .keep = "used"
  )

#seleciona somente as colunas ano, mes e dia
m <-  flights |> 
      select(year, month, day)

#todos as colunas exceto as do ano até o dia
m <-  flights |> 
  select(!year: day)


#somente colunas caracteres
m <-  flights |> 
      select(where(is.character))


#somente colunas que começam com DEP
m <-  flights |> 
  select(starts_with("dep"))


#renomeia o nome da coluna e seleciona os dados desta coluna
m <-  flights |> 
  rename(tail_num = tailnum) |>
  #new = old
  select(tail_num)


# aloca as colunas informadas no inicio da exibição dos dados
m <-  flights |> 
      relocate(time_hour, air_time)


m <-  flights |> 
      filter(dest == "IAH") |> 
      mutate(speed = distance / air_time * 60) |> 
      select(year:day, dep_time, carrier, flight, speed) |> 
      arrange(desc(speed))

#prepara os dados , deve utilizar o summarize para visualizar
m <-  flights |> 
      group_by(month)


#utilizando o summarize para agrupar
m <-  flights |> 
      group_by(month) |> 
      summarize(
      avg_delay = mean(dep_delay)
      )


mx <- m|> slice_head(n=5)


mx <- m|> slice_min(avg_delay, n = 3)



m <-  flights |> 
        group_by(dest) |> 
        slice_max(arr_delay, n = 1) |>
        relocate(dest, arr_delay)



m <-  billboard |> 
      pivot_longer(
        cols = starts_with("wk"), 
        names_to = "week", 
        values_to = "rank",
        values_drop_na = TRUE
      ) |>
      mutate(
        week = parse_number(week) #considera somente numero e descarta os demais caracteres
      )


p <-  m |> filter(artist == "Bon Jovi") |>
      ggplot(aes(x = week, y = rank)) +
      geom_line() +
      ylim(0,105) + #define escala do eixo Y
      theme_minimal()

print(p)




p <-  m |> filter(artist == "Aguilera, Christina") |>
      ggplot(aes(x = week, y = rank, group = track)) + 
      geom_line(alpha = 0.25) + 
      scale_y_reverse() +
      theme_minimal()

print(p)


m <-  who2 |> 
      pivot_longer(
        cols = !(country:year),
        names_to = c("diagnosis", "gender", "age"), 
        names_sep = "_",
        values_to = "count"
      ) |>
      mutate(age_new = case_when(
              age == "014" ~ "0-14",
              age == "1524" ~ "15-24",
              age == "2534" ~ "25-34",
              age == "3544" ~ "35-44",
              age == "4554" ~ "45-54",
              age == "5564" ~ "55-64",
              .default  = "65-999"
            ),
            .after = age,
          ) |>
      separate(age_new, c("age1","age2"), remove = F )



m <-  m |> mutate(gender2 = as.factor(gender)) |>
      mutate(gender2 = revalue(gender, c("m" = "Male", "f" = "Female"))) |>
      relocate(gender2 .after = gender)
  
  
  
  #      mutate(age1 = unlist(strsplit(age_new,"-"))[1]),
  #          age2 = unlist(strsplit(age_new,"-"))[2]),
  








print(address(x))


y <- function(x){
  x <<- 4
  return(address(x))
}
print(y(x))



7.4 Reading data from multiple files
https://r4ds.hadley.nz/data-import#sec-readr-directory

sales_files <- c("data/01-sales.csv", "data/02-sales.csv", "data/03-sales.csv")
read_csv(sales_files, id = "file")
#> # A tibble: 19 × 6
#>   file              month    year brand  item     n
#>   <chr>             <chr>   <dbl> <dbl> <dbl> <dbl>
#> 1 data/01-sales.csv January  2019     1  1234     3
#> 2 data/01-sales.csv January  2019     1  8721     9
#> 3 data/01-sales.csv January  2019     1  1822     2
#> 4 data/01-sales.csv January  2019     2  3333     1
#> 5 data/01-sales.csv January  2019     2  2156     9
#> 6 data/01-sales.csv January  2019     2  3987     6
#> # ℹ 13 more rows
#> 
#> 
#> 
#> fazer teste com multiplos arquivos
#> 