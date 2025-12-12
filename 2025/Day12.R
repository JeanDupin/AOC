# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/12/input") |>
  (\(.){strsplit(.,"\\n\\n")[[1]]})()

# Partie 1 ----

matrices <-
  input[grepl("^\\d:",input)] |> 
  (\(.){gsub("(^\\d:)|\\n","",.)})() |> 
  sapply(function(.x){
    sum(strsplit(.x,"")[[1]] == "#")
  },
  USE.NAMES = F)

instructions <-
  input[!grepl("^\\d:",input)]|>
  (\(.){strsplit(.,"\\n")[[1]]})()

fit <- function(.x){
  taille <-
    gsub(":.*","",.x) |> 
    (\(.){strsplit(.,"x")[[1]]})() |> 
    as.numeric() |> 
    prod()
  
  area <-
    gsub(".*:\\s","",.x) |> 
    (\(.){strsplit(.,"\\s+")[[1]]})() |> 
    as.numeric() |> 
    (\(.){.*matrices})() |> 
    sum()
  
  ifelse(area <= taille, T, F)
  
}

solution1 <-
  sapply(instructions, fit,
         USE.NAMES = F, simplify = T) |> 
  sum()
