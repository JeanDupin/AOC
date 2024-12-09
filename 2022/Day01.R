# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  paste(input, collapse = "-") |> 
  (\(.){strsplit(.,"--")[[1]]})() |> 
  lapply(function(.x){sum(as.numeric(strsplit(.x,"-")[[1]]))}) |> 
  unlist() |> 
  max()

# Partie 2 ----

solution2 <-
  paste(input, collapse = "-") |> 
  (\(.){strsplit(.,"--")[[1]]})() |> 
  lapply(function(.x){sum(as.numeric(strsplit(.x,"-")[[1]]))}) |> 
  unlist() |> 
  sort(decreasing = T) |> 
  (\(.){sum(.[1:3])})()
