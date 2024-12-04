# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 


# Partie 1 ----

solution1 <-
  length(gregexpr("\\(",input)[[1]]) -
  length(gregexpr("\\)",input)[[1]])

# Partie 2 ----

solution2 <-
  strsplit(input,"")[[1]] |> 
  sapply(function(.x){gsub("\\(","1",.x)}) |> 
  sapply(function(.x){gsub("\\)","-1",.x)}) |> 
  as.numeric() |> 
  cumsum() |> 
  (\(.){min(which(. == -1))})()

