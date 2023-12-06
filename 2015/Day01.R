# Input ----

input <-
  readLines("2015/Inputs/Day01.txt")


# Partie 1 ----

solution1 <-
  length(gregexpr("\\(",input)[[1]]) -
  length(gregexpr("\\)",input)[[1]])

# Partie 2 ----

solution1 <-
  strsplit(input,"")[[1]] |> 
  sapply(function(.x){gsub("\\(","1",.x)}) |> 
  sapply(function(.x){gsub("\\)","-1",.x)}) |> 
  as.numeric() |> 
  cumsum() |> 
  (\(.){min(which(. == -1))})()
