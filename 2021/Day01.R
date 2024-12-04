# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

solution1 <-
  sum(diff(input) > 0)

# Partie 2 ----

solution2 <-
  lapply(1:(length(input) - 2),
         function(.x){
           sum(input[.x:(.x + 2)])
         }) |> 
  unlist() |> 
  (\(.){sum(diff(.) > 0)})()
