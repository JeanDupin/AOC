# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 


# Partie 1 ----

solution1 <-
  sapply(input, USE.NAMES = F,
         function(.x){
           strsplit(.x," ")[[1]] |> 
             table() |> 
             max() |> 
             (\(.){ifelse(. > 1,F,T)})()
         }) |> 
  sum()


# Partie 2 ----

solution2 <-
  sapply(input, USE.NAMES = F,
         function(.x){
           strsplit(.x," ")[[1]] |> 
             sapply(function(.x){
               strsplit(.x,"")[[1]] |> 
                 sort() |> 
                 paste(collapse = "")
             },
             USE.NAMES = F) |> 
             duplicated() |> 
             (\(.){!any(.)})()
         }) |> 
  sum()

