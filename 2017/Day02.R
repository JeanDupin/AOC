# Input ----

input <-
  readLines("2017/Inputs/Day02.txt") |> 
  (\(.){strsplit(.,"\\t")})()

# Partie 1 ----

solution1 <-
  sapply(input, USE.NAMES = F, simplify = T,
         function(.x){
           as.numeric(.x) |> 
             (\(.){max(.) - min(.)})()
         }) |> 
  sum()

# Partie 2 ----

solution2 <-
  sapply(input, USE.NAMES = F, simplify = T,
         function(.x){
           expand.grid(
             as.numeric(.x),
             as.numeric(.x)
           ) |> 
             (\(.){.[.$Var1 != .$Var2,]})() |> 
             (\(.){.[(.$Var1 %% .$Var2 == 0),]})() |> 
             (\(.){.$Var1 / .$Var2})()
         }) |> 
  sum()


