# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/1/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

input <-
  input |> 
  (\(.){gsub("R","",.)})() |> 
  (\(.){gsub("L","-",.)})() |> 
  (\(.){c(50,as.numeric(.))})() 

x = 50
res = 0
for(i in seq_along(input)[-1]){
  x <- (x + input[i]) %% 100
  if(x == 0){res <- res + 1}
}

solution1 <-
  res

# Partie 2 ----

x = 50
res = 0
for(i in seq_along(test)[-1]){
  res <- res + sum((x:(x+test[i]))[-1] %% 100 == 0)
  x <- (x + test[i]) %% 100
}
  
solution2 <-
  res
