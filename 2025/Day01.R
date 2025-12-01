# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/1/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

input <-
  input |> 
  (\(.){gsub("R","",.)})() |> 
  (\(.){gsub("L","-",.)})() |> 
  (\(.){c(as.numeric(.))})() 

x = 50
res = 0
for(i in seq_along(input)){
  x <- (x + input[i]) %% 100
  if(x == 0){res <- res + 1}
}

solution1 <-
  res

# Partie 2 ----

x = 50
res = 0
for(i in seq_along(input)){
  res <- res + sum((x:(x+input[i]))[-1] %% 100 == 0)
  x <- (x + input[i]) %% 100
}
  
solution2 <-
  res
