# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

crabes <-
  strsplit(input,",")[[1]] |> 
  as.numeric()

fuel <-
  vector("numeric",length(min(crabes):max(crabes)))
for(i in min(crabes):max(crabes)){
  
  fuel[i+1] <-
    abs(crabes-i) |> 
    sum()
}

solution1 <-
  min(fuel)

# Partie 2 ----

fuel <-
  vector("numeric",length(min(crabes):max(crabes)))

for(i in min(crabes):max(crabes)){
  
  fuel[i+1] <-
    abs(crabes-i) |> 
    lapply(function(.x){sum(1:.x)}) |> 
    unlist() |> 
    sum()
}

solution2 <-
  min(fuel)
