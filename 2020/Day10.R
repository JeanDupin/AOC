# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

solution1 <-
  sort(c(0,input,max(input)+3)) |> 
  diff() |> 
  table() |> 
  prod()

# Partie 2 ----

input <-
  sort(c(0,input,max(input)+3))

sortie <-
  as.list(rep(0,max(input)+1))

sortie[[1]] = 1

for(i in 2:length(input)){
  etape = input[i]
  
  if(etape == 1){
    sortie[[etape+1]] = sortie[[etape]]
  } else if(etape == 2){
    sortie[[etape+1]] = sortie[[etape]] + sortie[[etape-1]]
  } else {
    sortie[[etape+1]] = sortie[[etape]] + sortie[[etape-1]] + sortie[[etape-2]] 
  }
  
}

solution2 <-
  sortie[[length(sortie)]]
