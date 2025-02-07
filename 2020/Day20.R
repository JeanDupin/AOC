# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/20/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrices <-
  lapply(which(grepl("Tile",input)), function(.x){
    input[(.x+1):(.x+10)] |> 
      strsplit("") |> 
      (\(.){matrix(unlist(.),nrow = 10, byrow = T)})()
  })

extract_sides <- function(.matrice){
  list(
    .matrice[1,],
    rev(.matrice[1,]),
    .matrice[nrow(.matrice),],
    rev(.matrice[nrow(.matrice),]),
    .matrice[,1],
    rev(.matrice[,1]),
    .matrice[,ncol(.matrice)],
    rev(.matrice[,ncol(.matrice)])
  ) |> 
    lapply(function(.x){paste(.x,collapse="")}) |> 
    unlist(recursive = F)
}

ids <-
  input[grepl("Tile",input)] |> 
  (\(.){gsub(":","",.)})()

res <- vector("list")
for(i in seq_along(matrices)){
  for(j in seq_along(matrices)){
    if(i == j){next}
    if(any(extract_sides(matrices[[i]]) %in% extract_sides(matrices[[j]]))){
      res <- append(res,ids[i])
    }
  }
}

solution1 <-
  table(unlist(res)) |> 
  (\(.){names(.)[. == 2]})() |> 
  (\(.){as.numeric(gsub("Tile ","",.))})() |> 
  prod()

# Partie 2 ----

solution2 <-
  NA