# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/4/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  matrix(
   unlist(strsplit(input,"")),
   nrow = length(input),
   byrow = T
  )

get_voisins <- function(mat, i, j){
  idx <- expand.grid(i+(-1:1), j+(-1:1))
  idx <- idx[!(idx[,1] == i & idx[,2] == j), ]
  
  ok <-
    idx[,1] >= 1 & idx[,1] <= nrow(mat) &
    idx[,2] >= 1 & idx[,2] <= ncol(mat)
  
  mat[as.matrix(idx[ok, ])]
}

solution1 <- 
  apply(which(matrice == "@", arr.ind = TRUE), 1, function(.x){
    sum(get_voisins(matrice, .x[1], .x[2]) == "@") < 4
  }) |> 
  sum()

# Partie 2 ----

res <- 0
coordonnees <- vector("list")

repeat{
  for(i in seq_len(nrow(matrice))){
    for(j in seq_len(ncol(matrice))){
      if(matrice[i,j] != "@"){next}
      if(sum(get_voisins(matrice,i,j) == "@") >= 4){next}
      coordonnees <- 
        append(coordonnees, list(c(i,j)))
      res <- res + 1
    }
  }
  if(length(coordonnees) == 0){break}
  matrice[do.call(rbind,coordonnees)] <- "."
  coordonnees <- vector("list")
}

solution2 <-
  res