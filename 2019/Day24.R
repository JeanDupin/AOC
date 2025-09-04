# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/24/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  matrix(
    unlist(strsplit(input,"")),
    nrow = length(input),
    byrow = T
  )

voisins <- function(x, y) {
  # Dimensions de la matrice
  n_rows <- nrow(matrice)
  n_cols <- ncol(matrice)
  
  # Initialiser une liste pour stocker les voisins
  resultat <- vector("character",4)
  
  # Haut
  if (x > 1) {
    resultat[1] <- matrice[x - 1, y]
  } else {
    resultat[1] <- NA
  }
  
  # Bas
  if (x < n_rows) {
    resultat[2] <- matrice[x + 1, y]
  } else {
    resultat[2] <- NA
  }
  
  # Gauche
  if (y > 1) {
    resultat[3] <- matrice[x, y - 1]
  } else {
    resultat[3] <- NA
  }
  
  # Droite
  if (y < n_cols) {
    resultat[4] <- matrice[x, y + 1]
  } else {
    resultat[4] <- NA
  }
  
  return(resultat[!is.na(resultat)])
}

sauvegarde <- vector("character", 10000)
k = 1


while(k <= 10000){
  next_matrice <- matrice
  for(i in seq_along(input)){
    for(j in seq_along(input)){
      case <- matrice[i,j]
      neigh <- voisins(i,j)
      if(case == "#" & sum(neigh == "#") != 1){
        case <- "."
      } else if(case == "." & sum(neigh == "#") %in% c(1,2)){
        case <- "#"
      }
      next_matrice[i,j] <- case
    }
  }
  matrice <- next_matrice
  marque <- digest::digest(matrice)
  if(marque %in% sauvegarde){
    break
  } else {
    sauvegarde[k] <- marque
  }
  k = k + 1
}


solution1 <-
  sum(2^(which(t(matrice) == "#")-1))

# Partie 2 ----

solution2 <-
  NA
