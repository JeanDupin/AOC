# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/12/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input),byrow = T)


get_adjacent_values <- function(row, col) {
  
  
  # Initialisez une liste pour stocker les valeurs adjacentes
  adjacent_values <- list()
  
  # Vérifiez les cases adjacentes
  if (row > 1) {
    adjacent_values$haut <- matrice[row - 1, col]
  }
  if (row < nrow(matrice)) {
    adjacent_values$bas <- matrice[row + 1, col]
  }
  if (col > 1) {
    adjacent_values$gauche <- matrice[row, col - 1]
  }
  if (col < ncol(matrice)) {
    adjacent_values$droite <- matrice[row, col + 1]
  }
  
  unlist(adjacent_values)
}


get_adjacent_indices <- function(row, col) {
  
  # Initialisez une liste pour stocker les indices adjacents
  adjacent_indices <- list()
  
  # Vérifiez les cases adjacentes
  if (row > 1) {
    adjacent_indices$haut <- c(row - 1, col)
  }
  if (row < nrow(matrice)) {
    adjacent_indices$bas <- c(row + 1, col)
  }
  if (col > 1) {
    adjacent_indices$gauche <- c(row, col - 1)
  }
  if (col < ncol(matrice)) {
    adjacent_indices$droite <- c(row, col + 1)
  }
  
  adjacent_indices |> 
    lapply(paste,collapse=";") |> 
    unlist()
}


memoire = vector("list")
for(i in seq_len(nrow(matrice))){
  for(j in seq_len(ncol(matrice))){
    lettre = matrice[i,j]
    if(is.null(memoire[[lettre]])){
      memoire[[lettre]][["A"]][["aire"]] = 1
      memoire[[lettre]][["A"]][["perim"]] = (4-sum(get_adjacent_values(i,j)==lettre))
    } else {
      
      
      
      memoire[[lettre]][["aire"]] = memoire[[lettre]][["aire"]] + 1
      memoire[[lettre]][["perim"]] = memoire[[lettre]][["perim"]] + (4-sum(get_adjacent_values(i,j)==lettre))
      
      
    }
    
  }
}

mapply(
  function(.x,.y){.x*.y},
  memoire,
  perimetre
)


solution1 <-
  NA

# Partie 2 ----

solution2 <-
  NA