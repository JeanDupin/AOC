# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/10/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

retrouver_adjacentes <-
  function(ligne, colonne, valeur) {
    
    n_lignes = nrow(matrice)
    n_colonnes = ncol(matrice)
    
    voisins <- 
      list(
        c(ligne - 1, colonne), 
        c(ligne + 1, colonne), 
        c(ligne, colonne - 1), 
        c(ligne, colonne + 1)  
      )
    
    voisins_valides <-
      lapply(voisins, function(coord) {
        if (coord[1] >= 1 && coord[1] <= n_lignes && coord[2] >= 1 && coord[2] <= n_colonnes) {
          coord
        } else {
          NULL
        }
      })
    
    voisins_valides <-
      Filter(Negate(is.null), voisins_valides)
    
    voisins_valide_valeur <-
      lapply(voisins_valides, function(coord) {
        if (matrice[coord[1], coord[2]] == valeur) {
          coord
        } else {
          NULL
        }
      })
    
    voisins_valide_valeur <-
      Filter(Negate(is.null), voisins_valide_valeur)
    
    do.call(rbind, voisins_valide_valeur)
  }

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  as.numeric() |> 
  matrix(nrow = length(input),byrow = T)

coords <-
  which(matrice == 9,arr.ind = T) |> 
  apply(1,function(.x){paste(.x,collapse = ";")})

res = vector("character")
for(j in seq_along(coords)){
  coordonnee = coords[j]
  for(i in 8:0){
    coordonnee <-
      lapply(coordonnee,
             function(.x){
               X = strsplit(.x,";")[[1]] |> as.numeric()
               temp = retrouver_adjacentes(X[1],X[2],i)
               if(is.null(temp)){
                 "0;0"
               } else {
                 temp |> 
                   apply(1,function(.y){paste(.y,collapse = ";")}) 
               }
               
             }) |> 
      unlist() |> 
      (\(.){.[. != "0;0"]})()
  }
  res = append(res,unique(coordonnee))
}

solution1 <-
  table(res) |> 
  sum()

# Partie 2 ----

for(i in 8:0){
  coords <-
    lapply(coords,
           function(.x){
             X = strsplit(.x,";")[[1]] |> as.numeric()
             temp = retrouver_adjacentes(X[1],X[2],i)
             if(is.null(temp)){
               "0;0"
             } else {
               temp |> 
                 apply(1,function(.y){paste(.y,collapse = ";")}) 
             }
             
           }) |> 
    unlist() |> 
    (\(.){.[. != "0;0"]})()
}

solution2 <-
  length(coords)
