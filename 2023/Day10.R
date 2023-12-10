# Input ----

input <-
  readLines("2023/Inputs/Day10.txt")


# Partie 1 ----

n.col = nchar(input[1])
n.row = length(input)

# Étape 1 - Détection de la position de départ

depart <-
  gregexpr("S",input) |> 
    unlist() |> 
    (\(.){
      paste(
        # Colonne
        .[. != -1],
        # Ligne
        which(. != -1),
        sep = ";"
      )
    })()


# Étape 2 - On cherche une des tuiles adjecentes reliée
## Haut
if(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]] != 1){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) - 1)]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]])]
  if(lettre %in% c("|","7","F")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]),
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) - 1,
            sep = ";")
  }
}
## Bas
if(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]] != n.row){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) + 1)]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]])]
  if(lettre %in% c("|","L","J")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]),
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) + 1,
            sep = ";")
    
  }
}
## Droite
if(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]] != n.col){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]))]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) + 1]
  if(lettre %in% c("-","J","7")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) + 1,
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]),
            sep = ";")
  }
}
## Gauche
if(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]] != 1){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]))]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) - 1]
  if(lettre %in% c("-","F","L")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) - 1,
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]),
            sep = ";")
    
  }
}; rm(lettre)


positions <- c(depart, position)
# Étape 3 - Tant qu'on ne revinet pas au départ, on fait le tour et on compte le nombre d'étapes

next_moves <- function(.l,.x,.y){
  if(.l == "|"){
    return(
      paste(.x,c(.y-1,.y+1),sep=";")
      )
  }
  if(.l == "-"){
    return(
      paste(c(.x-1,.x+1),.y,sep=";")
    )
  }
  if(.l == "L"){
    return(
      paste(c(.x,.x+1),c(.y-1,.y),sep=";")
    )
  }
  if(.l == "J"){
    return(
      paste(c(.x,.x-1),c(.y-1,.y),sep=";")
    )
  }
  if(.l == "7"){
    return(
      paste(c(.x,.x-1),c(.y+1,.y),sep=";")
    )
  }
  if(.l == "F"){
    return(
      paste(c(.x,.x+1),c(.y+1,.y),sep=";")
    )
  }
}
`%notin%` <- Negate(`%in%`)
while(length(position) != 0){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(position,gregexpr("[0-9]+$",position))[[1]]))]],"")[[1]][as.numeric(regmatches(position,gregexpr("^[0-9]+",position))[[1]])]
  id.col = as.numeric(regmatches(position,gregexpr("^[0-9]+",position))[[1]])
  id.row = as.numeric(regmatches(position,gregexpr("[0-9]+$",position))[[1]])
  
  next.moves <-
    next_moves(lettre,id.col,id.row)
  
  position <-
    next.moves[which(next.moves %notin% positions)]
  
  positions <- append(positions,
                      position)
  
}; rm(lettre, id.col, id.row,
      next.moves, position)

solution1 <-
  length(positions)/2





