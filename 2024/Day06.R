# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  matrix(unlist(strsplit(input,"")),
         nrow = length(input), byrow = T)

move <-
  function(direction){
    switch(direction,
           "haut" = c(-1,0),
           "bas" = c(1,0),
           "droite" = c(0,1),
           "gauche" = c(0,-1))
  }

turn <-
  function(direction){
    switch(direction,
           "haut" = "droite",
           "bas" = "gauche",
           "droite" = "bas",
           "gauche" = "haut")
  }

position <-
  which(matrice == "^", arr.ind = T) |>
  as.vector()

direction = "haut"

visites = list()


while(position[1] != 1 & position[1] != nrow(matrice) &
      position[2] != 1 & position[2] != ncol(matrice)){
  next_position = position+move(direction)

  if(matrice[next_position[1],next_position[2]] == "#"){
    visites[[length(visites) + 1]] = paste(position,collapse = ",")
    direction = turn(direction)
  } else {
    visites[[length(visites) + 1]] = paste(position,collapse = ",")
    position = next_position
  }
  
}

visites[[length(visites) +1]] <-
  paste(next_position,collapse = ",")

solution1 <-
  length(unique(visites))

# Partie 2 ----

position <-
  which(matrice == "^", arr.ind = T) |>
  as.vector()

visitesA <- visites

res = vector("logical",length = length(visites)-1)
k = 0
for(i in seq_along(visitesA)[-1]){
  
    k=i-1
    
    j = as.numeric(strsplit(visitesA[[i]],",")[[1]][2])
    i = as.numeric(strsplit(visitesA[[i]],",")[[1]][1])
    
    if(matrice[i,j] == "#" | matrice[i,j] == "^"){next}
    
    
    position <-
      which(matrice == "^", arr.ind = T) |> as.vector()
    
    direction = "haut"
    
    visites = list()
    
    matrice2 <- matrice |> 
      (\(.){.[i,j] = "#"; .})()
    
    while(position[1] != 1 & position[1] != nrow(matrice2) &
          position[2] != 1 & position[2] != ncol(matrice2) &
          !paste(paste(position,collapse = ","),direction,sep = ",") %in% visites[-length(visites)]){
      next_position = position+move(direction)
      
      if(matrice2[next_position[1],next_position[2]] == "#"){
        visites[[length(visites) + 1]] = paste(paste(position,collapse = ","),direction,sep = ",")
        direction = turn(direction)
      } else {
        position = next_position
      }
      
    }
    
    
    if(position[1] != 1 & position[1] != nrow(matrice) &
       position[2] != 1 & position[2] != ncol(matrice)){
      res[k] = T
    } else {
      res[k] = F
    }
    
}


solution2 <-
  unique(visitesA[-1][res]) |>
  length()

