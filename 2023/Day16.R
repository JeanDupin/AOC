# Input ----

input <-
  readLines("2023/Inputs/Day16.txt")

# Had this working via loops and list to store positions.
# Then find this solution with better implementation, and used it to rewrite :
# https://github.com/AdroMine/AdventOfCode/tree/main/2023/Day16

# Partie 1 ----

matrice <- 
  matrix(unlist(strsplit(input,"")),
         nrow = length(input),
         byrow = TRUE)

miroirs <-
  which(matrice != ".", arr.ind = T) |> 
  apply(1, function(.x){paste(.x, collapse = ";")})


mouvements <-
  list(
    "N" = c(-1,0),
    "E" = c(0,1),
    "S" = c(1,0),
    "O" = c(0,-1)
  )


laser <-
  function(position,
           direction){
    
    while(T) {
      position.test <-
        position + mouvements[[direction]]
      if(position.test[1] < 1 | position.test[1] > nrow(matrice) |
         position.test[2] < 1 | position.test[2] > ncol(matrice)){
        break
      }
      
      sortie[position.test[1],
             position.test[2]] <<- TRUE
      caractere <- 
        matrice[position.test[1],
                position.test[2]]
      
      if(caractere != ".") {
        id.position <- 
          paste(position.test,
                collapse = ";")
        
        
        if(direction %in% memoire[[id.position]]){
          break
        } else {
          
          memoire[[id.position]] <<-
            c(memoire[[id.position]], direction)
          
          if(caractere == "|" & direction %in% c("E", "O")){
            if(direction == "E"){
              memoire[[id.position]] <<-
                c(memoire[[id.position]], "O") 
            } else {
              memoire[[id.position]] <<-
                c(memoire[[id.position]], "E") 
            }
            
          }
          if(caractere == "-" & direction %in% c("N", "S")){
            if(direction == "N"){
              memoire[[id.position]] <<-
                c(memoire[[id.position]], "S") 
            } else {
              memoire[[id.position]] <<-
                c(memoire[[id.position]], "N") 
            }
          }
          
        }
      }
      
      position <-
        position.test
      if(caractere == ".") {
        next
      } else if (caractere == "/") {
        direction <- 
          switch(
            direction,
            "N" = "E",
            "E" = "N",
            "S" = "O",
            "O" = "S"
          )
      } else if(caractere == "\\") {
        direction <-
          switch(
            direction,
            "N" = "O",
            "E" = "S",
            "S" = "E",
            "O" = "N"
          )
      } else if (caractere == "-" & direction %in% c("E", "O")) {
        direction <-
          direction
      } else if (caractere == "|" & direction %in% c("N", "S")) {
        direction <-
          direction
      } else if (caractere == "|" & direction %in% c("E", "O")) {
        Recall(position.test, direction = "N")
        Recall(position.test, direction = "S")
        break
      } else if (caractere == "-" & direction %in% c("N", "S")) {
        Recall(position.test, direction = "E")
        Recall(position.test, dir = "O")
        break
      }
    }
  }



mouvement <-
  function(debut, direction.init){
  
  sortie <<- matrix(F,
                    nrow = nrow(matrice), 
                    ncol = ncol(matrice))
  
  memoire <<-
    vector("list", length(miroirs)) |> 
    (\(.){`names<-`(., miroirs)})()
  laser(debut, direction.init)
  return(sortie)
}

solution1 <-
  mouvement(c(1,0), "E") |> 
  sum()

# Partie 2 ----

energies <- vector("list")
for(i in 1:length(input)){
  energies[[i]] <-
    list(sum(mouvement(c(111,i), "N")),
         sum(mouvement(c(i, 0), "E")),
         sum(mouvement(c(0, i), "S")),
         sum(mouvement(c(i, 111), "O")))
}; rm(i)

solution2 <-
  unlist(energies) |> 
  max()



