# Input ----

input <-
  readLines("2023/Inputs/Day03.txt")


# Partie 1 ----


symbols <- c()
for(i in seq_along(input)){
  if(any(gregexpr("[^0-9\\.]",input[i])[[1]] != -1)){
    j = gregexpr("[^0-9\\.]",input[i])[[1]]
    if(length(j) == 1){
      symbols <-
        append(symbols,
               paste(
                 c(i-1,i,i+1),
                 c(j-1),
                 sep = ";"
               ))
      symbols <-
        append(symbols,
               paste(
                 c(i-1,i,i+1),
                 c(j),
                 sep = ";"
               ))
      symbols <-
        append(symbols,
               paste(
                 c(i-1,i,i+1),
                 c(j+1),
                 sep = ";"
               ))
    } else {
      for(z in seq_along(j)){
        jb = j[z]
        symbols <-
          append(symbols,
                 paste(
                   c(i-1,i,i+1),
                   c(jb-1),
                   sep = ";"
                 ))
        symbols <-
          append(symbols,
                 paste(
                   c(i-1,i,i+1),
                   c(jb),
                   sep = ";"
                 ))
        symbols <-
          append(symbols,
                 paste(
                   c(i-1,i,i+1),
                   c(jb+1),
                   sep = ";"
                 ))
      }
    }
    
  }
}; rm(i, j, jb, z)

OK <- c()
for(i in seq_along(input)){
  if(any(gregexpr("[0-9]+",input[i])[[1]] != -1)){
    
    nombres <-
      regmatches(input[i], gregexpr("[0-9]+",input[i]))[[1]]
    positions <-
      gregexpr("[0-9]+",input[i])[[1]][1:length(nombres)]
    
    for(j in seq_along(nombres)){
     nombre = nombres[j]
     position = positions[j]
     taille = nchar(nombre)
     
     index = paste(i,
                   seq(position, position + taille - 1),
                   sep = ";")
     
     if(any(index %in% symbols)){
       OK <- append(OK, nombre)
     }
     
    }
    
    
  }
  
}; rm(i,j, nombre, nombres,
      taille, index, position, positions)


solution1 <-
  sum(as.numeric(OK))


# Partie 2 ----

# On va reprendre le même principe que pour la partie 1, mais dans l'autre sens.
# D"abord, on va donner tous les indices des chiffres. Puis, pour toutes les *, on va voir
# Si 2 nombres appartiennent aux index
rm(OK, symbols)



nombres.index <- data.frame()
for(i in seq_along(input)){
  if(any(gregexpr("[0-9]+",input[i])[[1]] != -1)){
    nombres <-
      regmatches(input[i], gregexpr("[0-9]+",input[i]))[[1]]
    positions <-
      gregexpr("[0-9]+",input[i])[[1]][1:length(nombres)]
    for(j in seq_along(nombres)){
      nombre = nombres[j]
      position = positions[j]
      taille = nchar(nombre)
      index = paste(i,
                    seq(position, position + taille - 1),
                    sep = ";")
      nombres.index <-
        rbind(nombres.index,
              data.frame(
                nombre = nombre,
                index = index
              ))
    }
  }
}; rm(i, j, nombres, positions, nombre, taille, position, index)

# Détection des *, création des index
# Dans chaque ligne, il peut y avoir plusieurs étoiles.

OK <- c()
for(i in seq_along(input)){
  if(any(gregexpr("[\\*]",input[i])[[1]] != -1)){
    j = gregexpr("[\\*]",input[i])[[1]]
    if(length(j) == 1){
      etoiles.index <-
        paste(c(i - 1, i, i + 1),
              c(j - 1),
              sep = ";")
      etoiles.index <-
        append(etoiles.index,
               paste(
                 c(i-1,i,i+1),
                 c(j),
                 sep = ";"
               ))
      etoiles.index <-
        append(etoiles.index,
               paste(
                 c(i-1,i,i+1),
                 c(j+1),
                 sep = ";"
               ))
      
      nombres <-
        nombres.index[which(nombres.index[,2] %in% etoiles.index),"nombre"] |> 
        unique()
      print(nombres)
      
      if(length(nombres) == 2){
        OK <-
          append(OK,
                 prod(as.numeric(nombres)))
      }
    } else {
      for(z in seq_along(j)){
        jb = j[z]
        etoiles.index <-
          paste(
                   c(i-1,i,i+1),
                   c(jb-1),
                   sep = ";"
                 )
        etoiles.index <-
          append(etoiles.index,
                 paste(
                   c(i-1,i,i+1),
                   c(jb),
                   sep = ";"
                 ))
        etoiles.index <-
          append(etoiles.index,
                 paste(
                   c(i-1,i,i+1),
                   c(jb+1),
                   sep = ";"
                 ))
        
        nombres <-
          nombres.index[which(nombres.index[,2] %in% etoiles.index),"nombre"] |> 
          unique()
        print(nombres)
        
        if(length(nombres) == 2){
          OK <-
            append(OK,
                   prod(as.numeric(nombres)))
        }
      }
    }
    
      
    
  }
}; rm(i, j, nombres, etoiles.index,
      jb, z)

solution2 <-
  sum(OK)

