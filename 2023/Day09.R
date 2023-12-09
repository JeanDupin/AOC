# Input ----

input <-
  readLines("2023/Inputs/Day09.txt")


# Partie 1 ----

sortie = c()
for(i in seq_along(input)){
  # On crée la séquence des vecteurs
  seq.init <- input[i] |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() 
  vecteurs <- list(seq.init)
  while(!all(seq.init == 0)){
    seq.init = diff(seq.init)
    vecteurs <- append(vecteurs,
                       list(seq.init))
  }
  rm(seq.init)
  vecteurs <- rev(vecteurs)
  # On recalcule le dernier terme à chaque fois
  
  x = 0
  for(j in seq_along(vecteurs)){
    x <- x + vecteurs[[j]][length(vecteurs[[j]])]
  }
  
  sortie[i] = x
  rm(x)
  
}; rm(i, j)


solution1 <-
  sum(sortie)


# Partie 2 ----

sortie = c()
for(i in seq_along(input)){
  # On crée la séquence des vecteurs
  seq.init <- input[i] |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() 
  vecteurs <- list(seq.init)
  while(!all(seq.init == 0)){
    seq.init = diff(seq.init)
    vecteurs <- append(vecteurs,
                       list(seq.init))
  }
  rm(seq.init)
  vecteurs <- rev(vecteurs)
  # On recalcule le dernier terme à chaque fois
  
  x = 0
  for(j in seq_along(vecteurs)){
    x <- vecteurs[[j]][1] - x
  }
  
  sortie[i] = x
  rm(x)
  
}; rm(i, j)


solution2 <-
  sum(sortie)
