# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  "." |> 
  matrix(ncol = 50, nrow = 6)


for(i in input){
  coords <-
      regmatches(i,
                 gregexpr("\\d+",i))[[1]] |> 
      as.numeric()
  if(grepl("rect",i)){
    matrice[seq_len(coords[2]),seq_len(coords[1])] = "#"
  } else if(grepl("row",i)){
    coords[1] = coords[1] + 1
    matrice[coords[1],] <-
      c(matrice[coords[1],][(50-coords[2]+1):50],
        matrice[coords[1],][1:(50-coords[2])])
    
  } else {
    coords[1] = coords[1] + 1
    matrice[,coords[1]] <-
      c(matrice[,coords[1]][(6-coords[2]+1):6],
        matrice[,coords[1]][1:(6-coords[2])])
  }
}

solution1 <-
  length(which(matrice == "#"))

# Partie 2 ----

for(i in seq(1,50,5)){
  print(matrice[,i:(i+4)])
}

solution2 <-
  "UPOJFLBCEZ"