# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/10/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

make_matrix <- function(.x){
  taille <- nchar(gsub("] .*$","",.x))-1
  
  chiffres <-
    regmatches(.x,
               gregexpr("\\(.+?\\)",.x))[[1]]
  
  matrice <-
    matrix(0, taille, length(chiffres))
  
  for(i in seq_along(chiffres)){
    indices <-
      regmatches(chiffres[i],
                 gregexpr("\\d+",chiffres[i]))[[1]] |> 
      as.numeric() |> 
      (\(.){. + 1})()
    
    matrice[indices,i] <- 1
  }
  cbind(matrice,
        gsub("^\\[","",gsub("] .*$","",.x)) |> 
          (\(.){strsplit(.,"")[[1]]})() |> 
          (\(.){ifelse(. == ".",0,1)})())
}

solve <- function(.id){
  matrice <-
    make_matrix(input[.id])
  
  combinaisons <-
    expand.grid(rep(list(0:2),ncol(matrice)-1)) |> 
    (\(.){.[order(rowSums(.)),]})()
  
  for(i in seq_len(nrow(combinaisons))){
    if(all(((matrice[,seq_len(ncol(matrice)-1)] %*% t(combinaisons[i,])) %% 2) == matrice[,ncol(matrice)])){
      break
    }
  }
  
  if(i == nrow(combinaisons)){return(Inf)}
  
  sum(combinaisons[i,])
  
}


solution1 <-
  sapply(seq_along(input),solve) |> 
  sum()

# Partie 2 ----
  
make_matrix2 <- function(.x){
  taille <- nchar(gsub("] .*$","",.x))-1
  
  chiffres <-
    regmatches(.x,
               gregexpr("\\(.+?\\)",.x))[[1]]
  
  matrice <-
    matrix(0, taille, length(chiffres))
  
  for(i in seq_along(chiffres)){
    indices <-
      regmatches(chiffres[i],
                 gregexpr("\\d+",chiffres[i]))[[1]] |> 
      as.numeric() |> 
      (\(.){. + 1})()
    
    matrice[indices,i] <- 1
  }
  cbind(matrice,
        gsub("\\}","",gsub("^.* \\{","",.x)) |> 
          (\(.){strsplit(.,",")[[1]]})() |> 
          as.numeric())
}

solve2 <- function(.id){
  matrice <- make_matrix2(input[.id])
  
  f.obj <- rep(1, ncol(matrice)-1)
  f.con <- matrice[,-ncol(matrice)]
  f.dir <- rep("==",ncol(matrice)-1)
  f.rhs <- matrice[,ncol(matrice)]
  
  lpSolve::lp("min", f.obj, f.con, f.dir, f.rhs, all.int = TRUE)$objval
}

solution2 <-
  sapply(seq_along(input),solve2) |> 
  sum()
