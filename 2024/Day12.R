# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/12/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input),byrow = T)


get_adjacent_values <-
  function(row, col, include_corners = FALSE){
    
    adjacent_values <- list()
    
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
    if (include_corners) {
      if (row > 1 && col > 1) {
        adjacent_values$haut_gauche <- matrice[row - 1, col - 1]
      }
      if (row > 1 && col < ncol(matrice)) {
        adjacent_values$haut_droite <- matrice[row - 1, col + 1]
      }
      if (row < nrow(matrice) && col > 1) {
        adjacent_values$bas_gauche <- matrice[row + 1, col - 1]
      }
      if (row < nrow(matrice) && col < ncol(matrice)) {
        adjacent_values$bas_droite <- matrice[row + 1, col + 1]
      }
    }
    unlist(adjacent_values)
  }


get_adjacent_indices <-
  function(row, col){
    
    adjacent_indices <- list()
    
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
      memoire[[lettre]][[1]][["aire"]] = 1
      memoire[[lettre]][[1]][["perim"]] = (4-sum(get_adjacent_values(i,j)==lettre))
      memoire[[lettre]][[1]][["position"]] = paste(i,j,sep = ";")
    } else {
      
      position = paste(i,j,sep = ";")
      
      id <-
        lapply(memoire[[lettre]],
               function(.x){
                 any(get_adjacent_indices(i,j) %in% .x[["position"]])
               }) |> 
        unlist() |> 
        which()
      
      if(length(id) > 1){
        for(z in id[-1]){
          memoire[[lettre]][[id[1]]][["aire"]] = memoire[[lettre]][[id[1]]][["aire"]] + memoire[[lettre]][[z]][["aire"]]
          memoire[[lettre]][[id[1]]][["perim"]] = memoire[[lettre]][[id[1]]][["perim"]] + memoire[[lettre]][[z]][["perim"]]
          memoire[[lettre]][[id[1]]][["position"]] = c(memoire[[lettre]][[id[1]]][["position"]], memoire[[lettre]][[z]][["position"]])
          memoire[[lettre]] <- memoire[[lettre]][-z]
          
        }
        id = id[1]
      }
      
      
      if(length(id) == 0){id = length(memoire[[lettre]]) + 1}
      
      if(length(memoire[[lettre]]) < id){
        memoire[[lettre]][[id]] <- list()
        memoire[[lettre]][[id]][["aire"]] = 1
        memoire[[lettre]][[id]][["perim"]] = (4-sum(get_adjacent_values(i,j)==lettre))
        memoire[[lettre]][[id]][["position"]] = position
      } else {
        memoire[[lettre]][[id]][["aire"]] = memoire[[lettre]][[id]][["aire"]] + 1
        memoire[[lettre]][[id]][["perim"]] = memoire[[lettre]][[id]][["perim"]] + (4-sum(get_adjacent_values(i,j)==lettre))
        memoire[[lettre]][[id]][["position"]] = append(memoire[[lettre]][[id]][["position"]],
                                                       position)
      }
      
    }
    
  }
}

solution1 <-
  unlist(memoire, recursive = F) |> 
  lapply(
    function(.x){
      .x[["aire"]] * .x[["perim"]]
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

res = vector("numeric")
for(i in names(memoire)){
  for(j in seq_len(length(memoire[[i]]))){
    res <-
      memoire[[i]][[j]][["position"]] |> 
      lapply(
        function(.x){
          adj <-
            get_adjacent_values(as.numeric(gsub(";.*","",.x)),
                                as.numeric(gsub(".*;","",.x)),
                                F) |> 
            (\(.){.[. == i]})()
          
          if(length(adj) == 0){
            return(4)
          }
          if(length(adj) == 2 &
             (all(names(adj) %in% c("bas","haut")) | all(names(adj) %in% c("gauche","droite")))){
            return(0)
          }
          if(length(adj) == 1){
            return(2)
          }
          if(length(adj) == 2){
            noms <-
              names(adj) |> 
              (\(.){expand.grid(.,.)})() |> 
              apply(1,function(mot){paste(mot,collapse="_")})
            return(get_adjacent_values(as.numeric(gsub(";.*","",.x)),
                                       as.numeric(gsub(".*;","",.x)),
                                       T)  |> 
                     (\(.){.[!.%in% adj & names(.) %in% noms]})() |> 
                     (\(.){sum(. != i)+1})())
          }
          if(length(adj) == 3){
            noms <-
              names(adj) |> 
              (\(.){expand.grid(.,.)})() |> 
              apply(1,function(mot){paste(mot,collapse="_")})
            return(get_adjacent_values(as.numeric(gsub(";.*","",.x)),
                                       as.numeric(gsub(".*;","",.x)),
                                       T)  |> 
                     (\(.){.[!.%in% adj & names(.) %in% noms]})() |> 
                     (\(.){sum(. != i)})())
          }
          if(length(adj) == 4){
            return(get_adjacent_values(as.numeric(gsub(";.*","",.x)),
                                       as.numeric(gsub(".*;","",.x)),
                                       T)  |> 
                     (\(.){.[!.%in% adj]})() |> 
                     (\(.){sum(. != i)})())
          }
          
        }
      ) |> 
      unlist() |> 
      sum() |> 
      (\(.){. * memoire[[i]][[j]][["aire"]]})() |> 
      (\(.){append(res,.)})()
  }
}

solution2 <-
  sum(res)
