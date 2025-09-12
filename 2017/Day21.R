# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/21/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

sequences <-
  sapply(input,
         function(.x){
           strsplit(.x," => ")[[1]][1]
         },
         USE.NAMES = F)
sequences_out <-
  sapply(input,
         function(.x){
           strsplit(.x," => ")[[1]][2]
         },
         USE.NAMES = F)

rotate <- function(mat, angle = 90){
  switch(angle,
         90  = t(apply(mat, 2, rev)),         
         180 = rotate(rotate(mat,90),90),  
         270 = rotate(rotate(rotate(mat,90),90),90)         
  )
}


flip <- function(mat, direction = "H"){
  if(direction == "H"){
    rbind(
      mat[rev(seq_len(nrow(mat))),]
    )
  } else if(direction == "V"){
    cbind(
      mat[,rev(seq_len(nrow(mat)))]
    )
  } else{
    cbind(
      mat[,rev(seq_len(nrow(mat)))]
    ) |> 
      (\(.){
        rbind(
          .[rev(seq_len(nrow(.))),]
        )
      })()
  }
}

all_perm <- function(mat){
  list(
    mat,
    flip(mat),
    flip(mat, "V"),
    flip(mat, "VH"),
    rotate(mat),
    rotate(flip(mat)),
    rotate(flip(mat, "V")),
    rotate(flip(mat," VH"))
  ) |> 
    sapply(function(.x){
      apply(.x,1,function(.y){paste(.y, collapse = "")}) |> 
        paste(collapse = "/")
    })
}


split_matrix <- function(mat, block_size = 3){
  
  n_blocks_row <- nrow(mat) / block_size
  n_blocks_col <- ncol(mat) / block_size
  
  blocks <- list()
  index <- 1
  
  for(i in seq(1, nrow(mat), by = block_size)){
    for(j in seq(1, ncol(mat), by = block_size)){
      blocks[[index]] <- mat[i:(i+block_size-1), j:(j+block_size-1)]
      index <- index + 1
    }
  }
  return(blocks)
}


combine_blocks <- function(mats){
  n <- length(mats)
  k <- sqrt(n)
  
  rows <- vector("list", k)
  for(i in seq_len(k)){
    idx <- ((i - 1) * k + 1):(i * k)
    rows[[i]] <- do.call(cbind, mats[idx])
  }
  
  return(do.call(rbind, rows))
}

matrice <-
  c(".#...####") |> 
  (\(.){strsplit(.,"")[[1]]})() |> 
  matrix(nrow = 3,
         byrow = T)

for(i in seq_len(5)){
  if(ncol(matrice) %% 2 == 0){
    matrices <-
      split_matrix(matrice, 2)
    
    matrice <-
      lapply(matrices,
             function(.x){
               all_perm(.x) |> 
                 (\(.){which(sequences %in% .)})() |> 
                 (\(.){strsplit(sequences_out[.],"/")[[1]]})() |> 
                 (\(.){unlist(strsplit(.,""))})() |> 
                 matrix(ncol = 3, byrow = T)
             }) |> 
      combine_blocks()
  } else {
    matrices <-
      split_matrix(matrice, 3)
    
    matrice <-
      lapply(matrices,
             function(.x){
               all_perm(.x) |> 
                 (\(.){which(sequences %in% .)})() |> 
                 (\(.){strsplit(sequences_out[.],"/")[[1]]})() |> 
                 (\(.){unlist(strsplit(.,""))})() |> 
                 matrix(ncol = 4, byrow = T)
             }) |> 
      combine_blocks()
  }
}

solution1 <-
  sum(matrice == "#")

# Partie 2 ----
  
matrice <-
  c(".#...####") |> 
  (\(.){strsplit(.,"")[[1]]})() |> 
  matrix(nrow = 3,
         byrow = T)

for(i in seq_len(18)){
  if(ncol(matrice) %% 2 == 0){
    matrices <-
      split_matrix(matrice, 2)
    
    matrice <-
      lapply(matrices,
             function(.x){
               all_perm(.x) |> 
                 (\(.){which(sequences %in% .)})() |> 
                 (\(.){strsplit(sequences_out[.],"/")[[1]]})() |> 
                 (\(.){unlist(strsplit(.,""))})() |> 
                 matrix(ncol = 3, byrow = T)
             }) |> 
      combine_blocks()
  } else {
    matrices <-
      split_matrix(matrice, 3)
    
    matrice <-
      lapply(matrices,
             function(.x){
               all_perm(.x) |> 
                 (\(.){which(sequences %in% .)})() |> 
                 (\(.){strsplit(sequences_out[.],"/")[[1]]})() |> 
                 (\(.){unlist(strsplit(.,""))})() |> 
                 matrix(ncol = 4, byrow = T)
             }) |> 
      combine_blocks()
  }
}



solution2 <-
  sum(matrice == "#")
