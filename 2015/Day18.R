# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/18/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

get_adjacent_values <- function(row, col) {

  adjacent_positions <- expand.grid(
    row_offset = c(-1, 0, 1),
    col_offset = c(-1, 0, 1)
  )
  
  adjacent_positions <- subset(adjacent_positions, !(row_offset == 0 & col_offset == 0))
  
  adjacent_indices <- cbind(
    row = row + adjacent_positions$row_offset,
    col = col + adjacent_positions$col_offset
  )
  
  valid_indices <- adjacent_indices[
    adjacent_indices[, "row"] >= 1 & adjacent_indices[, "row"] <= nrow(matrice) &
    adjacent_indices[, "col"] >= 1 & adjacent_indices[, "col"] <= ncol(matrice), 
    ]
  
  adjacent_values <- apply(valid_indices, 1, function(index) {
    matrice[index["row"], index["col"]]
  })
  
  adjacent_values
}

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input),byrow = T)


for(steps in seq_len(100)){
  next_matrice = matrix(".",ncol = length(input),nrow = length(input))

  for(i in seq_len(nrow(matrice))){
    for(j in seq_len(ncol(matrice))){
      if(matrice[i,j] == "#"){
        next_matrice[i,j] <- ifelse(sum(get_adjacent_values(i,j) == "#") %in% c(2,3),'#',".")
      } else {
        next_matrice[i,j] <- ifelse(sum(get_adjacent_values(i,j) == "#") == 3,'#',".")
      }
    }
  }

  matrice = next_matrice

}; rm(steps,i,j)

solution1 <-
  sum(matrice == "#")

# Partie 2 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input),byrow = T)

matrice[1,1] = matrice[1,100] = matrice[100,1] = matrice[100,100] = "#"

for(steps in seq_len(100)){
  next_matrice = matrix(".",ncol = length(input),nrow = length(input))

  for(i in seq_len(nrow(matrice))){
    for(j in seq_len(ncol(matrice))){
      if(matrice[i,j] == "#"){
        next_matrice[i,j] <- ifelse(sum(get_adjacent_values(i,j) == "#") %in% c(2,3),'#',".")
      } else {
        next_matrice[i,j] <- ifelse(sum(get_adjacent_values(i,j) == "#") == 3,'#',".")
      }
    }
  }

  matrice = next_matrice
  matrice[1,1] = matrice[1,100] = matrice[100,1] = matrice[100,100] = "#"
}; rm(steps,i,j)

solution2 <-
  sum(matrice == "#")
