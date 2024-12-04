# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/11/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


adjacent_elements <- function(matrix, i, j) {
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  neighbors <- expand.grid(
    row = (i - 1):(i + 1),
    col = (j - 1):(j + 1)
  )
  
  neighbors <- neighbors[!(neighbors$row == i & neighbors$col == j), ]
  
  valid_neighbors <- neighbors[
    neighbors$row >= 1 & neighbors$row <= n_rows &
      neighbors$col >= 1 & neighbors$col <= n_cols, 
  ]
  
  adjacent_values <- apply(valid_neighbors, 1, function(pos) {
    matrix[pos[1], pos[2]]
  })
  
  return(adjacent_values)
}


matrice = matriceb <-
  strsplit(input,"") |>
  unlist() |> 
  matrix(ncol = nchar(input[1]),nrow=length(input),byrow = T)

while(T){
  
  for(i in 1:nrow(matrice)){
    for(j in 1:ncol(matrice)){
      
      if(matrice[i,j] == "."){
        next
      }
      
      voisins <-
        adjacent_elements(matrice,i,j)
      

      
      if(all(voisins != "#")){
        matriceb[i,j] <- "#"
      } else if(table(voisins)["#"] >= 4){
        matriceb[i,j] <- "L"
      }
      
      
    }
  }
  
  if(all(matrice == matriceb)){
    break
  }
  matrice <- matriceb
  
}

solution1 <-
  sum(matriceb == "#")

# Partie 2 ----

directional_neighbors <- function(matrix, i, j) {
  
  n_rows <- nrow(matrix)
  n_cols <- ncol(matrix)
  
  directions <- list(
    up = c(-1, 0),        
    down = c(1, 0),        
    left = c(0, -1),      
    right = c(0, 1),       
    up_left = c(-1, -1),   
    up_right = c(-1, 1),   
    down_left = c(1, -1),  
    down_right = c(1, 1)   
  )
  
  get_elements_in_direction <- function(delta_row, delta_col) {
    values <- c()
    current_row <- i
    current_col <- j
    while (TRUE) {
      current_row <- current_row + delta_row
      current_col <- current_col + delta_col
      if (current_row < 1 || current_row > n_rows || 
          current_col < 1 || current_col > n_cols) {
        break
      }
      values <- c(values, matrix[current_row, current_col])
    }
    return(values)
  }
  neighbors <- lapply(directions, function(direction) {
    get_elements_in_direction(direction[1], direction[2])
  })
  return(neighbors)
}


matrice = matriceb <-
  strsplit(input,"") |>
  unlist() |> 
  matrix(ncol = nchar(input[1]),nrow=length(input),byrow = T)

while(T){
  
  for(i in 1:nrow(matrice)){
    for(j in 1:ncol(matrice)){
      
      if(matrice[i,j] == "."){
        next
      }
      
      voisins <-
        directional_neighbors(matrice,i,j) |> 
        lapply(
          function(.x){
            if(is.null(.x)){
              0
            } else if(all(.x == ".")){
              0
            } else if(.x[.x != "."][1] == "L"){
              0
            } else if(.x[.x != "."][1] == "#"){
              1
            }
          }
        ) |> 
        unlist() |> 
        sum()
      
      
      
      if(voisins == 0){
        matriceb[i,j] <- "#"
      } else if(voisins >= 5){
        matriceb[i,j] <- "L"
      }
      
      
    }
  }
  
  if(all(matrice == matriceb)){
    break
  }
  matrice <- matriceb
  
}

solution2 <-
  sum(matriceb == "#")


