# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/9/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  as.numeric() |> 
  matrix(nrow = length(input), byrow = T)


get_adjacent_values <- 
  function(row, col) {
    c(
      haut = if (row > 1) matrice[row - 1, col] else NA,
      bas = if (row < nrow(matrice)) matrice[row + 1, col] else NA,
      gauche = if (col > 1) matrice[row, col - 1] else NA,
      droite = if (col < ncol(matrice)) matrice[row, col + 1] else NA
    ) |> 
      (\(.){.[!is.na(.)]})()
    
  }

height = 0
for(i in seq_len(nrow(matrice))){
  for(j in seq_len(ncol(matrice))){
    if(all(matrice[i,j] < get_adjacent_values(i,j))){
      height = height + matrice[i,j] + 1
    }
  }
}


solution1 <-
  height

# Partie 2 ----

points <- data.frame()

for(i in seq_len(nrow(matrice))){
  for(j in seq_len(ncol(matrice))){
    if(matrice[i,j] != 9){
      points <-
        rbind(points,data.frame(x = j, y = i))
    }
  }
}

get_polygon <-
  function(point){
  c(
    point + c(0,.6),
    point + c(.6,0),
    point - c(0,.6),
    point - c(.6,0),
    point + c(0,.6)
  ) |> 
      matrix(ncol = 2, byrow = T) |> 
      (\(.){sf::st_polygon(list(.))})()
}

polygons <-
  apply(points,1,function(.x){
    get_polygon(.x)
  }) |> 
  sf::st_as_sfc()

polygonsg <- 
  polygons |> 
  sf::st_union() |> 
  sf::st_cast("POLYGON") |> 
  (\(.){.[order(sf::st_area(.),decreasing = T),][c(1:3),]})()

solution2 <-
  sf::st_intersects(polygonsg, polygons) |> 
  lapply(function(.x){length(.x)}) |> 
  (\(.){prod(unlist(.))})()


