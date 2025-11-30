# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/22/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

turn <- function(direction, LR){
  if(LR == "droite"){
    switch (direction,
      "R" = "D",
      "D" = "L",
      "L" = "U",
      "U" = "R"
    )
  } else {
    switch (direction,
            "R" = "U",
            "U" = "L",
            "L" = "D",
            "D" = "R"
    )
  }
}

matrice <-
  matrix(c(".",".","#",
           "#",".",".",
           ".",".","."),
         ncol = 3,
         byrow = T)

i = j = ceiling(nrow(matrice)/2)
direction = "U"
bursts = 0

steps = 1
while(steps <= 10000){
  # On rajoute des lignes ou des colonnes si on est au bord
  if(i == 1){
    matrice <-
      rbind(rep(".",ncol(matrice)),
            matrice)
    i = i + 1
  } else if(i == nrow(matrice)){
    matrice <-
      rbind(matrice,
            rep(".",ncol(matrice)))
  }
  if(j == 1){
    matrice <-
      cbind(rep(".",nrow(matrice)),
            matrice)
    j = j + 1
  } else if(j == ncol(matrice)){
    matrice <-
      cbind(matrice,
            rep(".",nrow(matrice)))
  }
  # On change la direction & le statut du noeud
  if(matrice[i,j] == "#"){
    direction = turn(direction, "droite")
    matrice [i,j] = "."
  } else {
    direction = turn(direction, "gauche")
    matrice [i,j] = "#"
    bursts = bursts + 1
  }
  
  # On avance
  if(direction == "U"){
    i = i - 1
  } else if(direction == "D"){
    i = i + 1
  } else if(direction == "R"){
    j = j + 1
  } else if(direction == "L"){
    j = j - 1
  }
  steps = steps + 1
}

solution1 <-
  bursts

# Partie 2 ----

turn <- function(direction, LR){
  if(LR == "droite"){
    switch (direction,
            "R" = "D",
            "D" = "L",
            "L" = "U",
            "U" = "R"
    )
  } else if(LR == "gauche"){
    switch (direction,
            "R" = "U",
            "U" = "L",
            "L" = "D",
            "D" = "R"
    )
  } else {
    switch (direction,
            "R" = "L",
            "U" = "D",
            "L" = "R",
            "D" = "U"
    )
  }
}

matrice <-
  strsplit(input,"") |>
  unlist() |>
  matrix(nrow = length(input),
         byrow = T)


i = j = ceiling(nrow(matrice)/2)
direction = "U"
bursts = 0
steps = 1

while(steps <= 1e7){
  # On rajoute des lignes ou des colonnes si on est au bord
  if(i == 1){
    matrice <-
      rbind(rep(".",ncol(matrice)),
            matrice)
    i = i + 1
  } else if(i == nrow(matrice)){
    matrice <-
      rbind(matrice,
            rep(".",ncol(matrice)))
  }
  if(j == 1){
    matrice <-
      cbind(rep(".",nrow(matrice)),
            matrice)
    j = j + 1
  } else if(j == ncol(matrice)){
    matrice <-
      cbind(matrice,
            rep(".",nrow(matrice)))
  }
  # On change la direction & le statut du noeud
  if(matrice[i,j] == "."){
    direction = turn(direction, "gauche")
    matrice[i,j] = "W"
  } else if(matrice[i,j] == "W"){
    matrice[i,j] = "#"
    bursts = bursts + 1
  } else if(matrice[i,j] == "#"){
    direction = turn(direction, "droite")
    matrice[i,j] = "F"
  } else if(matrice[i,j] == "F"){
    direction = turn(direction, "reverse")
    matrice[i,j] = "."
  }
  
  # On avance
  if(direction == "U"){
    i = i - 1
  } else if(direction == "D"){
    i = i + 1
  } else if(direction == "R"){
    j = j + 1
  } else if(direction == "L"){
    j = j - 1
  }
  steps = steps + 1
}

solution2 <-
  bursts