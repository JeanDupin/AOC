# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/21/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


swap_positions <- function(X,Y,mot){
  mot = strsplit(mot,"")[[1]]
  temp = mot[X]
  mot[X] <- mot[Y]
  mot[Y] <- temp
  paste(mot, collapse = "")
}

swap_letters <- function(X,Y,mot){
  mot = strsplit(mot,"")[[1]]
  indicesX = which(mot == X)
  indicesY = which(mot == Y)
  mot[indicesX] = Y
  mot[indicesY] = X
  paste(mot, collapse = "")
}

rotate <- function(direction, X, mot){
  mot = strsplit(mot,"")[[1]]
  if(X == 0){
    return(paste(mot, collapse = ""))
  } 
  if(X > length(mot)){
    X = X %% length(mot)
  }
  if(direction == "right"){
    indices <- rev(rev(seq_along(mot))[seq_len(X)])
    mot = c(mot[indices],mot[-indices])
  } else {
    indices <- seq_len(X)
    mot = c(mot[-indices],mot[indices])
  }
  paste(mot, collapse = "")
}

rotate_based <- function(X, mot){
  mot = strsplit(mot,"")[[1]]
  indice = which(mot == X)[1] - 1
  indice = ifelse(indice >= 4, indice + 2, indice + 1)
  mot = paste(mot, collapse = "")
  rotate("right",indice, mot)
}

reverse <- function(X,Y,mot){
  mot = strsplit(mot,"")[[1]]
  if(X == 1 & Y == length(mot)){
    mot = rev(mot)
  } else if(X == 1){
    mot = c(rev(mot[X:Y]),mot[(Y+1):length(mot)])
  } else if(Y == length(mot)){
    mot = c(mot[1:(X-1)],rev(mot[X:Y]))
  } else {
    mot = c(mot[1:(X-1)],rev(mot[X:Y]),mot[(Y+1):length(mot)])
  }
  paste(mot, collapse = "")
}

move <- function(X, Y, mot){
  mot = strsplit(mot,"")[[1]]
  if(Y != 1 & Y != length(mot)){
    if(X < Y){
      mot = c(mot[-X][1:(Y-1)],mot[X],mot[-X][Y:(length(mot)-1)])
    } else {
      mot = c(mot[1:(Y-1)],mot[X],mot[-X][Y:(length(mot)-1)])
    }
  } else if(Y == 1){
    mot = c(mot[X],mot[-X])
  } else {
    mot = c(mot[-X],mot[X])
  }
  paste(mot, collapse = "")
}

password = "abcdefgh"
for(i in seq_along(input)){
  if(grepl("^swap position",input[i])){
    indices <-
      regmatches(input[i],
                 gregexpr("\\d+",input[i]))[[1]] |> 
      (\(.){as.numeric(.) + 1})()
    password = swap_positions(indices[1],indices[2],password)
  } else if(grepl("^swap letter",input[i])){
    indices <-
      regmatches(input[i],
                 gregexpr("letter [a-z]",input[i]))[[1]] |> 
      (\(.){gsub("letter ","",.)})()
    password = swap_letters(indices[1],indices[2],password)
  } else if(grepl("^rotate [left|right]",input[i])){
    sens <-
      ifelse(grepl("right",input[i]),"right","left")
    indice <-
      regmatches(input[i],
                 gregexpr("\\d+",input[i]))[[1]] |> 
      as.numeric()
    password <- rotate(sens, indice, password)
  } else if(grepl("^rotate based",input[i])){
    indice <-
      gsub("^.* ","",input[i])
    password <- rotate_based(indice,password)
  } else if(grepl("^reverse",input[i])){
    indices <-
      regmatches(input[i],
                 gregexpr("\\d+",input[i]))[[1]] |> 
      (\(.){as.numeric(.) + 1})()
    password = reverse(indices[1],indices[2],password)
  } else if(grepl("^move",input[i])){
    indices <-
      regmatches(input[i],
                 gregexpr("\\d+",input[i]))[[1]] |> 
      (\(.){as.numeric(.) + 1})()
    password = move(indices[1],indices[2],password)
  }
}

solution1 <-
  password

# Partie 2 ----

solution2 <-
  NA