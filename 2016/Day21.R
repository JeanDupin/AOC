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

solve <- function(x){
  
  for(i in seq_along(input)){
    
    ligne <- input[i]
    
    if(grepl("^swap position", ligne)){
      indices <-
        regmatches(ligne, gregexpr("\\d+", ligne))[[1]] |>
        (\(.){as.numeric(.) + 1})()
      x <- swap_positions(indices[1], indices[2], x)
      
    }else if(grepl("^swap letter", ligne)){
      indices <-
        regmatches(ligne, gregexpr("letter [a-z]", ligne))[[1]] |>
        (\(.){gsub("letter ", "", .)})()
      x <- swap_letters(indices[1], indices[2], x)
      
    }else if(grepl("^rotate (left|right)", ligne)){
      sens <- ifelse(grepl("right", ligne), "right", "left")
      indice <-
        regmatches(ligne, gregexpr("\\d+", ligne))[[1]] |>
        as.numeric()
      x <- rotate(sens, indice, x)
      
    }else if(grepl("^rotate based", ligne)){
      indice <- gsub("^.* ", "", ligne)
      x <- rotate_based(indice, x)
      
    }else if(grepl("^reverse", ligne)){
      indices <-
        regmatches(ligne, gregexpr("\\d+", ligne))[[1]] |>
        (\(.){as.numeric(.) + 1})()
      x <- reverse(indices[1], indices[2], x)
      
    }else if(grepl("^move", ligne)){
      indices <-
        regmatches(ligne, gregexpr("\\d+", ligne))[[1]] |>
        (\(.){as.numeric(.) + 1})()
      x <- move(indices[1], indices[2], x)
      
    }
  }
  
  x
}

password = "abcdefgh"

solution1 <-
  solve(password)

# Partie 2 ----

permuter <- function(.x){
  if(length(.x) == 1){return(list(.x))}
  
  lapply(seq_along(.x), function(.y){
    lapply(permuter(.x[-.y]), function(.z){
      c(.x[.y],.z)
    })
  }) |> 
    unlist(recursive = FALSE)
}

anagramme <- function(.x){
  lettres <- strsplit(.x, "")[[1]]
  sapply(permuter(lettres),
         paste, collapse = "",
         simplify = T, USE.NAMES = F) |> 
    unique()
}

passwords <-
  anagramme(password)

res = ""

for(i in seq_along(passwords)){
  if(i %% 1000 == 0){print(i)}
  res <- solve(passwords[[i]])
  if(res == "fbgdceah"){break}
}

solution2 <-
  passwords[i]
