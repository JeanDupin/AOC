# Input ----

input <-
  readLines("2023/Inputs/Day18.txt")

# Partie 1 ----

instructions <-
  gsub(" \\(.*\\)","",input)


positions <- "0;0"
for(i in seq_along(instructions)){
  
  direction = substr(instructions[i],1,1)
  pas = as.numeric(substring(instructions[i],3))
  
  position = rev(positions)[1]
  
  X <- as.numeric(regmatches(position, gregexpr("^(|-)[0-9]+",position))[[1]])
  Y <- as.numeric(regmatches(position, gregexpr("(|-)[0-9]+$",position))[[1]])
  
  if(direction == "R"){
    X = c(X,(X+pas))
  } else if(direction == "D"){
    Y = c(Y,(Y-pas))
  } else if(direction == "L"){
    X = c(X,(X-pas))
  } else if(direction == "U"){
    Y = c(Y,(Y + pas))
  } else {
    stop("Pas bonne direction")
  }
  
  
  positions <<- append(positions,
                       paste(X,Y, sep = ";")[-1])
  
}; rm(i, position, X, Y,
      direction, pas)


mondet = 0
borders = 0
for(i in seq_along(positions)[-length(positions)]){
  X1 <-
    regmatches(positions,
               gregexpr("^(|-)[0-9]+",positions))[[i]] |> 
    as.numeric()
  Y1 <-
    regmatches(positions,
               gregexpr("(|-)[0-9]+$",positions))[[i]] |> 
    as.numeric()
  X2 <-
    regmatches(positions,
               gregexpr("^(|-)[0-9]+",positions))[[i+1]] |> 
    as.numeric()
  Y2 <-
    regmatches(positions,
               gregexpr("(|-)[0-9]+$",positions))[[i+1]] |> 
    as.numeric()
  
  mondet = mondet + det(matrix(c(X1,Y1,X2,Y2),
                               nrow = 2, ncol = 2, byrow = T))
  borders = borders + abs(diff(c(X1,X2))) + abs(diff(c(Y1,Y2)))
  
}; rm(i, X1, Y1, X2, Y2)


solution1 <-
  abs(mondet)/2+1-borders/2 + borders

# Partie 2 ----

instructions <-
  gsub("(.*)([0-9])\\)","\\2",input) |> 
  (\(.){chartr("0123","RDLU",.)})() |> 
  paste(gsub("(.* )(\\(\\#)(.*)([0-9]\\))","\\3",input) |> 
          (\(.){paste0("0x",.)})() |> 
          as.numeric())


positions <- "0;0"
for(i in seq_along(instructions)){
  
  direction = substr(instructions[i],1,1)
  pas = as.numeric(substring(instructions[i],3))
  
  position = rev(positions)[1]
  
  X <- as.numeric(regmatches(position, gregexpr("^(|-)[0-9]+",position))[[1]])
  Y <- as.numeric(regmatches(position, gregexpr("(|-)[0-9]+$",position))[[1]])
  
  if(direction == "R"){
    X = c(X,(X+pas))
  } else if(direction == "D"){
    Y = c(Y,(Y-pas))
  } else if(direction == "L"){
    X = c(X,(X-pas))
  } else if(direction == "U"){
    Y = c(Y,(Y + pas))
  } else {
    stop("Pas bonne direction")
  }
  
  
  positions <<- append(positions,
                       paste(X,Y, sep = ";")[-1])
  
}; rm(i, position, X, Y,
      direction, pas)


mondet = 0
borders = 0
for(i in seq_along(positions)[-length(positions)]){
  X1 <-
    regmatches(positions,
               gregexpr("^(|-)[0-9]+",positions))[[i]] |> 
    as.numeric()
  Y1 <-
    regmatches(positions,
               gregexpr("(|-)[0-9]+$",positions))[[i]] |> 
    as.numeric()
  X2 <-
    regmatches(positions,
               gregexpr("^(|-)[0-9]+",positions))[[i+1]] |> 
    as.numeric()
  Y2 <-
    regmatches(positions,
               gregexpr("(|-)[0-9]+$",positions))[[i+1]] |> 
    as.numeric()
  
  mondet = mondet + det(matrix(c(X1,Y1,X2,Y2),
                               nrow = 2, ncol = 2, byrow = T))
  borders = borders + abs(diff(c(X1,X2))) + abs(diff(c(Y1,Y2)))
  
}; rm(i, X1, Y1, X2, Y2)


solution2 <-
  abs(mondet)/2+1-borders/2 + borders
