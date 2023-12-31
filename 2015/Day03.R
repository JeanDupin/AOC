# Input ----

input <-
  readLines("2015/Inputs/Day03.txt") 


# Partie 1 ----

inputb <-
  strsplit(input,"")[[1]]

x = c(0)
y = c(0)
for(i in seq_along(inputb)){
  if(inputb[i] == ">"){
    x[i+1] <- x[i] + 1
    y[i+1] <- y[i]
  }
  if(inputb[i] == "^"){
    x[i+1] <- x[i]
    y[i+1] <- y[i] + 1
  }
  if(inputb[i] == "<"){
    x[i+1] <- x[i] - 1
    y[i+1] <- y[i]
  }
  if(inputb[i] == "v"){
    x[i+1] <- x[i]
    y[i+1] <- y[i] - 1
  }
}; rm(i)

solution1 <-
  length(unique(paste(x,y,sep = ";")))


# Partie 2 ----

x = c(0)
y = c(0)
santa = inputb[seq(1,length(inputb),2)]
for(i in seq_along(santa)){
  if(santa[i] == ">"){
    x[i+1] <- x[i] + 1
    y[i+1] <- y[i]
  }
  if(santa[i] == "^"){
    x[i+1] <- x[i]
    y[i+1] <- y[i] + 1
  }
  if(santa[i] == "<"){
    x[i+1] <- x[i] - 1
    y[i+1] <- y[i]
  }
  if(santa[i] == "v"){
    x[i+1] <- x[i]
    y[i+1] <- y[i] - 1
  }
}; rm(i)


xb = c(0)
yb = c(0)
robot = inputb[seq(2,length(inputb),2)]
for(i in seq_along(robot)){
  if(robot[i] == ">"){
    xb[i+1] <- xb[i] + 1
    yb[i+1] <- yb[i]
  }
  if(robot[i] == "^"){
    xb[i+1] <- xb[i]
    yb[i+1] <- yb[i] + 1
  }
  if(robot[i] == "<"){
    xb[i+1] <- xb[i] - 1
    yb[i+1] <- yb[i]
  }
  if(robot[i] == "v"){
    xb[i+1] <- xb[i]
    yb[i+1] <- yb[i] - 1
  }
}; rm(i)

solution2 <-
  c(paste(x,y,sep = ";"),
    paste(xb,yb,sep = ";")) |> 
  unique() |> 
  length()
