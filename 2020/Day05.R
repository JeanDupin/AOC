# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      index <- c(0,127)
      instructions <-
        strsplit(.x,"")[[1]] |> 
        (\(.){.[which(!. %in% c("L","R"))]})()
      for(i in seq_along(instructions)){
        instruction <- instructions[i]
        if(instruction == "F"){
          dist = floor((index[2]-index[1])/2)
          index <- c(min(index),min(index) + dist)
        } else {
          dist = floor((index[2]-index[1])/2)
          index <- c(max(index)-dist,max(index))
        }
      }
      row = unique(index)
      
      
      
      index <- c(0,7)
      instructions <-
        strsplit(.x,"")[[1]] |> 
        (\(.){.[which(. %in% c("L","R"))]})()
      for(i in seq_along(instructions)){
        instruction <- instructions[i]
        if(instruction == "L"){
          dist = floor((index[2]-index[1])/2)
          index <- c(min(index),min(index) + dist)
        } else {
          dist = floor((index[2]-index[1])/2)
          index <- c(max(index)-dist,max(index))
        }
      }
      col = unique(index)
      
      return(row*8+col)
    }
  ) |> 
  unlist() |> 
  max()

# Partie 2 ----

solution2 <-
  lapply(
    input,
    function(.x){
      index <- c(0,127)
      instructions <-
        strsplit(.x,"")[[1]] |> 
        (\(.){.[which(!. %in% c("L","R"))]})()
      for(i in seq_along(instructions)){
        instruction <- instructions[i]
        if(instruction == "F"){
          dist = floor((index[2]-index[1])/2)
          index <- c(min(index),min(index) + dist)
        } else {
          dist = floor((index[2]-index[1])/2)
          index <- c(max(index)-dist,max(index))
        }
      }
      row = unique(index)
      
      
      
      index <- c(0,7)
      instructions <-
        strsplit(.x,"")[[1]] |> 
        (\(.){.[which(. %in% c("L","R"))]})()
      for(i in seq_along(instructions)){
        instruction <- instructions[i]
        if(instruction == "L"){
          dist = floor((index[2]-index[1])/2)
          index <- c(min(index),min(index) + dist)
        } else {
          dist = floor((index[2]-index[1])/2)
          index <- c(max(index)-dist,max(index))
        }
      }
      col = unique(index)
      
      return(row*8+col)
    }
  ) |> 
  unlist() |> 
  sort() |> 
  (\(.){
    c(min(.):max(.))[!c(min(.):max(.)) %in% .]
  })()