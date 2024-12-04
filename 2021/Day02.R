# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/2/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      dest <-
        regmatches(.x,
                   gregexpr("[a-z]+",.x))[[1]]
      value <-
        regmatches(.x,
                   gregexpr("\\d+",.x))[[1]] |> 
        as.numeric()
      
      if(dest == "forward"){
        c(value,0)
      } else if(dest == "down"){
        c(0,value)
      } else {
        c(0,-value)
      }
      
    }
  ) |> 
  (\(.){Reduce(`+`,.)})() |> 
  prod()

# Partie 2 ----

aim = 0
solution2 <-
  lapply(
    input,
    function(.x){
      dest <-
        regmatches(.x,
                   gregexpr("[a-z]+",.x))[[1]]
      value <-
        regmatches(.x,
                   gregexpr("\\d+",.x))[[1]] |> 
        as.numeric()
      
      if(dest == "forward"){
        c(value,aim*value)
      } else if(dest == "down"){
        aim <<- aim + value
        c(0,0)
      } else {
        aim <<- aim - value
        c(0,0)
      }
      
    }
  ) |> 
  (\(.){Reduce(`+`,.)})() |> 
  prod()
