# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      instructions <-
        regmatches(.x,
                   gregexpr("\\d+",.x))[[1]] |> 
        as.numeric()
      
      if(instructions[1] <= instructions[3] & instructions[2] >= instructions[4]){
        T
      } else if(instructions[3] <= instructions[1] & instructions[4] >= instructions[2]){
        T
      } else {
        F
      }
      
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(
    input,
    function(.x){
      instructions <-
        regmatches(.x,
                   gregexpr("\\d+",.x))[[1]] |> 
        as.numeric()
      
      if(length(intersect(instructions[1]:instructions[2],
                          instructions[3]:instructions[4])) > 0){
        T
      } else {
        F
      }
      
    }
  ) |> 
  unlist() |> 
  sum()
