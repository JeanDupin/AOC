# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]] |> 
        as.numeric() |> 
        (\(.){.[1] == .[3] | .[2] == .[4]})()
    }
  ) |> 
  (\(.){input[unlist(.)]})() |> 
  lapply(
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]] |> 
        as.numeric() |> 
        (\(.){
          paste(.[1]:.[3],.[2]:.[4],sep = "-")
        })()
    }
  ) |> 
  unlist() |> 
  table() |> 
  (\(.){sum(. >= 2)})()


# Partie 2 ----

solution2 <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]] |> 
        as.numeric() |> 
        (\(.){
          paste(.[1]:.[3],.[2]:.[4],sep = "-")
        })()
    }
  ) |> 
  unlist() |> 
  table() |> 
  (\(.){sum(. >= 2)})()
