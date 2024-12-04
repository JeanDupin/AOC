# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]][-1] |> 
        as.numeric() |> 
        (\(.){
          wide = (.[1]+1):(.[1]+.[3])
          tall = (.[2]+1):(.[2]+.[4])
          expand.grid(wide,tall)
        })() |> 
        (\(.){paste(.$Var1,.$Var2,sep="-")})()
    }
  ) |> 
  unlist() |> 
  table() |> 
  (\(.){.[which(. != 1)]})() |> 
  length()

# Partie 2 ----

overlaps <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]][-1] |> 
        as.numeric() |> 
        (\(.){
          wide = (.[1]+1):(.[1]+.[3])
          tall = (.[2]+1):(.[2]+.[4])
          expand.grid(wide,tall)
        })() |> 
        (\(.){paste(.$Var1,.$Var2,sep="-")})()
    }
  ) |> 
  unlist() |> 
  table() |> 
  (\(.){.[which(. != 1)]})() |> 
  names()

solution2 <-
  lapply(
    input,
    function(.x){
      coords <-
        regmatches(.x,
                   gregexpr("\\d+",.x))[[1]][-1] |> 
        as.numeric() |> 
        (\(.){
          wide = (.[1]+1):(.[1]+.[3])
          tall = (.[2]+1):(.[2]+.[4])
          expand.grid(wide,tall)
        })() |> 
        (\(.){paste(.$Var1,.$Var2,sep="-")})()
      
      if(any(coords %in% overlaps)){
        return(F)
      } else {
        T
      }
    }
  ) |> 
  unlist() |> 
  (\(.){which(.)})()
