# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  as.numeric(strsplit(input,"-")[[1]]) |> 
  (\(.){seq(.[1],.[2])})() |> 
  (\(.){.[. %% 10 != 0]})() |> 
  lapply(
    function(.x){
      if(!grepl("(00|11|22|33|44|55|66|77|88|99)",.x)){
        F
      } else if(!all(diff(as.numeric(strsplit(as.character(.x),"")[[1]])) >= 0)){
        F
      } else {
        T
      }
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  as.numeric(strsplit(input,"-")[[1]]) |> 
  (\(.){seq(.[1],.[2])})() |> 
  (\(.){.[. %% 10 != 0]})() |> 
  lapply(
    function(.x){
      if(!grepl("((?<!0)00(?!0)|(?<!1)11(?!1)|(?<!2)22(?!2)|(?<!3)33(?!3)|(?<!4)44(?!4)|(?<!5)55(?!5)|(?<!6)66(?!6)|(?<!7)77(?!7)|(?<!8)88(?!8)|(?<!9)99(?!9))",.x,perl = T)){
        F
      } else if(!all(diff(as.numeric(strsplit(as.character(.x),"")[[1]])) >= 0)){
        F
      } else {
        T
      }
    }
  ) |> 
  unlist() |> 
  sum()
