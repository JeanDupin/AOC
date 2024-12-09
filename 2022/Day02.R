# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/2/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      op = gsub(" [A-Z]","",.x)
      me = gsub("[A-Z] ","",.x)
      
      if(op == "A"){
        if(me == "X"){
          1 + 3
        } else if(me == "Y"){
          2 + 6
        } else {
          3 + 0
        }
      } else if(op == "B"){
        if(me == "X"){
          1 + 0
        } else if(me == "Y"){
          2 + 3
        } else {
          3 + 6
        }
      } else{
        if(me == "X"){
          1 + 6
        } else if(me == "Y"){
          2 + 0
        } else {
          3 + 3
        }
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
      op = gsub(" [A-Z]","",.x)
      me = gsub("[A-Z] ","",.x)
      
      if(op == "A"){
        if(me == "X"){
          3 + 0
        } else if(me == "Y"){
          1 + 3
        } else {
          2 + 6
        }
      } else if(op == "B"){
        if(me == "X"){
          1 + 0
        } else if(me == "Y"){
          2 + 3
        } else {
          3 + 6
        }
      } else{
        if(me == "X"){
          2 + 0
        } else if(me == "Y"){
          3 + 3
        } else {
          1 + 6
        }
      }
      
    }
  ) |> 
  unlist() |> 
  sum()
