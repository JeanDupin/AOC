# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

sequences <-
  strsplit(input,"")

solution1 <-
  lapply(
    sequences,
    function(.x){
      memoire = vector("character")
      res = ""
      for(i in 1:length(.x)){
        if(.x[i] %in% c("[","(","{","<")){
          memoire = append(memoire,.x[i])
        } else {
          
          if(.x[i] == "]"){
            if(memoire[length(memoire)] == "["){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == "}"){
            if(memoire[length(memoire)] == "{"){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == ")"){
            if(memoire[length(memoire)] == "("){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == ">"){
            if(memoire[length(memoire)] == "<"){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          }
        }
      }
      res
    }) |> 
  unlist() |> 
  (\(.){.[. != ""]})() |> 
  table() |> 
  (\(.){
    ifelse(names(.) == ")",.*3,.) |> 
      (\(.z){names(.z) <- names(.); .z})()
  })() |> 
  (\(.){
    ifelse(names(.) == "]",.*57,.) |> 
      (\(.z){names(.z) <- names(.); .z})()
  })() |> 
  (\(.){
    ifelse(names(.) == "}",.*1197,.) |> 
      (\(.z){names(.z) <- names(.); .z})()
  })() |> 
  (\(.){
    ifelse(names(.) == ">",.*25137,.) |> 
      (\(.z){names(.z) <- names(.); .z})()
  })() |> 
  sum()

# Partie 2 ----

change_symbol <-
  function(.x){
    switch (.x,
            "(" = ")",
            "[" = "]",
            "{" = "}",
            "<" = ">"
    )
  }

solution2 <-
  lapply(
    sequences,
    function(.x){
      memoire = vector("character")
      res = ""
      for(i in 1:length(.x)){
        if(.x[i] %in% c("[","(","{","<")){
          memoire = append(memoire,.x[i])
        } else {
          
          if(.x[i] == "]"){
            if(memoire[length(memoire)] == "["){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == "}"){
            if(memoire[length(memoire)] == "{"){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == ")"){
            if(memoire[length(memoire)] == "("){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          } else if(.x[i] == ">"){
            if(memoire[length(memoire)] == "<"){
              memoire <- memoire[-length(memoire)] 
            } else {
              res = .x[i]
              break
            }
          }
        }
      }
      
      if(res == ""){
        res = memoire |> 
          rev() |> 
          lapply(change_symbol) |> 
          unlist() |> 
          # (\(.){paste(.,collapse = "")})() |> 
          (\(.){gsub(")","1",.)})() |>
          (\(.){gsub("]","2",.)})() |>
          (\(.){gsub("}","3",.)})() |>
          (\(.){gsub(">","4",.)})() |> 
          as.numeric()
        
        score = 0
        for(j in seq_along(res)){
          score = score*5+res[j]
        }
        res = score
      }
      
      
      res
      
    }) |> 
  unlist() |> 
  (\(.){.[grepl("\\d",.)]})() |> 
  as.numeric() |> 
  sort() |> 
  (\(.){.[ceiling(length(.)/2)]})()
