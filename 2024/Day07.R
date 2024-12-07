# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

symbols <-
  c("+", "*")

evaluate_left_to_right <-
  function(expression){
    
    tokens <-
      unlist(strsplit(expression, "(?<=[+*])|(?=[+*])", perl = TRUE))
    
    result <-
      as.numeric(tokens[1])
    
    for (i in seq(2, length(tokens), 2)) {
      operator <-
        tokens[i]
      operand <-
        as.numeric(tokens[i + 1])
      if (operator == "+") {
        result <- result + operand
      } else if (operator == "*") {
        result <- result * operand
      }
    }
    result
  }

solution1 <-
  lapply(
    input,
    function(.x){
      vecteur <-
        gsub("^.*? ","",.x) |> 
        (\(.){strsplit(.," ")[[1]]})() |> 
        as.numeric()
      
      valeur = as.numeric(gsub(":.*$","",.x))
      
      combinations <- expand.grid(rep(list(symbols), length(vecteur) - 1),
                                  stringsAsFactors = FALSE)
      
      apply(combinations, 1, function(row){
        paste(vecteur, c(row, ""), sep = "", collapse = "")
      }, simplify = F) |> 
        lapply(evaluate_left_to_right) |> 
        unlist() |> 
        (\(.){any(. == valeur)})()
      
    }
  ) |> 
  unlist() |> 
  (\(.){input[.]})() |> 
  (\(.){gsub(":.*$","",.)})() |> 
  as.numeric() |> 
  sum()


# Partie 2 ----

symbols <- c("+", "*", "X")


concat_numbers <-
  function(a, b){
    as.numeric(paste0(a, b))
  }

evaluate_left_to_right <-
  function(expression,valeur){
    
    tokens <-
      unlist(strsplit(expression, "(?<=[+*X])|(?=[+*X])", perl = TRUE))
    
    result <-
      as.numeric(tokens[1])
    
    for (i in seq(2, length(tokens), 2)) {
      operator <- tokens[i]
      operand <- as.numeric(tokens[i + 1])
      if(result > valeur){break}
      if (operator == "+"){
        result <- result + operand
      } else if (operator == "*"){
        result <- result * operand
      } else if (operator == "X"){
        result <- concat_numbers(result, operand)
      }
    }
    result
  }





solution2 <-
  lapply(
    input,
    function(.x){
      vecteur <-
        gsub("^.*? ","",.x) |> 
        (\(.){strsplit(.," ")[[1]]})() |> 
        as.numeric()
      
      valeur = as.numeric(gsub(":.*$","",.x))
      
      combinations <- expand.grid(rep(list(symbols), length(vecteur) - 1),
                                  stringsAsFactors = FALSE)
      
      expressions <-
        apply(combinations, 1, function(row){
          paste(vecteur, c(row, ""), sep = "", collapse = "")
        }, simplify = F)
      
      for(j in seq_along(expressions)){
        res = evaluate_left_to_right(expressions[[j]], valeur = valeur) 
        if(res == valeur){
          res = T
          break
        }else{
          res = F
          next
        }
      }
      
      res |> 
        print()
      
      res
      
    }
  ) |> 
  unlist() |> 
  (\(.){input[.]})() |> 
  (\(.){gsub(":.*$","",.)})() |> 
  as.numeric() |> 
  sum()

