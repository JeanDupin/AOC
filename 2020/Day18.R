# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/18/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

res <- vector("numeric",length(input))
for(j in seq_along(input)){
  chaine <- input[j]

  while(grepl("\\(",chaine)){
    sequences <-
      regmatches(chaine,
        gregexpr("\\(\\d[^\\(]*?\\)",chaine))[[1]] 
    
    remplacements <-
      sequences |> 
      lapply(function(.input){
    
        operations <-
          regmatches(.input,
                     gregexpr("(\\+|\\*) \\d+",.input))[[1]]
        
        init <-
          regmatches(.input,
                     gregexpr("\\d+",.input))[[1]][1] |> 
          as.numeric()
        
        Reduce(
          function(.x,.y){
          as.numeric(eval(parse(text = paste0(.x,.y))))
          },
          operations,
          init = init
        )
    
      })
    
    sequences <-
      gsub("\\(","\\\\(",sequences) |> 
      (\(.){gsub("\\)","\\\\)",.)})() |> 
      (\(.){gsub("\\+","\\\\+",.)})() |> 
      (\(.){gsub("\\*","\\\\*",.)})()
    
    chaine <-
      Reduce(
        function(.x,i){
          gsub(sequences[i],remplacements[i],.x)
        },
        seq_along(sequences),
        init = chaine
      )
  }
  
  operations <-
    regmatches(chaine,
               gregexpr("(\\+|\\*) \\d+",chaine))[[1]]
        
  init <-
    regmatches(chaine,
               gregexpr("\\d+",chaine))[[1]][1] |> 
    as.numeric()
     
  res[j] <-
    Reduce(
      function(.x,.y){
      as.numeric(eval(parse(text = paste0(.x,.y))))
      },
      operations,
      init = init
    )
}

solution1 <-
  sum(unlist(res))

# Partie 2 ----

add_parentheses <- function(.x){
  gsub("(\\d+ \\+ \\d+)","(\\1)",.x)
}


res <- vector("numeric",length(input))
for(j in seq_along(input)){
  chaine <- add_parentheses(input[j])

  while(grepl("\\(",chaine)){
    sequences <-
      regmatches(chaine,
        gregexpr("\\(\\d[^\\(]*?\\)",chaine))[[1]] 
    
    remplacements <-
      sequences |> 
      lapply(function(.input){
    
        operations <-
          regmatches(.input,
                     gregexpr("(\\+|\\*) \\d+",.input))[[1]]
        
        init <-
          regmatches(.input,
                     gregexpr("\\d+",.input))[[1]][1] |> 
          as.numeric()
        
        Reduce(
          function(.x,.y){
          as.numeric(eval(parse(text = paste0(.x,.y))))
          },
          operations,
          init = init
        )
    
      })
    
    sequences <-
      gsub("\\(","\\\\(",sequences) |> 
      (\(.){gsub("\\)","\\\\)",.)})() |> 
      (\(.){gsub("\\+","\\\\+",.)})() |> 
      (\(.){gsub("\\*","\\\\*",.)})()
    
    chaine <-
      Reduce(
        function(.x,i){
          gsub(sequences[i],remplacements[i],.x)
        },
        seq_along(sequences),
        init = chaine
      ) |> 
      add_parentheses()
  }
  
  operations <-
    regmatches(chaine,
               gregexpr("(\\+|\\*) \\d+",chaine))[[1]]
        
  init <-
    regmatches(chaine,
               gregexpr("\\d+",chaine))[[1]][1] |> 
    as.numeric()
     
  res[j] <-
    Reduce(
      function(.x,.y){
      as.numeric(eval(parse(text = paste0(.x,.y))))
      },
      operations,
      init = init
    )
}

solution2 <-
  sum(unlist(res))