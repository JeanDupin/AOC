# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  regmatches(input,
            gregexpr("(-|)\\d+",input))[[1]] |> 
  as.numeric() |> 
  sum()

# Partie 2 ----

texte <-
  jsonlite::fromJSON(input,
                     simplifyVector = F)

sum_json <- function(x){
  
  if(is.null(x)){return(0)}
  if(is.numeric(x)){return(sum(x))}
  if(is.integer(x)){return(sum(as.numeric(x)))}
  if(is.atomic(x) & is.character(x)){return(0)}
  if(is.logical(x)){return(0)}
  
  if(is.list(x)){
    nom <- names(x)
    if(!is.null(nom)){
      for(i in x){
        if(is.character(i) && any(i == "red")){
          return(0)
        }
      }
    }
    suite <- 0
    for(i in x){
      suite <- suite + sum_json(i)
      }
    return(suite)
  }
  return(0)
}

solution2 <-
  sum_json(parsed)
