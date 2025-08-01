# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/24/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

balance <- function(n.groupes){
  for(i in seq_along(input)){
    combinaisons <-
      combn(input, i, simplify = F)
    
    if(!any(sapply(combinaisons, sum) == sum(input)/n.groupes)){
      next
    } else {
      combinaisons <-
        sapply(combinaisons[which(sapply(combinaisons, sum) == sum(input)/n.groupes)], prod)
      break
    }
  }
  min(combinaisons)
}

solution1 <-
  balance(3)
  

# Partie 2 ----

solution2 <-
  balance(4)
