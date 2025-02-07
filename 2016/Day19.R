# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/19/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

chaine = seq_len(input)
lastid = F
while(length(chaine) > 1){
  if(lastid){
    chaine = chaine[-1]
  }
  indices <-
    as.logical(seq_along(chaine) %% 2)
  lastid = rev(indices)[1]
  chaine <- chaine[indices]
}

solution1 <-
  chaine

# Partie 2 ----

first_to_last <- function(.x){
  c(.x[-1],.x[1])
}

chaine = seq_len(input)
while(length(chaine) > 1){
  chaine <-
    first_to_last(chaine[-ceiling(1 + length(chaine[-1]) /2)])
}

solution2 <-
  chaine