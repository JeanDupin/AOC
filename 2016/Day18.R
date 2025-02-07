# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/18/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,"")[[1]]})()

# Partie 1 ----

piege <- function(.x){
  if(.x %in% c("^^.",".^^","^..","..^")){
    "^"
  } else {
    "."
  }
}

chemin = vector("list",40)
chemin[[1]] = input
for(i in seq_len(40-1)){
  state = chemin[[i]]
  next_state = state
  for(j in seq_along(state)){
    if(j == 1){
      chaine = c(".",state[c(j,j+1)])
    } else if(j == length(state)){
      chaine = c(state[c(j-1,j)],".")
    } else {
      chaine = state[c(j-1,j,j+1)]
    }
    chaine = paste(chaine, collapse = "")
    next_state[j] = piege(chaine)
  }
  chemin[[i+1]] = next_state
}

solution1 <-
  sum(unlist(chemin) == ".")

# Partie 2 ----

chemin = vector("list",400000)
chemin[[1]] = input
for(i in seq_len(400000-1)){
  state = chemin[[i]]
  next_state = state
  for(j in seq_along(state)){
    if(j == 1){
      chaine = c(".",state[c(j,j+1)])
    } else if(j == length(state)){
      chaine = c(state[c(j-1,j)],".")
    } else {
      chaine = state[c(j-1,j,j+1)]
    }
    chaine = paste(chaine, collapse = "")
    next_state[j] = piege(chaine)
  }
  chemin[[i+1]] = next_state
}

solution2 <-
  sum(unlist(chemin) == ".")
