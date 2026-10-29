# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

next_step <- function(a){
  b <- 
    rev(strsplit(a,"")[[1]]) |> 
    (\(.){ifelse(. == "1","0","1")})() |> 
    paste(collapse = "")
  paste0(a,"0",b)
}

checksum <- function(x){
  mot = vector("character")
  for(i in seq_len(nchar(x)/2)){
    mot[i] <-
      strsplit(x,"")[[1]][c(2*i-1,2*i)] |> 
      paste(collapse = "")
  }
  ifelse(mot %in% c("11","00"),"1","0") |> 
    paste(collapse = "")
}

solve <- function(.x, .size){
  init = .x
  while(nchar(init) <= .size){
    init = next_step(init)
  }
  init = strsplit(init,"")[[1]][seq_len(.size)] |> 
    paste(collapse = "")
  while(nchar(init) %% 2 == 0){
    init = checksum(init)
  }
  init
}

solution1 <-
  solve(input, 272)

# Partie 2 ----

# Passage avec TRUE et FALSE pour pouvoir vectoriser les fonctions
# Reconversion en 1 et 0 à la fin

next_step <- function(x){
  c(x, F, rev(!x))
}

checksum <- function(x){
  n <- length(x)
  x[seq(1, n, 2)] == x[seq(2, n, 2)]
}

solve <- function(.x, .size){
  init <-
    strsplit(.x,"")[[1]] == "1"
  while(length(init) <= .size) {
    init <- next_step(init)
  }
  init = init[seq_len(.size)]
  while(length(init) %% 2 == 0){
    init = checksum(init)
  }
  paste(as.integer(init), collapse = "")
}

solution2 <-
  solve(input, 35651584)
