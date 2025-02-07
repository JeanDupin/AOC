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

solution2 <-
  NA
