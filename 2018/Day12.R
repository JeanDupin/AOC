# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

position <-
  gsub("^.*: ","",input[1]) |> 
  (\(.){strsplit(.,"")[[1]]})()

swaps <-
  input[seq_along(input)[-c(1,2)]] |> 
  (\(.){strsplit(.," => ")})() |> 
  sapply(function(.x){
    .x[2] |> 
      (\(.){names(.) <- .x[1]; .})()
  }, simplify = T, USE.NAMES = F)


state <- function(.x){
  if(!.x %in% names(swaps)){
    "."
  } else {
    swaps[[.x]]
  }
}

left <- 0
build_position <- function(.x){
  id1 <-
    paste(.x, collapse = "") |> 
    (\(.){
      regmatches(.,
                gregexpr("^\\.*",.))[[1]]
    })() |> 
    nchar() |> 
    (\(.){4 - .})()
  left <<- left + id1
  id2 <-
    paste(.x, collapse = "") |> 
    (\(.){
      regmatches(.,
                gregexpr("\\.*$",.))[[1]]
    })() |> 
    nchar() |> 
    (\(.){4 - .})()
  sortie <-
    if(id1 >= 0){
      c(rep(".",id1),.x)
    } else {
      .x[-seq_len(abs(id1))]
    }
  sortie <-
    if(id2 >= 0){
      c(sortie, rep(".",id2))
    } else {
      sortie[-((length(sortie)+id2-1):length(sortie))]
    }
  sortie
}



for(k in seq_len(2000)){
  position = build_position(position)
  next_position = position
  for(i in seq_along(position)[-c(1,2,length(position)-1,length(position))]){
    next_position[i] = state(paste(position[(i-2):(i+2)], collapse = ""))
  }
  position = next_position
}

solution1 <-
  sum(which(position == "#") - left - 1)

# Partie 2 ----

solution2 <-
  NA