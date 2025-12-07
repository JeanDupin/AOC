# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/6/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

chiffres <-
  lapply(input,
         function(.x){strsplit(gsub("^\\s+","",.x),"\\s+")[[1]]})

res = 0
for(i in seq_len(length(chiffres[[1]]))){
  if(chiffres[[5]][i] == "*"){
    res <- res + prod(
      as.numeric(chiffres[[1]][i]),
      as.numeric(chiffres[[2]][i]),
      as.numeric(chiffres[[3]][i]),
      as.numeric(chiffres[[4]][i])
    )
  } else {
    res <- res + sum(
      as.numeric(chiffres[[1]][i]),
      as.numeric(chiffres[[2]][i]),
      as.numeric(chiffres[[3]][i]),
      as.numeric(chiffres[[4]][i])
    )
  }
}



solution1 <-
  res

# Partie 2 ----

operations <-
  strsplit(input[5],"\\s+")[[1]] |> 
  rev()

chiffres <- 
  input[1:4] |> 
  strsplit("") |> 
  lapply(rev)

memoire <- vector("numeric")
index = 1
res = 0
for(i in seq_along(chiffres[[1]])){
  if(
    !all(c(chiffres[[1]][i],chiffres[[2]][i],chiffres[[3]][i],chiffres[[4]][i]) == " ") & i!= length(chiffres[[1]])
  ){
    memoire <-
      append(memoire,
             paste(c(chiffres[[1]][i],chiffres[[2]][i],chiffres[[3]][i],chiffres[[4]][i]),
                   collapse = "") |> 
               (\(.){gsub("\\s+","",.)})() |> 
               as.numeric())
  } else if(all(c(chiffres[[1]][i],chiffres[[2]][i],chiffres[[3]][i],chiffres[[4]][i]) == " ")) {
    operation <- operations[index]
    if(operation == "*"){
      res = res + prod(memoire)
    } else {
      res = res + sum(memoire)
    }
    memoire <- vector("numeric")
    index = index + 1
  } else {
    memoire <-
      append(memoire,
             paste(c(chiffres[[1]][i],chiffres[[2]][i],chiffres[[3]][i],chiffres[[4]][i]),
                   collapse = "") |> 
               (\(.){gsub("\\s+","",.)})() |> 
               as.numeric())
    operation <- operations[index]
    if(operation == "*"){
      res = res + prod(memoire)
    } else {
      res = res + sum(memoire)
    }
    memoire <- vector("numeric")
  }
}
  
solution2 <-
  res
