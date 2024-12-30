# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/24/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

memoire = vector("list")

input[grepl(":",input)] |> 
  strsplit(": ") |> 
  lapply(function(.x){
    memoire[.x[1]] <<- as.numeric(.x[2])
  })

solve <- function(.x,.y,.z){
  switch(.z,
         "XOR" = ifelse(memoire[[.x]] != memoire[[.y]],1,0),
         "AND" = ifelse(memoire[[.x]] == 1 & memoire[[.y]] == 1,1,0),
         "OR" = ifelse(memoire[[.x]] == 1 | memoire[[.y]] == 1,1,0))
}

to_decimal <- function(sortie){
  res = vector("numeric")
  for(i in seq_along(sortie)){
    res[i] = sortie[i]*2**(i-1)
  }
  sum(res)
}


instructions <-
  input[grepl("(OR|XOR|AND)",input)] |> 
  (\(.){gsub("-> ","",.)})() |> 
  strsplit(" ")

while(length(instructions) > 0){
  etape = instructions[[1]]
  valeur = solve(etape[1],etape[3],etape[2])
  # print(etape)
  # print(valeur)
  if(length(valeur) == 0){
    instructions = append(instructions[-1],instructions[1])
    next
  }
  
  memoire[etape[4]] <- valeur
  instructions <- instructions[-1]
  
}

solution1 <-
  memoire[grepl("^z",names(memoire))] |> 
  (\(.){.[order(names(.))]})() |> 
  unlist() |> 
  as.numeric() |> 
  to_decimal()


# Partie 2 ----

to_bin <- function(.x){
  res = vector("numeric")
  number = .x
  while(number!=0){
    res = append(res,number %% 2)
    number = number %/% 2
  }
  return(rev(res))
}

solution2 <-
  memoire[grepl("^z",names(memoire))] |> 
  (\(.){.[order(names(.))]})() |> 
  unlist() |> 
  as.numeric()
