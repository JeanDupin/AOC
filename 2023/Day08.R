# Input ----

input <-
  get_input("https://adventofcode.com/2023/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

instructions <-
  input[1] |> 
  (\(.){strsplit(.,"")[[1]]})() |> 
  (\(.){gsub("L","1",gsub("R","2",.))})()

moves <-
  paste(
    substr(input[3:length(input)],8,10),
    substr(input[3:length(input)],13,15),
    sep = ";") |> 
    strsplit(";") |> 
    (\(.){
      `names<-`(.,
                substr(input[3:length(input)],1,3))
    })()


position <- "AAA"

i = 1
n = 0
while(position != "ZZZ"){
  
  position <-
    moves[[position]][as.numeric(instructions[i])]
  
  if(i == length(instructions)){
    i = 1
  } else {i = i + 1}
  n = n + 1
}; rm(position)

solution1 <-
  n; rm(n, i, j)


# Partie 2 ----

positions <-
  names(moves)[grepl("A$",names(moves))]


gcd <- function(.x, .y) {
  while (.y != 0) {
    a <- .y
    .y <- .x %% .y
    .x <- a
  }
  return(abs(.x))
}
lcm <- function(.x, .y) {
  return(abs(.x) * abs(.y) / gcd(abs(.x), abs(.y)))
}


cycles <- c()
for(j in seq_along(positions)){
  position = positions[j]
  i = 1
  n = 0
  while(!grepl("Z$",position)){
    position <-
      moves[[position]][as.numeric(instructions[i])]
    if(i == length(instructions)){
      i = 1
    } else {i = i + 1}
    n = n + 1
  }
  cycles[j] <- n
}; rm(n, i, j, position)


solution2 <-
  as.character(Reduce(lcm, cycles))



