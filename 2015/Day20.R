# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/20/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----


maison <- function(.x){
  maisons = vector("numeric",1000000)
  for(i in 1:1000000){
    for(j in seq(i, 1000000, by = i)){
      maisons[j] <- maisons[j] + 10 * i
    }
  }
  which(maisons >= .x)[1]
}

solution1 <-
  maison(input)

# Partie 2 ----


maison2 <- function(.x) {
  maisons = vector("numeric",1000000)
  for(i in 1:1000000){
    maximum <- min(1000000, 50 * i)
    for(j in seq(i, maximum, by = i)){
      maisons[j] <- maisons[j] + 11 * i
    }
  }
  which(maisons >= .x)[1]
}

solution2 <-
  maison2(input)

