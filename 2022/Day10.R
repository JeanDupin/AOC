# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

instructions <-
  lapply(input, function(.x){
    if(grepl("noop",.x)){
      0
    } else {
      list(
        0,
        regmatches(.x,
                   gregexpr("(-|)\\d+",.x))[[1]] |> 
          as.numeric()
      )
    }
  }) |> 
  unlist()

X = cumsum(c(1,instructions))

solution1 <-
  c(20,60,100,140,180,220) |> 
  (\(.){. * X[.]})() |> 
  sum()

# Partie 2 ----

modR <- function(x){
  if(x <= 0){
    x
  } else if(x %% 40 == 0){
    40
  } else {
    x %% 40
  }
}

CRT <- vector("character")

for(i in seq_along(instructions)){
  if(modR(i) %in% c(modR(X[i]):(modR(X[i])+2))){
    CRT[i] = "#"
  } else {
    CRT[i] = "."
  }
}

solution2 <-
  CRT