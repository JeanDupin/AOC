# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

signal <-
  as.numeric(strsplit(input,"")[[1]])

patterne_ini <-
  c(0,1,0,-1)

j = 1
while(j <= 100){
  temp <- signal
  for(i in seq_along(signal)){
    patterne <-
      sapply(patterne_ini,function(.x){rep(.x,i)},simplify = F) |>
      unlist() |> 
      rep(length.out = (nchar(input)+1))
    temp[i] <- abs(sum(signal*patterne[-1])) %% 10
  }
  signal <- temp
  j = j + 1
}



solution1 <-
  paste(signal[1:8], collapse = "")

# Partie 2 ----

solution2 <-
  NA
