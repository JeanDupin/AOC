# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

positions <-
  regmatches(input,
            gregexpr("(-|)\\d+",input)) |> 
  lapply(function(.x){as.numeric(.x[1:2])*c(1,-1)}) |> 
  (\(.){do.call(rbind,.)})()

vitesses <-
  regmatches(input,
            gregexpr("(-|)\\d+",input)) |> 
  lapply(function(.x){as.numeric(.x[3:4])*c(1,-1)}) |> 
  (\(.){do.call(rbind,.)})()

n = 100
res = 100

while(n == res){
  n = n * 10
  variances <- vector("list",n)

  for(i in seq_len(n)){
    variances[[i]] <-
      (positions + i*vitesses) |> 
      data.frame() |> 
      (\(.){c(var(.[,1]),var(.[,2]))})()
  }

  res <-
    do.call(rbind,variances) |> 
    (\(.){c(which.min(.[,1]),which.min(.[,2]))})() |> 
     unique()

}


solution1 <-
  (positions + res*vitesses) |> 
  data.frame() |> 
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(X1,X2))

# Partie 2 ----

solution2 <-
  res