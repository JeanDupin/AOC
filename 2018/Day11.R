# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/11/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

input = 18

power <- matrix(nrow = 300, ncol = 300)

for(i in seq_len(300)){
  for(j in seq_len(300)){

    power[i,j] <-
      ((((((i + 10)*j + input)*(i+10)) %/% 100) %% 10) - 5)

  }
}


powerbis <- matrix(nrow = 300, ncol = 300)

for(i in seq_len(300)){
  for(j in seq_len(300)){
    powerbis[i,j] <-
      cbind(
        c(i,i,i,i+1,i+1,i+1,i+2,i+2,i+2),
        c(j,j+1,j+2,j,j+1,j+2,j,j+1,j+2)) |> 
      (\(.){.[.[,1] <= 300 & .[,2] <= 300,]})() |> 
      (\(.){
        power[.]
      })() |> 
      sum()

  }
}

solution1 <-
  paste(which(powerbis == max(powerbis), arr.ind = T),
        collapse = ",")

# Partie 2 ----

res <- vector("list",300)
powerbis <- matrix(nrow = 300, ncol = 300)

coords <- expand.grid(1:300, 1:300) |> as.matrix()


size = 150
i = j = 15
i = 3
j = 150
for(size in seq_len(300)[150]){
  if(size %% 10 == 0){print(size)}
  powerter <- vector("integer",300*300)
  k = 1
  for(i in seq_len(300)){
    if(i %% 10 == 0){print(i)}
    for(j in seq_len(300)){
      powerter[k] <-
        coords[coords[,1] >= i & coords[,1] <= i + size - 1 & coords[,2] >= j & coords[,2] <= j + size - 1,] |> 
        (\(.){
          power[.]
        })() |> 
        sum()
        k = k + 1
    }
  }

  res[[size]] <-
    list(
      max(powerbis),
      paste(which(powerbis == max(powerbis), arr.ind = T),
        collapse = ",") |> 
        paste(max(powerbis), sep = ",")
    )

}


solution2 <-
  paste(which(powerbis == max(powerbis), arr.ind = T),
        collapse = ",")