# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrices <-
  vector("list",nchar(input)/(25*6))

nombres <-
  strsplit(input,"")[[1]] |> 
  as.numeric() |> 
  (\(.){split(.,ceiling(seq_along(.)/(25*6)))})()

for(i in seq_along(matrices)){
  matrices[[i]] <-
    matrix(nombres[[i]],
           ncol = 25,
           byrow = T)
}; rm(i)

solution1 <-
  lapply(
    matrices,
    function(.x){sum(.x == 0)}
  ) |> 
  unlist() |> 
  which.min() |> 
  (\(.){matrices[[.]]})() |> 
  (\(.){sum(. == 1) * sum(. == 2)})()

# Partie 2 ----

res = matrix(ncol = 25, nrow = 6)

for(i in seq_len(6)){
  for(j in seq_len(25)){
    res[i,j] <-
    lapply(matrices,function(.x){.x[i,j]}) |> 
      unlist() |>
      (\(.){.[. != 2][1]})()
  }
}

solution2 <-
  image(t(res[nrow(res):1,]), col=c('white','black'), xaxt='n',yaxt='n',bty="n")
