# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  expand.grid(as.numeric(input),as.numeric(input)) |> 
  (\(.){.[which(rowSums(.) == 2020)[1],]})() |> 
  (\(.){.[1,1] * .[1,2]})()

# Partie 2 ----

solution2 <-
  expand.grid(as.numeric(input),as.numeric(input),as.numeric(input)) |> 
  (\(.){.[which(rowSums(.) == 2020)[1],]})() |> 
  (\(.){.[1,1] * .[1,2] * .[1,3]})()
