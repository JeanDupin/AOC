# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/17/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

instructions = as.integer(input)

res = vector("integer",length(instructions))

for(i in seq_along(instructions)){
  res[i] <-
    t(combn(instructions,i)) |> 
    (\(.){sum(rowSums(.) == 150)})()
}; rm(i)

solution1 <-
  sum(res)

# Partie 2 ----

solution2 <-
  res[res != 0][1]
