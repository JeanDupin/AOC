# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/14/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  sapply(input, function(.x){
    vitesses <-
      regmatches(.x,
                gregexpr("\\d+",.x))[[1]] |> 
      as.numeric()

    distance = vector("integer",vitesses[2] + vitesses[3])
    distance[seq_len(vitesses[2])] = vitesses[1]
    distance |> 
      rep_len(2503) |> 
      (\(.){cumsum(.)[2503]})()
  }) |> 
  max()


# Partie 2 ----

distances <-
  lapply(input, function(.x){
    vitesses <-
      regmatches(.x,
                gregexpr("\\d+",.x))[[1]] |> 
      as.numeric()

    distance = vector("integer",vitesses[2] + vitesses[3])
    distance[seq_len(vitesses[2])] = vitesses[1]
    distance |> 
      rep_len(2503) |> 
      cumsum()
  })

scores = vector("integer",length(input))

for(i in seq_len(2503)){
  sapply(distances, function(.x){.x[i]}) |> 
    (\(.){which(. == max(.))})() |> 
    (\(.){scores[.] <<- scores[.] + 1; .})()
}

solution2 <-
  max(scores)
