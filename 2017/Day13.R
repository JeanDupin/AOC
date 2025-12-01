# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/13/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

dist <-
  regmatches(input[length(input)],
             gregexpr("^\\d+",input[length(input)]))[[1]] |> 
  (\(.){as.numeric(.) + 1})()

solution1 <-
  lapply(input, function(.x){
    ids <-
      regmatches(.x,
                gregexpr("\\d+",.x))[[1]] |> 
      (\(.){as.numeric(.) + c(1,0)})()

    rep_len(list(seq_len(ids[2]),rev(seq_len(ids[2]))[-c(1,ids[2])]),dist) |> 
      unlist() |> 
      (\(.){.[ids[1]]})() |> 
      (\(.){ifelse(. == 1, (ids[1]-1) * ids[2],0)})()
  }) |> 
  (\(.){sum(unlist(.))})()


# Partie 2 ----

layers <- sapply(input,
                 function(.x){
                   regmatches(.x,
                              gregexpr("\\d+",.x))[[1]] |> 
                     as.numeric()
                   }) |> 
  t()
depths <-
  layers[,1]
ranges <-
  layers[,2]
periods = 2 * (ranges - 1)

delay = 0

repeat{
  if(!any(((depths + delay) %% periods) == 0)){break}
  delay <- delay + 1L
}

solution2 <-
  delay

