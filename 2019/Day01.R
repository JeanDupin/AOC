# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

solution1 <-
  sum(floor(input/3)-2)

# Partie 2 ----

solution2 <-
  lapply(
    input,
    function(.x){
      poids = .x
      fuel = vector("numeric")
      while(poids > 0){
        poids = floor(poids/3)-2
        fuel = append(fuel, poids)
      }
      sum(fuel[-length(fuel)])
    }
  ) |> 
  unlist() |> 
  sum()