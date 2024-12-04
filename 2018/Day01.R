# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

solution1 <-
  sum(input)

# Partie 2 ----
sortie = numeric()
i = 1
while(length(sortie) == 0){
  sortie <-
    c(0,cumsum(rep(input,i)))[duplicated(c(0,cumsum(rep(input,i))))]
  i <-
    i + 1
}

solution2 <-
  sortie[1]