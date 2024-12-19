# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/19/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

patterne <-
  strsplit(input[1],", ")[[1]] |> 
  paste(collapse = "|") |> 
  (\(.){paste0("^(",.,")*$")})()

solution1 <-
  sum(grepl(patterne,input[3:length(input)]))

# Partie 2 ----

count_combinations <- function(mot, elements){
  n = nchar(mot)
  memoire = vector("numeric", n+1)
  memoire[1] = 1 

  for(j in seq_len(n)){
    for(k in elements){
      taille = nchar(k)
      if(j >= taille){
        sous_mot <-
          substr(mot, j - taille + 1, j)
        if(sous_mot == k){
          memoire[j + 1] <-
            memoire[j + 1] + memoire[j - taille + 1]
        }
      }
    }
  }
  memoire[n + 1]
}



res = vector("list")
for(i in input[3:length(input)][grepl(patterne,input[3:length(input)])]){
  patterne <-
    strsplit(input[1],", ")[[1]] |> 
    lapply(function(.x){grepl(.x,i)}) |> 
    unlist() |> 
    (\(.){strsplit(input[1],", ")[[1]][.]})()

  res = append(res,count_combinations(i,patterne))
}

solution2 <-
  sum(unlist(res))
