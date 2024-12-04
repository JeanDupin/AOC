# Input ----

# On utilise la table des 92 éléments de Conway
input <-
  get_input("https://adventofcode.com/2015/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 
source("2015/Day10b.R")
input <-
  conway[conway$Sequence == input,
         "Element"]

n <-
  40

# Partie 1 ----

for(i in 1:n){
  if(i == 1){init = input}
  
  init <-
    sapply(init,
           function(.x){
             conway[conway$Element == .x,
                    "id"]
           },
           USE.NAMES = F) |> 
      (\(.){conway[.,"Transformation"]})() |> 
      strsplit(" ") |>
      unlist()
  
}; rm(i)


solution1 <-
  sapply(init,
       function(.x){
         conway[conway$Element == .x,
                "id"]
       },
       USE.NAMES = F) |> 
  (\(.){conway[.,"Sequence"]})() |> 
  nchar() |> 
  sum()

# Partie 2 ----


n <-
  50

for(i in 1:n){
  if(i == 1){init = input}
  
  init <-
    sapply(init,
           function(.x){
             conway[conway$Element == .x,
                    "id"]
           },
           USE.NAMES = F) |> 
    (\(.){conway[.,"Transformation"]})() |> 
    strsplit(" ") |> 
    unlist()
  
}; rm(i)


solution2 <-
  sapply(init,
         function(.x){
           conway[conway$Element == .x,
                  "id"]
         },
         USE.NAMES = F) |> 
  (\(.){conway[.,"Sequence"]})() |> 
  nchar() |> 
  sum()

