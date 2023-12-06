# Input ----

# On utilise la table des 92 éléments de Conway
conway <-
  read.table("2015/Inputs/Day10.csv",
             sep = ";", header = T,
             colClasses = "character")
conway$id <- 1:nrow(conway)
input <-
  conway[conway$Sequence == "3113322113",
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
      strsplit("\\.") |> 
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
    strsplit("\\.") |> 
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
