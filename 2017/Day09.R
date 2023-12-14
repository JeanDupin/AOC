# Input ----

input <-
  readLines("2017/Inputs/Day09.txt")

# Partie 1 ----


donnees <-
  gsub("!.","",input) |> 
  (\(.){gsub("<.*?>","",.)})() |> 
  (\(.){gsub("[^{}]","",.)})()
scores <- c()
for(i in 1:nchar(donnees)){
  if(i == 1){valeur = 0}
  
  caractere <-
    strsplit(donnees,"")[[1]][i]
  
  if(caractere == "{"){
    valeur = valeur + 1
  } else {
    scores <- append(scores,
                     valeur)
    valeur = valeur - 1
  }
}; rm(caractere, valeur, i)


solution1 <-
  sum(scores)


# Partie 2 ----

solution2 <-
  gsub("!.","",input) |> 
  (\(.){gsub("[^<]*<([^>]*)>[^<]*", "\\1", .)})() |> 
  nchar()
