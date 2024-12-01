# Input ----

input <-
  httr2::request("https://adventofcode.com/2017/day/9/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

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
