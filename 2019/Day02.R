# Input ----

input <-
  httr2::request("https://adventofcode.com/2019/day/2/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,",")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

commandes <-
  input
commandes[2] = 12
commandes[3] = 2

i = 1
while(commandes[i] != 99){
  if(commandes[i] == 1){
    commandes[(commandes[i+3]+1)] = commandes[(commandes[i+1]+1)] + commandes[(commandes[i+2]+1)]
  } else {
    commandes[(commandes[i+3]+1)] = commandes[(commandes[i+1]+1)] * commandes[(commandes[i+2]+1)]
  }
  i = i + 4
}

solution1 <-
  commandes[1]

# Partie 2 ----

combinaisons <-
  expand.grid(0:99,0:99)

res <- vector("numeric",nrow(combinaisons))

for(j in seq_len(nrow(combinaisons))){
  commandes <-
    input
  commandes[2] = combinaisons[j,"Var1"]
  commandes[3] = combinaisons[j,"Var2"]
  
  i = 1
  while(commandes[i] != 99){
    if(commandes[i] == 1){
      commandes[(commandes[i+3]+1)] = commandes[(commandes[i+1]+1)] + commandes[(commandes[i+2]+1)]
    } else {
      commandes[(commandes[i+3]+1)] = commandes[(commandes[i+1]+1)] * commandes[(commandes[i+2]+1)]
    }
    i = i + 4
  }
  
  res[j] = commandes[1]
  
}


solution2 <-
  100*combinaisons[which(res == 19690720),"Var1"] + combinaisons[which(res == 19690720),"Var2"]
