# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/9/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

sortie = c()
for(i in seq_along(input)){
  # On crée la séquence des vecteurs
  seq.init <- input[i] |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() 
  vecteurs <- list(seq.init)
  while(!all(seq.init == 0)){
    seq.init = diff(seq.init)
    vecteurs <- append(vecteurs,
                       list(seq.init))
  }
  rm(seq.init)
  vecteurs <- rev(vecteurs)
  # On recalcule le dernier terme à chaque fois
  
  x = 0
  for(j in seq_along(vecteurs)){
    x <- x + vecteurs[[j]][length(vecteurs[[j]])]
  }
  
  sortie[i] = x
  rm(x)
  
}; rm(i, j)


solution1 <-
  sum(sortie)


# Partie 2 ----

sortie = c()
for(i in seq_along(input)){
  # On crée la séquence des vecteurs
  seq.init <- input[i] |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() 
  vecteurs <- list(seq.init)
  while(!all(seq.init == 0)){
    seq.init = diff(seq.init)
    vecteurs <- append(vecteurs,
                       list(seq.init))
  }
  rm(seq.init)
  vecteurs <- rev(vecteurs)
  # On recalcule le dernier terme à chaque fois
  
  x = 0
  for(j in seq_along(vecteurs)){
    x <- vecteurs[[j]][1] - x
  }
  
  sortie[i] = x
  rm(x)
  
}; rm(i, j)


solution2 <-
  sum(sortie)
