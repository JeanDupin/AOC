# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

`%notin%` <- Negate(`%in%`)


# Partie 1 ----

# On va tester l'opération : si elle est réalisable, on la fait, sinon on la renvoie en fin de liste
while(length(input) > 0){
  
  lettres.necessaires <-
    strsplit(input[1]," -> ")[[1]][1] |> 
    (\(.){regmatches(.,gregexpr("[a-z]+",.))[[1]]})()
  
  # Si on a pas besoin de lettres
  if(length(lettres.necessaires) == 0){
    # 1 cas : l'association directe ... -> A
    lettre = regmatches(input[1],gregexpr("[a-z]+$",input[1]))[[1]]
    valeur = regmatches(input[1],gregexpr("^[0-9]+",input[1]))[[1]] |> as.numeric()
    assign(lettre, valeur, envir = .GlobalEnv)
    input <- input[-1]
    next
  }
  # Si on n'a pas les lettres nécessaires en stock, on renvoie en fin
  if(!all(sapply(lettres.necessaires, function(.x){.x %in% ls(envir = .GlobalEnv)}))){
    input[length(input) + 1] <- input[1]
    input <- input[-1]
    next
  }
  # Si on a les lettres, on fait l'opération
  if(all(sapply(lettres.necessaires, function(.x){.x %in% ls(envir = .GlobalEnv)}))){
    lettre = regmatches(input[1],gregexpr("[a-z]+$",input[1]))[[1]]
    # Cas 1 : LSHIFT
    if(grepl("LSHIFT",input[1])){
      valeur = bitwShiftL(as.numeric(get(lettres.necessaires[1])),
                          regmatches(input[1],gregexpr("LSHIFT [0-9]+",input[1]))[[1]] |> 
                            (\(.){as.numeric(gsub("LSHIFT ","",.))})())
    }
    # Cas 2 : RSHIFT
    if(grepl("RSHIFT",input[1])){
    valeur = bitwShiftR(get(lettres.necessaires[1]),
                        regmatches(input[1],gregexpr("RSHIFT [0-9]+",input[1]))[[1]] |> 
                          (\(.){as.numeric(gsub("RSHIFT ","",.))})())}
    # Cas 3 : AND avec un nombre
    if(grepl("1 AND", input[1])){
    valeur = bitwAnd(1,
                     get(lettres.necessaires[1]))}
    # Cas 4 : AND avec 2 lettres
    if(grepl("[a-z] AND", input[1])){
    valeur = bitwAnd(get(lettres.necessaires[1]),
                     get(lettres.necessaires[2]))}
    # Cas 5 : OR
    if(grepl("OR",input[1])){
    valeur = bitwOr(get(lettres.necessaires[1]),
                     get(lettres.necessaires[2]))}
    # Cas 6 : NOT
    if(grepl("NOT",input[1])){
    valeur = bitwNot(get(lettres.necessaires[1]))}
    # Cas 7 : association directe
    if(!grepl("[A-Z]",input[1])){
    valeur = get(lettres.necessaires[1])}
    
    if(valeur < 0){valeur = 65536 + valeur}
    
    assign(lettre, valeur, envir = .GlobalEnv)
    input <- input[-1]
    next
  }
}

rm(list = ls()[ls() != "a"])

solution1 <-
  a

# Partie 2 ----

input <-
  httr2::request("https://adventofcode.com/2015/day/7/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

input <-
  c("956 -> b",
    sort(input[input != "14146 -> b"]))

while(length(input) > 0){
  
  lettres.necessaires <-
    strsplit(input[1]," -> ")[[1]][1] |> 
    (\(.){regmatches(.,gregexpr("[a-z]+",.))[[1]]})()
  
  # Si on a pas besoin de lettres
  if(length(lettres.necessaires) == 0){
    # 1 cas : l'association directe ... -> A
    lettre = regmatches(input[1],gregexpr("[a-z]+$",input[1]))[[1]]
    valeur = regmatches(input[1],gregexpr("^[0-9]+",input[1]))[[1]] |> as.numeric()
    assign(lettre, valeur, envir = .GlobalEnv)
    input <- input[-1]
    next
  }
  # Si on n'a pas les lettres nécessaires en stock, on renvoie en fin
  if(!all(sapply(lettres.necessaires, function(.x){.x %in% ls(envir = .GlobalEnv)}))){
    input[length(input) + 1] <- input[1]
    input <- input[-1]
    next
  }
  # Si on a les lettres, on fait l'opération
  if(all(sapply(lettres.necessaires, function(.x){.x %in% ls(envir = .GlobalEnv)}))){
    lettre = regmatches(input[1],gregexpr("[a-z]+$",input[1]))[[1]]
    # Cas 1 : LSHIFT
    if(grepl("LSHIFT",input[1])){
      valeur = bitwShiftL(as.numeric(get(lettres.necessaires[1])),
                          regmatches(input[1],gregexpr("LSHIFT [0-9]+",input[1]))[[1]] |> 
                            (\(.){as.numeric(gsub("LSHIFT ","",.))})())
    }
    # Cas 2 : RSHIFT
    if(grepl("RSHIFT",input[1])){
      valeur = bitwShiftR(get(lettres.necessaires[1]),
                          regmatches(input[1],gregexpr("RSHIFT [0-9]+",input[1]))[[1]] |> 
                            (\(.){as.numeric(gsub("RSHIFT ","",.))})())}
    # Cas 3 : AND avec un nombre
    if(grepl("1 AND", input[1])){
      valeur = bitwAnd(1,
                       get(lettres.necessaires[1]))}
    # Cas 4 : AND avec 2 lettres
    if(grepl("[a-z] AND", input[1])){
      valeur = bitwAnd(get(lettres.necessaires[1]),
                       get(lettres.necessaires[2]))}
    # Cas 5 : OR
    if(grepl("OR",input[1])){
      valeur = bitwOr(get(lettres.necessaires[1]),
                      get(lettres.necessaires[2]))}
    # Cas 6 : NOT
    if(grepl("NOT",input[1])){
      valeur = bitwNot(get(lettres.necessaires[1]))}
    # Cas 7 : association directe
    if(!grepl("[A-Z]",input[1])){
      valeur = get(lettres.necessaires[1])}
    
    if(valeur < 0){valeur = 65536 + valeur}
    
    assign(lettre, valeur, envir = .GlobalEnv)
    input <- input[-1]
    next
  }
}

rm(list = ls()[ls() != "a"])

solution2 <-
  a
