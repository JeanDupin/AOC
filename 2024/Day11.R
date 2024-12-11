# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/11/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

diviser_mot <-
  function(mot){
    longueur = nchar(mot)
    
    point_division = ceiling(longueur / 2)
    
    partie1 <- 
      substr(mot, 1, point_division)
    partie2 <-
      substr(mot, point_division + 1, longueur)
    
    c(partie1, partie2)
  }

res <-
  strsplit(input," ")[[1]] |> 
  as.numeric()

for(i in seq_len(25)){
  res <-
    lapply(
      res,
      function(.x){
        if(.x == 0){
          1
        } else if(nchar(.x) %% 2 == 0){
          as.numeric(diviser_mot(.x))
        } else {
          .x * 2024
        }
      }
    ) |> 
    unlist()
}

solution1 <-
  length(res)

# Partie 2 ----

get_next <-
  function(.x){
    if(.x == 0){
      as.character(1)
    } else if(nchar(.x) %% 2 == 0){
      gsub("^[0]+","",diviser_mot(.x)) |> 
        (\(.){ifelse(. == "","0",.)})()
    } else {
      as.character(as.numeric(.x) * 2024)
    }
  }

add_memoire <-
  function(id,facteur = 0){
    if(is.null(memoire[[id]])){
      memoire[[id]] <<- 1
    } else {
      memoire[[id]] <<-
        memoire[[id]]+facteur
    }
  }

add_memoire2 <-
  function(id,facteur = 1){
    if(is.null(memoire2[[id]])){
      memoire2[[id]] <<- facteur
    } else {
      memoire2[[id]] <<-
        memoire2[[id]]+facteur
    }
  }

remove_memoire <-
  function(id){
    memoire[[id]] <- 0
  }

memoire = list()
for(i in strsplit(input," ")[[1]]){add_memoire(i)}

for(k in seq_len(75)){
  memoire2 = list()
  for(i in names(memoire)){
    for(j in get_next(i)){
      add_memoire2(j,memoire[[i]])
    }
  }
  memoire <- memoire2
}


solution2 <-
  sum(unlist(memoire))
