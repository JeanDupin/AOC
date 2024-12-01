# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/10/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

n.col = nchar(input[1])
n.row = length(input)

# Étape 1 - Détection de la position de départ

depart <-
  gregexpr("S",input) |> 
    unlist() |> 
    (\(.){
      paste(
        # Colonne
        .[. != -1],
        # Ligne
        which(. != -1),
        sep = ";"
      )
    })()


# Étape 2 - On cherche une des tuiles adjecentes reliée
## Haut
if(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]] != 1){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) - 1)]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]])]
  if(lettre %in% c("|","7","F")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]),
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) - 1,
            sep = ";")
  }
}
## Bas
if(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]] != n.row){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) + 1)]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]])]
  if(lettre %in% c("|","L","J")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]),
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]) + 1,
            sep = ";")
    
  }
}
## Droite
if(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]] != n.col){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]))]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) + 1]
  if(lettre %in% c("-","J","7")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) + 1,
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]),
            sep = ";")
  }
}
## Gauche
if(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]] != 1){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]))]],"")[[1]][as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) - 1]
  if(lettre %in% c("-","F","L")){
    position <-
      paste(as.numeric(regmatches(depart,gregexpr("^[0-9]+",depart))[[1]]) - 1,
            as.numeric(regmatches(depart,gregexpr("[0-9]+$",depart))[[1]]),
            sep = ";")
    
  }
}; rm(lettre)


positions <- c(depart, position)
# Étape 3 - Tant qu'on ne revinet pas au départ, on fait le tour et on compte le nombre d'étapes

next_moves <- function(.l,.x,.y){
  if(.l == "|"){
    return(
      paste(.x,c(.y-1,.y+1),sep=";")
      )
  }
  if(.l == "-"){
    return(
      paste(c(.x-1,.x+1),.y,sep=";")
    )
  }
  if(.l == "L"){
    return(
      paste(c(.x,.x+1),c(.y-1,.y),sep=";")
    )
  }
  if(.l == "J"){
    return(
      paste(c(.x,.x-1),c(.y-1,.y),sep=";")
    )
  }
  if(.l == "7"){
    return(
      paste(c(.x,.x-1),c(.y+1,.y),sep=";")
    )
  }
  if(.l == "F"){
    return(
      paste(c(.x,.x+1),c(.y+1,.y),sep=";")
    )
  }
}
`%notin%` <- Negate(`%in%`)
while(length(position) != 0){
  lettre <-
    strsplit(input[[(as.numeric(regmatches(position,gregexpr("[0-9]+$",position))[[1]]))]],"")[[1]][as.numeric(regmatches(position,gregexpr("^[0-9]+",position))[[1]])]
  id.col = as.numeric(regmatches(position,gregexpr("^[0-9]+",position))[[1]])
  id.row = as.numeric(regmatches(position,gregexpr("[0-9]+$",position))[[1]])
  
  next.moves <-
    next_moves(lettre,id.col,id.row)
  
  position <-
    next.moves[which(next.moves %notin% positions)]
  
  positions <- append(positions,
                      position)
  
}; rm(lettre, id.col, id.row,
      next.moves, position)

solution1 <-
  length(positions)/2



# Partie 2 ----


positions <- append(
  positions,
  depart
)


library(sf)

X <-
  regmatches(positions,
           gregexpr("^[0-9]+",positions)) |> 
  unlist() |> 
  as.numeric()
Y <-
  regmatches(positions,
             gregexpr("[0-9]+$",positions)) |> 
  unlist() |> 
  as.numeric()

grille <- 
  st_sf(geometry = st_sfc(st_polygon(list(as.matrix(data.frame(x = X, y = Y))))))

unv.positions <-
  expand.grid(
    c(1:n.col),
    c(1:n.row)
  ) |> 
  (\(.){
    paste(.$Var1,.$Var2,sep =";")
  })() |> 
  (\(.){
    .[. %notin% positions]
  })()

solution2 <-
  st_intersects(
    st_as_sf(data.frame(
      x = unlist(regmatches(unv.positions,gregexpr("^[0-9]+",unv.positions))),
      y = unlist(regmatches(unv.positions,gregexpr("[0-9]+$",unv.positions)))
    ), coords = c("x","y")),
    grille
  ) |> 
  unlist() |> 
  sum()


