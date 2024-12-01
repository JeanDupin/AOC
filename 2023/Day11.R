# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/11/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

# On étend la grille
grille <-
  sapply(input, USE.NAMES = F,
         function(.x) {
           if (!grepl("#", .x)) {
             return(c(.x,.x))
           } else {
             return(.x)
           }
         }) |>
  unlist() |>
  sapply(USE.NAMES = F,
         function(.x) {
           strsplit(.x, "")
         }) |>
  Reduce(f = paste0) |>
  sapply(USE.NAMES = F,
         function(.x) {
           if (!grepl("#", .x)) {
             return(c(.x,.x))
           } else {
             return(.x)
           }
         }) |>
  unlist()

# On récupère la position de toutes les galaxies (#)

galaxies <- c()
for(i in seq_along(grille)){
  if(all(gregexpr("#",grille[i])[[1]] == -1)){
    next
  } else {
    galaxies <-
      append(galaxies,
             paste(i,
                   gregexpr("#",grille[i])[[1]],
                   sep = ";"))
  }
}; rm(i)


# Calcul de la distance 2 à 2
dist.gal <- function(coords1, coords2){
  Ax = as.numeric(regmatches(coords1,gregexpr("^[0-9]+",coords1))[[1]])
  Ay = as.numeric(regmatches(coords1,gregexpr("[0-9]+$",coords1))[[1]])
  Bx = as.numeric(regmatches(coords2,gregexpr("^[0-9]+",coords2))[[1]])
  By = as.numeric(regmatches(coords2,gregexpr("[0-9]+$",coords2))[[1]])
  return(
    abs(diff(c(Ax,Bx))) + abs(diff(c(Ay,By)))
  )
}

solution1 <-
  mapply(
    dist.gal,
    coords1 = as.character(expand.grid(galaxies,galaxies)$Var1),
    coords2 = as.character(expand.grid(galaxies,galaxies)$Var2),
    USE.NAMES = F
  ) |> 
  sum()/2


# Partie 2 ----

# Pour cette étape, on va récupérer les ID des lignes et des colonnes pour lesquelles
# on devrait multiplier par 1M. On regarde si le chemin passe par elles, pour chaque
# ligne ou colonne, on rajoute 999999 d'étapes

# On récupère la position de toutes les galaxies (#)
galaxies <- c()
for(i in seq_along(input)){
  if(all(gregexpr("#",input[i])[[1]] == -1)){
    next
  } else {
    galaxies <-
      append(galaxies,
             paste(gregexpr("#",input[i])[[1]],
                   i,
                   sep = ";"))
  }
}; rm(i)

# On récupère les colonnes et lignes vides
lignes <-
  sapply(input, USE.NAMES = F,
         function(.x) {
           if (!grepl("#", .x)) {
             return(TRUE)
           } else {
             return(FALSE)
           }
         }) |> 
  (\(.){
    c(1:length(input))[.]
  })()

colonnes <-
  sapply(input, USE.NAMES = F,
         function(.x) {
           strsplit(.x, "")
         }) |>
  Reduce(f = paste0) |> 
  sapply(USE.NAMES = F,
         function(.x) {
           if (!grepl("#", .x)) {
             return(TRUE)
           } else {
             return(FALSE)
           }
         }) |> 
  (\(.){
    c(1:length(input))[.]
  })()


# On modifie la fonction de calcul de distance
dist.gal <- function(coords1, coords2){
  Ax = as.numeric(regmatches(coords1,gregexpr("^[0-9]+",coords1))[[1]])
  Ay = as.numeric(regmatches(coords1,gregexpr("[0-9]+$",coords1))[[1]])
  Bx = as.numeric(regmatches(coords2,gregexpr("^[0-9]+",coords2))[[1]])
  By = as.numeric(regmatches(coords2,gregexpr("[0-9]+$",coords2))[[1]])
  
  n.colonnes = sum(c(Ax:Bx) %in% colonnes)
  n.lignes = sum(c(Ay:By) %in% lignes)
  
  return(
    abs(diff(c(Ax,Bx))) + abs(diff(c(Ay,By))) + 999999*(n.colonnes + n.lignes)
  )
}

solution2 <-
  mapply(
    dist.gal,
    coords1 = as.character(expand.grid(galaxies,galaxies)$Var1),
    coords2 = as.character(expand.grid(galaxies,galaxies)$Var2),
    USE.NAMES = F
  ) |> 
  sum()/2

