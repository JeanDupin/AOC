# Input ----

input <-
  readLines("2015/Inputs/Day06.txt")


# Partie 1 ----

`%notin%` <- Negate(`%in%`)

fonctions <-
  sapply(input,
         function(.x){sub("(.*?)[0-9].*", "\\1",.x)},
         USE.NAMES = FALSE)

X <-
  sapply(regmatches(input,gregexpr("[0-9]+\\.*[0-9]*", input)),
         function(.x){
           seq(as.numeric(.x)[1],
               as.numeric(.x)[3],
               1)
         })
Y <-
  sapply(regmatches(input,gregexpr("[0-9]+\\.*[0-9]*", input)),
         function(.x){
           seq(as.numeric(.x)[2],
               as.numeric(.x)[4],
               1)
         })


off <- apply(expand.grid(0:999,0:999),1, paste0, collapse = ";")
on <- c()

# Boucle pour déplacer les éléments
for (i in seq_along(fonctions)) {
  
  grid_to_change = apply(
    expand.grid(X[[i]],Y[[i]]),
    1, paste0, collapse = ";")
  
  if(fonctions[i] == "turn off "){
    on <- on[on %notin% grid_to_change]
    off <- unique(c(off,grid_to_change))
  } else if(fonctions[i] == "turn on "){
    off <- off[off %notin% grid_to_change]
    on <- unique(c(on,grid_to_change))
  } else {
    allumes <- grid_to_change[grid_to_change %in% on]
    eteints <- grid_to_change[grid_to_change %notin% allumes]
    
    on <- on[on %notin% allumes]
    on <- unique(c(on, eteints))
    off <- off[off %notin% eteints]
    off <- unique(c(off, allumes))
    
  }
  
}; rm(i, allumes, eteints, grid_to_change)

solution1 <-
  length(on)

# Partie 2 ----

rm(on, off)


mydf <-
  data.frame(
    grid.id = apply(expand.grid(0:999,0:999),1, paste0, collapse = ";"),
    light = 0)
for (i in seq_along(fonctions)) {
  
  grid_to_change = apply(
    expand.grid(X[[i]],Y[[i]]),
    1, paste0, collapse = ";")
  
  if(fonctions[i] == "turn on "){
    mydf[mydf$grid.id %in% grid_to_change,"light"] <- 
      mydf[mydf$grid.id %in% grid_to_change,"light"]+1
  } else if(fonctions[i] == "turn off "){
    mydf[mydf$grid.id %in% grid_to_change,"light"] <- 
      pmax(0,mydf[mydf$grid.id %in% grid_to_change,"light"]-1)
  } else {
    mydf[mydf$grid.id %in% grid_to_change,"light"] <- 
      mydf[mydf$grid.id %in% grid_to_change,"light"]+2
  }
  
}; rm(i, grid_to_change)

solution2 <-
  sum(mydf$light)
