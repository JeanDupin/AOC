# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

# Partie 1 ----

distance_manhattan <-
  function(point1, point2) {
    sum(abs(point1 - point2))
  }

points <-
  regmatches(input,
             gregexpr("\\d+",input)) |> 
  lapply(as.numeric)

x <-
  regmatches(input,
           gregexpr("\\d+",input)) |> 
  lapply(function(.x){as.numeric(.x[1])}) |> 
  (\(.){c(min(unlist(.)):max(unlist(.)))})()
y <-
  regmatches(input,
             gregexpr("\\d+",input)) |> 
  lapply(function(.x){as.numeric(.x[2])}) |> 
  (\(.){c(min(unlist(.)):max(unlist(.)))})()

grille <-
  expand.grid(x,y) |> 
  apply(1,as.numeric,simplify = FALSE)
bb <-
  expand.grid(c(x-1,x,x+1),c(y-1,y,y+1)) |> 
  apply(1,as.numeric,simplify = FALSE) |> 
  setdiff(grille)

distances.bb <-
  vector("character")
for(i in seq_along(bb)){
  distances.bb[i] <-
    sapply(points, distance_manhattan, point1 = bb[[i]], USE.NAMES = F, simplify = T) |> 
    (\(.){
      if(length(which(. == min(.))) > 1){"P0"} else {
        paste0("P",1:length(input))[which(. == min(.))]
      }})()
}

distances <-
  vector("character")
for(i in seq_along(grille)){
  distances[i] <-
    sapply(points, distance_manhattan, point1 = grille[[i]], USE.NAMES = F, simplify = T) |> 
    (\(.){
      if(length(which(. == min(.))) > 1){"P0"} else {
        paste0("P",1:length(input))[which(. == min(.))]
      }})()
}


solution1 <-
  max(table(distances[!distances %in% distances.bb]))

# Partie 2 ----

distances <-
  vector("character")
for(i in seq_along(grille)){
  distances[i] <-
    sapply(points, distance_manhattan, point1 = grille[[i]], USE.NAMES = F, simplify = T) |> 
    sum() |> 
    (\(.){
      if(. >= 10000){0} else {
       i
      }})()
}

solution2 <-
  length(distances[distances != "0"])

