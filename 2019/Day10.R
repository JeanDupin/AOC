# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input), byrow = T)

aste <-
  which(matrice == "#",arr.ind = T)

res = list()

for(i in seq_len(nrow(aste))){
  if(i %% 10 == 0){print(i)}
  
  res[[i]] <-
    aste |> 
    (\(.){cbind(.,aste[i,][[1]])})() |> 
    (\(.){cbind(.,aste[i,][[2]])})() |> 
    (\(.){colnames(.) <- c("X1","Y1","X2","Y2"); .})() |> 
    (\(.){.[-i,]})() |> 
    apply(1, function(row) {
      coords <- matrix(c(row["X1"], row["Y1"], row["X2"], row["Y2"]), ncol = 2, byrow = TRUE)
      sf::st_linestring(coords)
    },simplify = F) |> 
    sf::st_sfc() |> 
    sf::st_as_sf() |> 
    sf::st_contains() |> 
    lapply(length) |> 
    unlist() |> 
    (\(.){sum(. == 1)})()
  
}

solution1 <-
  max(unlist(res))

# Partie 2 ----

coords = aste[which.max(unlist(res)),]

laser <-
  aste |> 
  (\(.){cbind(.,aste[which.max(unlist(res)),][[1]])})() |> 
  (\(.){cbind(.,aste[which.max(unlist(res)),][[2]])})() |> 
  (\(.){colnames(.) <- c("X1","Y1","X2","Y2"); .})() |> 
  (\(.){.[-which.max(unlist(res)),]})() |> 
  apply(1, function(row) {
    coords <- matrix(c(row["X1"], row["Y1"], row["X2"], row["Y2"]), ncol = 2, byrow = TRUE)
    sf::st_linestring(coords)
  },simplify = F) |> 
  sf::st_sfc() |> 
  sf::st_as_sf()

solution2 <-
  NA