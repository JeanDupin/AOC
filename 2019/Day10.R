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
  (which(matrice == "#",arr.ind = T) - 1)

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
  (\(.){colnames(.) <- c("Y1","X1","Y2","X2"); .})() |> 
  (\(.){.[-which.max(unlist(res)),]})() |> 
  apply(1, function(row) {
    coords <- matrix(c(row["X1"], row["Y1"], row["X2"], row["Y2"]), ncol = 2, byrow = TRUE)
    sf::st_linestring(coords)
  },simplify = F) |> 
  sf::st_sfc() |> 
  sf::st_as_sf()


laser$angles <-
  lapply(
    seq_len(nrow(laser)),
    function(.x){
      sf::st_cast(laser[.x,],"POINT") |> 
        (\(.){nngeo::st_azimuth(.[1,],.[2,])})() |> 
        (\(.){ifelse(. == 0,0,360-.)})()
    }
  ) |> 
  unlist()
laser$dist = sf::st_length(laser)
laser$id = seq_len(nrow(laser))

laser <-
  laser[order(laser$angles,laser$dist),]


solution2 <-
  laser[!duplicated(laser$angles),][200,] |> 
  (\(.){sf::st_coordinates(.)[1,c("X","Y")]})() |> 
  (\(.){(.[1]*100+.[2])[[1]]})()
