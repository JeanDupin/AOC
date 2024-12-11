# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

lunes <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("(-|)\\d+",.x))[[1]] |> 
        as.numeric()
    }
  )

vitesse <-
  list(
    c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0)
  )


for(z in seq_len(1000)){
  for(i in seq_along(lunes)){
    velocite = c(0,0,0)
    for(j in seq_along(lunes)){
      velocite = velocite + sign(lunes[[j]] - lunes[[i]])
    }
    vitesse[[i]] = vitesse[[i]] + velocite
  }
  for(k in seq_along(lunes)){
    lunes[[k]] = lunes[[k]] + vitesse[[k]]
  }
}


solution1 <-
  lapply(
    seq_along(lunes),
    function(.x){
      sum(abs(lunes[[.x]])) * sum(abs(vitesse[[.x]]))
    }
  ) |> 
  unlist() |> 
  sum()


# Partie 2 ----


lunes <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("(-|)\\d+",.x))[[1]] |> 
        as.numeric()
    }
  )

X = c(lunes[[1]][1],lunes[[2]][1],lunes[[3]][1],lunes[[4]][1])
Y = c(lunes[[1]][2],lunes[[2]][2],lunes[[3]][2],lunes[[4]][2])
Z = c(lunes[[1]][3],lunes[[2]][3],lunes[[3]][3],lunes[[4]][3])

vitesse <-
  list(
    c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0)
  )

Xz = Yz = Zz = -1
z = 1
while(any(c(Xz,Yz,Zz) == -1)){
  for(i in seq_along(lunes)){
    velocite = c(0,0,0)
    for(j in seq_along(lunes)){
      velocite = velocite + sign(lunes[[j]] - lunes[[i]])
    }
    vitesse[[i]] = vitesse[[i]] + velocite
  }
  for(k in seq_along(lunes)){
    lunes[[k]] = lunes[[k]] + vitesse[[k]]
  }
  if(all(c(lunes[[1]][1],lunes[[2]][1],lunes[[3]][1],lunes[[4]][1]) == X) &
     all(c(vitesse[[1]][1],vitesse[[2]][1],vitesse[[3]][1],vitesse[[4]][1]) == 0)){
    Xz = z
  }
  if(all(c(lunes[[1]][2],lunes[[2]][2],lunes[[3]][2],lunes[[4]][2]) == Y) &
     all(c(vitesse[[1]][2],vitesse[[2]][2],vitesse[[3]][2],vitesse[[4]][2]) == 0)){
    Yz = z
  }
  if(all(c(lunes[[1]][3],lunes[[2]][3],lunes[[3]][3],lunes[[4]][3]) == Z) &
     all(c(vitesse[[1]][3],vitesse[[2]][3],vitesse[[3]][3],vitesse[[4]][3]) == 0)){
    Zz = z
  }
  z = z + 1
  if(z %% 10 == 0){print(z)}
}

ppcm <-
  function(a, b){
  pgcd <-
    function(a, b){
    while (b != 0) {
      temp <- b
      b <- a %% b
      a <- temp
    }
    abs(a)
  }
  abs(a * b) / pgcd(a, b)
}

solution2 <-
  Reduce(ppcm,c(Xz,Yz,Zz))
