# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/22/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

cartes <-
  as.numeric(input[input != "" & !grepl("Player",input)])

joueur1 <-
  cartes[1:(length(cartes)/2)]
joueur2 <-
  cartes[(1+length(cartes)/2):length(cartes)]


while(length(joueur1) > 0 & length(joueur2) > 0){
  carte1 = joueur1[1]
  carte2 = joueur2[1]
  if(carte1 == carte2){
    stop("Draw")
  } else if(carte1 > carte2){
    joueur1 <- c(joueur1[-1],carte1,carte2)
    joueur2 <- joueur2[-1]
  } else {
    joueur1 <- joueur1[-1]
    joueur2 <- c(joueur2[-1],carte2,carte1)
  }
}

solution1 <-
  sum(rev(joueur2) * seq_along(joueur2)) + sum(rev(joueur1) * seq_along(joueur1))

# Partie 2 ----

solution2 <-
  NA