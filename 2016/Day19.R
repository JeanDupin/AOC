# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/19/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

chaine = seq_len(input)
lastid = F
while(length(chaine) > 1){
  if(lastid){
    chaine = chaine[-1]
  }
  indices <-
    as.logical(seq_along(chaine) %% 2)
  lastid = rev(indices)[1]
  chaine <- chaine[indices]
}

solution1 <-
  chaine

# Partie 2 ----

# Pour un input donné (par exemple 212), voici l'avancement du
# gagnant :
# De 1 en 1 à partir de 212/3 = 81
# Puis de 2 en 2 jusqu'à 212.
# Le gagnant est par boucle des puissances de 3 (3, 9, 27, 81, 243...)
# Donc on repart à 1 à chaque puissance de 3, puis on avance de 1 en 1
# Jusqu'à 2*3^n puis de 2 en 2 jusqu'à la nouvelle puissance de 3.

solve <- function(n){
  m = 1
  # On trouve la puissance de 3
  while(3*m <= n){m = 3*m}
  # Cas où on aurait n = une puissance de trois
  if(n == m){
    n
  } else if(n <= 2 * m){
    # Cas où on avance de 1 en 1
    n-m
  } else {
    # Cas où on avance de 2 en 2
    2*n-3*m
  }
}


solution2 <-
  solve(input)
