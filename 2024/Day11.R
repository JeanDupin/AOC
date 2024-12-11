# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/11/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


diviser_mot <- function(mot) {
  # Calculer la longueur du mot
  longueur <- nchar(mot)
  
  # Trouver le point de division
  point_division <- ceiling(longueur / 2)
  
  # Diviser le mot en deux parties
  partie1 <- substr(mot, 1, point_division)
  partie2 <- substr(mot, point_division + 1, longueur)
  
  # Retourner les deux parties sous forme de liste
  c(partie1, partie2)
}

res = strsplit(input," ")[[1]]

for(i in seq_len(25)){
  res <-
  lapply(
    res,
    function(.x){
      if(as.numeric(.x == "0")){
        1
      } else if(nchar(.x) %% 2 == 0){
        as.numeric(diviser_mot(.x))
      } else {
        as.numeric(.x) * 2024
      }
    }
  ) |> 
    unlist()
}

solution1 <-
  length(res)

# Partie 2 ----

solution2 <-
  NA