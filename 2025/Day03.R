# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/3/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  sapply(
    input,
    function(.x){
      res = 0
      nombres = as.numeric(strsplit(.x,"")[[1]])
      for(i in seq_along(nombres)[-length(nombres)]){
        for(j in seq_along(nombres)[-seq_len(i)]){
          if(i == j){next}
          res <-
            max(res,
                10*nombres[i] + nombres[j])
        }
      }
      res
    },
    USE.NAMES = F,
    simplify = T
  ) |> 
  sum()

# Partie 2 ----

max_sequence <- function(.x){
  nombres <- as.integer(strsplit(.x, "")[[1]])
  n <- length(nombres)
  taille <- n - 12
  res <- vector("integer")
  
  for(i in nombres){
    while(length(res) > 0 &&
           res[length(res)] < i &&
           taille > 0){
      res <- res[-length(res)]
      taille <- taille - 1
    }
    res <- c(res, i)
  }
  as.numeric(paste0(res[seq_len(12)], collapse = ""))
}


solution2 <-
  sapply(input, max_sequence, USE.NAMES = F, simplify = T) |> 
  sum()
