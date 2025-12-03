# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/3/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

max_sequence <- function(.x, n){
  nombres <- as.integer(strsplit(.x, "")[[1]])
  taille <- length(nombres) - n
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
  as.numeric(paste0(res[seq_len(n)], collapse = ""))
}

solution1 <-
  sapply(input, max_sequence, n = 2, USE.NAMES = F, simplify = T) |> 
  sum()

# Partie 2 ----

solution2 <-
  sapply(input, max_sequence, n = 12, USE.NAMES = F, simplify = T) |> 
  sum()
