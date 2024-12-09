# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    strsplit(input,""),
    function(.x){
      partie1 = .x[1:(length(.x)/2)]
      partie2 = .x[((length(.x)/2)+1):length(.x)]
      
      which(intersect(partie1,partie2) == c(letters,LETTERS))
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(
    seq(1,length(input),3),
    function(.x){
      strsplit(input[c(.x,.x+1,.x+2)],"") |> 
        (\(.){Reduce(intersect,.)})() |> 
        (\(.){which(. == c(letters,LETTERS))})()
    }
  ) |> 
  unlist() |> 
  sum()
