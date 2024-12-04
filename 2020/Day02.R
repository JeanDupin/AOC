# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/2/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  sapply(
    input,
    function(.x){
      nmin <-
        regmatches(.x,
                   gregexpr("^[0-9]*",.x))[[1]] |> 
        as.numeric()
      nmax <-
        regmatches(.x,
                   gregexpr("-[0-9]*",.x))[[1]] |> 
        (\(.){as.numeric(gsub("-",'',.))})()
      lettre <-
        regmatches(.x,
                   gregexpr("[a-z]:",.x))[[1]] |> 
        (\(.){gsub(":",'',.)})()
      
      strsplit(strsplit(.x,": ")[[1]][2],"")[[1]] |> 
        table() |> 
        (\(.){.[names(.) == lettre]})() |> 
        (\(.){ifelse(. >= nmin & . <= nmax,T,F)})() |> 
        (\(.){ifelse(length(.) == 0,F,.[[1]])})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

# Partie 2 ----

solution2 <-
  sapply(
    input,
    function(.x){
      nmin <-
        regmatches(.x,
                   gregexpr("^[0-9]*",.x))[[1]] |> 
        as.numeric()
      nmax <-
        regmatches(.x,
                   gregexpr("-[0-9]*",.x))[[1]] |> 
        (\(.){as.numeric(gsub("-",'',.))})()
      lettre <-
        regmatches(.x,
                   gregexpr("[a-z]:",.x))[[1]] |> 
        (\(.){gsub(":",'',.)})()
      
      strsplit(strsplit(.x,": ")[[1]][2],"")[[1]] |> 
        (\(.){sum(.[c(nmin,nmax)] == lettre) == 1})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()