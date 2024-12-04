# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/4/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

input = clipr::read_clip()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]),byrow = T)


a <-
  apply(matrice,1,function(.x){
  a <-
  paste(.x,collapse = "") |>
    (\(.){regmatches(.,
                     gregexpr("XMAS",.))[[1]]})() |> 
    length()
  b <-
  paste(rev(.x),collapse = "") |>
    (\(.){regmatches(.,
                     gregexpr("XMAS",.))[[1]]})() |> 
    length()
  
  a + b
  }) |> 
  sum()

b <-
  apply(matrice,2,function(.x){
  a <-
    paste(.x,collapse = "") |>
    (\(.){regmatches(.,
                     gregexpr("XMAS",.))[[1]]})() |> 
    length()
  b <-
    paste(rev(.x),collapse = "") |>
    (\(.){regmatches(.,
                     gregexpr("XMAS",.))[[1]]})() |> 
    length()
  
  a + b
}) |> 
  sum()

d <-
  lapply(
  split(matrice,(row(matrice) - col(matrice))),
  function(.x){
    a <-
      paste(.x,collapse = "") |>
      (\(.){regmatches(.,
                       gregexpr("XMAS",.))[[1]]})() |> 
      length()
    b <-
      paste(rev(.x),collapse = "") |>
      (\(.){regmatches(.,
                       gregexpr("XMAS",.))[[1]]})() |> 
      length()
    
    a + b
  }
) |> 
  unlist() |> 
  sum()

e <-
  lapply(
  split(matrice,(row(matrice) + col(matrice))),
  function(.x){
    a <-
      paste(.x,collapse = "") |>
      (\(.){regmatches(.,
                       gregexpr("XMAS",.))[[1]]})() |> 
      length()
    b <-
      paste(rev(.x),collapse = "") |>
      (\(.){regmatches(.,
                       gregexpr("XMAS",.))[[1]]})() |> 
      length()
    
    a + b
  }
) |> 
  unlist() |> 
  sum()




solution1 <-
  a + b + d + e

# Partie 2 ----


d <-
  lapply(
    split(matrice,(row(matrice) - col(matrice))),
    function(.x){
      a <-
        paste(.x,collapse = "") |>
        (\(.){regmatches(.,
                         gregexpr("MAS",.))[[1]]})() |> 
        length()
      b <-
        paste(rev(.x),collapse = "") |>
        (\(.){regmatches(.,
                         gregexpr("MAS",.))[[1]]})() |> 
        length()
      
      list(a,b)
    }
  ) |> 
  unlist() |> 
  sum()

e <-
  lapply(
    split(matrice,(row(matrice) + col(matrice))),
    function(.x){
      a <-
        paste(.x,collapse = "") |>
        (\(.){regmatches(.,
                         gregexpr("MAS",.))[[1]]})() |> 
        length()
      b <-
        paste(rev(.x),collapse = "") |>
        (\(.){regmatches(.,
                         gregexpr("MAS",.))[[1]]})() |> 
        length()
      
      a + b
    }
  ) |> 
  unlist() |> 
  sum()
  
solution2 <-
  NA