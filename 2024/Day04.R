# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/4/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_cache(tempdir(),debug = T) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]),byrow = T)

get_xmas <-
  function(matrice){
    
    nrows <-
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
    
    ncols <-
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
    
    ndiags1 <-
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
    
    ndiags2 <-
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
    
    nrows + ncols + ndiags1 + ndiags2
  }


solution1 <-
 get_xmas(matrice)

# Partie 2 ----

get_mas <-
  function(matrice){
    solution = 0
    for(i in 1:(ncol(matrice)-2)){
      for(j in 1:(nrow(matrice)-2)){
        if(matrice[i,j] == "S"){
          if(matrice[i+2,j+2] == "M" & matrice[i+1,j+1] == "A"){
            if(matrice[i+2,j] == "M" & matrice[i,j+2] == "S"){
              solution = solution + 1
            } else if (matrice[i+2,j] == "S" & matrice[i,j+2] == "M"){
              solution = solution + 1
            } else {
              next
            }
          } else {
            next
          }
        } else if(matrice[i,j] == "M"){
          if(matrice[i+2,j+2] == "S" & matrice[i+1,j+1] == "A"){
            if(matrice[i+2,j] == "S" & matrice[i,j+2] == "M"){
              solution = solution + 1
            } else if (matrice[i+2,j] == "M" & matrice[i,j+2] == "S"){
              solution = solution + 1
            } else {
              next
            }
          } else {
            next
          }
        } else {
          next
        }
      }
    }
    solution
  }

solution2 <-
  get_mas(matrice)
