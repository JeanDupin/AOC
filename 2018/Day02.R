# Input ----

input <-
  httr2::request("https://adventofcode.com/2018/day/2/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,'\\n')[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    input,
    function(.x){
      strsplit(.x,"")[[1]] |> 
        table() |> 
        (\(.){
          c(ifelse(length(.[which(. == 2)]) > 0,1,0),
            ifelse(length(.[which(. == 3)]) > 0,1,0))
        })()
    }
  ) |> 
  (\(.){Reduce(`+`,.)})() |> 
  prod()

# Partie 2 ----

solution2 <-
  expand.grid(input,input) |> 
  (\(.){.[c(which(stringdist::stringdist(.$Var1,.$Var2) == 1)),"Var1"]})() |>
  as.character() |> 
  strsplit("") |> 
  (\(.){paste(.[[1]][.[[1]] == .[[2]]],collapse = "")})()