# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/3/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  regmatches(input,
             gregexpr("mul\\(\\d+,\\d+\\)",input)) |> 
  unlist() |> 
  lapply(
    function(x){
      regmatches(x,
                 gregexpr("\\d+",x))[[1]] |> 
        as.numeric() |> 
        prod()
    }
  ) |> 
  unlist() |> 
  sum()


# Partie 2 ----

solution2 <-
  regmatches(input,
             gregexpr("(mul\\(\\d+,\\d+\\)|do\\(\\)|don\\'t\\(\\))",input)) |> 
  unlist() |> 
  paste(collapse = "") |> 
  (\(.){gsub("don\\'t\\(\\).*?(do\\(\\)|$)","",.)})() |> 
  (\(.){gsub("do\\(\\)","",.)})() |> 
  (\(.){
    regmatches(.,
               gregexpr("mul\\(\\d+,\\d+\\)",.))
  })() |> 
  unlist() |> 
  lapply(
    function(x){
      regmatches(x,
                 gregexpr("\\d+",x))[[1]] |> 
        as.numeric() |> 
        prod()
    }
  ) |> 
  unlist() |> 
  sum()
