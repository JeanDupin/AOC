# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  NA

# Partie 2 ----

solution2 <-
  NA