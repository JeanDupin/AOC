# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  (sort(as.numeric(gsub("^.*   ","",input))) -
  sort(as.numeric(gsub("   .*$","",input)))) |>
  abs() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(as.numeric(gsub("   .*$","",input)),
       function(.x){
         length(as.numeric(gsub("^.*   ","",input))[as.numeric(gsub("^.*   ","",input)) == .x]) * .x
       }) |> 
  unlist() |> 
  sum()
