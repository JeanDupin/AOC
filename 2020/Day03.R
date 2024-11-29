# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/3/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  sapply(
    seq_along(input),
    function(.x){
      (seq(from = 1, length.out = length(input), by = 3) %% nchar(input[1]))[.x] |> 
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)[[1]]})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

# Partie 2 ----


slope1 <-
  sapply(
    seq_along(input),
    function(.x){
      (seq(from = 1, length.out = length(input), by = 1) %% nchar(input[1]))[.x] |> 
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)[[1]]})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

slope2 <-
  sapply(
    seq_along(input),
    function(.x){
      (seq(from = 1, length.out = length(input), by = 3) %% nchar(input[1]))[.x] |> 
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)[[1]]})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

slope3 <-
  sapply(
    seq_along(input),
    function(.x){
      (seq(from = 1, length.out = length(input), by = 5) %% nchar(input[1]))[.x] |> 
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)[[1]]})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

slope4 <-
  sapply(
    seq_along(input),
    function(.x){
      (seq(from = 1, length.out = length(input), by = 7) %% nchar(input[1]))[.x] |> 
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)[[1]]})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()

slope5 <-
  sapply(
    seq_along(input)[seq_along(input) %% 2 == 1],
    function(.x){
      (seq(from = 1, length.out = length(input), by = 1) %% nchar(input[1]))[which(seq_along(input)[seq_along(input) %% 2 == 1] == .x)] |>
        (\(.){ifelse(. == 0, nchar(input[1]), .)})() |>
        (\(.){strsplit(input[.x],"")[[1]][.]})() |>
        (\(.){ifelse(. == ".",F,T)})()
    },
    USE.NAMES = F, simplify = T
  ) |> 
  sum()


solution2 <-
  slope1 * slope2 * slope3 * slope4 * slope5

