# Input ----

input <-
  httr2::request("https://adventofcode.com/2017/day/2/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,"\\t")})()

# Partie 1 ----

solution1 <-
  sapply(input, USE.NAMES = F, simplify = T,
         function(.x){
           as.numeric(.x) |> 
             (\(.){max(.) - min(.)})()
         }) |> 
  sum()

# Partie 2 ----

solution2 <-
  sapply(input, USE.NAMES = F, simplify = T,
         function(.x){
           expand.grid(
             as.numeric(.x),
             as.numeric(.x)
           ) |> 
             (\(.){.[.$Var1 != .$Var2,]})() |> 
             (\(.){.[(.$Var1 %% .$Var2 == 0),]})() |> 
             (\(.){.$Var1 / .$Var2})()
         }) |> 
  sum()


