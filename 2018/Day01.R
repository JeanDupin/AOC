# Input ----

input <-
  httr2::request("https://adventofcode.com/2018/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){as.numeric(strsplit(.,"\\n")[[1]])})()

# Partie 1 ----

solution1 <-
  sum(input)

# Partie 2 ----
sortie = numeric()
i = 1
while(length(sortie) == 0){
  sortie <-
    c(0,cumsum(rep(input,i)))[duplicated(c(0,cumsum(rep(input,i))))]
  i <-
    i + 1
}

solution2 <-
  sortie[1]