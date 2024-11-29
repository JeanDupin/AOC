# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/9/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()


# Partie 1 ----

i = 26

while(i >= 26){
  combinaisons <-
    (i-25):(i-1) |> 
    (\(.){input[.]})() |> 
    (\(.){expand.grid(.,.)})() |> 
    (\(.){.[.$Var1 != .$Var2,]})() |> 
    rowSums() |> 
    unique()
  
  if(input[i] %in% combinaisons){
    i = i + 1
  } else {
    solution1 <-
      input[i]
    i = 0
  }
  
}

# Partie 2 ----

i = 1

while(i > 0 & i <= length(input)){
  verif <- cumsum(input[c(i:length(input))])
  if(solution1 %in% verif){
    solution2 <-
      input[i:(which(verif == solution1)+i-1)] |> 
      (\(.){min(.) + max(.)})()
    i = 0
  } else {
    i = i + 1
  }
}
