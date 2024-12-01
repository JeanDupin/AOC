# Input ----

input <-
  httr2::request("https://adventofcode.com/2017/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

solution1 <-
  {strsplit(input,"")[[1]] == 
    c(strsplit(input,"")[[1]][2:length(strsplit(input,"")[[1]])],
      strsplit(input,"")[[1]][length(strsplit(input,"")[[1]])])} |> 
  (\(.){strsplit(input,"")[[1]][.]})() |> 
  as.numeric() |> 
  sum()
  

# Partie 2 ----

solution2 <-
  {strsplit(input,"")[[1]] == 
      c(strsplit(input,"")[[1]][((length(strsplit(input,"")[[1]])/2)+1):((length(strsplit(input,"")[[1]])/2)*2)],
        strsplit(input,"")[[1]][1:(length(strsplit(input,"")[[1]])/2)])} |> 
  (\(.){strsplit(input,"")[[1]][.]})() |> 
  as.numeric() |> 
  sum()



