# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/6/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  lapply(
    2:(length(which(input == ""))+2),
    function(.x){
      indices.min <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[1]
      indices.max <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[2]
      
      indices<-
        Map(":", indices.min, indices.max)[[1]]
      
      paste(input[indices], collapse = "")}) |> 
  lapply(function(.x){
    strsplit(.x,"")[[1]] |> 
      table() |> 
      length()
  }) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(
    2:(length(which(input == ""))+2),
    function(.x){
      indices.min <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[1]
      indices.max <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[2]
      
      indices<-
        Map(":", indices.min, indices.max)[[1]]
      
      paste(input[indices], collapse = " ")}) |> 
  lapply(function(.x){
    npers <- length(strsplit(.x," ")[[1]])
    strsplit(gsub(" ","",.x),"")[[1]] |> 
      table() |> 
      (\(.){.[which(. == npers)]})() |> 
      length()
  }) |> 
  unlist() |> 
  sum()