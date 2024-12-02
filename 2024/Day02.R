# Input ----

input <-
  httr2::request("https://adventofcode.com/2024/day/2/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.," ")})() |> 
  lapply(as.numeric)

# Partie 1 ----

solution1 <-
  lapply(input,
         function(.x){
           A = diff(.x)
           if(length(unique(sign(A))) != 1){
             F
           } else if(min(A) < -3 | max(A) > 3){
             F
           } else {
             T
           }
         }) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(input,
         function(.x){
           
           A = diff(.x)
           if(length(unique(sign(A))) != 1){
             I = F
           } else if(min(A) < -3 | max(A) > 3){
             I = F
           } else {
             I = T
           }
           if(I){
             return(T)
           }
           
           res = vector()
           for(i in seq_along(.x)){
             A = diff(.x[-i])
             if(length(unique(sign(A))) != 1){
               res = append(res,F)
             } else if(min(A) < -3 | max(A) > 3){
               res = append(res,F)
             } else {
               res = append(res,T)
             }
           }
           
           if(any(res)){
             return(T)
           } else {
             return(F)
           }
           
         }) |> 
  unlist() |> 
  sum()

