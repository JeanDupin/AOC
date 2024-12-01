# Input ----

input <-
  httr2::request("https://adventofcode.com/2017/day/10/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

hash <-
  function(.x, taille, skip.size, current.position){
    
    selection <-
      c(current.position:(current.position+taille-1)) %% length(.x)|> 
      (\(.){ifelse(. == 0,length(.x),.)})()
    
    .x[selection] = rev(.x[selection])
    
    current.position <<- current.position + taille + skip.size
    skip.size <<- skip.size + 1
    
    elements <<- .x
    
  }

elements <- c(0:255)
consignes <- as.numeric(strsplit(input,",")[[1]])
skip.size = 0
current.position = 1

for(i in 1:length(consignes)){
  hash(elements, consignes[i],
       skip.size, current.position)
}; rm(i)

solution1 <-
  elements[1] * elements[2]

# Partie 2 ----

consignes <-
  c(utf8ToInt(input),
    17, 31, 73, 47, 23)
  

elements <- c(0:255)
skip.size = 0
current.position = 1

for(j in 1:64){
  for(i in 1:length(consignes)){
    hash(elements, consignes[i],
         skip.size, current.position)
  }
}; rm(i, j)

sparse.hash <-
  elements


dense.hash <-
  sapply(seq(1, 256, by = 16),
         function(.x){
           sparse.hash[.x:min(.x + 15, 256)] |> 
             (\(.){Reduce(bitwXor,.)})()
           }, simplify = F, USE.NAMES = F) |> 
  unlist()


solution2 <-
  dense.hash |> 
  (\(.){sprintf("%X",.)})() |> 
  (\(.){ifelse(nchar(.) == 1,
               paste0("0",.),
               .)})() |> 
  paste(collapse = "")
  
