# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/14/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

patterne <-
  c(
    paste(letters,letters,letters,
          sep = ""),
    paste(0:9,0:9,0:9,
          sep = "")
  )

patterne2 <-
  c(
    paste(letters,letters,letters,letters,
          sep = ""),
    paste(0:9,0:9,0:9,0:9,
          sep = "")
  )


i = 1
keys = vector("numeric")
while(i <= 23000){
  if(i %% 1000 == 0){print(i)}
  hash <-
    digest::digest(paste0(input,i),
                   serialize = F)
  
  if(!grepl(paste(patterne, collapse = "|"),hash)){
    i = i + 1
    next
  }
  
  # Quel patterne ?
  next_patterne <-
    regmatches(hash,
               gregexpr("(.)\\1\\1",hash))[[1]][1] |> 
    (\(.){patterne2[which(patterne == .)]})() 
  
  if(length(next_patterne) > 1){break}
  
  is_key <-
    sapply((i+1):(i+1000),
           function(.x){
             digest::digest(paste0(input,.x),
                            serialize = F) |> 
               (\(.){
                 regmatches(.,
                            gregexpr("(.)\\1\\1\\1?",.))[[1]][1]
               })()
           }) |> 
    (\(.){any(.[!is.na(.)] == next_patterne)})()
  
  if(is_key){
    keys = append(keys, i)
  }
 i = i + 1
}


solution1 <-
  NA

send_solution(2016,14,1,solution1)

# Partie 2 ----

solution2 <-
  NA
  
send_solution(2016,14,2,solution2)