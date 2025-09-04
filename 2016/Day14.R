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
    paste(letters,letters,letters,letters,letters,
          sep = ""),
    paste(0:9,0:9,0:9,0:9,0:9,
          sep = "")
  )


i = 0
keys = vector("numeric")
while(length(keys) < 64){
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
                            gregexpr("(.)\\1\\1\\1?\\1?",.))[[1]][1]
               })()
           }) |> 
    (\(.){any(.[!is.na(.)] == next_patterne)})()
  
  if(is_key){
    keys = append(keys, i)
  }
 i = i + 1
}


solution1 <-
  keys[64]

# Partie 2 ----

super_hash <- function(x){
  for(i in seq_len(2017)){
    x <- openssl::md5(x) 
  }
  x
}

cache_hashes <- vector("character",30000)
for(i in 1:30000){
  cache_hashes[i] <- as.character(super_hash(paste0(input,i)))
  if(i %% 1000 == 0){print(i)}
}


i = 1
keys = vector("numeric")
while(length(keys) < 64){
  if(i %% 100 == 0){print(i)}
  hash <-
    cache_hashes[i]
  
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
    cache_hashes[(i+1):(i+1000)] |> 
    (\(.){
      regmatches(.,
                 gregexpr("(.)\\1\\1\\1?\\1?",.))
    })() |> 
    (\(.){any(unlist(.) == next_patterne)})()
  
  
  if(is_key){
    keys = append(keys, i)
    print(i)
  }
  i = i + 1
}


solution2 <-
  keys[64]