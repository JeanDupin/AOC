# Input ----

input <-
  "abbhdwsy"


# Partie 1 ----

x = c()
i = 0
while(length(x) < 8){
  test = paste0(input,i)
  hash = digest::digest(test,
                        algo = "md5",
                        serialize = F)
  if(substr(hash,1,5) == "00000"){
    x <<- append(x, substr(hash,6,6))
  }
  i <- i + 1
}; rm(test, hash, i)


solution1 <-
  paste(x, collapse = "")

# Partie 2 ----

x = c()
position = c()
i = 0
while(length(x) < 8){
  test = paste0(input,i)
  hash = digest::digest(test,
                        algo = "md5",
                        serialize = F)
  if(substr(hash,1,5) == "00000" & substr(hash,6,6) %in% paste(0:7) & !(substr(hash,6,6) %in% position)){
    x <<- append(x, substr(hash,7,7))
    position <<- append(position, substr(hash,6,6))
  }
  i <- i + 1
}; rm(test, hash, i)


solution2 <-
  paste(x[order(as.numeric(position))],
        collapse = "")

