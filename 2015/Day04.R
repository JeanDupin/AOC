# Input ----

input <-
  "bgvyzdsv"


# Partie 1 ----

input <- 
  paste0("bgvyzdsv",
         c(1:999999))
x = c(0)
for(i in seq_along(input)){
  hash = digest::digest(input[i],algo = "md5",serialize = F)
  if(substr(hash,1,5) == "00000"){
    x <<- append(x, input[i])
  } 
}; rm(i, hash)

solution_1 <-
  gsub("bgvyzdsv","",x[2]) |> 
  as.numeric()

# Partie 2 ----

input <- 
  paste0("bgvyzdsv",
         c(1000000:2000000))
x = c(0)
for(i in seq_along(input)){
  hash = digest::digest(input[i],algo = "md5",serialize = F)
  if(substr(hash,1,6) == "000000"){
    x <<- append(x, input[i])
  } 
}; rm(i, hash)

solution_2 <-
  gsub("bgvyzdsv","",x[2]) |> 
  as.numeric()
