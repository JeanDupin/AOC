# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/22/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

input = as.numeric(input)

to_bin <- function(.x){
  res = vector("numeric")
  number = .x
  while(number!=0){
    res = append(res,number %% 2)
    number = number %/% 2
  }
  return(rev(res))
}

myxor <- function(.x,.y){
  if(length(.x) != length(.y)){
    n = abs(length(.x)-length(.y))
    if(length(.x) < length(.y)){
      .x <-
        strsplit(strrep(0,n),"")[[1]] |> 
        (\(.){c(.,.x)})() |> 
        as.numeric()
    } else {
      .y <-
        strsplit(strrep(0,n),"")[[1]] |> 
        (\(.){c(.,.y)})() |> 
        as.numeric()
    }
  }

  sortie = vector("numeric")
  for(i in seq_len(length(.x))){
    sortie[i] = ifelse((.x[i] == 0 & .y[i] == 1) | (.x[i] == 1 & .y[i] == 0),1,0)
  }
  sortie = rev(sortie)

  res = vector("numeric")
  for(i in seq_along(sortie)){
    res[i] = sortie[i]*2**(i-1)
  }
  
  sum(res)
}

mix <- function(.x,.y){
  myxor(to_bin(.x),to_bin(.y))
}

prune <- function(.x){
  .x %% 16777216
}

operations <- function(.x){
  res = mix(.x,.x*64)
  res = prune(res)
  res = mix(res,floor(res/32))
  res = prune(res)
  res = mix(res, res*2048)
  res = prune(res)
  res
}

res = vector("numeric",length(input))
for(i in seq_along(input)){
  secret = input[i]
  for(j in seq_len(2000)){
    secret = operations(secret)
  }
  res[i] = secret
}


solution1 <-
  sum(res)

# Partie 2 ----

get_price <- function(.x){
  strsplit(as.character(.x),"")[[1]][nchar(.x)] |> 
    as.numeric()
}


res = vector("list",length(input))
for(i in seq_along(input)){
  secret = input[i]
  memoire = vector("numeric",2000)
  for(j in seq_len(2000)){
    memoire[j] = get_price(secret)
    secret = operations(secret)
  }

  res[[i]][["valeurs"]] = memoire[5:length(memoire)]

  memoire2 = vector("character",2000-4)
  for(j in 4:length(diff(memoire))){
    memoire2[(j-3)] <- 
      paste(diff(memoire)[(j-3):j],collapse = ";")
  }
  res[[i]][["suite"]] = memoire2
}

solution2 <-
  do.call(rbind, lapply(res,function(.x){
    as.data.frame(.x) |> (\(.){.[!duplicated(.$suite),]
    })()})) |> 
  (\(.){aggregate(valeurs ~ suite, data = ., sum)})() |> 
  (\(.){max(.$valeurs)})()

