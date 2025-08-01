# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/25/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

i = 20151125
k = 252533
n = 33554393

indices <-
  regmatches(input,
             gregexpr("\\d+",input))[[1]] |> 
  as.numeric() |> 
  setNames(c("row","col"))

memoire <- vector("numeric", n+1)
memoire2 <- vector("numeric",n+1)-1
memoire[i+1] = 1

a = 1

while(a < 1e8){
  memoire2[a] = i
  i = (i*k) %% n
  if(memoire[i+1] == 1){
    break
  } else {
    a = a+1
    if(a %% 10000 == 0){print(a)}
    memoire[i+1] = 1
  }
}

memoire2 <-
  memoire2[seq_len(a)]

d = indices[1] + indices[2] - 1

solution1 <-
  (indices[1] + indices[2] - 1) |> 
  (\(.){.*(.+1)/2-(.-indices[2])})() |> 
  (\(.){rep(memoire2,. %/% length(memoire2) + 1)[.]})()


# Partie 2 ----
  
solution2 <-
  NA