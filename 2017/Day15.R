# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/15/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

generator <- function(.x, AB){
  facteur = ifelse(AB == "A",16807,48271)
  (.x * facteur) %% 2147483647
}

to_bin <- function(x){
  as.integer(intToBits(x))[1:16]
}

res = 0
A <-
  regmatches(input,
             gregexpr("\\d+$",input))[[1]][1] |> 
  as.numeric()
B <-
  regmatches(input,
             gregexpr("\\d+$",input))[[2]][1] |> 
  as.numeric()

for(i in seq_len(4e7)){
  if(i %% 100000 == 0){print(i)}
  A = generator(A, "A")
  B = generator(B, "B")
  
  if(all(to_bin(A) == to_bin(B))){
    res = res + 1
  }

}

solution1 <-
  res

# Partie 2 ----


A <-
  regmatches(input,
             gregexpr("\\d+$",input))[[1]][1] |> 
  as.numeric()
B <-
  regmatches(input,
             gregexpr("\\d+$",input))[[2]][1] |> 
  as.numeric()
memoireA = memoireB = vector("numeric",5e6)

i = 1
while(memoireA[5e6] == 0){
  A = generator(A, "A")
  if(A %% 4 == 0){
    memoireA[i] = A
    i = i + 1
  }
}

i = 1
while(memoireB[5e6] == 0){
  B = generator(B, "B")
  if(B %% 8 == 0){
    memoireB[i] = B
    i = i + 1
  }
}

res = 0
for(i in seq_along(memoireA)){
  if(all(to_bin(memoireA[i]) == to_bin(memoireB[i]))){
    res = res + 1
  }
}
  
solution2 <-
  res