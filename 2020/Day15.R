# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/15/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

numbers <-
  as.numeric(strsplit(input,",")[[1]])

i = length(numbers) + 1

while(i <= 2020){
  if(sum(numbers == numbers[i-1]) == 1){
    numbers[i] = 0
    i = i + 1
  } else {
    numbers[i] <-
      which(numbers == numbers[i-1]) |> 
      rev() |> 
      (\(.){.[c(1,2)]})() |> 
      (\(.){diff(rev(.))})()
    i = i + 1
  }
}


solution1 <-
  numbers[2020]

# Partie 2 ----

numbers <-
  as.numeric(strsplit(input,",")[[1]])

nombres = vector("list",30000001)
nombres[seq_along(nombres)] = -1

for(i in seq_along(numbers)){
  nombres[(numbers[i]+1)] = i
}

i = length(numbers) + 1
nombre = 0

while(i < 30000001){
  if(nombres[[(nombre+1)]] != -1){
    nextnombre = i - nombres[[(nombre+1)]]
  } else {
    nextnombre = 0
  }
  nombres[[(nombre+1)]] = i
  i = i + 1
  nombre = nextnombre
}



solution2 <-
  which(unlist(nombres) == 30000000)-1
