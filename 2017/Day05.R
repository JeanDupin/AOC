# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 


# Partie 1 ----

position = 1
i = 0
inputa = as.numeric(input)
while(position <= length(input)){
  inputa[position] = inputa[position] + 1
  position = position + inputa[position] - 1
  i = i + 1
}

solution1 <-
  i; rm(i, position)

# Partie 2 ----

position = 1
i = 0
inputb = as.numeric(input)
while(position <= length(input)){
  offset = inputb[position]
  if(is.na(offset)){break}
  if(offset >= 3){
    inputb[position] = inputb[position] - 1
  } else {
    inputb[position] = inputb[position] + 1
  }
  position = position + offset
  i = i + 1
}

solution2 <-
  i; rm(i, position, offset)
