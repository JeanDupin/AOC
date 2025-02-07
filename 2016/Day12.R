# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

registers <-
  list(a = 0, b = 0, c = 0, d = 0)

i = 1
while(i <= length(input)){
  if(grepl("^cpy",input[i])){
    y <- strsplit(input[i]," ")[[1]][3]
    x <- strsplit(input[i]," ")[[1]][2]
    if(x %in% letters){
      registers[[y]] <- registers[[x]]
    } else {
      registers[[y]] <- as.numeric(x)
    }
    i = i + 1
  } else if(grepl("^inc",input[i])){
    x <- strsplit(input[i]," ")[[1]][2]
    registers[[x]] <- registers[[x]] + 1
    i = i + 1
  } else if(grepl("^dec",input[i])){
    x <- strsplit(input[i]," ")[[1]][2]
    registers[[x]] <- registers[[x]] - 1
    i = i + 1
  } else if(grepl("^jnz",input[i])){
    y <- as.numeric(strsplit(input[i]," ")[[1]][3])
    x <- strsplit(input[i]," ")[[1]][2]
    x <- ifelse(x %in% letters, registers[[x]], as.numeric(x))
    if(x == 0){
      i = i + 1
    } else {
      i = i + y
    }
  }
}

solution1 <-
  registers[["a"]]

# Partie 2 ----

registers <-
  list(a = 0, b = 0, c = 1, d = 0)


solution2 <-
  registers[["a"]]
