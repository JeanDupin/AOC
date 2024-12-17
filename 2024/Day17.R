# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/17/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

register <-
  input[seq_len(which(input == "")-1)] |> 
  (\(.){gsub(".*: ","",.)})() |> 
  as.numeric() |> 
  as.list() |> 
  (\(.){names(.) <- c("A","B","C"); .})()

instructions <-
  input[which(input == "")+1] |> 
  (\(.){gsub(".*: ","",.)})() |> 
  (\(.){strsplit(.,",")[[1]]})()

output = vector("numeric")
i = 1
while(i < length(instructions)){
  opcode = instructions[i]
  operand = as.numeric(instructions[i+1])

  if(opcode == "0"){
    register[["A"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
  } else if(opcode == "1"){
    register[["B"]] <- bitwXor(register[["B"]],operand)
  } else if(opcode == "2"){
    register[["B"]] <- c(0,1,2,3,unlist(register))[operand+1] %% 8
  } else if(opcode == "3"){
    i <- ifelse(register[["A"]] == 0,i+2,operand+1)
    next
  } else if(opcode == "4"){
    register[["B"]] <- bitwXor(register[["B"]],register[["C"]])
  } else if(opcode == "5"){
    output <- append(output,c(0,1,2,3,unlist(register))[operand+1] %% 8)
  } else if(opcode == "6"){
    register[["B"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
  } else if(opcode == "7"){
    register[["C"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
  }

  i = i + 2

}

solution1 <-
  paste(output,collapse = ",")

# Partie 2 ----


for(vA in seq_len(1000000)){

  register <-
  input[seq_len(which(input == "")-1)] |> 
  (\(.){gsub(".*: ","",.)})() |> 
  as.numeric() |> 
  as.list() |> 
  (\(.){names(.) <- c("A","B","C"); .})()
  register[["A"]] = vA


  output = vector("numeric")
  i = 1
  while(i < length(instructions)){
    opcode = instructions[i]
    operand = as.numeric(instructions[i+1])

    if(opcode == "0"){
      register[["A"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
    } else if(opcode == "1"){
      register[["B"]] <- bitwXor(register[["B"]],operand)
    } else if(opcode == "2"){
      register[["B"]] <- c(0,1,2,3,unlist(register))[operand+1] %% 8
    } else if(opcode == "3"){
      i <- ifelse(register[["A"]] == 0,i+2,operand+1)
      next
    } else if(opcode == "4"){
      register[["B"]] <- bitwXor(register[["B"]],register[["C"]])
    } else if(opcode == "5"){
      output <- append(output,c(0,1,2,3,unlist(register))[operand+1] %% 8)
    } else if(opcode == "6"){
      register[["B"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
    } else if(opcode == "7"){
      register[["C"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
    }

    i = i + 2

  }

if(length(output) != length(instructions)){
  next
} else if(all(as.character(output) == instructions)){
  break
} else {
  next
}
  
}
  
solution2 <-
  vA