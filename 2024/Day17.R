# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/17/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

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
    register[["B"]] <- myxor(to_bin(register[["B"]]),to_bin(operand))
  } else if(opcode == "2"){
    register[["B"]] <- c(0,1,2,3,unlist(register))[operand+1] %% 8
  } else if(opcode == "3"){
    i <- ifelse(register[["A"]] == 0,i+2,operand+1)
    next
  } else if(opcode == "4"){
    register[["B"]] <- myxor(to_bin(register[["B"]]),to_bin(register[["C"]]))
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


get_candidates <- function(vA){
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
        register[["B"]] <- myxor(to_bin(register[["B"]]),to_bin(operand))
       } else if(opcode == "2"){
         register[["B"]] <- c(0,1,2,3,unlist(register))[operand+1] %% 8
       } else if(opcode == "3"){
         i <- ifelse(register[["A"]] == 0,i+2,operand+1)
         next
       } else if(opcode == "4"){
         register[["B"]] <- myxor(to_bin(register[["B"]]),to_bin(register[["C"]]))
       } else if(opcode == "5"){
         output <- append(output,c(0,1,2,3,unlist(register))[operand+1] %% 8)
       } else if(opcode == "6"){
         register[["B"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
       } else if(opcode == "7"){
         register[["C"]] <- floor(register[["A"]] / (2**c(0,1,2,3,unlist(register))[operand+1]))
       }
   
       i = i + 2
   
     }
   
     output |> 
       (\(.){names(.) <- NULL;.})()
   }

calcul_niveau <- function(...){
  params <- c(...)
  resultat <- params[1]

  for(i in 2:length(params)){
    resultat <- resultat * 8 + params[i]
  }
  
  resultat
}

etape = 1
candidats = expand.grid(0,0:7)
while(etape < length(instructions)){
  candidats <-
    apply(candidats, 1, function(.x){
      I1 = calcul_niveau(unlist(.x)) |> get_candidates() |> paste(collapse = ";")
      I2 = instructions |> 
            (\(.){.[(length(.)-etape+1):length(.)]})() |> 
            paste(collapse = ";")
      I1 == I2
    },simplify = F) |> 
    unlist() |> 
    (\(.){candidats[.,]})()
  
  candidats = merge(candidats |> (\(.){names(.) <- NULL; .})(),0:7, all = T)
  
  etape = etape + 1
}

solution2 <-
  apply(candidats, 1, function(.x){
    I1 = calcul_niveau(unlist(.x)) |> get_candidates() |> paste(collapse = ";")
    I2 = instructions |> 
    (\(.){.[(length(.)-etape+1):length(.)]})() |> 
    paste(collapse = ";")
    I1 == I2
  },simplify = F) |> 
  unlist() |> 
  (\(.){candidats[.,]})() |> 
  apply(1, function(.x){
    calcul_niveau(unlist(.x))
  },simplify = F) |> 
  unlist() |> 
  min()
