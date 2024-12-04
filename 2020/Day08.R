# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

accumulator = 0
i = 1
stored <- vector("numeric")

while(!i %in% stored){
  instruction <-
    input[i]
  
  if(grepl("nop",instruction)){
    stored <- append(stored,i)
    i = i + 1
  } else if(grepl("jmp",instruction)){
    stored <- append(stored,i)
    i = i + as.numeric(strsplit(instruction," ")[[1]][2])
  } else if(grepl("acc",instruction)){
    stored <- append(stored,i)
    accumulator = accumulator + as.numeric(strsplit(instruction," ")[[1]][2])
    i = i + 1
  }
  
}

solution1 <-
  accumulator

# Partie 2 ----

instructions <-
  lapply(grep("nop",input),
         function(.x) {
           new_instructions <- input
           new_instructions[.x] <- sub("^nop", "jmp", new_instructions[.x])
           return(new_instructions)
         })
instructions <-
  lapply(grep("jmp",input),
         function(.x) {
           new_instructions <- input
           new_instructions[.x] <- sub("jmp", "nop", new_instructions[.x])
           return(new_instructions)
         }) |> 
  (\(.){append(instructions,.)})()

solution2 <-
lapply(
  instructions,
  function(.x){
    accumulator = 0
    i = 1
    stored <- vector("numeric")
    
    while(!i %in% stored){
      instruction <-
        .x[i]
      
      if(i > length(input)){
        break
      }
      
      if(grepl("nop",instruction)){
        stored <- append(stored,i)
        i = i + 1
      } else if(grepl("jmp",instruction)){
        stored <- append(stored,i)
        i = i + as.numeric(strsplit(instruction," ")[[1]][2])
      } else if(grepl("acc",instruction)){
        stored <- append(stored,i)
        accumulator = accumulator + as.numeric(strsplit(instruction," ")[[1]][2])
        i = i + 1
      }
      
    }
    
    data.frame(
      acc = accumulator,
      I = ifelse(i <= length(input),F,T)
    )
  }
) |> 
  (\(.){Reduce("rbind",.)})() |> 
  (\(.){.[which(.$I),"acc"]})()


