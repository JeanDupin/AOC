# Input ----

input <-
  get_input("https://adventofcode.com/2023/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

seeds <-
  as.numeric(strsplit(gsub("seeds: ","",input[1])," ")[[1]])

operations.ls <-
  input[3:length(input)] |> 
  paste(collapse = "\\n") |>
  (\(.){strsplit(.,split = "\\\\n\\\\n")[[1]]})() |> 
  sapply(function(.x){
    gsub("^.* map:\\\\n","",.x) |> 
      strsplit(split = "\\\\n")
  },USE.NAMES = F)


for(i in seq_along(operations.ls)){
  
  operations <-
    operations.ls[[i]]
  
  inputb <- seeds
  nouveaux <- c()
  for(j in seq_along(operations)){
    
    id.src = as.numeric(strsplit(operations[j]," ")[[1]][2])
    id.dst = as.numeric(strsplit(operations[j]," ")[[1]][1])
    pas = as.numeric(strsplit(operations[j]," ")[[1]][3])
    
    if(any(inputb >= id.src & inputb <= (id.src + pas - 1))){
      dedans = inputb[inputb >= id.src & inputb <= (id.src + pas - 1)]
      dedans = (dedans - id.src) + id.dst
      nouveaux = append(nouveaux,
                        dedans)
      inputb = inputb[!(inputb >= id.src & inputb <= (id.src + pas - 1))]
    } 
    
  }

  seeds <- c(nouveaux, inputb)
  
 }; rm(i, j,
      id.src, id.dst, pas, dedans,
      nouveaux, inputb, operations)


solution1 <-
  min(seeds)


# Partie 2 ----

seeds2 <-
  as.numeric(strsplit(gsub("seeds: ","",input[1])," ")[[1]]); rm(seeds)



minimums <- c()
for(a in c(1:length(seeds2))[c(1:length(seeds2)) %% 2 == 1]){
  seeds <- seq(seeds2[a],seeds2[a] - 1 + seeds2[a + 1])
  
  for(i in seq_along(operations.ls)){
    
    operations <-
      operations.ls[[i]]
    
    inputb <- seeds
    nouveaux <- c()
    for(j in seq_along(operations)){
      
      id.src = as.numeric(strsplit(operations[j]," ")[[1]][2])
      id.dst = as.numeric(strsplit(operations[j]," ")[[1]][1])
      pas = as.numeric(strsplit(operations[j]," ")[[1]][3])
      
      if(any(inputb >= id.src & inputb <= (id.src + pas - 1))){
        dedans = inputb[inputb >= id.src & inputb <= (id.src + pas - 1)]
        dedans = (dedans - id.src) + id.dst
        nouveaux = append(nouveaux,
                          dedans)
        inputb = inputb[!(inputb >= id.src & inputb <= (id.src + pas - 1))]
      } 
      
    }
    
    seeds <- c(nouveaux, inputb)
    
  }
  
  minimums <- append(minimums,
                     min(seeds))
  
}




rm(a, i, j,
      id.src, id.dst, pas, dedans,
      nouveaux, inputb, operations, seeds)


solution2 <-
  min(minimums)
