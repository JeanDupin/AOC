# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


permute <-
  function(.x) {
  if (length(.x) == 1) {
    return(list(.x))
  }
  
  result <- list()
  for (i in seq_along(.x)) {
    remaining <- .x[-i]
    sub_permutations <- permute(remaining)
    for (perm in sub_permutations) {
      result <- append(result, list(c(.x[i], perm)))
    }
  }
  result
}

puissances <-
  permute(0:4)

solution1 <-
  lapply(
    puissances,
    function(.pui){
      output = c(0,0)
      
      for(j in seq_along(.pui)){
        I = T
        output[1] = .pui[j]
        commandes <-
          input |> 
          (\(.){strsplit(.,",")[[1]]})()
        i = 1
        while(!grepl(commandes[i],"99$")){
          commande <-
            strrep("0",5-nchar(commandes[i])) |> paste0(commandes[i])
          
          if(grepl("3$",commande)){
            position = as.numeric(commandes[i+1])+1
            if(I){I = F;commandes[position] = output[1]}else{commandes[position] = output[2]}
            i = i + 2
          } else if(grepl("4$",commande)){
            
            position = as.numeric(commandes[i+1])+1
            outputf = as.numeric(commandes[position])
            
            i = i + 2
          } else {
            
            I2 = substr(commande,2,2)
            I3 = substr(commande,3,3)
            
            v1 <-
              ifelse(I3 == "0",as.numeric(commandes[(as.numeric(commandes[i+1])+1)]),as.numeric(commandes[i+1]))
            
            v2 <-
              ifelse(I2 == "0",as.numeric(commandes[(as.numeric(commandes[i+2])+1)]),as.numeric(commandes[i+2]))
            
            if(grepl("1$",commande)){
              commandes[(as.numeric(commandes[i+3])+1)] = v1 + v2
              i = i + 4 
            } else if(grepl("2$",commande)){
              commandes[(as.numeric(commandes[i+3])+1)] = v1 * v2
              i = i + 4 
            } else if(grepl("5$",commande)){
              if(v1 != 0){i = v2+1}else{i=i+3}
            } else if(grepl("6$",commande)){
              if(v1 == 0){i = v2+1}else{i=i+3}
            } else if(grepl("7$",commande)){
              if(v1 < v2){
                commandes[(as.numeric(commandes[i+3])+1)] = 1
                i = i + 4 
              } else{
                commandes[(as.numeric(commandes[i+3])+1)] = 0
                i = i + 4
              }
            } else if(grepl("8$",commande)){
              if(v1 == v2){
                commandes[(as.numeric(commandes[i+3])+1)] = 1
                i = i + 4 
              } else{
                commandes[(as.numeric(commandes[i+3])+1)] = 0
                i = i + 4
              }
            }
          }
        }
        output[2] = outputf
        
        
        
      }
      
      output[2]
      
    }
  ) |> 
  unlist() |> 
  max()


# Partie 2 ----

solution2 <-
  NA