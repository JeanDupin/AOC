# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


commandes <-
  input |> 
  (\(.){strsplit(.,",")[[1]]})()


output = vector("numeric")

i = 1
while(!grepl(commandes[i],"99$")){
  commande <-
    strrep("0",5-nchar(commandes[i])) |> paste0(commandes[i])
  
  if(grepl("3$",commande)){
    position = as.numeric(commandes[i+1])+1
    commandes[position] = 1
    i = i + 2
  } else if(grepl("4$",commande)){
    
    position = as.numeric(commandes[i+1])+1
    output = append(output,as.numeric(commandes[position]))
    
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
    } else {
      commandes[(as.numeric(commandes[i+3])+1)] = v1 * v2
    }
    i = i + 4 
  }
  if(i %% 10 == 0){print(i)}
}

solution1 <-
  output[length(output)]


# Partie 2 ----

commandes <-
  input |> 
  (\(.){strsplit(.,",")[[1]]})()


output = vector("numeric")

i = 1
while(!grepl(commandes[i],"99$")){
  print(i)
  commande <-
    strrep("0",5-nchar(commandes[i])) |> paste0(commandes[i])
  
  if(grepl("3$",commande)){
    position = as.numeric(commandes[i+1])+1
    commandes[position] = 5
    i = i + 2
  } else if(grepl("4$",commande)){
    
    position = as.numeric(commandes[i+1])+1
    output = append(output,as.numeric(commandes[position]))
    
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

solution2 <-
  output[length(output)]
