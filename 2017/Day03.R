# Input ----

input = 265149

# Partie 1 ----

create.grid <-
  function(size,last.number){
    numbers <-
      last.number + (1:(4*(size-1)))
    
    pos.seq = seq(-(size-1)/2,(size-1)/2)
    l.seq = length(pos.seq)
    positions <-
      c(paste(pos.seq[l.seq],
              (pos.seq[1:(l.seq-1)]+1),
              sep = ";"),
        paste((rev(pos.seq)[1:(l.seq-1)]-1),
              pos.seq[l.seq],
              sep = ";"),
        paste(pos.seq[1],
              (rev(pos.seq)[1:(l.seq-1)]-1),
              sep = ";"),
        paste((pos.seq[1:(l.seq-1)]+1),
              pos.seq[1],
              sep = ";")
        )
    
    return(numbers |> 
             (\(.){`names<-`(.,positions)})())
    
  }


i.size = 3
i.nb = 1
sortie = c("0;0" = i.nb)
while(max(sortie) < input){
  
  sortie <- append(sortie,
                   create.grid(size = i.size,
                               i.nb))
  
  i.nb = max(create.grid(size = i.size,
                         i.nb))
  i.size = i.size + 2
}

solution1 <-
  sortie[sortie == input] |> 
  names() |> 
  (\(.){strsplit(.,";")[[1]]})() |> 
  (\(.){abs(as.numeric(.))})() |> 
  sum()
  

# Partie 2 ----


i.size = 3
i.nb = 1
sortie = c("0;0" = i.nb)
while(max(sortie) < 100){
  
  sortie <- append(sortie,
                   create.grid(size = i.size,
                               i.nb))
  
  i.nb = max(create.grid(size = i.size,
                         i.nb))
  i.size = i.size + 2
}

sortie[1:9] = c(1,1,2,4,5,10,11,23,25)
sortie[10:length(sortie)] <- rep(1,length(sortie)-9)


i = 10

while(sortie[(i-1)] < input){
  
  X = gsub(";.*","",names(sortie[i])) |> 
    as.numeric() |> 
    (\(.){c(.-1,.,.+1)})()
  Y = gsub(".*;","",names(sortie[i])) |> 
    as.numeric() |> 
    (\(.){c(.-1,.,.+1)})()
  
  
  
  sortie[i] <-
    expand.grid(X,Y) |> 
    (\(.){paste(.$Var1,.$Var2,sep = ";")})() |> 
    (\(.){.[. != names(sortie[i])]})() |> 
    (\(.){which(names(sortie) %in% .)})() |> 
    (\(.){.[. < i]})() |> 
    (\(.){sortie[.]})() |> 
    sum()
  
  i = i + 1

  
}

solution2 <-
  sortie[(i-1)]
