# Input ----

input <-
get_input("https://adventofcode.com/2024/day/15/input") |>
(\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  input[seq_len(which(input == "")-1)] |> 
  strsplit("") |> 
  unlist() |> 
  (\(.){gsub("@","X",.)})() |> 
  matrix(ncol = nchar(input[1]), byrow = T)


instructions <-
  input[(which(input == "")+1):length(input)] |> 
  strsplit("") |> 
  unlist() |> 
  (\(.){gsub("\\^","H",.)})() |> 
  (\(.){gsub(">","D",.)})() |> 
  (\(.){gsub("<","G",.)})() |> 
  (\(.){gsub("v","B",.)})()

position <-
  which(matrice == "X",arr.ind = T) |> 
  as.numeric()


for(i in instructions){
  if(i == "H"){
    sequence = rev(matrice[seq_len(position[1]),position[2]])
  } else if(i == "B"){
    sequence = matrice[(position[1]:nrow(matrice)),position[2]]
  } else if(i == "G"){
    sequence = rev(matrice[position[1],seq_len(position[2])])
  } else if(i == "D"){
    sequence = matrice[position[1],(position[2]:ncol(matrice))]
  }
  
  if(sequence[2] == "#"){
  next
  } else if(sequence[2] == "."){
    if(i == "H"){
      matrice[position[1]-1,position[2]] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(-1,0)
    } else if(i == "B"){
      matrice[position[1]+1,position[2]] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(1,0)
    } else if(i == "G"){
      matrice[position[1],position[2]-1] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(0,-1)
    } else if(i == "D"){
      matrice[position[1],position[2]+1] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(0,1)
    }
  } else {
    if(!grepl("X[O]+?\\.",paste(sequence,collapse = ""))){
      next
    } else {
      sequence <-
      paste(sequence,collapse = "") |> 
      (\(.){gsub("(X[O]+?)\\.",".\\1",.)})() |> 
      (\(.){strsplit(.,"")[[1]]})()
      if(i == "H"){
        matrice[seq_len(position[1]),position[2]] <- rev(sequence)
        position = position + c(-1,0)
      } else if(i == "B"){
        matrice[(position[1]:nrow(matrice)),position[2]] <- sequence
        position = position + c(1,0)
      } else if(i == "G"){
        matrice[position[1],seq_len(position[2])] <- rev(sequence)
        position = position + c(0,-1)
      } else if(i == "D"){
        matrice[position[1],(position[2]:ncol(matrice))] <- sequence
        position = position + c(0,1)
      }
    }
  }
}


solution1 <-
  which(matrice == "O",arr.ind = T) |> 
  apply(1,
    function(.x){
      100*(.x[1]-1) + (.x[2]-1)
    },
    simplify = F
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----


get_above <-
  function(coords, H = T){
    coords <-
    regmatches(coords,
      gregexpr("\\d+",coords))[[1]] |> 
      as.numeric()
    cx = coords[1]
    cy = coords[2]
    above = matrice[cx-1,cy]
    below = matrice[cx+1,cy]
    if(H){
      if(!above %in% c("J","L")){
        if(above == "."){
          return("")
         } else {
          return(NA)
        }
      }
      if(above == "J"){
          c(
            paste(cx-1,cy,sep = ";"),
            paste(cx-1,cy-1,sep = ";")
          )
        } else {
          c(
            paste(cx-1,cy,sep = ";"),
            paste(cx-1,cy+1,sep = ";")
          )
        }
      } else {
        if(!below %in% c("J","L")){
          if(below == "."){
            return("")
          } else {
            return(NA)
          }
        }
        if(below == "J"){
          c(
            paste(cx+1,cy,sep = ";"),
            paste(cx+1,cy-1,sep = ";")
          )
        } else {
          c(
            paste(cx+1,cy,sep = ";"),
            paste(cx+1,cy+1,sep = ";")
          )
        }
      }
  }
 
  


matrice <-
input[seq_len(which(input == "")-1)] |> 
strsplit("") |> 
unlist() |> 
(\(.){gsub("#","##",.)})() |> 
(\(.){gsub("\\.","..",.)})() |> 
(\(.){gsub("O","LJ",.)})() |> 
(\(.){gsub("@","X.",.)})() |> 
strsplit("") |> 
unlist() |> 
matrix(ncol = nchar(input[1])*2, byrow = T)


instructions <-
input[(which(input == "")+1):length(input)] |> 
strsplit("") |> 
unlist() |> 
(\(.){gsub("\\^","H",.)})() |> 
(\(.){gsub(">","D",.)})() |> 
(\(.){gsub("<","G",.)})() |> 
(\(.){gsub("v","B",.)})()

position <-
which(matrice == "X",arr.ind = T) |> 
as.numeric()

for(i in instructions){
  if(i == "H"){
    sequence = rev(matrice[seq_len(position[1]),position[2]])
  } else if(i == "B"){
    sequence = matrice[(position[1]:nrow(matrice)),position[2]]
  } else if(i == "G"){
    sequence = rev(matrice[position[1],seq_len(position[2])])
  } else if(i == "D"){
    sequence = matrice[position[1],(position[2]:ncol(matrice))]
  }
  
  if(sequence[2] == "#"){
  next
} else if(sequence[2] == "."){
  if(i == "H"){
    matrice[position[1]-1,position[2]] = "X"
    matrice[position[1],position[2]] = "."
    position = position + c(-1,0)
  } else if(i == "B"){
    matrice[position[1]+1,position[2]] = "X"
    matrice[position[1],position[2]] = "."
    position = position + c(1,0)
  } else if(i == "G"){
    matrice[position[1],position[2]-1] = "X"
    matrice[position[1],position[2]] = "."
    position = position + c(0,-1)
  } else if(i == "D"){
    matrice[position[1],position[2]+1] = "X"
    matrice[position[1],position[2]] = "."
    position = position + c(0,1)
  }
} else {
  if(!grepl("X[LJ]+?\\.",paste(sequence,collapse = ""))){
    next
  } else {
    sequence <-
    paste(sequence,collapse = "") |> 
    (\(.){gsub("(X[LJ]+?)\\.",".\\1",.)})() |> 
    (\(.){strsplit(.,"")[[1]]})()
    if(i == "H"){
      visited = vector("character")
      connected_cells = paste(position,collapse = ";")
      while(T){
        connected_cells = lapply(connected_cells, function(.y){get_above(.y,H = T)}) |> unlist()
        if(any(is.na(connected_cells))){break}
        connected_cells = connected_cells[connected_cells != ""]
        if(length(connected_cells) == 0){
          break
        } else {
          visited <-
          append(visited,connected_cells)
        }
      }
      if(any(is.na(connected_cells))){next}
      visited = unique(visited)
      visited <-
        regmatches(visited,
                   gregexpr("^\\d+",visited)) |> 
        unlist() |> 
        as.numeric() |> 
        (\(.){visited[order(.,decreasing = F)]})()
      for(j in visited){
        coords = regmatches(j,
                   gregexpr("\\d+",j))[[1]] |> as.numeric()
        matrice[coords[1]-1,coords[2]] = matrice[coords[1],coords[2]]
        matrice[coords[1],coords[2]] = "."
      }
      matrice[position[1]-1,position[2]] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(-1,0)
    } else if(i == "B"){
      visited = vector("character")
      connected_cells = paste(position,collapse = ";")
      while(T){
        connected_cells = lapply(connected_cells, function(.y){get_above(.y,H = F)}) |> unlist()
        if(any(is.na(connected_cells))){break}
        connected_cells = connected_cells[connected_cells != ""]
        if(length(connected_cells) == 0){
          break
        } else {
          visited <-
          append(visited,connected_cells)
        }
      }
      if(any(is.na(connected_cells))){next}
      visited = unique(visited)
      visited <-
        regmatches(visited,
                   gregexpr("^\\d+",visited)) |> 
        unlist() |> 
        as.numeric() |> 
        (\(.){visited[order(.,decreasing = T)]})()
      for(j in visited){
        coords = regmatches(j,
                   gregexpr("\\d+",j))[[1]] |> as.numeric()
        matrice[coords[1]+1,coords[2]] = matrice[coords[1],coords[2]]
        matrice[coords[1],coords[2]] = "."
      }
      matrice[position[1]+1,position[2]] = "X"
      matrice[position[1],position[2]] = "."
      position = position + c(1,0)
    } else if(i == "G"){
      matrice[position[1],seq_len(position[2])] <- rev(sequence)
      position = position + c(0,-1)
    } else if(i == "D"){
      matrice[position[1],(position[2]:ncol(matrice))] <- sequence
      position = position + c(0,1)
    }
    
  }
}

}


solution2 <-
which(matrice == "L",arr.ind = T) |> 
apply(1,
  function(.x){
    100*(.x[1]-1) + (.x[2]-1)
  },
  simplify = F
) |> 
unlist() |> 
sum()

  
  
  
