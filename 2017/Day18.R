# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/18/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

registres <-
  gsub("(^.*? )([a-z])(( |).*$)","\\2",input) |> 
  unique() |> 
  (\(.){.[. %in% letters]})() |> 
  (\(.){
    as.list(rep(0, length(.))) |> 
      (\(.z){names(.z) <- .; .z})()
  })()

i = 1
son = 0
while(i > 0 & i <= length(input)){
  instruction = input[i]
  if(grepl("snd",instruction)){
    indice = gsub("snd ","",instruction)
    if(indice %in% names(registres)){indice = registres[[indice]]}
    son = as.numeric(indice)
    i = i + 1
  } else if(grepl("set",instruction)){
    indices = strsplit(gsub("set ","",instruction)," ")[[1]]
    if(indices[2] %in% names(registres)){indices[2] = registres[[indices[2]]]}
    registres[[indices[1]]] = as.numeric(indices[2])
    i = i + 1
  } else if(grepl("add",instruction)){
    indices = strsplit(gsub("add ","",instruction)," ")[[1]]
    if(indices[2] %in% names(registres)){indices[2] = registres[[indices[2]]]}
    registres[[indices[1]]] = registres[[indices[1]]] + as.numeric(indices[2])
    i = i + 1
  } else if(grepl("mul",instruction)){
    indices = strsplit(gsub("mul ","",instruction)," ")[[1]]
    if(indices[2] %in% names(registres)){indices[2] = registres[[indices[2]]]}
    registres[[indices[1]]] = registres[[indices[1]]] * as.numeric(indices[2])
    i = i + 1
  } else if(grepl("mod",instruction)){
    indices = strsplit(gsub("mod ","",instruction)," ")[[1]]
    if(indices[2] %in% names(registres)){indices[2] = registres[[indices[2]]]}
    registres[[indices[1]]] = registres[[indices[1]]] %% as.numeric(indices[2])
    i = i + 1
  } else if(grepl("rcv",instruction)){
    indices = strsplit(gsub("rcv ","",instruction)," ")[[1]]
    if(registres[[indices[1]]] != 0){
      break
    } else {
      i = i + 1
    }
  } else if(grepl("jgz",instruction)){
    indices = strsplit(gsub("jgz ","",instruction)," ")[[1]]
    if(indices[[1]] %in% names(registres)){
      if(registres[[indices[1]]] > 0){
        i = i + as.numeric(indices[2])
      } else {
        i = i + 1
      }
    } else {
      i = i + as.numeric(indices[2])
    }
  }

}

solution1 <-
  son

# Partie 2 ----

solution2 <-
  NA