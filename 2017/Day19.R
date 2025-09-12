# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/19/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input),
         byrow = T)


position <-
  which(matrice == "|",
        arr.ind = T) |> 
  data.frame() |> 
  (\(.){.[order(.$row),][1,]})()

fin <-
  which(matrice == "Z",
        arr.ind = T)

direction = "D"

memoire = vector("character")

while(position[,"row"] != fin[,"row"] | position[,"col"] != fin[,"col"]){
  if(direction == "D"){
    position[,"row"] <- position[,"row"] + 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "R"){
    position[,"col"] <- position[,"col"] + 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "L"){
    position[,"col"] <- position[,"col"] - 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "U"){
    position[,"row"] <- position[,"row"] - 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  }
}


solution1 <-
  paste(memoire, collapse = "")

# Partie 2 ----

position <-
  which(matrice == "|",
        arr.ind = T) |> 
  data.frame() |> 
  (\(.){.[order(.$row),][1,]})()

direction = "D"
i = 1

while(position[,"row"] != fin[,"row"] | position[,"col"] != fin[,"col"]){
  i = i + 1
  if(direction == "D"){
    position[,"row"] <- position[,"row"] + 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "R"){
    position[,"col"] <- position[,"col"] + 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "L"){
    position[,"col"] <- position[,"col"] - 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["D"] = matrice[position[,"row"]+1,position[,"col"]]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  } else if(direction == "U"){
    position[,"row"] <- position[,"row"] - 1
    
    if(matrice[position[,"row"],position[,"col"]] %in% c("|","-")){
      next
    }
    if(matrice[position[,"row"],position[,"col"]] %in% LETTERS){
      memoire = append(memoire,
                       matrice[position[,"row"],position[,"col"]])
      next
    }
    if(matrice[position[,"row"],position[,"col"]] == "+"){
      directions <-
        c("U" = " ", "D" = " ",
          "L" = " ", "R" = " ")
      directions["U"] = matrice[position[,"row"]-1,position[,"col"]]
      directions["R"] = matrice[position[,"row"],position[,"col"]+1]
      directions["L"] = matrice[position[,"row"],position[,"col"]-1]
      direction <-
        names(directions)[which(directions != " ")]
    }
  }
}


solution2 <-
  i
