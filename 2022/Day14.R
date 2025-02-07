# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/14/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

ncol <-
  regmatches(input,
             gregexpr("\\d+,",input)) |> 
  (\(.){gsub(",","",unlist(.))})() |> 
  (\(.){c(min(as.numeric(.)),max(as.numeric(.)))})()

nrow <-
  regmatches(input,
             gregexpr(",\\d+",input)) |> 
  (\(.){gsub(",","",unlist(.))})() |> 
  (\(.){max(as.numeric(.))})()

matrice <-
  matrix(".", nrow, diff(ncol) + 1)

coords <-
  lapply(input, function(.x){
    instructions <-
      strsplit(.x," -> ")[[1]]
    res <- vector("list")
    for(i in seq_along(instructions)[-length(instructions)]){
      res[[i]] <-
      regmatches(instructions[c(i,i+1)],
                gregexpr("\\d+",instructions[c(i,i+1)])) |> 
        (\(.){as.numeric(unlist(.))})() |> 
        (\(.){
          cbind(.[2]:.[4],(.[1]:.[3])-min(ncol)+1)
        })()
    }
    do.call(rbind,res)
  }) |> 
  (\(.){do.call(rbind,.)})()

sandfall <- function(.x,.y){
  # Chute à l'infini
  if(.x > nrow(matrice) | .y %in% c(1,ncol(matrice))){
    return("Infini")
  }
  # Tombe droit
  if(matrice[.x+1,.y] == "."){
    return(c(.x+1,.y))
  }
  # Tombe à gauche
  if(matrice[.x+1,.y] == "#" & matrice[.x+1,.y-1] == "."){
    return(c(.x+1,.y-1))
  }
  # Tombe à droite
  if(matrice[.x+1,.y] == "#" & matrice[.x+1,.y-1] == "#" & matrice[.x+1,.y+1] == "."){
    return(c(.x+1,.y+1))
  }
  # Sinon reste en position
  return(c(.x,.y))
}

matrice[coords] <- "#"
matrice <-
  rbind(
    matrix(".", 1, diff(ncol) + 1),
    matrice
  )

while(T){
  sable = c(1,501-min(ncol))
  nextsable = sandfall(sable[1],sable[2])
  while(paste(sable, collapse = ";") != paste(nextsable, collapse = ";")){
    sable = nextsable
    nextsable = sandfall(sable[1],sable[2])
  }
  if(paste(sable,collapse = "") == "Infini"){
    break
  }
  matrice[sable[1],sable[2]] <- "#"
}


solution1 <-
  sum(matrice == "#") - nrow(unique(coords))

# Partie 2 ----

matrice <-
  matrix(".", nrow, max(ncol)*2)

coords <-
  lapply(input, function(.x){
    instructions <-
      strsplit(.x," -> ")[[1]]
    res <- vector("list")
    for(i in seq_along(instructions)[-length(instructions)]){
      res[[i]] <-
      regmatches(instructions[c(i,i+1)],
                gregexpr("\\d+",instructions[c(i,i+1)])) |> 
        (\(.){as.numeric(unlist(.))})() |> 
        (\(.){
          cbind(.[2]:.[4],(.[1]:.[3]))
        })()
    }
    do.call(rbind,res)
  }) |> 
  (\(.){do.call(rbind,.)})()

matrice[coords] <- "#"
matrice <-
  rbind(
    matrix(".", 1, ncol(matrice)),
    matrice,
    matrix(".", 1, ncol(matrice)),
    matrix("#", 1, ncol(matrice))
  )


while(T){
  sable = c(1,500)
  nextsable = sandfall(sable[1],sable[2])
  while(paste(sable, collapse = ";") != paste(nextsable, collapse = ";")){
    sable = nextsable
    nextsable = sandfall(sable[1],sable[2])
  }
  if(paste(sable,collapse = "") == "Infini"){
    break
  }
  if(sable[1] == 1 & sable[2] == 500){
    break
  }
  matrice[sable[1],sable[2]] <- "#"
  matrice
}


solution2 <-
  sum(matrice[-nrow(matrice),] == "#") - nrow(unique(coords)) + 1
