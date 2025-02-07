# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/11/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()
input = "vzbxkghb"

# Partie 1 ----


init_password <- function(.input){
  if(!grepl("i|l|o",.input)){
    return(.input)
  } 

  sortie = vector("character",nchar(.input))
  mot = strsplit(input,"")[[1]]
  id = which(mot %in% c("i","l","o"))[1]
  sortie[((id+1):length(sortie))] = "a"
  sortie[1:(id-1)] = mot[1:(id-1)]
  sortie[id] = letters[which(letters == mot[3]) + 1]
  return(paste(sortie,collapse = ""))
}

next_password <- function(.mot){
  
  mot = rev(strsplit(.mot,"")[[1]])
  
  mot_suivant <-
    sapply(mot, function(.x){which(letters == .x)+1}) |> 
    (\(.){
      ifelse(.%in% c(9,12,15),.+1,.)
    })() |> 
      (\(.){
        letters[ifelse(. == 27,1,.)]
      })()
  
  nouveau_mot = vector("character",length(mot))
  for(i in seq_along(mot)){
    if(i == 1){
      nouveau_mot[i] = mot_suivant[i]
    } else {
      nouveau_mot[i] = ifelse(mot_suivant[(i-1)] == "a" & nouveau_mot[(i-1)] != mot[(i-1)], mot_suivant[i], mot[i])
    }
  }; rm(i)
  
  paste(rev(nouveau_mot),collapse = "")
}

check_password <- function(.password){
  verif3 <-
    paste0(letters,letters) |> 
    sapply(function(.x){grepl(.x,.password)}) |> 
    (\(.){sum(.) >= 2})()
  if(!verif3){return(F)}

  verif1 <-
    expand.grid(1:26,1:26,1:26) |> 
    (\(.){
      .[(.$Var3 == .$Var2 + 1) & (.$Var2 == .$Var1 + 1),]
    })() |> 
    apply(1,function(.x){paste(letters[.x],collapse = "")}) |> 
    paste(collapse = "|") |> 
    grepl(.password)
  if(!verif1){return(F)}

  verif2 <-
    !grepl("i|l|o",.password)
  if(!verif2){return(F)}

  return(T)
}


mot_de_passe = init_password(input)

while(!check_password(mot_de_passe)){
  mot_de_passe = next_password(mot_de_passe)
}

solution1 <-
  mot_de_passe

# Partie 2 ----

mot_de_passe = next_password(solution1)

while(!check_password(mot_de_passe)){
  mot_de_passe = next_password(mot_de_passe)
}

solution2 <-
  mot_de_passe
