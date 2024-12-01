# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/21/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


matrice = matrix(
  unlist(strsplit(input,"")),
  ncol = length(input),
  byrow = T
)

get_adjacent_elements <- function(row, col) {
  n_rows <- nrow(matrice)
  n_cols <- ncol(matrice)
  adjacent_elements <- list()
  
  if (row > 1) {
    if(matrice[row - 1, col] != "#"){
      adjacent_elements$N <- paste((row - 1), col,sep = ";")
    }
  }
  
  if (row < n_rows) {
    if(matrice[row + 1, col] != "#"){
      adjacent_elements$S <- paste((row + 1), col,sep = ";")
    }
  }
  
  
  if (col > 1) {
    if(matrice[row, col - 1] != "#"){
      adjacent_elements$O <- paste(row, (col-1),sep = ";")
    }
  }
  
  
  if (col < n_cols) {
    if(matrice[row, col + 1] != "#"){
      adjacent_elements$E <- paste(row, (col+1),sep = ";")
    }
  }
  
  return(adjacent_elements)
}


to.visit <-
  which(matrice == "S", arr.ind = T) |> 
  paste(collapse = ";")
deja.visite = vector("character")
odds = vector("character")
evens = to.visit


for(i in 1:64){
  
  a.visiter.apres <-
    lapply(to.visit,
           function(.x){
             r = as.numeric(gsub(";.*","",.x))
             c = as.numeric(gsub(".*;","",.x))
             
             get_adjacent_elements(r,c)
           }) |> 
    unlist() |> 
    unique() |> 
    (\(.){.[which(!(. %in% deja.visite))]})()
  
  
  if(i %% 2 == 1){
    odds <-
      unique(c(odds, a.visiter.apres))
  } else {
    evens <-
      unique(c(evens, a.visiter.apres))
  }
  
  deja.visite <-
    c(deja.visite,to.visit)
  
  to.visit <-
    a.visiter.apres
}

solution1 <-
  length(evens)

# Partie 2 ----

# Pour chaque élément de la matrice accessible, il faut sa parité, et ensuite si elle est située à plus de 65 cases


i = 65
while(length(to.visit) > 1){
  
  a.visiter.apres <-
    lapply(to.visit,
           function(.x){
             r = as.numeric(gsub(";.*","",.x))
             c = as.numeric(gsub(".*;","",.x))
             
             get_adjacent_elements(r,c)
           }) |> 
    unlist() |> 
    unique() |> 
    (\(.){.[which(!(. %in% deja.visite))]})()
  
  
  if(i %% 2 == 1){
    odds <-
      unique(c(odds, a.visiter.apres))
  } else {
    evens <-
      unique(c(evens, a.visiter.apres))
  }
  
  deja.visite <-
    c(deja.visite,to.visit)
  
  to.visit <-
    a.visiter.apres
  
  if(i == 65){
    odds.m65 <- odds
    evens.m65 <- evens
  }
  
  i = i + 1
}

evens.p65 = evens[which(!(evens %in% evens.m65))]
odds.p65 = odds[which(!(odds %in% odds.m65))]

n = (26501365-65)%/%131

solution2 <-
  (n+1)*(n+1)*length(odds) + n*n*length(evens) - (n+1)*length(odds.p65) + n*length(evens.p65)


