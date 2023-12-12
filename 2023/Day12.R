# Input ----

input <-
  readLines("2023/Inputs/Day12.txt")

# Partie 1 ----

solutions <- c()
for(i in seq_along(input)){
  
  positions.points = gregexpr("\\?",input[i])[[1]]
  nb.points = length(positions.points)
  
  if(nb.points == 0){stop("Pas de ?")}
  
  
  possibilites <-
    expand.grid(replicate(nb.points,c(".","#"), simplify = F)) |> 
    apply(1, function(.x){Reduce(paste0,.x)}) |> 
    sapply(USE.NAMES = F, simplify = F,
           function(.x){
             changements = strsplit(.x,"")[[1]]
             texte = strsplit(input[i],"")[[1]]
             
             texte[positions.points] <- changements
             
             return(gsub(" [0-9].*$","",paste(texte, collapse = "")))
           }) |> 
    unlist()
  
  patternes <-
    regmatches(input[i],gregexpr("[0-9].*$", input[i]))[[1]] |> 
    (\(.){strsplit(.,",")[[1]]})()
  n.patternes = length(patternes)
  
  
  solutions[i] <-
    sapply(possibilites, USE.NAMES = F, simplify = F,
         function(.x){
          dieses <-
           strsplit(.x,"\\.")[[1]] |> 
             (\(.){.[nchar(.) > 0]})()
          
          if(length(dieses) != n.patternes){
            return(FALSE)
          } else if (all(nchar(dieses) == as.numeric(patternes))){
            return(TRUE)
          } else {
            return(FALSE)
          }
         }) |> 
    unlist() |> 
    sum()
}; rm(i, n.patternes, nb.points,
      patternes, positions.points, possibilites)

solution1 <-
  sum(solutions)




