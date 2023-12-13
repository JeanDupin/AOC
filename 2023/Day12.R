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


# Partie 2 ----
# Source : https://github.com/Naturage/Advent-of-code-2023/blob/main/Day%2012/Scripts/Day%2012%20attempt%202.R

patternes <-
  sapply(input, function(.x){
    gsub(" .*$","",.x) |> 
      rep(5) |> 
      paste(collapse = "?") |>
      (\(.){strsplit(.,"")[[1]]})()
  }, USE.NAMES = F, simplify = F)


positions <-
  sapply(input, function(.x){
    gsub("^.* ","",.x) |>
      (\(.){strsplit(.,",")[[1]]})() |> 
      as.numeric() |> 
      rep(5)
  }, USE.NAMES = F, simplify = F)




bigsolver <- function(.x, .y){
  
  positions <<-
    matrix(rep(-1, (length(.x)+1) * (length(.y) + 1)),
           nrow = (length(.x) + 1))
  
  for (i in 1:length(.x)){
    positions[i, length(.y) + 1] <<-
      ifelse(all(.x[i:length(.x)] %in% c(".","?")), 1, 0)
  }
  
  positions[length(.x) + 1, ] <<-
    0
  positions[length(.x) + 1, length(.y) + 1] <<-
    1
  
  for (i in seq(length(.x), 1, by = -1)){
    for (j in seq(length(.y), 1, by = -1)){
      MAJ.position(i, j, .x, .y)
    }
  }
  return(positions[1,1])
}




MAJ.position <- function(i, j, .x, .y){
  
  positions[i, j] <<-
    0
  next_string <-
    .y[j]
  
  if(.x[i] %in% c(".","?")){
    positions[i, j] <<-
      positions[i, j] + positions[i + 1, j]
  }
  if (.x[i] %in% c("#","?")){
    # but is too short, do nothing.
    if (i + next_string > length(.x) + 1){
      
    } else if (i + next_string == length(.x) + 1){
      if (all(.x[i:(i - 1 + next_string)] %in% c("?","#"))){
        positions[i, j] <<-
          positions[i, j] + positions[length(.x) + 1, j + 1]
      }
    } else {
      if ((all(.x[i:(i - 1 + next_string)] %in% c("?","#"))) & (.x[i + next_string] %in% c(".","?"))){
        positions[i, j] <<-
          positions[i, j] + positions[i + next_string + 1, j + 1]
      }
    }
  }
  return(NULL)
}

solution2 <-
  mapply(bigsolver, 
         patternes,
         positions) |> 
  sum()

