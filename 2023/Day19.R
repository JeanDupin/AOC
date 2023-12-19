# Input ----

input = clipr::read_clip()
input <-
  readLines("2023/Inputs/Day19.txt")


# Partie 1 ----

workflows <-
  input[1:(which(input == "")-1)]
parts <-
  input[(which(input == "")+1):length(input)]

instructions <-
  function(instruction){
    
    if(instruction == "A"){
      return(TRUE)
    }
    if(instruction == "R"){
      return(FALSE)
    }
    
    wfs <-
      workflows[grepl(paste0("^",instruction,"\\{"),workflows)] |> 
      (\(.){gsub("^.*\\{","",.)})() |> 
      (\(.){gsub("\\}","",.)})() |> 
      (\(.){strsplit(.,",")}[[1]])()
    
    for(j in wfs){
      if(grepl("(<|>)",j)){
        if(eval(parse(text = gsub(":.*$","",j)))){
          return(Recall(gsub("^.*:","",j)))
        } else {
          next
        }
      } else {
        return(Recall(j))
      }
    }
    
    
  }


status <- NULL
for(i in seq_along(parts)){
  # On crée les variables
  gsub("\\{","",parts[i]) |> 
    (\(.){gsub("\\}","",.) })() |> 
    (\(.){strsplit(.,",")}[[1]])() |> 
    lapply(function(.x){eval(parse(text = .x),
                             envir = .GlobalEnv)})
  
  # On applique les instructions jusqu'à ce que l'on trouve A ou R
  if(instructions("in")){
    status[i] <-
      x + m + a + s
  } else {
    status[i] <-
      0
  }
  
}; rm(x, m, a, s,
      i)

solution1 <-
  sum(status)


# Partie 2 ----

# Il faudrait obtenir toutes les combinaisons de chemins possibles
# Et ensuite de regarder les conditions pour que cela soit valide


generate_paths <- function(graph, start_node, current_path = NULL) {
  if (is.null(current_path)) {
    current_path <- character(0)
  }
  
  current_path <- c(current_path, start_node)
  if (!(start_node %in% names(graph)) || is.null(graph[[start_node]])) {
    return(list(current_path))
  }
  
  paths <- lapply(graph[[start_node]], function(neighbor) {
    generate_paths(graph, neighbor, current_path)
  })
  return(unlist(paths, recursive = FALSE))
}


# Pour les combinaisons qui peuvent amener à plusieurs A,
# On les distingue pour faciliter la suite. On mondifie workflows également
combinaisons <-
lapply(workflows,
       function(i){
         patternes <-
           gsub(".*\\{","",i) |> 
           (\(.){gsub("\\}$","",.)})() |> 
           (\(.){strsplit(.,",")[[1]]})() |> 
           (\(.){gsub(".*:","",.)})() 
         
         if(sum(patternes == "A") > 1){
           patternes[which(patternes == "A")] <-
             paste0("A",1:(sum(patternes == "A")))
         }
         
         return(patternes)
       }) |> 
  (\(.){`names<-`(.,
                  regmatches(workflows,
                             gregexpr("^[a-z]+",workflows)))})()

workflows <-
  lapply(workflows,
       function(.x){
         if(length(gregexpr("A",.x)[[1]]) > 1){
           indices_A <- unlist(gregexpr("A", .x)[[1]])
           
           for (i in seq_along(indices_A)) {
             index <- indices_A[i] + i - 1
             .x <- paste0(substr(.x, 1, index - 1), "A", i, substr(.x, index + 1, nchar(.x)))
           }
         }
         return(.x)
       })



all_paths <-
  generate_paths(combinaisons, start_node = "in")

all_paths <-
  lapply(all_paths,
       function(.x){
         any(grepl("A",.x))
       }) |> 
  (\(.){all_paths[unlist(.)]})()


nb.possibilites <- NULL
for(k in seq_along(all_paths)){
  
  conditions <- vector("list")
  for(i in 1:(length(all_paths[[k]])-1)){
    conditions <-
      append(conditions,
             workflows[grepl(paste0("^",all_paths[[k]][i],"\\{"),workflows)] |> 
               (\(.){gsub(".*\\{","",.)})() |> 
               (\(.){gsub("\\}$","",.)})() |> 
               (\(.){strsplit(.,",")[[1]]})() |> 
               (\(.){which(grepl(paste0(all_paths[[k]][i+1],"$"),.))})() |> 
               list())
  }; rm(i)
  
  conditions <-
    mapply(function(.x,.y){
      consignes <-
        workflows[grepl(paste0("^",.x,"\\{"),workflows)] |> 
        (\(.){gsub(".*\\{","",.)})() |> 
        (\(.){gsub("\\}$","",.)})() |> 
        (\(.){strsplit(.,",")[[1]]})() |> 
        (\(.){.[1:.y]})()
      
      if(.y > 1){
        consignes[1:(.y-1)] <-
          gsub("<","+",consignes[1:(.y-1)])
        consignes[1:(.y-1)] <-
          gsub(">","-",consignes[1:(.y-1)])
        consignes[1:(.y-1)] <-
          gsub("\\+",">=",consignes[1:(.y-1)])
        consignes[1:(.y-1)] <-
          gsub("\\-","<=",consignes[1:(.y-1)])
      }
      return(consignes)
      
    },
    all_paths[[k]][-length(all_paths[[k]])],
    conditions) |> 
    unlist() |> 
    (\(.){.[grepl("<",.) | grepl(">",.)]})() |> 
    (\(.){gsub(":.*","",.)})()
  
  x = m = a = s = 1:4000
  
  appliquer_conditions <-
    function(formule,lettre, valeur){
      
      sortie <-
        switch(formule,
               ">" = eval(parse(text = lettre))[eval(parse(text = lettre)) > valeur],
               ">=" = eval(parse(text = lettre))[eval(parse(text = lettre)) >= valeur],
               "<=" = eval(parse(text = lettre))[eval(parse(text = lettre)) <= valeur],
               "<" = eval(parse(text = lettre))[eval(parse(text = lettre)) < valeur])
      
      assign(lettre,
             sortie,
             envir = .GlobalEnv)
      
    }
  
  for(i in conditions){
    appliquer_conditions(
      lettre = substr(i,1,1),
      valeur = as.numeric(regmatches(i,gregexpr("[0-9]+$",i))[[1]]),
      formule = gsub("[[:alnum:]]","",i)
    )
  }; rm(i)
  
  nb.possibilites[k] <-
    prod(length(x),length(m),length(a),length(s))
}


solution2 <-
  as.character(sum(nb.possibilites))
