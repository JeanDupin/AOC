# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/11/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

passage <-
  c("thulium" = "A", "plutonium" = "B",
    "strontium" = "C", "promethium" = "D",
    "ruthenium" = "E")

input1 <-
  regmatches(input,
             gregexpr("(\\w+)\\s+generator",input)) |> 
  sapply(function(.x){gsub(" generator","",.x)}) |> 
  sapply(function(.x){passage[.x] |> 
      setNames(NULL)}) |> 
  sapply(function(.x){
    if(length(.x) >= 1){
      paste0(.x,"G")
    } else {
      .x
    }
  })
input2 <-
  regmatches(gsub("-compatible","",input),
             gregexpr("(\\w+)\\s+microchip",gsub("-compatible","",input))) |> 
  sapply(function(.x){gsub(" microchip","",.x)}) |> 
  sapply(function(.x){passage[.x] |> 
      setNames(NULL)}) |> 
  sapply(function(.x){
    if(length(.x) >= 1){
      paste0(.x,"M")
    } else {
      .x
    }
  })

etat <-
  list(
    etage = 1,
    f4 = c(input1[[4]],input2[[4]]),
    f3 = c(input1[[3]],input2[[3]]),
    f2 = c(input1[[2]],input2[[2]]),
    f1 = c(input1[[1]],input2[[1]],"YG","YM","ZG","ZM"),
    n_moves = 0
  )


make_hash <- function(.x){
  sapply(.x[-6],function(.y){paste(sort(gsub("[GM]","",.y)), collapse = ";")}) |> 
    paste(collapse = "_") |> 
    digest::digest(serialize = F)
}

explode <- function(x){
  if(is.null(x) | length(x) == 0){return(T)}
  if(!any(grepl("M$",x))){return(T)}
  chips <-
    gsub("M$","",x[grepl("M$",x)])
  generators <-
    gsub("G$","",x[grepl("G$",x)])
  
  all(chips %in% generators) | length(generators) == 0
}


etats <- list(etat)
hashes <- vector("character")

min_moves = Inf
secu = 1

while(min_moves > 200 & secu <= 150000){
  if(secu %% 100 == 0){print(secu)}
  secu = secu + 1
  id <-
    sapply(etats,function(.x){.x[["n_moves"]]}) |> 
    which.min()
  
  etat <- etats[[id]]
  etats[[id]] <- NULL
  
  if(etat$etage == 1){
    direction = 2
  } else if(etat$etage == 4){
    direction = 3
  } else {
    direction = c(etat$etage+1,etat$etage-1)
  }
  
  elements <-
    etat[[c(4:1)[etat$etage] + 1]]
  if(length(elements) > 1){
    elements <-
      c(
        combn(elements,1,simplify = F),
        combn(elements,2,simplify = F)
      )
  } else {
    elements <-
      c(
        combn(elements,1,simplify = F)
      )
  }
  
  for(i in direction){
    for(j in elements){
      etat2 <- etat
      etat2[[c(4:1)[etat$etage] + 1]] <-
        etat2[[c(4:1)[etat$etage] + 1]] |> 
        (\(.){.[!(. %in% j)]})()
      etat2[[c(4:1)[i] + 1]] <-
        etat2[[c(4:1)[i] + 1]] |> 
        append(j)
      etat2$etage = i
      etat2$n_moves = etat2$n_moves + 1
      
      if(!all(sapply(etat2[2:5],explode))){
        next
      }
      
      if(all(sapply(etat2[3:5],function(.x){length(.x) == 0}))){
        min_moves <-
          min(min_moves, etat2$n_moves)
      }
      
      hash = make_hash(etat2)
      if(!hash %in% hashes){
        hashes <-
          append(hashes,hash)
        etats <-
          append(etats,
                 list(etat2))
      }
      
    }
  }
}


solution1 <-
  min_moves

# Partie 2 ----
  

etat <-
  list(
    etage = 1,
    f4 = c(input1[[4]],input2[[4]]),
    f3 = c(input1[[3]],input2[[3]]),
    f2 = c(input1[[2]],input2[[2]]),
    f1 = c(input1[[1]],input2[[1]]),
    n_moves = 0
  )

etats <- vector("list",50000)
etats[1] <- list(etat)
hashes <- vector("character",1000000)
idh <- 1

min_moves = Inf
secu = 1

while(min_moves > 200 & secu <= 300000){
  if(secu %% 100 == 0){print(secu)}
  secu = secu + 1
  id <-
    sapply(etats,function(.x){.x[["n_moves"]] |> 
        (\(.){ifelse(is.null(.),Inf,.)})()}) |> 
    which.min()
  
  etat <- etats[[id]]
  etats[id] <- list(NULL)
  
  if(etat$etage == 1){
    direction = 2
  } else if(etat$etage == 4){
    direction = 3
  } else {
    direction = c(etat$etage+1,etat$etage-1)
  }
  
  elements <-
    etat[[c(4:1)[etat$etage] + 1]]
  if(length(elements) > 1){
    elements <-
      c(
        combn(elements,1,simplify = F),
        combn(elements,2,simplify = F)
      )
  } else {
    elements <-
      c(
        combn(elements,1,simplify = F)
      )
  }
  
  for(i in direction){
    for(j in elements){
      etat2 <- etat
      etat2[[c(4:1)[etat$etage] + 1]] <-
        etat2[[c(4:1)[etat$etage] + 1]] |> 
        (\(.){.[!(. %in% j)]})()
      etat2[[c(4:1)[i] + 1]] <-
        etat2[[c(4:1)[i] + 1]] |> 
        append(j)
      etat2$etage = i
      etat2$n_moves = etat2$n_moves + 1
      
      if(!all(sapply(etat2[2:5],explode))){
        next
      }
      
      if(all(sapply(etat2[3:5],function(.x){length(.x) == 0}))){
        min_moves <-
          min(min_moves, etat2$n_moves)
      }
      
      hash = make_hash(etat2)
      if(!hash %in% hashes){
        hashes[idh] <- hash
        idh <- idh + 1
        
        etats[which(sapply(etats, is.null))[1]] <-
          list(etat2)
      }
      
    }
  }
}


solution2 <-
  min_moves
