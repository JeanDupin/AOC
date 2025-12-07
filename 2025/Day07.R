# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/7/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  strsplit("") |> 
  (\(.){matrix(unlist(.), nrow = 142, byrow = T)})()

# Partie 1 ----

laser = list(which(input == "S", arr.ind = T))
visited = vector("character")
splited <- 0

while(length(laser) > 0){
  
  next_laser <- vector("list")
  for(i in seq_along(laser)){
    visited = append(visited, paste(laser[[i]], collapse = ";"))
    if(laser[[i]][1] == nrow(input)){next}
    if(input[laser[[i]][1]+1,laser[[i]][2]] == "."){
      if(paste(c(laser[[i]][1]+1,laser[[i]][2]),collapse = ";") %in% visited){
        next
      } else {
        next_laser <- append(next_laser,list(c(laser[[i]][1]+1,laser[[i]][2])))
      }
    } else {
      splited <- splited + 1
      if(!(paste(c(laser[[i]][1]+1,laser[[i]][2]-1), collapse = ";")%in% visited)){
        next_laser <- append(next_laser,list(c(laser[[i]][1]+1,laser[[i]][2]-1)))
      }
      if(!(paste(c(laser[[i]][1]+1,laser[[i]][2]+1), collapse = ";")%in% visited)){
        next_laser <- append(next_laser,list(c(laser[[i]][1]+1,laser[[i]][2]+1)))
      }
    }
  }
  laser <- unique(next_laser)
  
}

solution1 <-
  splited

# Partie 2 ----
  
lasers = paste(which(input == "S", arr.ind = T), collapse = ";")
lasers = list(1) |> setNames(lasers)
res = 0

while(length(lasers) > 0){
  
  next_laser <- vector("list")
  for(i in seq_along(lasers)){
    laser <- names(lasers)[[i]] |> (\(.){strsplit(.,";")[[1]]})() |> as.numeric()
    if(laser[1] == nrow(input)){next}
    if(input[laser[1]+1,laser[2]] == "."){
    next_laser[[paste(laser[1]+1,laser[2], sep = ";")]] <-
      ifelse(is.null(next_laser[[paste(laser[1]+1,laser[2], sep = ";")]]),0,next_laser[[paste(laser[1]+1,laser[2], sep = ";")]]) + lasers[[i]]
    } else {
      next_laser[[paste(laser[1]+1,laser[2]-1, sep = ";")]] <-
        ifelse(is.null(next_laser[[paste(laser[1]+1,laser[2]-1, sep = ";")]]),0,next_laser[[paste(laser[1]+1,laser[2]-1, sep = ";")]]) + lasers[[i]]
      next_laser[[paste(laser[1]+1,laser[2]+1, sep = ";")]] <-
        ifelse(is.null(next_laser[[paste(laser[1]+1,laser[2]+1, sep = ";")]]),0,next_laser[[paste(laser[1]+1,laser[2]+1, sep = ";")]]) + lasers[[i]]
    }
  }
  lasers <- next_laser
  res <- max(res, sum(unlist(lasers)))
}


solution2 <-
  res
