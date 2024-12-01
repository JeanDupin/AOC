# Input ----

input <-
  httr2::request("https://adventofcode.com/2016/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\, ")[[1]]})()


# Partie 1 ----

facing <- 1
position <- c(0,0)

for(i in seq_along(input)){
  
  if(substr(input[i],1,1) == "R"){
    facing = facing + 1
    if(facing == 5){facing = 1}
  } else {
    facing = facing - 1
    if(facing == 0){facing = 4}
  }
  
  if(facing == 1){
    position = c(position[1],
                 position[2] + as.numeric(substring(input[i],2)))
  } else if(facing == 2){
    position = c(position[1] + as.numeric(substring(input[i],2)),
                 position[2])
  } else if(facing == 3){
    position = c(position[1],
                 position[2] - as.numeric(substring(input[i],2)))
  } else {
    position = c(position[1] - as.numeric(substring(input[i],2)),
                 position[2])
  }
  
  
}; rm(facing, i)

solution1 <-
  sum(abs(position))



# Partie 2 ----


facing <- 1
position <- c(0,0)
positions.visited <- c("0;0")

for(i in seq_along(input)){
  
  if(substr(input[i],1,1) == "R"){
    facing = facing + 1
    if(facing == 5){facing = 1}
  } else {
    facing = facing - 1
    if(facing == 0){facing = 4}
  }
  
  if(facing == 1){
    positions.walk <-
      paste(position[1],
            c((position[2]+1):(position[2] + as.numeric(substring(input[i],2)))),
            sep = ";")
    position = c(position[1],
                 position[2] + as.numeric(substring(input[i],2)))
  } else if(facing == 2){
    positions.walk <-
      paste(c((position[1]+1):(position[1] + as.numeric(substring(input[i],2)))),
            position[2],
            sep = ";")
    position = c(position[1] + as.numeric(substring(input[i],2)),
                 position[2])
  } else if(facing == 3){
    positions.walk <-
      paste(position[1],
            c((position[2]-1):(position[2] - as.numeric(substring(input[i],2)))),
            sep = ";")
    position = c(position[1],
                 position[2] - as.numeric(substring(input[i],2)))
  } else {
    positions.walk <-
      paste(c((position[1]-1):(position[1] - as.numeric(substring(input[i],2)))),
            position[2],
            sep = ";")
    position = c(position[1] - as.numeric(substring(input[i],2)),
                 position[2])
  }
  
  positions.visited <-
    append(positions.visited,
           positions.walk)
  
  
}; rm(facing, i,
      positions.walk, position)

solution2 <-
  positions.visited[duplicated(positions.visited)][1] |> 
  (\(.){strsplit(.,";")[[1]] |> 
      as.numeric() |> 
      abs()})() |> 
  sum()
