# Input ----

input <-
  readLines("2016/Inputs/Day02.txt")

# Partie 1 ----


code <- c()
for(i in seq_along(input)){
  if(i == 1){
    id.r = 2
    id.c = 2
  }
  
  letters.input <- strsplit(input[i],"")[[1]]
  
  for(j in seq_along(letters.input)){
    if(letters.input[j] == "U"){
      id.r = max(1,id.r-1)
    } else if(letters.input[j] == "R"){
      id.c = min(3, id.c + 1)
    } else if(letters.input[j] == "D"){
      id.r = min(3,id.r+1)
    } else {
      id.c = max(1, id.c - 1)
    }
  }
  
  code <-
    append(code,
           list(
             c(1,2,3),
             c(4,5,6),
             c(7,8,9)
           )[[id.r]][id.c])
  
}; rm(i, j,
      letters.input,
      id.r, id.c)

solution1 <-
  paste(code, collapse = "")


# Partie 2 ----

positions.imp <-
  c("1;1","1;2","1;4","1;5",
    "2;1","2;5",
    "4;1","4;5",
    "5;1","5;2","5;4","5;5")

code <- c()
for(i in seq_along(input)){
  if(i == 1){
    position = "1;3"
  }
  
  letters.input <- strsplit(input[i],"")[[1]]
  
  for(j in seq_along(letters.input)){
    if(letters.input[j] == "U"){
      position.test <-
        paste(
          substr(position,1,1),
          min(as.numeric(substr(position,3,3)) + 1,5),
          sep = ";"
        )
    } else if(letters.input[j] == "R"){
      position.test <-
        paste(
          min(as.numeric(substr(position,1,1)) + 1,5),
          substr(position,3,3),
          sep = ";"
        )
    } else if(letters.input[j] == "D"){
      position.test <-
        paste(
          substr(position,1,1),
          max(as.numeric(substr(position,3,3)) - 1,1),
          sep = ";"
        )
    } else {
      position.test <-
        paste(
          max(as.numeric(substr(position,1,1)) - 1,1),
          substr(position,3,3),
          sep = ";"
        )
    }
    
    if(position.test %in% positions.imp){
      next
    } else {
      position <-
        position.test
    }
    
  }
  code <-
    append(code,
           position)
  
}; rm(i, j,
      letters.input,
      position.test)

solution2 <-
  sapply(code, function(.x){
    list(1:9,
         LETTERS[1:4]) |> 
      unlist() |> 
      as.list() |> 
      (\(.){
        `names<-`(.,
                  c("3;5",
                    "2;4","3;4","3;4",
                    "1;3","2;3","3;3","4;3","5;3",
                    "2;2","3;2","4;2",
                    "3;1"))
      })() |> 
      (\(.){.[[.x]]})()
  },
  USE.NAMES = F) |> 
  paste(collapse = "")

