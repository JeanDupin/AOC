# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/15/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

y = 2000000
beacons <-
  regmatches(input,
    gregexpr("(-|)\\d+",input)) |> 
  lapply(function(.x){paste(as.numeric(.x[c(3,4)]), collapse = ";")}) |> 
  unlist()


res <- vector("list", length(input))
for(i in seq_along(input)){
  distances <-
    regmatches(input[i],
               gregexpr("(-|)\\d+",input[i]))[[1]] |> 
    as.numeric() |> 
    (\(.){
      if(.[2] >= y){
        c(.[2] - (abs(.[1] - .[3]) + abs(.[2] - .[4])),.[1],-1)
      } else {
        c(.[2] + (abs(.[1] - .[3]) + abs(.[2] - .[4])),.[1],1)
      }
    })()
  
  if(distances[1] < y & distances[3] == 1){
    res[[i]] = NA
  } else if(distances[1] > y & distances[3] == -1){
    res[[i]] = NA
  } else if(distances[1] < y){
    res[[i]] <-
      (distances[2] - (y - distances[1])):(distances[2] + (y - distances[1])) |> 
      paste(y, sep = ";")
  } else if(distances[1] > y){
    res[[i]] <-
      (distances[2] - (distances[1]-y)):(distances[2] + (distances[1]-y)) |> 
      paste(y, sep = ";")
  }

}

solution1 <-
  unique(unlist(res)) |> 
  (\(.){.[!is.na(.) & !. %in% beacons]})() |> 
  length()

# Partie 2 ----

manhattan <- function(X, Y, D){
  coordinates <- list()

  for (dx in -D:D) {
    for (dy in -D:D) {
      if (abs(dx) + abs(dy) <= D) {
        coordinates <- append(coordinates, list(c(X + dx, Y + dy)))
      }
    }
  }

  do.call(rbind, coordinates) |> 
    (\(.){.[.[,1] >= 0 & .[,2] >= 0 & .[,1] <= 4000000 & .[,2] <= 4000000,]})()
}

res <- vector("list", length(input))
for(i in seq_along(input)[1]){
  res[[i]] <-
    regmatches(input[i],
                gregexpr("(-|)\\d+",input[i]))[[1]] |> 
    as.numeric() |> 
    (\(.){
      manhattan(.[1],.[2],(abs(.[1] - .[3]) + abs(.[2] - .[4])))
    })()
}

do.call(rbind,res) |> 
  apply(1,function(.x){paste(.x, collapse = ";")}) |> 
  (\(.){
    apply(expand.grid(0:4000000,0:4000000),1,function(.x){paste(.x, collapse = ";")}) |> 
      (\(.z){
        .z[!.z %in% .]
      })()
  })() |> 
  (\(.){
    as.numeric(strsplit(.,";")[[1]])
  })() |> 
  (\(.){.[1] * 4000000 + .[2]})()


solution2 <-
  NA