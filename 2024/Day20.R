# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/20/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow =length(input), byrow = T)

start = which(matrice == "S", arr.ind = T)
end = which(matrice == "E", arr.ind = T)

directions <-
  matrix(
    c(-1, 0,
      1, 0,
      0, -1,
      0, 1),
    ncol = 2, byrow = TRUE)

dijkstra <-
  function(start, end, return_type = "cost"){
    n = nrow(matrice)
    m = ncol(matrice)
    inf = 1000000000
    
    costs <-
      array(inf, dim = c(n, m, nrow(directions)))
    for(i in 1:nrow(directions)){
      costs[start[1], start[2], i] = 0
    }
    
    paths <-
      vector("list")
    for(i in 1:nrow(directions)){
      paths[[i]] <-
        vector("list")
      paths[[i]][[paste(start[1], start[2], sep = ",")]] <-
        list(start)
    }
    
    queue <- list(list(pos = start, cost = 0, direction = NULL, path = list(start)))
    
    while(length(queue) > 0){
      queue <-
        queue[order(unlist(lapply(queue, function(.x){.x$cost})))]
      current = queue[[1]]
      queue = queue[-1]
      
      pos = current$pos
      cost = current$cost
      prev_dir = current$direction
      path = current$path

      
      if(all(pos == end)){
        if(return_type == "cost"){
          return(cost)
        } else if(return_type == "path"){
          return(path)
        }
      }
      
      for(dir in 1:nrow(directions)){
        new_pos <-
          pos + directions[dir, ]
        
        if(new_pos[1] > 0 && new_pos[1] <= n && new_pos[2] > 0 && new_pos[2] <= m &&
            matrice[new_pos[1], new_pos[2]] != "#"){
          move_cost <- if(is.null(prev_dir) || prev_dir == dir){1}else{1}
          new_cost <- cost + move_cost
          
          if(new_cost < costs[new_pos[1], new_pos[2], dir]){
            costs[new_pos[1], new_pos[2], dir] = new_cost
            
            new_path <-
              append(path, list(new_pos))
            paths[[dir]][[paste(new_pos[1], new_pos[2], sep = ",")]] <-
              new_path
            
            queue <- append(queue,
                            list(list(pos = new_pos, cost = new_cost, direction = dir, path = new_path)))
          }
        }
      }
    }
}

count_neighbors <- function(x, y){
  directions <- matrix(c(-1,  0,  
                          1,  0,  
                          0, -1,  
                          0,  1), 
                       ncol = 2, byrow = TRUE)
  
  count <- 0
  for (dir in 1:nrow(directions)) {
    nx <- x + directions[dir, 1]
    ny <- y + directions[dir, 2]
    if (nx >= 1 && nx <= nrow(matrice) && ny >= 1 && ny <= ncol(matrice)) {
      if (matrice[nx, ny] != "#") {
        count <- count + 1
      }
    }
  }
  count
}



reference <-
  dijkstra(start,end,"path") |> 
  lapply(function(.x){paste(.x,collapse=";")}) |> 
  unlist()

triches <-
  which(matrice == "#",arr.ind = T) |> 
  (\(.){.[apply(., 1, function(.x){count_neighbors(.x[1], .x[2]) >= 2}), ]})()
scores = vector("numeric",nrow(triches))

for(i in seq_len(nrow(triches))){
  scores[i] <-
    triches[i,] |> 
    (\(.){
      c(
        paste(.[1]-1,.[2],sep = ";"),
        paste(.[1]+1,.[2],sep = ";"),
        paste(.[1],.[2]-1,sep = ";"),
        paste(.[1],.[2]+1,sep = ";")
      )
    })() |> 
    (\(.){reference %in% .})() |> 
    (\(.){which(.)[c(1,length(which(.)))]})() |> 
    (\(.){.[2]-.[1]-2})()
}

solution1 <-
  sum(scores >= 100)

# Partie 2 ----

triches <-
  lapply(seq_along(reference), function(.x){
    data.table::CJ(reference[.x],reference[.x:length(reference)],sorted = F) |> 
      (\(.){.$"path" = (0:(nrow(.)-1)); .})()
  }) |> 
  (\(.){data.table::rbindlist(.)[V1 != V2]})()


triches[, c("A1","A2") := data.table::tstrsplit(V1,";")]
triches[, c("A3","A4") := data.table::tstrsplit(V2,";")]
triches[,"A1" := as.integer(A1)]
triches[,"A2" := as.integer(A2)]
triches[,"A3" := as.integer(A3)]
triches[,"A4" := as.integer(A4)]
triches[,shortcut := {abs(A1-A3) + abs(A2-A4)}]

triches <-
  triches[shortcut <= 20][,gain := path - shortcut]

solution2 <-
  triches[gain >= 100] |> 
  (\(.){table(.$gain)})() |> 
  sum()
