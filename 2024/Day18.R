# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/18/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

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


matrice <-
  matrix(".",71,71)

for(i in input[seq_len(1024)]){
  coords = as.numeric(strsplit(i,",")[[1]])
  matrice[coords[1]+1,coords[2]+1] = "#"
}

matrice[1,1] = "S"
matrice[nrow(matrice),ncol(matrice)] = "E"

start = which(matrice == "S", arr.ind = T)
end = which(matrice == "E", arr.ind = T)


solution1 <- 
  dijkstra(start, end,"cost")

# Partie 2 ----


for(j in rev(seq_along(input))){
  if(j %% 10 == 0){print(j)}
  matrice <-
  matrix(".",71,71)

  for(i in input[seq_len(j)]){
    coords = as.numeric(strsplit(i,",")[[1]])
    matrice[coords[1]+1,coords[2]+1] = "#"
  }

  matrice[1,1] = "S"
  matrice[nrow(matrice),ncol(matrice)] = "E"

  start = which(matrice == "S", arr.ind = T)
  end = which(matrice == "E", arr.ind = T)

  res = dijkstra(start, end,"cost")
  if(!is.null(res)){
    solution2 = input[j+1]
    break
  } else {
    next
  }
}
