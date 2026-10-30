# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/22/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

nodes <-
  input[grepl("node",input)] |> 
  (\(.){
    regmatches(.,
               gregexpr("\\d+T",.))
  })() |> 
  lapply(function(.x){as.numeric(gsub("T","",.x))})

total = 0
for(i in seq_along(nodes)){
  for(j in seq_along(nodes)){
    if(nodes[[i]][2] == 0){
      next
    } else if(i == j){
      next
    } else if(nodes[[i]][2] <= nodes[[j]][3]){
      total = total + 1
    }
  }
}

solution1 <-
  total

# Partie 2 ----

nodes <-
  lapply(input[3:length(input)], function(.x){
    
    x <-
      regmatches(.x,
                 gregexpr("x\\d+",.x))[[1]] |> 
      (\(.){as.numeric(gsub("x","",.)) + 1})()
    y <-
      regmatches(.x,
                 gregexpr("y\\d+",.x))[[1]] |> 
      (\(.){as.numeric(gsub("y","",.)) + 1})()
    used <-
      regmatches(.x,
                 gregexpr("\\d+T",.x))[[1]][2] |> 
      (\(.){as.numeric(gsub("T","",.))})()
    dispo <-
      regmatches(.x,
                 gregexpr("\\d+T",.x))[[1]][3] |> 
      (\(.){as.numeric(gsub("T","",.))})()
    
    c(
      x = x,
      y = y,
      used = used,
      dispo = dispo
    )
    
  }) |> 
  (\(.){do.call(rbind,.)})() |> 
  as.data.frame()


matrice_dispo <- matrice <-
  matrix(nrow = max(nodes$y),
         ncol = max(nodes$x),
         1)

matrice[cbind(nodes$y, nodes$x)] <- nodes$used
matrice_dispo[cbind(nodes$y, nodes$x)] <- nodes$dispo
mur <- matrice > max(matrice_dispo)


matrice2 <- ifelse(mur, "#", ".")
matrice2[which(matrice == 0, arr.ind = T)] <- "S"
matrice2[1,(ncol(matrice2)-1)] <- "E"
matrice <- matrice2


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
          move_cost <- 1
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

solution2 <-
  dijkstra(start, end,"cost") + 1 + 5*(ncol(matrice) - 2)
