# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input), byrow = T)

start = which(matrice == "S", arr.ind = T)
end = which(matrice == "E", arr.ind = T)

directions <-
  matrix(
    c(-1, 0,
      1, 0,
      0, -1,
      0, 1),
    ncol = 2, byrow = TRUE)

cout1 = 1
cout2 = 1001

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
          move_cost <- if(is.null(prev_dir) || prev_dir == dir){cout1}else{cout2}
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

cout <- dijkstra(start, end,"cost")
chemin <- dijkstra(start, end,"path")

if(chemin[[1]][2] != chemin[[2]][2]-1){
  cout = cout + 1000
}

solution1 <-
  cout

# Partie 2 ----

dijkstra_all_paths <-
  function(start, end) {
    n = nrow(matrice)
    m = ncol(matrice)
    inf = 1000000000
    
    costs <-
      array(inf, dim = c(n, m, nrow(directions)))
    for(i in 1:nrow(directions)){
      costs[start[1], start[2], i] = 0
    }
    
    queue <-
      list(list(pos = start, cost = 0, direction = NULL, path = list(start)))
    min_cost = inf  
    all_min_paths <-
      vector("list")
    
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
        if(cost < min_cost){
          min_cost = cost
          all_min_paths <-
            list(path)
        } else if(cost == min_cost){
          
          all_min_paths <-
            append(all_min_paths,
                   list(path))
        }
        next
      }
      
      for(dir in 1:nrow(directions)){
        new_pos <-
          pos + directions[dir, ]
        
       
        if(new_pos[1] > 0 && new_pos[1] <= n && new_pos[2] > 0 && new_pos[2] <= m &&
            matrice[new_pos[1], new_pos[2]] != "#"){

          move_cost <- if(is.null(prev_dir) || prev_dir == dir){cout1}else{cout2}
          new_cost = cost + move_cost
          
          if(new_cost <= costs[new_pos[1], new_pos[2], dir]){
            costs[new_pos[1], new_pos[2], dir] = new_cost
            
            queue <- append(queue, list(list(
              pos = new_pos, 
              cost = new_cost, 
              direction = dir, 
              path = append(path, list(new_pos))
            )))
          }
        }
      }
    }
    return(all_min_paths)
}

result <-
  dijkstra_all_paths(start, end)

solution2 <-
  lapply(result, function(.x){
  .x |> 
    (\(.){do.call(rbind,.)})() |> 
    apply(1, function(.y){paste(.y,collapse = ";")}, simplify = F) |> 
    unlist()
}) |> 
  unlist() |> 
  unique() |> 
  length()
