# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/13/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

wall <- function(x,y){
  intToBits(x*x + 3*x + 2*x*y + y + y*y + input) |> 
    (\(.){strsplit(as.character(.),"")})() |> 
    (\(.){sum(unlist(.) == "1")})() |> 
    (\(.){ifelse(. %% 2 == 0,T,F)})()
}

matrice <-
  matrix(NA,200,200)

for(i in seq_len(ncol(matrice))){
  for(j in seq_len(nrow(matrice))){
    matrice[j,i] = ifelse(wall(i-1,j-1),".","#")
  }
}

matrice[2,2] <- "S"
matrice[40,32] <- "E"

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
cout2 = 1

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

solution1 <-
  dijkstra(start, end,"cost")

# Partie 2 ----
  
ma_matrice <-
  matrix(NA,200,200)

for(i in seq_len(ncol(ma_matrice))){
  for(j in seq_len(nrow(ma_matrice))){
    ma_matrice[j,i] = ifelse(wall(i-1,j-1),".","#")
  }
}

total = 1
for(i in seq_len(50)){
  print(i)
  for(j in seq_len(50)){
    if(i == 2 & j == 2){next}
    if(ma_matrice[j,i] == "#"){next}
    
    matrice = ma_matrice
    matrice[2,2] <- "S"
    matrice[j,i] <- "E"
    start = which(matrice == "S", arr.ind = T)
    end = which(matrice == "E", arr.ind = T)
    cout = dijkstra(start, end,"cost")
    cout = ifelse(is.null(cout),99,cout)
    if(cout <= 50){
      total = total + 1
    }
  }
}


solution2 <-
  total
