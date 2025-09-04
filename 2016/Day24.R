# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/24/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice_initiale <-
  matrix(
    unlist(strsplit(input,"")),
    nrow = length(input),
    byrow = T)

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

directions <-
  matrix(
    c(-1, 0,
      1, 0,
      0, -1,
      0, 1),
    ncol = 2, byrow = TRUE)


paires <-
  regmatches(input,
           gregexpr("\\d+",input)) |> 
  (\(.){max(as.numeric(unlist(.)))})() |> 
  (\(.){combn(0:.,2, simplify = F)})()


for(i in seq_along(paires)){
  coords <- paires[[i]]
  
  matrice <-
    matrice_initiale
  start <-
    which(matrice == coords[1], arr.ind = T)
  matrice[start[1],start[2]] <- "S"
  end <-
    which(matrice == coords[2], arr.ind = T)
  matrice[end[1],end[2]] <- "E"
  
  paires[[i]][3] = dijkstra(start,end)
}

paires <-
  sapply(paires,
         function(.x){
           c(.x[3],.x[3]) |> 
             setNames(c(paste(.x[1],.x[2],sep=";"),
                        paste(.x[2],.x[1],sep=";")))
         },
         simplify = F) |> 
  unlist()

chemins <-
  regmatches(input,
             gregexpr("\\d+",input)) |> 
  (\(.){max(as.numeric(unlist(.)))})() |> 
  (\(.){c(list(0), replicate(., seq_len(.), simplify = FALSE))})() |> 
  (\(.){do.call(expand.grid,.)})() |> 
  (\(.){
    .[apply(.,1,function(.x){length(unique(.x))}) == ncol(.),]
  })()


solution1 <-
  apply(chemins,
        1,
        function(.x){
          .x |> 
            (\(.){paste(.[-length(.)], .[-1], sep = ";")})() |> 
            (\(.){paires[names(paires) %in% .]})() |> 
            sum()
        }, simplify = T) |>
  min()

# Partie 2 ----
  
chemins <-
  cbind(chemins, 0)

solution2 <-
  apply(chemins,
        1,
        function(.x){
          .x |> 
            (\(.){paste(.[-length(.)], .[-1], sep = ";")})() |> 
            (\(.){paires[names(paires) %in% .]})() |> 
            sum()
        }, simplify = T) |>
  min()
