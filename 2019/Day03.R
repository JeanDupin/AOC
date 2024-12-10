# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


move <-
  function(.x,.y,z.){
    switch(.x,
           "U" = c(0,.y),
           "D" = c(0,-.y),
           "R" = c(.y,0),
           "L" = c(-.y,0))
  }


get_wire <-
  function(fil){
    position = list(c(0,0))
    for(j in seq_along(strsplit(fil,",")[[1]])){
      
      mouvement <-
        strsplit(fil,",")[[1]][j]
      
      position[[j+1]] <-
        move(gsub("[0-9]","",mouvement),as.numeric(gsub("[^0-9]","",mouvement))) + position[[j]]
      
      
    }
    
    sortie = vector("list",length(position)-1)
    for(i in seq_along(position)[-length(position)]){
      sortie[[i]] <-
        data.frame(V1 = position[[i]][1]:position[[i+1]][1],
                   V2 = position[[i]][2]:position[[i+1]][2]) |> 
        (\(.){.[-1,]})()
    }
    do.call(rbind,sortie) |> 
      apply(1,function(.x){paste(.x,collapse = ";")},simplify = T) |> 
      (\(.){names(.) <- NULL;.})()
  }

chemin1 = get_wire(input[1])
chemin2 = get_wire(input[2])

solution1 <-
  intersect(chemin1,chemin2) |> 
  lapply(
    function(.x){
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]] |> 
        as.numeric() |> 
        sum()
    }
  ) |> 
  unlist() |> 
  min(); solution1

# Partie 2 ----

solution2 <-
  intersect(chemin1,chemin2) |> 
  lapply(
    function(.x){
      which(chemin1 == .x)[1] + which(chemin2 == .x)[1]
    }
  ) |> 
  unlist() |> 
  min()
