# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/23/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

lettres = unique(unlist(strsplit(input,"-")))

find_connections <- function(.x){
  c(
    input[grepl(paste0("^",.x,"-"),input)],
    input[grepl(paste0("-",.x,"$"),input)] 
  ) |> 
    (\(.){gsub(paste0("^",.x,"-"),"",.)})() |> 
    (\(.){gsub(paste0("-",.x,"$"),"",.)})() |> 
    (\(.){expand.grid(.x,.)})()
}

find_all_connections <- function(.lettre){
  temp <-
    find_connections(.lettre) |> 
    (\(.){.[,2]})() |> 
    lapply(find_connections) |> 
    (\(.){do.call(rbind,.)})() |> 
    (\(.){
      merge(
        find_connections(.lettre),
        .,
        by.x = "Var2",
        by.y = "Var1",
        all = T
      )
    })()
  
  temp[,3] |> 
    lapply(find_connections) |> 
    (\(.){do.call(rbind,.)})() |> 
    (\(.){
      merge(temp,.,
            by.x = "Var2.y",
            by.y = "Var1",
            all = T)
    })() |> 
    (\(.){.[.$"Var2.y.y" == .lettre,]})() |> 
    unique()
  
}

solution1 <-
  lapply(lettres, find_all_connections) |> 
  (\(.){do.call(rbind,.)[,c(1:3)]})() |> 
  (\(.){
    .[apply(.,1, function(.x){any(grepl("^t",as.vector(.x)))}),]
  })() |> 
  apply(1,function(.x){paste(sort(.x),collapse = "-")}) |> 
  unique() |> 
  length()

# Partie 2 ----


find_max_group <-
  function(.lettre){
    res <-
      find_connections(.lettre)$Var2 |> 
      as.character()
    
    res2 <-
    lapply(res, function(.x){find_connections(.x)$Var2 |>
        as.character() |> 
        c(.x)})
    
    X = length(res2)
    
    unlist(res2) |> 
      c(res,.lettre) |> 
      table() |> 
      (\(.){.[. >= X]})() |> 
      names() |> 
      (\(.){
        data.frame(n = length(.),
                   res = paste(sort(.),collapse = ","))
      })()
    
  }

solution2 <-
  lapply(lettres, find_max_group) |> 
  (\(.){do.call(rbind,.)})() |> 
  (\(.){.[order(.$n,decreasing = T),][1,2]})()

