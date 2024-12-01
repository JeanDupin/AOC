# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/14/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

transform_to_columns <-
  function(.x){
    strsplit(.x,"") |> 
      as.data.frame() |> 
      unname() |> 
      t() |> 
      (\(.){
        apply(X = .,
              MARGIN = 2,
              FUN = function(.x){Reduce(paste0,.x)})
      })()
  }


fall.rocks <-
  function(.x,direction){
    if(direction %in% c("N","O")){
      while(grepl("\\.O",.x)){
        .x <- gsub("\\.O","O.",.x)
      }
    } else {
      while(grepl("O\\.",.x)){
        .x <- gsub("O\\.",".O",.x)
      }
    }
    
    return(.x)
  }

rotate <-
  function(schema, direction){
    if(direction %in% c("N","S")){
      sapply(transform_to_columns(schema), fall.rocks, direction = direction,
             USE.NAMES = F, simplify = T) |> 
        transform_to_columns()
    } else {
      sapply(schema, fall.rocks, direction = direction,
             USE.NAMES = F, simplify = T)
    }
  }

solution1 <-
  rotate(input,"N")  |> 
  sapply(USE.NAMES = F, simplify = T,
         function(.){
           length(which(gregexpr("O",.)[[1]] != -1))
         }) |> 
  (\(.){.*c(length(.):1)})() |> 
  sum()


# Partie 2 ----

do_a_cycle <-
  function(.x){
    rotate(.x,"N") |> 
      rotate("O") |> 
      rotate("S") |> 
      rotate("E")
  }


detect_cycle <-
  function(.x){
    memoire <- c(paste(.x, collapse = ";"))
    i = 1
    while(i < 1000000000){
      if(i == 1){
        a = do_a_cycle(.x)
      } else {
        a = do_a_cycle(a)
      }
      
      if(paste(a, collapse = ";") %in% memoire){
        break
      } else {
        memoire <-
          append(memoire,
                 paste(a, collapse = ";"))
      }
      i = i + 1
    }
    
    num.cycle = which(memoire == paste(a, collapse = ";")) - 1
    taille.cycle = i - (which(memoire == paste(a, collapse = ";")) - 1)
    nb.sauter = (1000000000-num.cycle) %/% taille.cycle
    nb.rep = 1000000000-(nb.sauter*taille.cycle + num.cycle)
    
    if(nb.rep > 0){
      for(i in 1:nb.rep){
        a = do_a_cycle(a)
      }
    }
    
    return(a)
  }

solution2 <-
  detect_cycle(input) |> 
  sapply(USE.NAMES = F, simplify = T,
         function(.){
           length(which(gregexpr("O",.)[[1]] != -1))
         }) |> 
  (\(.){.*c(length(.):1)})() |> 
  sum()

