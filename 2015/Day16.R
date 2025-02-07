# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

infos <-
  c(3,7,2,3,0,0,5,3,2,1) |> 
  (\(.){names(.) <- c("children","cats","samoyeds","pomeranians","akitas","vizslas","goldfish",
                      "trees","cars","perfumes");.})()

solution1 <-
  lapply(input, function(.x){
    
    tante = gsub("Sue \\d+: ","",.x)
    valeurs <-
      regmatches(tante,
                gregexpr("\\d+",tante))[[1]] |> 
      as.numeric() |> 
      as.list()
    valeurs <-
      regmatches(tante,
                gregexpr("[a-z]+",tante))[[1]] |> 
      (\(.){names(valeurs) <- .;valeurs})()
    valeurs

    sapply(names(valeurs), function(.y){
      valeurs[[.y]] == infos[[.y]]
    }) |> 
      all()

  }) |> 
  unlist() |> 
  which()


# Partie 2 ----

solution2 <-
  lapply(input, function(.x){
    
    tante = gsub("Sue \\d+: ","",.x)
    valeurs <-
      regmatches(tante,
                gregexpr("\\d+",tante))[[1]] |> 
      as.numeric() |> 
      as.list()
    valeurs <-
      regmatches(tante,
                gregexpr("[a-z]+",tante))[[1]] |> 
      (\(.){names(valeurs) <- .;valeurs})()
    valeurs

    sapply(names(valeurs), function(.y){
      if(.y %in% c("cats","trees")){
        valeurs[[.y]] > infos[[.y]]
      } else if(.y %in% c("pomeranians","goldfish")){
        valeurs[[.y]] < infos[[.y]]
      } else {
        valeurs[[.y]] == infos[[.y]]
      }
      
    }) |> 
      all()

  }) |> 
  unlist() |> 
  which()
