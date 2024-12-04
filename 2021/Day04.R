# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

bingo <-
  strsplit(input[1],",")[[1]]

is_winner <-
  function(matrice){
    any(
      apply(matrice,1,function(.x){paste(.x,collapse = "")}) == "XXXXX",
      apply(matrice,2,function(.x){paste(.x,collapse = "")}) == "XXXXX"
    )
  }

matrices <-
  mapply(function(x,y){
    input[x:y] |> 
      (\(.){regmatches(.,
                       gregexpr("\\d+",.))})() |> 
      unlist() |> 
      matrix(5,5,byrow = T)
  },
  seq(3,length(input),6),
  seq(7,length(input),6),
  SIMPLIFY = F)


for(i in seq_along(bingo)){
  tirage = bingo[i]
  
  matrices <-
    lapply(
      matrices,
      function(.x){
        .x[which(.x == tirage)] = "X"; .x
      }
    )
  
  if(any(unlist(lapply(matrices,is_winner)))){
    solution1 <-
      matrices[[which(unlist(lapply(matrices,is_winner)))]] |> 
      (\(.){.[. != "X"]})() |> 
      (\(.){sum(as.numeric(.))*as.numeric(tirage)})()
    break
  } else {
    next
  }
}; rm(i, tirage)


# Partie 2 ----

matrices <-
  mapply(function(x,y){
    input[x:y] |> 
      (\(.){regmatches(.,
                       gregexpr("\\d+",.))})() |> 
      unlist() |> 
      matrix(5,5,byrow = T)
  },
  seq(3,length(input),6),
  seq(7,length(input),6),
  SIMPLIFY = F)

for(i in seq_along(bingo)){
  tirage = bingo[i]
  
  matrices <-
    lapply(
      matrices,
      function(.x){
        .x[which(.x == tirage)] = "X"; .x
      }
    )
  
  if(any(unlist(lapply(matrices,is_winner))) & length(matrices) > 1){
    matrices <-
      matrices[-which(unlist(lapply(matrices,is_winner)))]
  }
  
  if(any(unlist(lapply(matrices,is_winner))) & length(matrices) == 1){
    solution2 <-
      matrices[[which(unlist(lapply(matrices,is_winner)))]] |> 
      (\(.){.[. != "X"]})() |> 
      (\(.){sum(as.numeric(.))*as.numeric(tirage)})()
    break
  }
  
  
}