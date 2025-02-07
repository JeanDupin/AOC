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

solution2 <-
  NA