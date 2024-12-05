# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/5/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

consignes <-
  input[grepl("\\|",input)]

sequences <-
  input[grepl(",",input)] |> 
  strsplit(",")

solution1 <-
  lapply(
    sequences,
    function(.x){
      apply(combn(.x,2), 2, function(pair){paste(pair[2], pair[1], sep = "|")}) |> 
        (\(.){!any(. %in% consignes)})()
    }
  ) |>
  unlist() |> 
  (\(.){sequences[.]})() |> 
  lapply(
    function(.x){
      as.numeric(.x[ceiling(length(.x)/2)])
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(
    sequences,
    function(.x){
      
      I=F
      while(apply(combn(.x,2), 2, function(pair){paste(pair[2], pair[1], sep = "|")}) |>
            (\(.){any(. %in% consignes)})()){
        
        I = T
        
        remplacements <-
          apply(combn(.x,2), 2, function(pair){paste(pair[2], pair[1], sep = "|")}) |>
          (\(.){.[. %in% consignes]})() |> 
          (\(.){strsplit(.,"\\|")[[1]]})()
        
        .x <-
          gsub(remplacements[1],"A",.x) |> 
          (\(.){gsub(remplacements[2],remplacements[1],.)})() |> 
          (\(.){gsub("A",remplacements[2],.)})()
      }
      
      if(I){.x}else{""}
      
    }) |> 
  (\(.){
    .[unlist(lapply(.,function(.x){all(.x != "")}))]
  })() |> 
  lapply(
    function(.x){
      as.numeric(.x[ceiling(length(.x)/2)])
    }
  ) |> 
  unlist() |> 
  sum()
