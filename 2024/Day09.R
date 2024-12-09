# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/9/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

concat <-
  function(.x){
    paste(.x,collapse = "")
  }

sequence <-
  lapply(
    seq_len(nchar(input)),
    function(.x){
      if(.x %%2 == 1){
        # rep(.x %/% 2,
        #     as.numeric(strsplit(input,"")[[1]][.x]))
        rep("1",
            as.numeric(strsplit(input,"")[[1]][.x]))
        
      } else {
        rep(".",
            as.numeric(strsplit(input,"")[[1]][.x]))
      }
    }
  ) |> 
  unlist()


for(i in seq_along(grep("\\.",sequence))[16560:44905]){
  id = grep("\\.",sequence)[seq_len(i)]
  nombre = rev(grep("[0-9]",sequence))[seq_len(i)]
  
  if(grepl("\\d\\.+\\d",concat(sequence[-c(id,nombre)]))){
    next
  } else {
    # sequence[id] = sequence[nombre]
    # sequence[nombre] = "."
    break
  }
}



sequence2 <-
  lapply(
    seq_len(nchar(input)),
    function(.x){
      if(.x %%2 == 1){
        rep(.x %/% 2,
            as.numeric(strsplit(input,"")[[1]][.x]))
        # rep("1",
        #     as.numeric(strsplit(input,"")[[1]][.x]))
        
      } else {
        rep(".",
            as.numeric(strsplit(input,"")[[1]][.x]))
      }
    }
  ) |> 
  unlist() |> 
  as.list()

id <-
  lapply(
    sequence2,
    function(.x){
      .x == "."
    }
  ) |> 
  unlist() |> 
  (\(.){which(.)[seq_len(i)]})()

nombres <-
  lapply(
    sequence2,
    function(.x){
      .x != "."
    }
  ) |> 
  unlist() |> 
  (\(.){rev(which(.))[seq_len(i)]})()



sequence3 <- sequence2

sequence3[id] = sequence3[nombres]
sequence3[nombres] = "."

solution1 <-
  lapply(sequence3,
         function(.x){.x == "."}) |> 
  unlist() |> 
  (\(.){which(.)[1]-1})() |> 
  (\(.){sequence3[seq_len(.)]})() |> 
  unlist() |> 
  as.numeric() |> 
  (\(.){. * (seq_along(.)-1)})() |> 
  sum() |> 
  as.character()


# Partie 2 ----

sequence4 = vector("list",nchar(input))

lapply(
  seq_len(nchar(input)),
  function(.x){
    if(.x %%2 == 1){
      sequence4[[.x]]<<-
        rep(.x %/% 2,
            as.numeric(strsplit(input,"")[[1]][.x]))
      # rep("1",
      #     as.numeric(strsplit(input,"")[[1]][.x]))
      
    } else {
      sequence4[[.x]]<<-
        rep(".",
            as.numeric(strsplit(input,"")[[1]][.x]))
    }
  }
)

sequence4 <-
  lapply(
    sequence4,
    length
  ) |> 
  unlist() |> 
  (\(.){which(. == 0)})() |> 
  (\(.){sequence4[-.]})()




sequence5 <- unlist(sequence4)




find_first_n_consecutive <-
  function(vec, char, n) {
  matches <- vec == char
  run_lengths <- rle(matches)
  index <- which(run_lengths$values & run_lengths$lengths >= n)[1]
  
  if (!is.na(index)) {
    
    start_index <- sum(run_lengths$lengths[1:(index - 1)]) + 1
    return(start_index)
  } else {
    return(NA)  # Si aucune répétition trouvée
  }
}

find_first_n_consecutive(sequence5,".",6)

for(i in 9999:1){
  
  taille = sum(sequence5 == i)
  ids = find_first_n_consecutive(sequence5,".",taille)
  ids2 = which(sequence5 == i)
  if(is.na(ids)){next}
  if(ids2[1] < ids){next}
  
  sequence5[ids:(ids+taille-1)] = as.character(i)
  
}

solution2 <-
  sequence2 |> 
  (\(.){ifelse(. == ".",0,.)})() |> 
  as.numeric()|> 
  (\(.){. * (seq_along(.)-1)})() |> 
  sum() |> 
  as.character()
