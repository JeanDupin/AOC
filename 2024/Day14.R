# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/14/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

wide = 101
tall = 103

instructions = input

for(i in seq_len(100)){
  instructions <-
    lapply(
      instructions,
      function(.x){
        coords <-
          regmatches(.x,
                     gregexpr("(-|)\\d+",.x))[[1]] |> 
          as.numeric()
        
        coords
        X = (coords[1] + coords[3]) %% wide
        Y = (coords[2] + coords[4]) %% tall
        paste0("p=",X,",",Y," v=",coords[3],",",coords[4])
      }
    ) |> 
    unlist()
}


solution1 <-
  lapply(instructions,
         function(.x){
           coords <-
             regmatches(.x,
                        gregexpr("(-|)\\d+",.x))[[1]][1:2] |> 
             as.numeric()
           
           if(coords[1] < floor(wide/2) & coords[2] < floor(tall/2)){
             "HG"
           } else if(coords[1] > floor(wide/2) & coords[2] < floor(tall/2)){
             "HD"
           } else if(coords[1] < floor(wide/2) & coords[2] > floor(tall/2)){
             "BG"
           } else if(coords[1] > floor(wide/2) & coords[2] > floor(tall/2)){
             "BD"
           } else {
             "MID"
           }
           
         }) |> 
  unlist() |> 
  table() |> 
  (\(.){.[names(.) != "MID"]})() |> 
  prod()


# Partie 2 ----

instructions = input
coords <-
  regmatches(instructions,
             gregexpr("\\d+",instructions)) |> 
  lapply(function(.x){paste(.x[1],.x[2],sep = ";")}) |> 
  unlist() |> 
  unique()

i = 0
while(length(coords) != length(instructions)){
  instructions <-
    lapply(
      instructions,
      function(.x){
        coords <-
          regmatches(.x,
                     gregexpr("(-|)\\d+",.x))[[1]] |>
          as.numeric()

        coords
        X = (coords[1] + coords[3]) %% wide
        Y = (coords[2] + coords[4]) %% tall
        paste0("p=",X,",",Y," v=",coords[3],",",coords[4])
      }
    ) |>
    unlist()
  coords <-
    regmatches(instructions,
               gregexpr("\\d+",instructions)) |> 
    lapply(function(.x){paste(.x[1],.x[2],sep = ";")}) |> 
    unlist() |> 
    unique()
  
  i = i + 1
}

solution2 <-
  i

