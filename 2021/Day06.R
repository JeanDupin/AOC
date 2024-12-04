# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

fish <-
  strsplit(input,",")[[1]] |> 
  as.numeric() |> 
  table() |>
  as.list()

fish <-
  append(fish,rep(0,9-length(fish))) |> 
  (\(.){names(.) <- c(names(fish),(0:8)[!0:8 %in% names(fish)]);.})() |> 
  (\(.){.[order(names(.))]})()


for(i in 1:80){
  if(fish[["0"]] == 0){
    fish[-9] <- fish[-1]
    fish[["8"]] <- 0
  } else {
    bb = fish[[1]]
    fish[-9] <- fish[-1]
    fish["8"] = bb
    fish["6"] = fish[["6"]] + bb
  }
}

solution2 <-
  sum(unlist(fish))


# Partie 2 ----


fish <-
  strsplit(input,",")[[1]] |> 
  as.numeric() |> 
  table() |>
  as.list()

fish <-
  append(fish,rep(0,9-length(fish))) |> 
  (\(.){names(.) <- c(names(fish),(0:8)[!0:8 %in% names(fish)]);.})() |> 
  (\(.){.[order(names(.))]})()


for(i in 1:256){
  if(fish[["0"]] == 0){
    fish[-9] <- fish[-1]
    fish[["8"]] <- 0
  } else {
    bb = fish[[1]]
    fish[-9] <- fish[-1]
    fish["8"] = bb
    fish["6"] = fish[["6"]] + bb
  }
}

solution2 <-
  sum(unlist(fish))
