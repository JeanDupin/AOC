# Inputs ----

input <-
  readLines("2023/Inputs/Day02.txt")


# Partie 1 ----

get_max <- function(.x){
  as.numeric(regmatches(.x, regexpr("\\b\\d+\\.?\\d*\\b", .x))) |> 
    max()
}


red <-
  sapply(regmatches(input, gregexpr("[0-9]* red", input)),
       get_max)
green <-
  sapply(regmatches(input, gregexpr("[0-9]* green", input)),
       get_max)
blue <-
  sapply(regmatches(input, gregexpr("[0-9]* blue", input)),
       get_max)


games <- c()
for(i in seq_along(input)){
  if(green[i] > 13){
    games[i] <- FALSE
  } else if(red[i] > 12){
    games[i] <- FALSE
  } else if(blue[i] > 14){
    games[i] <- FALSE
  } else {
    games[i] <- TRUE
  }
}; rm(i)


solution1 <-
  sum(c(1:100)[games])


# Partie 2 ----

solution2 <-
  sum(red*green*blue)
