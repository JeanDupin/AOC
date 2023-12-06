# Input ----

input <-
  readLines("2023/Inputs/Day06.txt")


# Partie 1 ----

times <-
  as.numeric(regmatches(input,gregexpr("[0-9]+",input))[[1]])
distance <-
  as.numeric(regmatches(input,gregexpr("[0-9]+",input))[[2]])


solution1 <- 1
for(i in seq_along(times)){
  solution1 <-
    prod(solution1,
         sum(times[i]*c(0:times[i])-c(0:times[i])^2 > distance[i]))
}; rm(i)

solution1

# Partie 2 ----

times <-
  as.numeric(paste(regmatches(input,gregexpr("[0-9]+",input))[[1]],collapse = ""))
distance <-
  as.numeric(paste(regmatches(input,gregexpr("[0-9]+",input))[[2]],collapse = ""))


solution2 <- 1
for(i in seq_along(times)){
  solution2 <-
    prod(solution2,
         sum(times[i]*c(0:times[i])-c(0:times[i])^2 > distance[i]))
}; rm(i)

solution2
