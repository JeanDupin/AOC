# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/20/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

points <-
  lapply(input,
         function(.x){
           regmatches(.x,
                      gregexpr("(-|)\\d+",.x))[[1]] |> 
             (\(.){as.list(as.numeric(.))})() |> 
             setNames(c("px","py","pz","vx","vy","vz","ax","ay","az"))
         })

solution1 <-
  sapply(points,
         function(.x){
           abs(.x$ax) + abs(.x$ay) + abs(.x$az)
         }) |> 
  (\(.){which.min(.) - 1})()

# Partie 2 ----

for(i in seq_len(1e5)){
  for(j in seq_along(points)){
    points[[j]]$px <- points[[j]]$px + points[[j]]$vx + points[[j]]$ax*i
    points[[j]]$py <- points[[j]]$py + points[[j]]$vy + points[[j]]$ay*i
    points[[j]]$pz <- points[[j]]$pz + points[[j]]$vz + points[[j]]$az*i
  }
  test <-
    sapply(points, function(.x){paste(.x$px,.x$py,.x$pz,sep =";")}) |>
    (\(.){duplicated(.) | duplicated(., fromLast = T)})() |> 
    which()
  if(length(test) > 0){
    points[test] <- NULL
  }
}
  
solution2 <-
  length(points)
