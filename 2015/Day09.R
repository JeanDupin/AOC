# Input ----

input <-
  readLines("2015/Inputs/Day09.txt")


# Partie 1 ----

towns <-
  strsplit(gsub(" = [0-9]+$","",input)," to ") |> 
  unlist() |> 
  unique()

df <-
  rep(list(towns),
      length(towns)) |> 
    expand.grid() |> 
  (\(.){.[!apply(., 1, anyDuplicated), ]})() |> 
  apply(1, paste, collapse = ";")

towns <-
  strsplit(gsub(" = [0-9]+$","",input)," to ") |> 
  sapply(function(.x){paste(.x,collapse = ";")}) |> 
  (\(.){
    c(.,
      paste(
        gsub(";","",regmatches(., gregexpr(";[A-Za-z]+",.))),
        gsub(";","",regmatches(., gregexpr("[A-Za-z]+;",.))),
        sep = ";"))
  })()


dist <-
  as.numeric(rep(gsub("[^0-9]*","",input),2))


all_dist <- c()
for(i in seq_along(df)){
  all_dist <- append(
    all_dist,
    sapply(towns, function(.x){
      grepl(.x,df[i])
    },USE.NAMES = F) |> 
      (\(.){sum(dist[.])})())
}; rm(i)

solution1 <-
  min(all_dist)

# Partie 2 ----

solution2 <-
  max(all_dist)
