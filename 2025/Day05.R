# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/5/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

items <-
  input[(which(input == "")+1):length(input)] |> 
  as.numeric()
bornes1 <-
  as.numeric(gsub("-.*","",input[seq_len(which(input == "")-1)]))
bornes2 <-
  as.numeric(gsub(".*-","",input[seq_len(which(input == "")-1)]))

solution1 <-
  sapply(
    items,
    function(.x){
      any(.x >= bornes1 & .x <= bornes2)
    }
  ) |> 
  sum()

# Partie 2 ----

intervalles <-
  data.frame(
    debut = bornes1,
    fin = bornes2
  ) |> 
  (\(.){.[order(.$debut),]})()


intervalle <- intervalles[1,]
res <- 0

for(i in seq_len(nrow(intervalles))[-1]){
  if(intervalles[i,"debut"] <= intervalle[["fin"]]){
    intervalle[["fin"]] <- max(intervalles[i,"fin"], intervalle[["fin"]])
  } else {
    res <- res + (intervalle[["fin"]] - intervalle[["debut"]] + 1)
    intervalle <- intervalles[i,]
  }
  if(i == nrow(intervalles)){
    res <- res + (intervalle[["fin"]] - intervalle[["debut"]] + 1)
  }
}

solution2 <-
  res
