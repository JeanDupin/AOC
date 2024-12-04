# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  (sort(as.numeric(gsub("^.*   ","",input))) -
  sort(as.numeric(gsub("   .*$","",input)))) |>
  abs() |> 
  sum()

# Partie 2 ----

solution2 <-
  lapply(as.numeric(gsub("   .*$","",input)),
       function(.x){
         length(as.numeric(gsub("^.*   ","",input))[as.numeric(gsub("^.*   ","",input)) == .x]) * .x
       }) |> 
  unlist() |> 
  sum()
