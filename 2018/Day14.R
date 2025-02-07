# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/14/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

next_elf <- function(.x){
  sortie <-
    (as.numeric(recettes[.x]) + 1 + .x) %% length(recettes)
  ifelse(sortie == 0, length(recettes),sortie)
}

elf1 = 1
elf2 = 2

recettes <- c(3,7)

while(length(recettes) < input + 10){
  recettes <-
    (recettes[elf1] + recettes[elf2]) |> 
    (\(.){strsplit(as.character(.),"")[[1]]})() |> 
    (\(.){c(recettes,as.numeric(.))})()
  
  elf1 = next_elf(elf1)
  elf2 = next_elf(elf2)
}

solution1 <-
  paste(recettes[-seq_len(input)][seq_len(10)], collapse = "")

# Partie 2 ----

elf1 = 1
elf2 = 2

recettes <- c(3,7)
while(!grepl(input,paste(recettes, collapse = ""))){
  recettes <-
    (recettes[elf1] + recettes[elf2]) |> 
    (\(.){strsplit(as.character(.),"")[[1]]})() |> 
    (\(.){c(recettes,as.numeric(.))})()
  
  elf1 = next_elf(elf1)
  elf2 = next_elf(elf2)
}

solution2 <-
  nchar(gsub(paste0(input,".*$"),"",paste(recettes,collapse = "")))