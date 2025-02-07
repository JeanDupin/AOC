# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,",")[[1]]})()

# Partie 1 ----

taille = 16
programs <- letters[1:taille]

dance <- function(.x,.mot){
  if(grepl("^s",.x)){
    id = as.numeric(gsub("^s","",.x))
    .mot = c(.mot[(taille-id+1):taille],.mot[seq_len(taille-id)])
  } else if(grepl("^x",.x)){
    id <-
      regmatches(.x,
                 gregexpr("\\d+",.x))[[1]] |> 
      (\(.){as.numeric(.) + 1})()
    .mot <- 
      .mot |> 
      (\(.){.[c(id[1],id[2])] <- .[c(id[2],id[1])]; .})()
  } else {
    lettres <-
      gsub("^p","",.x) |> 
      (\(.){regmatches(.,
                 gregexpr("[a-z]",.))[[1]]})()
    id = which(.mot %in% lettres)
    .mot <- 
      .mot |> 
      (\(.){.[c(id[1],id[2])] <- .[c(id[2],id[1])]; .})()
  }
  .mot
}


for(i in input){
  programs = dance(i, programs)
}

solution1 <-
  paste(programs, collapse = "")

# Partie 2 ----

programs <- letters[1:16]
positions = vector("list")

while(T){
  for(i in input){
    programs = dance(i, programs)
  }
  if(is.null(positions[[paste(programs, collapse = "")]])){
    positions[[paste(programs, collapse = "")]] = 1
  } else {
    break
  }
}

solution2 <-
  names(positions)[1000000000 %% length(positions)]