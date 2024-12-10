# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

orbits <-
  strsplit(input,"\\)") |> 
  unlist() |> 
  (\(.){unique(.)[unique(.) != "COM"]})()


solution1 <-
  lapply(
    orbits,
    function(.x){
      chemin = 0
      while(.x != "COM"){
        chemin = chemin + 1
        .x <-
          input[grepl(paste0(")",.x,"$"),input)] |> 
          (\(.){gsub("\\).*","",.)})()
      }
      chemin
    }
  ) |> 
  unlist() |> 
  sum()


# Partie 2 ----

chemin = -2
atm = 'YOU'
visited = vector("character")
while(!"SAN" %in% atm){
  visited = append(visited,atm)
  atm <-
    lapply(atm,
           function(.x){
             c(input[grepl(paste0(")",.x,"$"),input)],
               input[grepl(paste0("^",.x,")"),input)]) |> 
               (\(.){gsub(paste0(")",.x,"$"),"",.)})() |> 
               (\(.){gsub(paste0("^",.x,")"),"",.)})()
           }) |> 
    unlist() |> 
    (\(.){.[!. %in% visited]})()
  chemin = chemin + 1 
}


solution2 <-
  chemin
-