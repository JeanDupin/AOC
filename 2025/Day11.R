# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/11/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

destinations <-
  lapply(input,function(.x){
    strsplit(gsub("^.*:\\s","",.x),"\\s+")[[1]]
  }) |> 
  setNames(gsub(":\\s+.*$","",input))


chemin <- function(.start,.end){
  elements <- setNames(1,.start)
  
  res = 0
  while(length(elements) >= 1){
    next_nodes <- vector("list", length(elements))
    for(i in names(elements)){
      valeur <- elements[[i]]
      if(i == .end){
        res = res + valeur
        next
      }
      if(i == "out" & .end != "out"){
        next
      }
      for(j in destinations[[i]]){
        next_nodes[[j]] <-
          ifelse(is.null(next_nodes[[j]]),
                 valeur,
                 next_nodes[[j]] + valeur)
      }
    }
    elements <- unlist(next_nodes)
  }
  return(res)

}

solution1 <-
  chemin("you","out")

# Partie 2 ----
  
solution2 <-
  chemin("svr","fft") * chemin("fft","dac") * chemin("dac","out")
