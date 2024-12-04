# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  strsplit("\t") |> 
  unlist() |> 
  as.numeric()

# Partie 1 ----


init <-
  input
memoire <- 
  list()
while(!(paste(init,
              collapse = ";") %in% memoire)){
  
  memoire <-
    append(memoire,
           paste(init,collapse = ";"))
  
  if(which(init == max(init))[1] == length(init)){
      ordre.distribution <-
        c(1:length(init))
    } else if (which(init == max(init))[1] == 1){
      ordre.distribution <-
        c(length(init),
          1:(length(init)-1))
    } else {
      ordre.distribution <-
        c(1:length(init))[order(c((which(init == max(init))[1]+1):length(init),
                                  1:which(init == max(init))[1]))]
    }
  
  
  
  to_add <-
    rep(max(init) %/% length(init),length(init)) +
      c(rep(1,max(init) %% length(init)),
        rep(0,length(init) - max(init) %% length(init))) |> 
    (\(.){.[ordre.distribution]})()
  
  init[which(init == max(init))[1]] <- 0
  init = init + 
    to_add
  
}; rm(to_add, ordre.distribution)

solution1 <-
  length(memoire)


# Partie 2 ----

solution2 <-
  length(memoire) + 1 - which(memoire == paste(init, collapse = ";"))




