# Input ----

input <-
  readLines("2017/Inputs/Day07.txt")

# Partie 1 ----

mots.l <-
  input[grepl("->",input)] |> 
  sapply(function(.x){
    regmatches(.x,
               gregexpr("^[a-z]*",.x))
  }, USE.NAMES = F) |> 
  unlist()


mots.d <-
  input[grepl("->",input)] |> 
    sapply(function(.x){
      strsplit(.x," -> ")[[1]][2] |> 
        strsplit(", ")
    }, USE.NAMES = F) |> 
  unlist()

solution1 <-
  mots.l[!(mots.l %in% mots.d)]; rm(mots.l, mots.d)


# Partie 2 ----

get_tower_height <-
  function(thislevel, height = 1){
    
    ligne <-
      input[grepl(paste0("^",thislevel),input)]
    
    termes <<- append(termes, thislevel)
    niveau <<- append(niveau, height)
    
    if(grepl(" -> ",ligne)){
     prochaines_instructions <-
       ligne |> 
       sapply(function(.x){
         strsplit(.x," -> ")[[1]][2] |> 
           strsplit(", ")
       }, USE.NAMES = F, simplify = F) |> 
       unlist()
     sapply(prochaines_instructions,
            get_tower_height, height = height + 1, USE.NAMES = F, simplify = F)
    }
    
  }

get_poids <-
  function(.x){
    input[grepl(paste0("^",.x),input)] |> 
      (\(.){regmatches(.,gregexpr("[0-9]+",.))[[1]]})() |> 
      as.numeric()
  }

termes <- list()
niveau <- list()
invisible(get_tower_height(solution1))

groupes <- numeric(length(unlist(niveau)))
for (i in 2:length(unlist(niveau))) {
  if (unlist(niveau)[i] < unlist(niveau)[i-1]) {
    groupes[i] <- groupes[i-1] + 1
  } else {
    groupes[i] <- groupes[i-1]
  }
}; rm(i)


df2 <-
  data.frame(
    termes = unlist(termes),
    etage = unlist(niveau),
    poids = sapply(termes, get_poids),
    groupes = groupes
  )

rm(niveau, termes, groupes)


pb = F
detect_problem_and_solve <-
  function(moninput){
    df <- moninput
    id.etage = 2
    while(!pb){
      df$E <- cumsum(df$etage == id.etage)
      
      test <-
        df[df$etage > (id.etage-1),] |> 
        (\(.){split(.,.$E)})() |> 
        sapply(function(.x){sum(.x$poids)}) |>
        unique() |>
        length()
      
      if(test > 1){
        
        valeur_a_retirer <<- 
          df[df$etage > (id.etage-1),] |> 
          (\(.){split(.,.$E)})() |> 
          sapply(function(.x){sum(.x$poids)}) |>
          unique() |> 
          diff()
        
        valeur.pb <-
          df[df$etage > (id.etage-1),] |> 
          (\(.){split(.,.$E)})() |> 
          sapply(function(.x){sum(.x$poids)}) |> 
          table() |> 
          sort() |> 
          (\(.){names(.[1])})()
        
        groupe.pb <-
          df[df$etage > (id.etage-1),] |> 
          (\(.){split(.,.$E)})() |> 
          sapply(function(.x){sum(.x$poids)}) |> 
          (\(.){names(which(. == valeur.pb))})()
        
        df <-
          df[df$E == groupe.pb,]
        id.etage = id.etage + 1
      } else {
        pb = T
      }
    }
    
    return(df)
  }


solution2 <-
  detect_problem_and_solve(df2)[1,"poids"] - valeur_a_retirer
