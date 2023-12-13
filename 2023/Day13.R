# Input ----

input <- 
  readLines("2023/Inputs/Day13.txt")

parties <- list()
j = 1
partie = c()
for(i in seq_along(input)){
  if(nchar(input[i]) == 0){
    parties[[j]] <- partie
    j = j + 1
    partie <- c()
  } else if(i == length(input)){
    partie <- append(partie,
                     input[i])
    parties[[j]] <- partie
    j = j + 1
    partie <- c()
  } else {
    partie <- append(partie,
                     input[i])
  }
}; rm(i,j,partie)

# Partie 1 ----

transform_to_columns <-
  function(.x){
    strsplit(.x,"") |> 
      as.data.frame() |> 
      unname() |> 
      t() |> 
      (\(.){
        apply(X = .,
              MARGIN = 2,
              FUN = function(.x){Reduce(paste0,.x)})
      })()
  }

symetrie <-
  function(.x,ligneid){
    partie_haute <-
      .x[1:ligneid]
    partie_basse <-
      .x[(ligneid+1):(ligneid+length(partie_haute))]
    
  
    partie_haute <-
      partie_haute[!is.na(rev(partie_basse))]
    partie_basse <-
      partie_basse[!is.na(partie_basse)] |> 
      rev()
    
    return(
      all(partie_haute == partie_basse)
    )
  }

id.lignes <- c()
id.colonnes <- c()
sapply(parties,simplify = F,USE.NAMES = F,
       function(.x){
         lignes <-
           sapply(1:(length(.x)-1), USE.NAMES = F, simplify = F,
                  function(.y){symetrie(.x = .x,ligneid = .y)}) |> 
           unlist()
         colonnes <-
           sapply(1:(length(transform_to_columns(.x))-1), USE.NAMES = F, simplify = F,
                  function(.y){symetrie(.x = transform_to_columns(.x),ligneid = .y)}) |> 
           unlist()
         
         if(!any(lignes) & !any(colonnes)){
           
         } else if(!any(lignes) & any(colonnes)){
           id.colonnes <<-
             append(id.colonnes,
                    which(colonnes))
         } else {
           id.lignes <<-
             append(id.lignes,
                    which(lignes))
         }
         
       }) |> 
  invisible()

solution1 <-
  sum(id.colonnes) + sum(100*id.lignes)


# Partie 2 ----

symetrie_partielle <-
  function(.x,ligneid){
    partie_haute <-
      .x[1:ligneid]
    partie_basse <-
      .x[(ligneid+1):(ligneid+length(partie_haute))]
    
    
    partie_haute <-
      partie_haute[!is.na(rev(partie_basse))]
    partie_basse <-
      partie_basse[!is.na(partie_basse)] |> 
      rev()
    
    
    if(length(unlist(strsplit(partie_basse,""))) == 
       sum(unlist(strsplit(partie_haute,"")) == unlist(strsplit(partie_basse,""))) + 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }


id.lignes <- c()
id.colonnes <- c()
sapply(parties,simplify = F,USE.NAMES = F,
       function(.x){
         lignes <-
           sapply(1:(length(.x)-1), USE.NAMES = F, simplify = F,
                  function(.y){symetrie_partielle(.x = .x,ligneid = .y)}) |> 
           unlist()
         colonnes <-
           sapply(1:(length(transform_to_columns(.x))-1), USE.NAMES = F, simplify = F,
                  function(.y){symetrie_partielle(.x = transform_to_columns(.x),ligneid = .y)}) |> 
           unlist()
         
         if(!any(lignes) & !any(colonnes)){
           
         } else if(!any(lignes) & any(colonnes)){
           id.colonnes <<-
             append(id.colonnes,
                    which(colonnes))
         } else {
           id.lignes <<-
             append(id.lignes,
                    which(lignes))
         }
         
       }) |> 
  invisible()

solution2 <-
  sum(id.colonnes) + sum(100*id.lignes)

