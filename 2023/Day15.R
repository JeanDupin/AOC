# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/15/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,",")[[1]]})()
  

# Partie 1 ----

hash <-
  function(caractere){
    valeur <<- 
      ((valeur + utf8ToInt(caractere))*17) %% 256
  }

valeurs <- c()
for(i in seq_along(input)){
  consignes <-
    strsplit(input[i],"")[[1]]
  valeur <- 0
  sapply(consignes,
         hash, USE.NAMES = F)
  valeurs <- append(valeurs,
                    valeur)
}; rm(i, consignes,
      valeur)

solution1 <-
  sum(valeurs); rm(valeurs)


# Partie 2 ----

boxes <-
  vector("list",256) |> 
  (\(.){`names<-`(.,c(0:255))})()


change_boite <-
  function(.x){
    label.lens <-
      gsub("[^a-z]*","",.x)
    
    for(i in 1:nchar(label.lens)){
      consignes <-
        strsplit(label.lens,"")[[1]]
      valeur <<- 0
      sapply(consignes,
             hash, USE.NAMES = F)
    }
    
    if(grepl("=",.x)){
      valeur.lens <-
        as.numeric(gsub("[^1-9]*","",.x))
      if(label.lens %in% names(boxes[[as.character(valeur)]])){
        boxes[[as.character(valeur)]][label.lens] <<- valeur.lens
      } else {
        boxes[[as.character(valeur)]] <<-
          append(boxes[[as.character(valeur)]],
                 c(valeur.lens) |> (\(.){`names<-`(.,label.lens)})())
      }
    }
    
    if(grepl("-",.x)){
      if(label.lens %in% names(boxes[[as.character(valeur)]])){
        boxes[[as.character(valeur)]] <<-
          boxes[[as.character(valeur)]][-which(names(boxes[[as.character(valeur)]]) == label.lens)]
      } else {}
    }
  }

invisible(sapply(input, change_boite)); rm(valeur)

valeurs <- c()
for(i in seq_along(boxes)){
  if(length(boxes[[i]]) == 0){
    valeurs[i] = 0
  } else {
    valeurs[i] <-
      sum(i * boxes[[i]] * c(1:length(boxes[[i]])))
  }
}; rm(i)

solution2 <-
  sum(valeurs); rm(valeurs)

