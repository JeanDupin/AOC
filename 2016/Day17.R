# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/17/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

is_open <- function(x){
  as.character(x) %in% c("b","c","d","e","f")
}

hash <- function(x){
  digest::digest(x, "md5", serialize = F) |> 
    (\(.){paste(strsplit(.,"")[[1]][seq_len(4)])})()
}


etats <-
  list(list(x = 1, y = 1, password = input))

while(length(etats) >= 1){
  etat = etats[[1]]
  etats[[1]] <- NULL
  
  if(etat$x == 4 & etat$y == 4){
    print(etat$password)
    break
  }
  
  moves <-
    c("U","D","L","R")[is_open(hash(etat$password))]
  
  if(etat$x == 1){moves <- moves[moves != "U"]}
  if(etat$x == 4){moves <- moves[moves != "D"]}
  if(etat$y == 1){moves <- moves[moves != "L"]}
  if(etat$y == 4){moves <- moves[moves != "R"]}
  
  if(length(moves) == 0){next}
  for(i in moves){
    etat2 <- etat
    etat2$x <-
      switch(i,
             "U" = etat2$x - 1,
             "D" = etat2$x + 1,
             "R" = etat2$x,
             "L" = etat2$x)
    etat2$y <-
      switch(i,
             "L" = etat2$y - 1,
             "R" = etat2$y + 1,
             "U" = etat2$y,
             "D" = etat2$y)
    etat2$password <- paste0(etat2$password,i)
    etats <- append(etats, list(etat2))
  }
  
  
}

solution1 <-
  gsub(input,"",etat$password)
  
send_solution(2016,17,1,solution1)

# Partie 2 ----
  
etats <-
  list(list(x = 1, y = 1, password = input))

chemin_long = 0

while(length(etats) >= 1){
  etat = etats[[1]]
  etats[[1]] <- NULL
  
  if(etat$x == 4 & etat$y == 4){
    chemin_long <-
      max(chemin_long,nchar(gsub(input,"",etat$password)))
    next
  }
  
  moves <-
    c("U","D","L","R")[is_open(hash(etat$password))]
  
  if(etat$x == 1){moves <- moves[moves != "U"]}
  if(etat$x == 4){moves <- moves[moves != "D"]}
  if(etat$y == 1){moves <- moves[moves != "L"]}
  if(etat$y == 4){moves <- moves[moves != "R"]}
  
  if(length(moves) == 0){next}
  for(i in moves){
    etat2 <- etat
    etat2$x <-
      switch(i,
             "U" = etat2$x - 1,
             "D" = etat2$x + 1,
             "R" = etat2$x,
             "L" = etat2$x)
    etat2$y <-
      switch(i,
             "L" = etat2$y - 1,
             "R" = etat2$y + 1,
             "U" = etat2$y,
             "D" = etat2$y)
    etat2$password <- paste0(etat2$password,i)
    etats <- append(etats, list(etat2))
  }
  
  
}

solution2 <-
  chemin_long