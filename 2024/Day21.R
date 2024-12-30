# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/21/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

robot <- function(.x,.y){
  switch(paste(.x,.y,sep = ";"),
    "A;A" = "A",
    "^;A" = ">A",
    "v;A" = ">^A",
    "<;A" = ">>^A",
    ">;A" = "^A",
    "A;^" = "<A",
    "^;^" = "A",
    "v;^" = "^A",
    "<;^" = ">^A",
    ">;^" = "<^A",
    "A;v" = "v<A",
    "^;v" = "vA",
    "v;v" = "A",
    "<;v" = ">A",
    ">;v" = "<A",
    "A;<" = "v<<A",
    "^;<" = "v<A",
    "v;<" = "<A",
    "<;<" = "A",
    ">;<" = "<<A",
    "A;>" = "vA",
    "^;>" = ">vA",
    "v;>" = ">A",
    "<;>" = ">>A",
    ">;>" = "A"
  )
}

main_F <- function(.z){
  paste0("A",.z) |> 
    (\(.){strsplit(.,"")[[1]]})() |> 
    (\(.){
      mapply(
        function(.x,.y){robot(.x,.y)},
        .[1:(length(.)-1)],
        .[2:(length(.))],
        USE.NAMES = F,
        SIMPLIFY = T
      ) |> 
        paste(collapse = "")
    })()
}

get_moves <- function(.x){
  main_F(.x) |> 
    main_F()
}

grille <- matrix(c("7", "8", "9",
                   "4", "5", "6",
                   "1", "2", "3",
                   NA,  "0", "A"), nrow = 4, byrow = TRUE)
coords <- function(val) {
  which(grille == val, arr.ind = TRUE)[1, ]
}


mouvements <- function(depart, arrivee) {
  pos_depart <- coords(depart)
  pos_arrivee <- coords(arrivee)
  dx <- pos_arrivee[2] - pos_depart[2]
  dy <- pos_arrivee[1] - pos_depart[1]

  if(dx<= 0){
    res <- c(
      rep(ifelse(dy > 0, "v", "^"), abs(dy)),
      rep(ifelse(dx > 0, ">", "<"), abs(dx))
    )
  } else {
    res <- c(
      rep(ifelse(dx > 0, ">", "<"), abs(dx)),
      rep(ifelse(dy > 0, "v", "^"), abs(dy))
    )
  }
  

  paste(res, collapse = "") |> 
    paste0("A")
}


permute <- function(x) {
  if (length(x) == 1) {
    return(list(x))
  }
  permutations <- list()
  for (i in seq_along(x)) {
    remaining <- x[-i]
    sub_permutations <- permute(remaining)
    for (sub in sub_permutations) {
      permutations <- append(permutations, list(paste(c(x[i], sub),collapse = "")))
    }
  }
  return(permutations)
}

combinaisons <- list()
for(i in c("A",0:9)){
  print(i)
  for(j in c("A",0:9)){
    print(j)
    sequences <-
      mouvements(i,j) |> 
      (\(.){strsplit(.,"")[[1]][seq_len(nchar(.)-1)]})() |> 
      (\(.){unique(unlist(permute(.)))})() |> 
      paste0("A") 
    if(i == "A" & j %in% c("1","4","7")){
      sequences <- sequences[!grepl("^<<",sequences)]
    }
    if(i == "7" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^vvv",sequences)]
    }
    if(i == "4" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^vv",sequences)]
    }
    if(i == "1" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^v",sequences)]
    }
    combinaisons[[i]][[j]] <-
      sequences |> 
      lapply(function(.x){nchar(get_moves(.x))}) |> 
      (\(.){sequences[which.min(unlist(.))]})() |> 
      get_moves()
  }
}


solution1 <-
  lapply(
    input,
    function(.x){
      A = c("A",strsplit(.x,"")[[1]])
      res = vector("character")
      for(i in seq_along(A)[-length(A)]){
        res <-
          combinaisons[[A[i]]][[A[i+1]]] |> 
          (\(.){paste0(res,.)})()
      }
      nchar(res) * as.numeric(gsub("[A-Z]","",.x))
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----


combinaisons <- list()
for(i in c("A",0:9)){
  print(i)
  for(j in c("A",0:9)){
    print(j)
    sequences <-
      mouvements(i,j) |> 
      (\(.){strsplit(.,"")[[1]][seq_len(nchar(.)-1)]})() |> 
      (\(.){unique(unlist(permute(.)))})() |> 
      paste0("A") 
    if(i == "A" & j %in% c("1","4","7")){
      sequences <- sequences[!grepl("^<<",sequences)]
    }
    if(i == "7" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^vvv",sequences)]
    }
    if(i == "4" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^vv",sequences)]
    }
    if(i == "1" & j %in% c("0","A")){
      sequences <- sequences[!grepl("^v",sequences)]
    }
    combinaisons[[i]][[j]] <-
      sequences |> 
      lapply(function(.x){nchar(get_moves(.x))}) |> 
      (\(.){sequences[which.min(unlist(.))]})()
  }
}


test <- function(.x,.y){
  main_F(.x) |> 
    (\(.){
      regmatches(.,
                 gregexpr(".*?A",.))[[1]]
    })() |> 
    table() |> 
    (\(.){. * .y})()
}

input = c("029A","980A","179A","456A","379A","003A")

get_all_moves <- function(.x,.y){
  sequences <-
    mouvements(.x,.y) |> 
    (\(.){strsplit(.,"")[[1]][seq_len(nchar(.)-1)]})() |> 
    (\(.){unique(unlist(permute(.)))})() |> 
    paste0("A") 
  
  
  if(.x == "A" & .y %in% c("1","4","7")){
    sequences <- sequences[!grepl("^<<",sequences)]
  }
  if(.x == "7" & .y %in% c("0","A")){
    sequences <- sequences[!grepl("^vvv",sequences)]
  }
  if(.x == "4" & .y %in% c("0","A")){
    sequences <- sequences[!grepl("^vv",sequences)]
  }
  if(.x == "1" & .y %in% c("0","A")){
    sequences <- sequences[!grepl("^v",sequences)]
  }
  sequences
}

solution2 <-
lapply(
  input[1],
  function(.ordre){
    A = c("A",strsplit(.ordre,"")[[1]])
    sortie = vector("numeric")
    for(b in seq_along(A)[-1]){
      chemins = vector("numeric")
      for(r in get_all_moves(A[(b-1)],A[b])){
        print(r)
        memoire = list()
        memoire[r] = 1
        
        for(z in seq_len(3)){
          memoire2 = list()
          for(i in seq_along(memoire)){
            res = test(names(memoire[i]),memoire[[i]])
            for(j in seq_along(res)){
              if(is.null(memoire2[[names(res)[j]]])){
                memoire2[[names(res)[j]]] = as.numeric(res[j])
              } else {
                memoire2[[names(res)[j]]] = as.numeric(res[j] + memoire2[[names(res)[j]]])
              }
            }
          }
          memoire = memoire2
        }
        jean <<- memoire
        chemins <-
          lapply(seq_along(memoire),function(.x){nchar(names(memoire[.x])) * memoire[[.x]]}) |> 
          unlist() |> 
          sum() |> 
          (\(.){append(chemins,.)})() 
      }
      print(chemins)
      sortie <-
        append(sortie,min(chemins))
    }
    sum(sortie)
  }
) |> 
  unlist() |> 
  (\(.){. * as.numeric(gsub("[A-Z]","",input))})() |> 
  sum()






memoire = list()
memoire[combinaisons[["9"]][["A"]]] = 1

for(z in seq_len(2)){
  memoire2 = list()
  for(i in seq_along(memoire)){
    res = test(names(memoire[i]),memoire[[i]])
    for(j in seq_along(res)){
      if(is.null(memoire2[[names(res)[j]]])){
        memoire2[[names(res)[j]]] = as.numeric(res[j])
      } else {
        memoire2[[names(res)[j]]] = as.numeric(res[j] + memoire2[[names(res)[j]]])
      }
    }
  }
  memoire = memoire2
}

lapply(seq_along(memoire),function(.x){nchar(names(memoire[.x])) * memoire[[.x]]}) |> 
  unlist() |> 
  sum()


test2 <- function(.in){
  .in |> 
    (\(.){mapply(
      function(.x,.y){
        test(.y,.x)
      },
      .,
      names(.),
      SIMPLIFY = F,
      USE.NAMES = F
    )})()
}


table(combinaisons[["A"]][["0"]]) |> 
  test2() |> 
  (\(.){test2(.[[1]])})()


test2(table(combinaisons[["A"]][["0"]])) |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2() |> 
  test2()


get_moves <- function(.x){
  x = main_F(.x) 
  for(i in seq_len(25-1)){
    x = main_F(x)
  }
  x
}

solution2 <-
  NA