# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/24/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----


revert <- function(x){
  strsplit(x,"/") |> 
    unlist() |> 
    (\(.){paste(.[2],.[1],
                sep = "/")})()
}

cout <- function(x){
  regmatches(x,
             gregexpr("\\d+",x)) |> 
    (\(.){as.numeric(unlist(.))})() |> 
    sum()
}

memoire <-
  input[grepl("^0\\/",input)] |> 
  lapply(function(.x){
    list(chaine = .x,
         pieces = input[input != .x],
         cout = cout(.x))
  })

strongest = 0
link = vector("character")


while(length(memoire) >= 1){
  # On prend le cout le plus élevé actuellement
  id <-
    sapply(memoire,
           function(.x){.x[["cout"]]}) |> 
    which.max()
  state <- memoire[[id]]
  memoire[[id]] <- NULL
  
  if(state$cout >= strongest){
    link = state$chaine
  }
  strongest = max(state$cout, strongest)
  
  # On regarde si on peut ajouter à sa chaine des éléments
  noeud <-
    gsub("^.*\\/","",state$chaine)
  candidats <-
    state$pieces[grepl(paste0("^",noeud,"\\/"), state$pieces)]
  candidatsrev <-
    state$pieces[grepl(paste0("^(?!",noeud,"/",noeud,"$).+/",noeud,"$"), state$pieces, perl = T)]
  
  # On ajoute les éléments
  if(length(candidats) > 0){
    memoire <-
      lapply(candidats,
             function(.x){
               list(chaine = paste(state$chaine, .x, sep = "/"),
                    pieces = state$pieces[state$pieces != .x],
                    cout = cout(paste(state$chaine, .x, sep = "/")))
             }) |> 
      (\(.){append(memoire, .)})()
  }
  if(length(candidatsrev) > 0){
    memoire <-
      lapply(candidatsrev,
             function(.x){
               list(chaine = paste(state$chaine, revert(.x), sep = "/"),
                    pieces = state$pieces[state$pieces != .x],
                    cout = cout(paste(state$chaine, revert(.x), sep = "/")))
             }) |> 
      (\(.){append(memoire, .)})()
  }
}

solution1 <-
  strongest

# Partie 2 ----


memoire <-
  input[grepl("^0\\/",input)] |> 
  lapply(function(.x){
    list(chaine = .x,
         pieces = input[input != .x],
         cout = cout(.x))
  })

strongest = 0
link = ""


while(length(memoire) >= 1){
  # On prend le chemin le plus long actuellement & cout plus haut en cas d'égalité
  id <-
    sapply(memoire,
           function(.x){
             strsplit(.x[["chaine"]],"/")[[1]] |> length()
             }) |> 
    (\(.){which(. == max(.))})()
  if(length(id) > 1){
    id <-
      sapply(memoire[id],
             function(.x){.x[["cout"]]}) |> 
      which.max()
  }
  state <- memoire[[id]]
  memoire[[id]] <- NULL
  
  if(length(strsplit(state$chaine,"/")[[1]]) > length(strsplit(link,"/")[[1]])){
    link = state$chaine
    strongest = state$cout
  } else if(length(strsplit(state$chaine,"/")[[1]]) == length(strsplit(link,"/")[[1]])){
    if(state$cout > strongest){
      link = state$chaine
      strongest = state$cout
    }
  }
  
  # On regarde si on peut ajouter à sa chaine des éléments
  noeud <-
    gsub("^.*\\/","",state$chaine)
  candidats <-
    state$pieces[grepl(paste0("^",noeud,"\\/"), state$pieces)]
  candidatsrev <-
    state$pieces[grepl(paste0("^(?!",noeud,"/",noeud,"$).+/",noeud,"$"), state$pieces, perl = T)]
  
  # On ajoute les éléments
  if(length(candidats) > 0){
    memoire <-
      lapply(candidats,
             function(.x){
               list(chaine = paste(state$chaine, .x, sep = "/"),
                    pieces = state$pieces[state$pieces != .x],
                    cout = cout(paste(state$chaine, .x, sep = "/")))
             }) |> 
      (\(.){append(memoire, .)})()
  }
  if(length(candidatsrev) > 0){
    memoire <-
      lapply(candidatsrev,
             function(.x){
               list(chaine = paste(state$chaine, revert(.x), sep = "/"),
                    pieces = state$pieces[state$pieces != .x],
                    cout = cout(paste(state$chaine, revert(.x), sep = "/")))
             }) |> 
      (\(.){append(memoire, .)})()
  }
  print(length(memoire))
}

  
solution2 <-
  strongest
  
send_solution(2017,24,2,solution2)
