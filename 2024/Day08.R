# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  strsplit(input,"") |> 
  unlist() |> 
  matrix(nrow = length(input), byrow = T)

antennes = unique(matrice[matrice != "."])

solution1 <-
  lapply(
    antennes,
    function(.x){
      
      coords <-
        which(matrice == .x, arr.ind = T) |> 
        apply(1,as.vector,simplify = F) |> 
        combn(2,simplify = F)
      
      antinodes <-
        coords |> 
        lapply(
          function(.y){
            dist = .y[[1]] - .y[[2]]
            list(
              .y[[1]] + dist,
              .y[[1]] - dist,
              .y[[2]] + dist,
              .y[[2]] - dist
            ) |> 
              (\(.){do.call(rbind,.)})()
          }
        ) |> 
        (\(.){do.call(rbind,.)})() |> 
        unique() |> 
        apply(1,function(.z){paste(.z,collapse = ";")})
      
      coords |>
        unlist() |>
        matrix(ncol = 2, byrow = T) |>
        unique() |>
        apply(1,function(.z){paste(.z,collapse = ";")}) |> 
        (\(.){antinodes[!antinodes %in% .]})()
      
      
    }
  ) |> 
  unlist() |> 
  (\(.){.[grepl(paste0("^(",paste(1:length(input),collapse = ";|"),";)"),.)]})() |> 
  (\(.){.[grepl(paste0("(;",paste(1:length(input),collapse = "|;"),")$"),.)]})() |> 
  unique() |> 
  length()


# Partie 2 ----


solution2 <-
  lapply(
    antennes,
    function(.x){
      
      coords <-
        which(matrice == .x, arr.ind = T) |> 
        apply(1,as.vector,simplify = F) |> 
        combn(2,simplify = F)
      
      antinodes <-
        coords |> 
        lapply(
          function(.y){
            dist = .y[[1]] - .y[[2]]
            list(
              lapply(seq_along(input),function(.z){.y[[1]] + .z*dist}) |> (\(.){do.call(rbind,.)})(),
              lapply(seq_along(input),function(.z){.y[[1]] - .z*dist}) |> (\(.){do.call(rbind,.)})(),
              lapply(seq_along(input),function(.z){.y[[2]] + .z*dist}) |> (\(.){do.call(rbind,.)})(),
              lapply(seq_along(input),function(.z){.y[[2]] - .z*dist}) |> (\(.){do.call(rbind,.)})()
            ) |> 
              (\(.){do.call(rbind,.)})()
          }
        ) |> 
        (\(.){do.call(rbind,.)})() |> 
        unique() |> 
        apply(1,function(.z){paste(.z,collapse = ";")})
      
      
    }
  ) |> 
  unlist() |> 
  (\(.){.[grepl(paste0("^(",paste(1:length(input),collapse = ";|"),";)"),.)]})() |> 
  (\(.){.[grepl(paste0("(;",paste(1:length(input),collapse = "|;"),")$"),.)]})() |> 
  unique() |> 
  length()
