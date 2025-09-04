# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/19/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

mot = input[!grepl("=",input) & input != ""]
instructions1 <-
  input[grepl("=",input)] |> 
  (\(.){gsub(" .*$","",.)})() |> 
  (\(.){paste0("(?=(",.,"))")})()
instructions2 <-
  input[grepl("=",input)] |> 
  (\(.){gsub("^.* ","",.)})()

generer_variantes <- function(motif, remplacement){
  positions <- gregexpr(motif, mot, perl = T)[[1]]
  
  if(positions[1] == -1){return(list(NA))}
  
  variantes <- list()
  
  for(i in seq_along(positions)){
    debut <- substr(mot, 1, positions[i] - 1)  
    fin <- substr(mot, positions[i] + nchar(gsub("[^A-Za-z]","",motif)), nchar(mot))
    variante <- paste0(debut, remplacement, fin)  
    variantes[[i]] <- variante
  }
  
  return(variantes)
}

solution1 <-
  mapply(generer_variantes,
        instructions1,
        instructions2,
        SIMPLIFY = F,
        USE.NAMES = F) |> 
  unlist() |> 
  unique() |> 
  (\(.){sum(!is.na(.))})()


# Partie 2 ----

molecule <-
  input[length(input)]

elements <-
  regmatches(molecule,
             gregexpr("[A-Z][a-z]?",molecule)) |> 
  unlist()

solution2 <-
  length(elements) - sum(elements == "Rn") - sum(elements == "Ar") - 2*sum(elements == "Y") - 1
