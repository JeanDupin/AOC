# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  gsub(".* \\| ","",input) |> 
  lapply(function(.x){
    regmatches(.x,
               gregexpr("\\w+",.x))[[1]] |> 
      nchar() |> 
      (\(.){sum(. %in% c(2,3,4,7))})()
  }) |> 
  unlist() |> 
  sum()

# Partie 2 ----

combinaisons <-
  expand.grid(letters[1:7],letters[1:7],letters[1:7],letters[1:7],
              letters[1:7],letters[1:7],letters[1:7],stringsAsFactors = F) |> 
  (\(.){
    .[apply(.,1,function(.x){length(unique(.x)) == 7}),]
  })()

chiffres <-
  c("Var1-Var2-Var3-Var5-Var6-Var7",
    "Var3-Var6",
    "Var1-Var3-Var4-Var5-Var7",
    "Var1-Var3-Var4-Var6-Var7",
    "Var2-Var3-Var4-Var6",
    "Var1-Var2-Var4-Var6-Var7",
    "Var1-Var2-Var4-Var5-Var6-Var7",
    "Var1-Var3-Var6",
    "Var1-Var2-Var3-Var4-Var5-Var6-Var7",
    "Var1-Var2-Var3-Var4-Var6-Var7")

find_combinaisons <-
  function(instructions){
    instructions_utiles <-
      regmatches(instructions,
                 gregexpr("\\w+",instructions))[[1]] |> 
      (\(.){.[order(nchar(.))]})() |> 
      (\(.){.[nchar(.) %in% c(2,3,4)]})()
    
    lettres1 <-
      instructions_utiles[nchar(instructions_utiles) == 2] |> 
      (\(.){strsplit(.,"")[[1]]})() |> 
      unlist() |> 
      unique()
    
    lettres7 <-
      instructions_utiles[nchar(instructions_utiles) == 3] |> 
      (\(.){strsplit(.,"")[[1]]})() |> 
      unlist() |> 
      unique() |> 
      (\(.){.[!. %in% lettres1]})()
    
    lettres4 <-
      instructions_utiles[nchar(instructions_utiles) == 4] |> 
      (\(.){strsplit(.,"")[[1]]})() |> 
      unlist() |> 
      unique() |> 
      (\(.){.[!. %in% lettres1]})()
    
    combi <-
      combinaisons |> 
      (\(.){.[.$Var3 %in% lettres1 & .$Var6 %in% lettres1 &
                .$Var1 == lettres7 & .$Var2 %in% lettres4 & .$Var4 %in% lettres4,]})()
    
    combi
    
    
  }

solution2 <-
  lapply(input,
         function(.a){
           combi_possibles <-
             gsub(" \\| .*$","",.a) |> 
             find_combinaisons()
           
           combi_possibles <-
             apply(combi_possibles,1,function(.x){
               gsub(".* \\| ","",.a) |>
                 (\(.){strsplit(.," ")[[1]]})() |>
                 strsplit("") |> 
                 lapply(function(.){
                   sort(names(.x)[match(.,.x)]) |> 
                     paste(collapse = "-") |> 
                     (\(.z){.z %in% chiffres})()
                 }) |> 
                 unlist() |> 
                 all()
               
             },simplify = F) |> 
             unlist() |> 
             (\(.){combi_possibles[.,][1,]})()
           
           gsub(".* \\| ","",.a) |>
             (\(.){strsplit(.," ")[[1]]})() |>
             strsplit("") |> 
             lapply(function(.){
               sort(names(combi_possibles)[match(.,combi_possibles)]) |> 
                 paste(collapse = "-") |> 
                 (\(.z){which(chiffres == .z)-1})()
             }) |> 
             paste(collapse = "") |> 
             as.numeric()
           
           
         }) |> 
  unlist() |> 
  sum()
