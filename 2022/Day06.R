# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/6/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

get_first <-
  function(.x){
    texte = strsplit(.x,"")[[1]]
    for(i in seq_along(texte)[-c(1:3)]){
      test <-
        texte[i:(i-3)] |> 
        (\(.){!any(table(.) > 1)})()
     
      if(test){break}
    }
    i
  }

solution1 <-
  get_first(input)

# Partie 2 ----
  
get_first <-
  function(.x){
    texte = strsplit(.x,"")[[1]]
    for(i in seq_along(texte)[-c(1:13)]){
      test <-
        texte[i:(i-13)] |> 
        (\(.){!any(table(.) > 1)})()
      
      if(test){break}
    }
    i
  }

solution2 <-
  get_first(input)
