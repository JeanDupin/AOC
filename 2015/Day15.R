# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/15/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

instructions <-
  lapply(input, function(.x){
    regmatches(.x,
               gregexpr("(-|)\\d+",.x))[[1]] |> 
      as.numeric() |> 
      (\(.){.[-5]})()
  })

grille <-
  expand.grid(1:99,1:99,1:99,1:99) |> 
  (\(.){
    .[rowSums(.) == 100,]
  })() 

solution1 <-
  apply(grille, 1, function(coefs){
    (coefs[1]*instructions[[1]] + coefs[2]*instructions[[2]] + coefs[3]*instructions[[3]] + coefs[4]*instructions[[4]]) |> 
      pmax(0) |> 
      prod()
  }) |> 
  max()


# Partie 2 ----

calories <-
  lapply(input, function(.x){
    regmatches(.x,
               gregexpr("(-|)\\d+",.x))[[1]] |> 
      as.numeric() |> 
      (\(.){.[5]})()
  })

solution2 <-
  apply(grille, 1, function(coefs){
      
    calo <-
      (coefs[1]*calories[[1]] + coefs[2]*calories[[2]] + coefs[3]*calories[[3]] + coefs[4]*calories[[4]])
    if(calo != 500){
      0
    } else {
      (coefs[1]*instructions[[1]] + coefs[2]*instructions[[2]] + coefs[3]*instructions[[3]] + coefs[4]*instructions[[4]]) |> 
       pmax(0) |> 
        prod()
    }
  }) |> 
  max()
