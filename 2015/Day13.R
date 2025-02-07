# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/13/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

instructions <-
  gsub("lose ","-",input) |> 
  (\(.){
    regmatches(.,
               gregexpr("([A-Z][a-z]+)|((-|)\\d+)",.))
  })() |> 
  sapply(function(.x){paste(.x,collapse = "/")})

personnes <-
  unique(gsub("\\/.*$","",instructions))

combinaisons <-
  combinat::permn(personnes) |> 
  lapply(function(.x){c(.x[length(.x)],.x,.x[1])})


solution1 <-
  lapply(combinaisons, function(.x){

    score = 0
    for(i in 2:(1+length(personnes))){
      main = paste0("^",.x[i],"\\/.*\\/")
      to = paste0(.x[(i-1)],"$")
      to2 = paste0(.x[(i+1)],"$")
      score1 = instructions[grepl(paste0(main,to),instructions)] |>
        (\(.){
          regmatches(.,
                    gregexpr("(-|)\\d+",.))[[1]]
        })() |> as.numeric()
      score2 = instructions[grepl(paste0(main,to2),instructions)] |>
        (\(.){
          regmatches(.,
                    gregexpr("(-|)\\d+",.))[[1]]
        })() |> as.numeric()
      score = score + score1 + score2
    }

    score
  }) |> 
  unlist() |> 
  max()


# Partie 2 ----

instructions <- append(instructions,paste0(personnes,"/0/Jean"))
instructions <- append(instructions,paste0("Jean/0/",personnes))

personnes <-
  unique(gsub("\\/.*$","",instructions))


combinaisons <-
  combinat::permn(personnes) |> 
  lapply(function(.x){c(.x[length(.x)],.x,.x[1])})

solution2 <-
  lapply(combinaisons, function(.x){

    score = 0
    for(i in 2:(1+length(personnes))){
      main = paste0("^",.x[i],"\\/.*\\/")
      to = paste0(.x[(i-1)],"$")
      to2 = paste0(.x[(i+1)],"$")
      score1 = instructions[grepl(paste0(main,to),instructions)] |>
        (\(.){
          regmatches(.,
                    gregexpr("(-|)\\d+",.))[[1]]
        })() |> as.numeric()
      score2 = instructions[grepl(paste0(main,to2),instructions)] |>
        (\(.){
          regmatches(.,
                    gregexpr("(-|)\\d+",.))[[1]]
        })() |> as.numeric()
      score = score + score1 + score2
    }

    score
  }) |> 
  unlist() |> 
  max()
