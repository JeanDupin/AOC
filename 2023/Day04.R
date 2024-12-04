# Input ----

input <-
  get_input("https://adventofcode.com/2023/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----

score <- c()
for(i in seq_along(input)){
  winning <-
    gsub(" \\|.*","",gsub("Card [0-9]*\\: ","",input[i])) |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() |> 
    (\(.){.[which(!is.na(.))]})()
  
  mynumbers <-
    gsub(".* \\| ","",gsub("Card [0-9]*\\: ","",input[i])) |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() |> 
    (\(.){.[which(!is.na(.))]})()
  
  if(sum(mynumbers %in% winning) > 0){
    score <-
      append(score,
             1*2^(sum(mynumbers %in% winning)-1))
  } else {
    score <-
      append(score,
             0)
  }
  
}; rm(i, winning, mynumbers)

solution1 <-
  sum(score)

# Partie 2 ----

cards<- seq_along(input)
for(i in seq_along(input)){
  winning <-
    gsub(" \\|.*","",gsub("Card [0-9]*\\: ","",input[i])) |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() |> 
    (\(.){.[which(!is.na(.))]})()
  
  mynumbers <-
    gsub(".* \\| ","",gsub("Card [0-9]*\\: ","",input[i])) |> 
    (\(.){strsplit(.," ")[[1]]})() |> 
    as.numeric() |> 
    (\(.){.[which(!is.na(.))]})()
  
  if(sum(mynumbers %in% winning) > 0){
    cardN <-
      length(cards[which(cards == i)])
    cards <-
      append(cards,
             rep(seq(i+1,i+sum(mynumbers %in% winning),1),cardN))
  }
  
}; rm(i, winning, mynumbers)


solution2 <-
  length(cards)


