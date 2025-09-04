# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/9/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

sequence <-
  strsplit(input,"")[[1]]


sequence_finale <-
  vector("character",0)

i = 1
while(i <= length(sequence)){
  if(sequence[i] != "("){
    sequence_finale <-
      append(sequence_finale,sequence[i])
    i = i + 1
  } else {
    ids <-
      paste(sequence[i:length(sequence)],
            collapse = "") |> 
      (\(.){
        regmatches(.,
                   gregexpr("\\d+",.))[[1]][1:2]
      })() |> 
      as.numeric()
    
    
    sequence_finale <-
      gsub("^\\(.+?\\)","",
           paste(sequence[i:length(sequence)],
                 collapse = "")) |> 
      (\(.){strsplit(.,"")[[1]][seq_len(ids[1])]})() |> 
      rep(ids[2]) |> 
      (\(.){append(sequence_finale,.)})()
    
    i <-
      paste(sequence[i:length(sequence)],
            collapse = "") |> 
      (\(.){
        regmatches(.,
                   gregexpr("^\\(.+?\\)",.))[[1]]
      })() |> 
      (\(.){nchar(.) + ids[1] + i})()
  }
}


solution1 <-
  length(sequence_finale)

# Partie 2 ----

solution2 <-
  NA
