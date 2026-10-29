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

solve <-
  function(.x){
    total = 0
    i = 1
    n = nchar(.x)

    while(i <= n){
      ch <-
        substr(.x, i, i)
      if(ch == "("){
        j = i + regexpr("\\)", substr(.x, i, n)) - 1
        parts = as.numeric(strsplit(substr(.x, i + 1, j - 1), "x")[[1]])
        len   <- parts[1]
        times <- parts[2]
        total = total + times * solve(substr(.x, j + 1, j + len))
        i = j + len + 1
      } else {
        total = total + 1
        i = i + 1
      }
    }
    total
}

solution2 <-
  solve(paste(input, collapse = ""))
