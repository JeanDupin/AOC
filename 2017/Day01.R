# Input ----

input <-
  readLines("2017/Inputs/Day01.txt")


# Partie 1 ----

solution1 <-
  {strsplit(input,"")[[1]] == 
    c(strsplit(input,"")[[1]][2:length(strsplit(input,"")[[1]])],
      strsplit(input,"")[[1]][length(strsplit(input,"")[[1]])])} |> 
  (\(.){strsplit(input,"")[[1]][.]})() |> 
  as.numeric() |> 
  sum()
  

# Partie 2 ----

solution2 <-
  {strsplit(input,"")[[1]] == 
      c(strsplit(input,"")[[1]][((length(strsplit(input,"")[[1]])/2)+1):((length(strsplit(input,"")[[1]])/2)*2)],
        strsplit(input,"")[[1]][1:(length(strsplit(input,"")[[1]])/2)])} |> 
  (\(.){strsplit(input,"")[[1]][.]})() |> 
  as.numeric() |> 
  sum()



