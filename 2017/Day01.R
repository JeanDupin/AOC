# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/1/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 


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



