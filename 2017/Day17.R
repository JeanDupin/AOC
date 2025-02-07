# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/17/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  as.numeric()

# Partie 1 ----

tornado = 0
position = 1

for(i in seq_len(2017)){
  position = (position + input) %% length(tornado)
  position = ifelse(position == 0,length(tornado), position)
  tornado = append(tornado, i, after = position)
  position = position + 1
}

solution1 <-
  tornado[which(tornado == 2017)+1]

# Partie 2 ----

tornado = 0
position = 1

for(i in seq_len(50000000)){
  position = (position + input) %% i
  position = ifelse(position == 0,i, position)
  tornado = ifelse(position == 1, i, tornado)
  position = position + 1
}

solution2 <-
  tornado
