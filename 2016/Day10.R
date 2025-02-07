# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/10/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

robots <-
  regmatches(input,
             gregexpr("bot \\d+",input)) |> 
  (\(.){unique(unlist(.))})() |>
  (\(.){
    regmatches(.,
               gregexpr("\\d+",.)) |> 
      as.numeric() |> 
      max()
  })() |> 
  (\(.){vector("list",.+1)})()

output <-
  regmatches(input,
             gregexpr("output \\d+",input)) |> 
              (\(.){unique(unlist(.))})() |>
                (\(.){
                  regmatches(.,
                             gregexpr("\\d+",.)) |> 
                    as.numeric() |> 
                    max()
                })() |> 
                (\(.){vector("list",.+1)})()

jetons <-
  input[grepl("^value",input)]
for(i in seq_along(jetons)){
  regmatches(jetons[i],
             gregexpr("\\d+",jetons[i]))[[1]] |> 
    as.numeric() |> 
    (\(.){
      robots[[.[2]+1]] <<- sort(append(robots[[.[2]+1]],.[1]))
    })()
}

instructions <-
  input[grepl("^bot",input)]

comparator = vector("integer",0)

while(length(instructions) > 0 & length(comparator) == 0){
  comparator <-
    lapply(robots, function(.x){
      paste(.x, collapse = ";") == "17;61"
    }) |> 
    (\(.){which(unlist(.))})() |> 
    (\(.){append(comparator,.)})()

  instruction = instructions[1]
  
  bot = regmatches(instruction,
             gregexpr("^bot \\d+",instruction))[[1]] |> 
    (\(.){as.numeric(gsub("bot ","",.)) + 1})()

  instructions = append(instructions, instruction)
  instructions = instructions[-1]

  if(length(robots[[bot]]) != 2){
    next
  }

  tolow <-
    regmatches(instruction,
               gregexpr("low to .* and",instruction))[[1]] |> 
    (\(.){gsub("(low to )(.*)( and)","\\2",.)})()
  tohigh <-
    regmatches(instruction,
               gregexpr("high to .*$",instruction))[[1]] |> 
    (\(.){gsub("(high to )(.*$)","\\2",.)})()

  # LOW
  if(grepl("output",tolow)){
    output[[as.numeric(gsub("output ","",tolow)) + 1]] <-
      append(output[[as.numeric(gsub("output ","",tolow)) + 1]],robots[[bot]][1])
    robots[[bot]] <- robots[[bot]][-1]
  } else {
    robots[[as.numeric(gsub("bot ","",tolow)) + 1]] <-
      append(robots[[as.numeric(gsub("bot ","",tolow))+1]],
             robots[[bot]][1]) |>  sort()
    robots[[bot]] <- robots[[bot]][-1]
  }
  # HIGH
  if(grepl("output",tohigh)){
    output[[as.numeric(gsub("output ","",tohigh)) + 1]] <-
      append(output[[as.numeric(gsub("output ","",tohigh)) + 1]],robots[[bot]][1])
    robots[[bot]] <- robots[[bot]][-1]
  } else {
    robots[[as.numeric(gsub("bot ","",tohigh)) + 1]] <-
      append(robots[[as.numeric(gsub("bot ","",tohigh))+1]],
             robots[[bot]][1]) |>  sort()
    robots[[bot]] <- robots[[bot]][-1]
  }

}


solution1 <-
  comparator - 1

# Partie 2 ----


robots <-
  regmatches(input,
             gregexpr("bot \\d+",input)) |> 
  (\(.){unique(unlist(.))})() |>
  (\(.){
    regmatches(.,
               gregexpr("\\d+",.)) |> 
      as.numeric() |> 
      max()
  })() |> 
  (\(.){vector("list",.+1)})()

output <-
  regmatches(input,
             gregexpr("output \\d+",input)) |> 
              (\(.){unique(unlist(.))})() |>
                (\(.){
                  regmatches(.,
                             gregexpr("\\d+",.)) |> 
                    as.numeric() |> 
                    max()
                })() |> 
                (\(.){vector("list",.+1)})()

jetons <-
  input[grepl("^value",input)]
for(i in seq_along(jetons)){
  regmatches(jetons[i],
             gregexpr("\\d+",jetons[i]))[[1]] |> 
    as.numeric() |> 
    (\(.){
      robots[[.[2]+1]] <<- sort(append(robots[[.[2]+1]],.[1]))
    })()
}

instructions <-
  input[grepl("^bot",input)]

comparator = vector("integer",0)

while(length(instructions) > 0 & (length(output[[1]]) == 0 | length(output[[2]]) == 0 | length(output[[3]]) == 0)){

  comparator <-
    lapply(robots, function(.x){
      paste(.x, collapse = ";") == "17;61"
    }) |> 
    (\(.){which(unlist(.))})() |> 
    (\(.){append(comparator,.)})()

  instruction = instructions[1]
  
  bot = regmatches(instruction,
             gregexpr("^bot \\d+",instruction))[[1]] |> 
    (\(.){as.numeric(gsub("bot ","",.)) + 1})()

  instructions = append(instructions, instruction)
  instructions = instructions[-1]

  if(length(robots[[bot]]) != 2){
    next
  }

  tolow <-
    regmatches(instruction,
               gregexpr("low to .* and",instruction))[[1]] |> 
    (\(.){gsub("(low to )(.*)( and)","\\2",.)})()
  tohigh <-
    regmatches(instruction,
               gregexpr("high to .*$",instruction))[[1]] |> 
    (\(.){gsub("(high to )(.*$)","\\2",.)})()

  # LOW
  if(grepl("output",tolow)){
    output[[as.numeric(gsub("output ","",tolow)) + 1]] <-
      append(output[[as.numeric(gsub("output ","",tolow)) + 1]],robots[[bot]][1])
    robots[[bot]] <- robots[[bot]][-1]
  } else {
    robots[[as.numeric(gsub("bot ","",tolow)) + 1]] <-
      append(robots[[as.numeric(gsub("bot ","",tolow))+1]],
             robots[[bot]][1]) |>  sort()
    robots[[bot]] <- robots[[bot]][-1]
  }
  # HIGH
  if(grepl("output",tohigh)){
    output[[as.numeric(gsub("output ","",tohigh)) + 1]] <-
      append(output[[as.numeric(gsub("output ","",tohigh)) + 1]],robots[[bot]][1])
    robots[[bot]] <- robots[[bot]][-1]
  } else {
    robots[[as.numeric(gsub("bot ","",tohigh)) + 1]] <-
      append(robots[[as.numeric(gsub("bot ","",tohigh))+1]],
             robots[[bot]][1]) |>  sort()
    robots[[bot]] <- robots[[bot]][-1]
  }

}

solution2 <-
  output[[1]] * output[[2]] * output[[3]]
