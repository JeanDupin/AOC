# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/23/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

registers <- list("a" = 0, "b" = 0)
operation = 1

move <- function(instruction){
  if(grepl("^inc",instruction)){
    register = gsub("^.* ","",instruction)
    registers[[register]] <<- registers[[register]] + 1
    operation <<- operation + 1
  }
  if(grepl("^hlf",instruction)){
    register = gsub("^.* ","",instruction)
    registers[[register]] <<- registers[[register]]/2
    operation <<- operation + 1
  }
  if(grepl("^tpl",instruction)){
    register = gsub("^.* ","",instruction)
    registers[[register]] <<- registers[[register]]*3
    operation <<- operation + 1
  }
  if(grepl("^jmp",instruction)){
    operation <<- operation + as.numeric(gsub("^.* ","",instruction))
  }
  if(grepl("^jie",instruction)){
    register = gsub("(^.*? )(.*?)(, .*$)","\\2",instruction)
    if(registers[[register]] %% 2 == 0){
      operation <<- operation + as.numeric(gsub("^.* ","",instruction))
    } else {
      operation <<- operation + 1
    }
  }
  if(grepl("^jio",instruction)){
    register = gsub("(^.*? )(.*?)(, .*$)","\\2",instruction)
    if(registers[[register]] == 1){
      operation <<- operation + as.numeric(gsub("^.* ","",instruction))
    } else {
      operation <<- operation + 1
    }
  }
}

while(operation <= length(input)){
  move(input[operation])
}


solution1 <-
  registers[["b"]]

# Partie 2 ----

registers <- list("a" = 1, "b" = 0)
operation = 1

while(operation <= length(input)){
  move(input[operation])
}

solution2 <-
  registers[["b"]]