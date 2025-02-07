# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/20/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

instructions <-
  input[order(
    regmatches(input,
               gregexpr("^\\d+",input)) |> 
      (\(.){as.numeric(unlist(.))})()
  )]

res = vector("logical",length(instructions))
for(i in seq_along(instructions)[-1]){
  res[i] <-
    !((as.numeric(strsplit(instructions[i-1],"-")[[1]][2]) + 1) >= as.numeric(strsplit(instructions[i],"-")[[1]][1]))
}

solution1 <-
  regmatches(instructions[which(res)[1]],
           gregexpr("^\\d+",instructions[which(res)[1]]))[[1]] |> 
  (\(.){as.numeric(.) - 1})()

# Partie 2 ----

check_range <- function(range1, range2){
  ((as.numeric(strsplit(range2,"-")[[1]][2]) + 1) >= as.numeric(strsplit(range1,"-")[[1]][1]))
}


forbidden <- vector("list")

for(i in seq_along(instructions)){
  if(i == 1){
    forbidden[[i]] = instructions[i]
  } else {
    can_increase <-
      sapply(forbidden, function(.x){
        check_range(instructions[i],.x)
      }, USE.NAMES = F, simplify = T)
    if(!any(can_increase)){
      forbidden <- append(forbidden, instructions[i])
    } else {
      borne <-
        max(
          as.numeric(gsub("^.*-","",instructions[i])),
          as.numeric(gsub("^.*-","",forbidden[which(can_increase)[1]]))
        )
      forbidden[which(can_increase)[1]] <-
        paste0(
          gsub("-.*$","",forbidden[which(can_increase)[1]]),
          "-",
          borne
        )
    }
  }
}

allowed <-
  0
for(i in seq_along(forbidden)[-length(forbidden)]){
  allowed <-
    as.numeric(gsub("-.*$","",forbidden[[i+1]])) - as.numeric(gsub("^.*-","",forbidden[[i]])) - 1 + allowed
}

solution2 <-
  allowed