# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/13/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

res = vector("numeric")
for(i in seq(1,length(input),4)){
  A <-
    regmatches(input[i],
               gregexpr("\\d+",input[i]))[[1]] |> 
    as.numeric()
  B <-
    regmatches(input[i+1],
               gregexpr("\\d+",input[i+1]))[[1]] |> 
    as.numeric()
  goal <-
    regmatches(input[i+2],
               gregexpr("\\d+",input[i+2]))[[1]] |> 
    as.numeric()
  
  n <-
    (A[2]*goal[1] - A[1]*goal[2])/(A[2]*B[1]-A[1]*B[2])
  m <-
    (goal[2]-n*B[2])/A[2]
  
  if(n%%1 == 0 & m%%1 == 0){
    res <-
      append(res,n+3*m)
  }
  
}

solution1 <-
  sum(res)

# Partie 2 ----


res = vector("numeric")
for(i in seq(1,length(input),4)){
  A <-
    regmatches(input[i],
               gregexpr("\\d+",input[i]))[[1]] |> 
    as.numeric()
  B <-
    regmatches(input[i+1],
               gregexpr("\\d+",input[i+1]))[[1]] |> 
    as.numeric()
  goal <-
    regmatches(input[i+2],
               gregexpr("\\d+",input[i+2]))[[1]] |> 
    (\(.){as.numeric(.) + 10000000000000})()
  
  n <-
    (A[2]*goal[1] - A[1]*goal[2])/(A[2]*B[1]-A[1]*B[2])
  m <-
    (goal[2]-n*B[2])/A[2]
  
  if(n%%1 == 0 & m%%1 == 0){
    res <-
      append(res,n+3*m)
  }
  
}

solution2 <-
  sum(res)