# Input ----

input <-
  get_input("https://adventofcode.com/2021/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  strsplit(input,"") |> 
  (\(.){matrix(unlist(.),ncol = length(.[[1]]), byrow = T)})() |> 
  apply(2,
        function(.x){
          c(
            c(0,1)[which.max(table(.x))],
            c(0,1)[which.min(table(.x))])
        }) |> 
  apply(1,
        function(.x){
          strtoi(paste(.x,collapse = ""),2)
        }) |> 
  prod()


# Partie 2 ----

oxygen = co2 = input
for(i in seq_len(nchar(input[1]))){
  
  oxygen <-
    strsplit(oxygen,"") |> 
    lapply(function(.x){.x[i]}) |> 
    (\(.){rev(table(unlist(.)))})() |> 
    (\(.){names(.)[which.max(.)[[1]]]})() |> 
    (\(.){
      oxygen[lapply(strsplit(oxygen,""),function(.x){.x[i]}) == .]
    })()
  
  if(length(oxygen) == 1){break}
  
}

for(i in seq_len(nchar(input[1]))){
  
  co2 <-
    strsplit(co2,"") |> 
    lapply(function(.x){.x[i]}) |> 
    (\(.){table(unlist(.))})() |> 
    (\(.){names(.)[which.min(.)[[1]]]})() |> 
    (\(.){
      co2[lapply(strsplit(co2,""),function(.x){.x[i]}) == .]
    })()
  
  if(length(co2) == 1){break}
  
}


solution2 <-
  strtoi(oxygen,2) * strtoi(co2,2)

