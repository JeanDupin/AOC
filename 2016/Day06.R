# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/6/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 


# Partie 1 ----


solution1 <-
  sapply(1:unique(nchar(input)),
         USE.NAMES = F,
         function(.y){
           
           sapply(input,USE.NAMES = F,
                  function(.x){
                    substr(.x,.y,.y)
                  }) |> 
             table() |> 
             sort(decreasing = T) |> 
             (\(.){names(.[1])})()
           
         }) |> 
  paste(collapse = "")


solution2 <-
  sapply(1:unique(nchar(input)),
         USE.NAMES = F,
         function(.y){
           
           sapply(input,USE.NAMES = F,
                  function(.x){
                    substr(.x,.y,.y)
                  }) |> 
             table() |> 
             sort(decreasing = F) |> 
             (\(.){names(.[1])})()
           
         }) |> 
  paste(collapse = "")

