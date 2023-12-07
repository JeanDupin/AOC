# Input ----

input <-
  readLines("2016/Inputs/Day04.txt")


# Partie 1 ----

solution1 <-
  sapply(
    input,
    function(.x){
      if(gsub("[0-9].*$","",.x) |> 
         (\(.){gsub("-","",.)})() |> 
         strsplit("") |> 
         table() |> 
         data.frame() |> 
         (\(.){.[order(-.$Freq, .$Var1),]})() |> 
         (\(.){as.character(.[1:5,"Var1"])})() |> 
         paste(collapse = "") == gsub("(.*\\[)(.*)(\\]$)","\\2",.x)){
        as.numeric(regmatches(.x,gregexpr("[0-9]+",.x))[[1]])
      } else {
        0
      }
    },USE.NAMES = F) |> 
  sum()

# Partie 2 ----

translated <- c()
for(i in seq_along(input)){
  
  n = as.numeric(regmatches(input[i],gregexpr("[0-9]+",input[i]))[[1]])
  
  translated[i] <-
    sapply(strsplit(gsub("-[0-9].*","",input[i]),"")[[1]],
           function(.x){
             if(.x == "-"){" "} else {
               n = (which(letters == .x) + n)%%26
               if(n == 0){n = 26}
               letters[n]
             }
           },USE.NAMES = F) |> 
      paste(collapse = "")
  
  
}; rm(i, n)

solutions2 <-
  input[which(grepl("north",translated))] |> 
  (\(.x){as.numeric(regmatches(.x,gregexpr("[0-9]+",.x))[[1]])})()

