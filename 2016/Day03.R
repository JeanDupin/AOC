# Input ----

input <-
  readLines("2016/Inputs/Day03.txt")


# Partie 1 ----

solution1 <-
  sapply(input, function(.x){
    
    numbers <-
      regmatches(.x, gregexpr("[0-9]+",.x))[[1]] |> 
      as.numeric() |> 
      sort()
    
    if(numbers[1] + numbers[2] <= numbers[3]){
      return(FALSE)
    } else {
      return(TRUE)
    }
    
  },
  USE.NAMES = F) |> 
    sum()

# Partie 2 ----



inputb <- c()
for(i in c(1:length(input))[c(1:length(input)) %% 3 == 1]){
  inputb <-
    append(inputb,
           sapply(input, function(.x){gsub("^\\s*","",.x)},USE.NAMES = F) |> 
             (\(.){
               gsub("\\s*","",unlist(regmatches(.,gregexpr("^[0-9]+\\s",.))))
             })() |> 
             (\(.){
               paste(
                 .[i],.[i+1],.[i+2],
                 sep = " "
               )
             })())
  inputb <-
    append(inputb,
           sapply(input, function(.x){gsub("^\\s*","",.x)},USE.NAMES = F) |> 
             (\(.){
               gsub("\\s*","",unlist(regmatches(.,gregexpr("\\s[0-9]+\\s",.))))
             })() |> 
             (\(.){
               paste(
                 .[i],.[i+1],.[i+2],
                 sep = " "
               )
             })())
  inputb <-
    append(inputb,
           sapply(input, function(.x){gsub("^\\s*","",.x)},USE.NAMES = F) |> 
             (\(.){
               gsub("\\s*","",unlist(regmatches(.,gregexpr("[0-9]+$",.))))
             })() |> 
             (\(.){
               paste(
                 .[i],.[i+1],.[i+2],
                 sep = " "
               )
             })())
}; rm(i)
 

solution2 <-
  sapply(inputb, function(.x){
    
    numbers <-
      regmatches(.x, gregexpr("[0-9]+",.x))[[1]] |> 
      as.numeric() |> 
      sort()
    
    if(numbers[1] + numbers[2] <= numbers[3]){
      return(FALSE)
    } else {
      return(TRUE)
    }
    
  },
  USE.NAMES = F) |> 
  sum()


