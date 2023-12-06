# Input ----

input <-
  readLines("2015/Inputs/Day05.txt")


# Partie 1 ----

is_good <-
  function(.x){
    if(max(gregexec("ab|cd|pq|xy",.x)[[1]]) > 0){
      return(FALSE)
    }
    if(length(gregexec("[aeiou]",.x)[[1]]) < 3){
      return(FALSE)
    }
    if(!any(sapply(paste0(letters,letters), function(.y){grepl(.y,.x)}))){
      return(FALSE)
    }
    return(TRUE)
  }

solution1 <-
  sum(sapply(input,is_good))

# Partie 2 ----

doubles = apply(expand.grid(letters,letters),1, paste0, collapse = "")


is_good <-
  function(.x){
   if(!any(sapply(doubles, function(.y){
     length(gregexpr(.y,.x)[[1]]) >= 2
   }))){
     return(FALSE)
   }
    any(sapply(letters, function(.y){
      if(length(gregexpr(.y,.x)[[1]]) < 2){
        return(FALSE)
      } else {
        return(any(diff(gregexpr(.y,.x)[[1]]) == 2))
      }
    }))
}

solution2 <-
  sum(sapply(input,is_good))


