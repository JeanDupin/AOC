# Input ----

input <-
  readLines("2015/Inputs/Day08.txt")
input <-
  readLines("2015/test.txt")


# Partie 1 ----

solution1 <-
  sum(nchar(input)) - sum(stringi::stri_length(stringi::stri_unescape_unicode(input))-2)


# Partie 2 ----


get_length <-
  function(.x){
    
    bits <- paste(as.character(charToRaw(.x)),
                  collapse = "")
    
    if(all(gregexpr("22",bits)[[1]] != -1)){
      n.guillemets <- length(gregexpr("22",bits)[[1]])
    } else {
      n.guillemets <- 0
    }
    if(all(gregexpr("5c",bits)[[1]] != -1)){
      n.x <- length(gregexpr("5c",bits)[[1]])
    } else {
      n.x <- 0
    }
    
    reste <- nchar(bits)/2-n.guillemets-n.x
    
    return(reste + 2*(n.guillemets + n.x + 1))
  }

solution2 <-
  sum(sapply(input, get_length,
             USE.NAMES = F)) - sum(nchar(input))




