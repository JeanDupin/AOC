# Input ----

input <-
  readLines("2017/Inputs/Day11.txt")

# Partie 1 ----

move <-
  function(deplacement){
    switch (deplacement,
      "n" = {Y <<- Y + 1},
      "ne" = {Y <<- Y + .5;X <<- X + .5},
      "se" = {Y <<- Y - .5;X <<- X + .5},
      "s" = {Y <<- Y - 1},
      "sw" = {Y <<- Y - .5;X <<- X - .5},
      "nw" = {Y <<- Y + .5;X <<- X - .5}
    )
  }


X = Y = 0

invisible(sapply(strsplit(input,",")[[1]],
       move, USE.NAMES = F))

solution1 <-
  abs(X) + abs(Y)

# Partie 2 ----


X = Y = 0
position <-
  paste(X,Y,sep = ";")

invisible(
  sapply(strsplit(input,",")[[1]], USE.NAMES = F,
         function(.x){
           move(.x)
           position <<- append(position,
                              paste(X,Y,sep = ";"))
         })
  )

solution2 <-
  max(abs(as.numeric(gsub("^.*;","",position))) +
        abs(as.numeric(gsub(";.*$","",position))))
