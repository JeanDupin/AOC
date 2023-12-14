# Input ----

input <- 
  readLines("2017/Inputs/Day08.txt")


# Partie 1 ----

all.letters <-
  c(
    sapply(input, USE.NAMES = F, simplify = T,
           function(.x){regmatches(.x,gregexpr("^[a-z]*",.x))[[1]]}),
    sapply(input, USE.NAMES = F, simplify = T,
           function(.x){gsub("^.* if ","",.x) |> 
               (\(.){regmatches(.,
                                gregexpr("^[a-z]*",.))[[1]]})()})
  ) |> 
  unique()


all.letters <-
  rep(0,length(all.letters)) |> 
  as.list() |> 
  (\(.){`names<-`(.,all.letters)})()



check_condition <- function(x,y,condition){
  switch(condition,
         PP = x < y,
         PPEG = x <= y,
         EG = x == y,
         PGEG = x >= y,
         PG = x > y,
         NE = x != y)
}

make_operation <-
  function(x,y,operation){
    switch(operation,
           inc = x + y,
           dec = x - y)
  }


do_instruction <-
  function(consigne){
    # Partie condition
    if(grepl(" < ",consigne)){
      condition = "PP"
    } else if(grepl(" <= ",consigne)){
      condition = "PPEG"
    } else if(grepl(" == ",consigne)){
      condition = "EG"
    } else if(grepl(" >= ",consigne)){
      condition = "PGEG"
    } else if(grepl(" > ",consigne)){
      condition = "PG"
    } else if(grepl(" != ",consigne)){
      condition = "NE"
    }
    
    y = regmatches(consigne,gregexpr("(|-)[0-9]+$",consigne))
    x = all.letters[[gsub("(.* if )([a-z]+)( .*)","\\2",consigne)]]
    
    if(check_condition(x,y,condition)){
      
      x = regmatches(consigne,gregexpr("^[a-z]+",consigne))[[1]]
      y = regmatches(consigne,gregexpr("(|-)[0-9]+",consigne))[[1]][1] |> 
        as.numeric()
      operation = regmatches(consigne,gregexpr("inc|dec",consigne))[[1]]
      
      all.letters[[x]] <<- make_operation(all.letters[[x]],y,operation)
      
      
    }
    
  }


sapply(input,
       do_instruction) |> 
  invisible()

solution1 <-
  max(unlist(all.letters))

# Partie 2 ----

all.letters <-
  c(
    sapply(input, USE.NAMES = F, simplify = T,
           function(.x){regmatches(.x,gregexpr("^[a-z]*",.x))[[1]]}),
    sapply(input, USE.NAMES = F, simplify = T,
           function(.x){gsub("^.* if ","",.x) |> 
               (\(.){regmatches(.,
                                gregexpr("^[a-z]*",.))[[1]]})()})
  ) |> 
  unique()


all.letters <-
  rep(0,length(all.letters)) |> 
  as.list() |> 
  (\(.){`names<-`(.,all.letters)})()


all.known = c()

do_instruction <-
  function(consigne){
    # Partie condition
    if(grepl(" < ",consigne)){
      condition = "PP"
    } else if(grepl(" <= ",consigne)){
      condition = "PPEG"
    } else if(grepl(" == ",consigne)){
      condition = "EG"
    } else if(grepl(" >= ",consigne)){
      condition = "PGEG"
    } else if(grepl(" > ",consigne)){
      condition = "PG"
    } else if(grepl(" != ",consigne)){
      condition = "NE"
    }
    
    y = regmatches(consigne,gregexpr("(|-)[0-9]+$",consigne))
    x = all.letters[[gsub("(.* if )([a-z]+)( .*)","\\2",consigne)]]
    
    if(check_condition(x,y,condition)){
      
      x = regmatches(consigne,gregexpr("^[a-z]+",consigne))[[1]]
      y = regmatches(consigne,gregexpr("(|-)[0-9]+",consigne))[[1]][1] |> 
        as.numeric()
      operation = regmatches(consigne,gregexpr("inc|dec",consigne))[[1]]
      
      all.letters[[x]] <<- make_operation(all.letters[[x]],y,operation)
      
      all.known <<- append(all.known, all.letters[[x]])
      
    }
    
  }

sapply(input,
       do_instruction) |> 
  invisible()

solution2 <-
  max(all.known)

