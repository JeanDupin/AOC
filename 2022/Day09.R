# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/9/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

close_gap <- function(.xh,.yh,.xt,.yt){
  if(.xt %in% c(.xh-1,.xh,.xh + 1) & .yt %in% c(.yh-1,.yh,.yh+1)){
    return(c(.xt,.yt))
  }

  if(.xh == .xt & .yh == .yt){
    return(c(.xt,.yt))
  } else if(.xh == .xt){
    if(.yh > .yt){
      return(c(.xt,.yt + 1))
    } else {
      return(c(.xt,.yt - 1))
    }
  } else if(.yh == .yt){
    if(.xh > .xt){
      return(c(.xt + 1,.yt))
    } else {
      return(c(.xt - 1,.yt))
    }
  } else if(.xh > .xt & .yh > .yt){
    return(c(.xt + 1,.yt + 1))
  } else if(.xh > .xt & .yh < .yt){
    return(c(.xt + 1,.yt - 1))
  } else if(.xh < .xt & .yh < .yt){
    return(c(.xt - 1,.yt - 1))
  } else if(.xh < .xt & .yh > .yt){
    return(c(.xt - 1,.yt + 1))
  }
  
}


mouvement <- function(.d,.y){
  switch(.d,
    "R" = rep(list(c(1,0)),.y),
    "L" = rep(list(c(-1,0)),.y),
    "U" = rep(list(c(0,1)),.y),
    "D" = rep(list(c(0,-1)),.y)
  )
}

instructions <-
  lapply(input, function(.x){
    strsplit(.x," ")[[1]] |> 
      (\(.){mouvement(.[1],as.numeric(.[2]))})()
  }) |> 
  unlist(recursive = F)

head = c(1,1)
tail = c(1,1)
memoire <- vector("character", length(instructions))

for(i in seq_along(instructions)){
  head <- head + instructions[[i]]
  tail <-
    close_gap(head[1],head[2],tail[1],tail[2])
  memoire[i] = paste(tail, collapse = ";")
}

solution1 <-
  length(unique(memoire))

# Partie 2 ----


head = c(1,1)
tail1 = c(1,1)
tail2 = c(1,1)
tail3 = c(1,1)
tail4 = c(1,1)
tail5 = c(1,1)
tail6 = c(1,1)
tail7 = c(1,1)
tail8 = c(1,1)
tail9 = c(1,1)
memoire <- vector("character", length(instructions))

for(i in seq_along(instructions)){
  head <- head + instructions[[i]]
  tail1 <-
    close_gap(head[1],head[2],tail1[1],tail1[2])
  tail2 <-
    close_gap(tail1[1],tail1[2],tail2[1],tail2[2])
  tail3 <-
    close_gap(tail2[1],tail2[2],tail3[1],tail3[2])
  tail4 <-
    close_gap(tail3[1],tail3[2],tail4[1],tail4[2])
  tail5 <-
    close_gap(tail4[1],tail4[2],tail5[1],tail5[2])
  tail6 <-
    close_gap(tail5[1],tail5[2],tail6[1],tail6[2])
  tail7 <-
    close_gap(tail6[1],tail6[2],tail7[1],tail7[2])
  tail8 <-
    close_gap(tail7[1],tail7[2],tail8[1],tail8[2])
  tail9 <-
    close_gap(tail8[1],tail8[2],tail9[1],tail9[2])
  memoire[i] = paste(tail9, collapse = ";")
}

solution2 <-
  length(unique(memoire))
