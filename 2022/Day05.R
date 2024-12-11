# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/5/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

n <-
  grepl("move",input) |>
  cumsum() |> 
  (\(.){input[which(. == 1)-2]})() |> 
  (\(.){
    regmatches(.,
               gregexpr("\\d+",.))[[1]] |> 
      as.numeric() |> 
      max()
  })()

instructions <-
  input[grepl("move",input)]

stacks <- vector("list",length = n)
grepl("move",input) |>
  cumsum() |> 
  (\(.){input[seq_len(which(. == 1)-3)]})() |> 
  (\(.){gsub("(\\[|\\])"," ",.)})() |>
  (\(.){strsplit(.,"")})() |> 
  lapply(
    function(.x){
      split(.x, cut(seq_along(.x), n, labels = FALSE)) |> 
        lapply(function(.y){gsub("\\s+","",paste(.y,collapse = ""))})
    }
  ) |> 
  lapply(
    function(.x){
      for(i in seq_along(stacks)){
        stacks[[i]] <<-
          append(stacks[[i]],.x[[i]]) |> 
          (\(.){.[. != ""]})()
      }
    }
  )

move <-
  function(from,to,much){
    
    to_move <-
      rev(stacks[[from]][seq_len(much)])
    stacks[[from]] <<- stacks[[from]][-seq_len(much)]
    stacks[[to]] <<- append(to_move, stacks[[to]])
  }


for(i in instructions){
  regmatches(i,
             gregexpr("\\d+",i))[[1]] |> 
    as.numeric() |> 
    (\(.){
      move(.[2],.[3],.[1])
    })()
}


solution1 <-
  lapply(stacks,function(.x){.x[1]}) |>
  paste(collapse = "")




# Partie 2 ----

stacks <- vector("list",length = n)
grepl("move",input) |>
  cumsum() |> 
  (\(.){input[seq_len(which(. == 1)-3)]})() |> 
  (\(.){gsub("(\\[|\\])"," ",.)})() |>
  (\(.){strsplit(.,"")})() |> 
  lapply(
    function(.x){
      split(.x, cut(seq_along(.x), n, labels = FALSE)) |> 
        lapply(function(.y){gsub("\\s+","",paste(.y,collapse = ""))})
    }
  ) |> 
  lapply(
    function(.x){
      for(i in seq_along(stacks)){
        stacks[[i]] <<-
          append(stacks[[i]],.x[[i]]) |> 
          (\(.){.[. != ""]})()
      }
    }
  )

move <-
  function(from,to,much){
    
    to_move <-
      stacks[[from]][seq_len(much)]
    stacks[[from]] <<- stacks[[from]][-seq_len(much)]
    stacks[[to]] <<- append(to_move, stacks[[to]])
  }


for(i in instructions){
  regmatches(i,
             gregexpr("\\d+",i))[[1]] |> 
    as.numeric() |> 
    (\(.){
      move(.[2],.[3],.[1])
    })()
}


solution2 <-
  lapply(stacks,function(.x){.x[1]}) |>
  paste(collapse = "")
