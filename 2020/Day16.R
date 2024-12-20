# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/16/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

chaines <-
  input[grepl(" or ",input)] |> 
  (\(.){regmatches(.,
             gregexpr("\\d+",.))})() |> 
  lapply(function(.x){
    as.numeric(.x) |> 
      (\(temp){
        c(
          seq(temp[1],temp[2]),
          seq(temp[3],temp[4])
        )
      })()
  }) |> 
  unlist()

solution1 <-
  which(grepl("nearby",input)) |> 
  (\(.){input[(.+1):length(input)]})() |> 
  strsplit(",") |> 
  unlist() |> 
  as.integer() |> 
  (\(.){.[!. %in% chaines]})() |> 
  sum()

# Partie 2 ----


chaines <-
  input[grepl(" or ",input)] |> 
  (\(.){regmatches(.,
             gregexpr("\\d+",.))})() |> 
  lapply(function(.x){
    as.numeric(.x) |> 
      (\(temp){
        c(
          seq(temp[1],temp[2]),
          seq(temp[3],temp[4])
        )
      })()
  }) |> 
  (\(.){
    names(.) <-
      input[grepl(" or ",input)] |> 
      (\(.z){gsub(":.*$","",.z)})()
    .
  })()

tickets <-
  input[grepl("^\\d",input)] |> 
  (\(.a){
    lapply(.a,function(.x){
      as.integer(strsplit(.x,",")[[1]]) |> 
        (\(.){all(. %in% unlist(chaines))})()
    }) |> 
    unlist() |> 
    (\(.){.a[.]})()
  })()


i = 1
res = vector("list",length(chaines))
while(any(lapply(res, length) == 0)){
  if(length(res[[i]]) == 1){
    i = ifelse(i == 20,1,i+1)
    next
  }

  candidats <-
    lapply(tickets, function(.x){
      as.integer(strsplit(.x,",")[[1]])[i]
    }) |> 
    unlist() |> 
    (\(.){
     lapply(chaines, function(.x){
       all(. %in% .x)
     }) 
    })() |> 
    (\(.){names(chaines)[which(unlist(.))]})()

    if(length(candidats) == 1){
      res[[i]] = candidats
      chaines <-
        chaines[-which(names(chaines) == candidats)]
    }

  i = ifelse(i == 20,1,i+1)
}

solution2 <-
  lapply(res, function(.x){grepl("departure",.x)}) |> 
  (\(.){
    as.integer(strsplit(tickets[1],",")[[1]])[unlist(.)]
  })() |> 
    prod()


