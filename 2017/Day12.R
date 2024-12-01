# Input ----

input <-
  httr2::request("https://adventofcode.com/2017/day/12/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Partie 1 ----


find_connected <-
  function(starting){
    
    to.check = starting
    checked = NULL
    
    while(length(to.check) > 0){
      
      id = to.check[1]
      checked <- append(checked,id)
      to.check <- to.check[-1]
      
      next.to.check <-
        input[grepl(paste0("^",id," "),input)] |> 
        (\(.){gsub("^.* <-> ","",.)})() |> 
        (\(.){strsplit(.,", ")[[1]]})()
      
      
      
      to.check <-
        append(to.check,
               next.to.check[!(next.to.check %in% checked)])
      to.check <-
        unique(to.check)
    }
    
    return(checked)
    
  }

solution1 <-
  length(find_connected("0"))

# Partie 2 ----


find_groups <-
  function(input){
    
    id <- 
      regmatches(input,
                 gregexpr("^[0-9]+",input)) |> 
      unlist()
    
    n.groupes = 0
    
    while(length(id) > 0){
      id <-
        id[-which(id %in% find_connected(id[1]))]
      n.groupes = n.groupes + 1
    }
    return(n.groupes)
  }

solution2 <-
  find_groups(input)
