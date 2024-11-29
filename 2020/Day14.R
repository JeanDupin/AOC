# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/14/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

convert_to_36_bits <-
  function(.x) {
  
    quotient = .x %/% 2
    reste = .x %% 2
    a0 <- reste
    while(quotient != 0){
      a = quotient %/% 2
      reste = quotient %% 2
      a0 <- append(a0,reste)
      quotient <- a
    }
    
    paste0(strrep("0",36-length(a0)),paste(rev(a0),collapse = ""))
    
}

convert_to_int <-
  function(.x){
    nb <-
      gsub("^0+","",.x) |> 
      (\(.){strsplit(.,"")[[1]]})() |> 
      as.numeric()
    v = 0
    
    for(i in seq_along(nb)){
      v = nb[i] + 2*v
    }
    v
  }

mem <- list()
mask = ""

for(i in seq_along(input)){
  if(grepl("mask",input[i])){
    mask = gsub("mask = ","",input[i])
  } else {
    mem[paste0("V",sub(".*\\[(\\d+)\\].*", "\\1", input[i]))] <-
      strsplit(mask,"")[[1]] |> 
      (\(.){
        ifelse(
          . == "X",
          strsplit(convert_to_36_bits(as.numeric(gsub(".* = ","",input[i]))),"")[[1]],
          .
        )
      })() |> 
      paste(collapse = "")
  }
}


solution1 <-
  lapply(mem,convert_to_int) |> 
  unlist() |> 
  sum()

# Partie 2 ----


get_possibilities <-
  function(original_string){
    
    positions_X <- which(strsplit(original_string, "")[[1]] == "X")
    
    num_X <- length(positions_X)
    
    combinations <- expand.grid(rep(list(c(0, 1)), num_X))
    
    replace_X <- function(combination, string, positions_X) {
      modified_string <- strsplit(string, "")[[1]]
      for (i in 1:length(positions_X)) {
        modified_string[positions_X[i]] <- as.character(combination[i])
      }
      return(paste(modified_string, collapse = ""))
    }
    
    all_strings <- apply(combinations, 1, replace_X, string = original_string, positions_X = positions_X)
    
    all_strings
  }



mem <- list()
mask = ""

for(i in seq_along(input)){
  if(grepl("mask",input[i])){
    mask = gsub("mask = ","",input[i])
  } else {
    strsplit(mask,"")[[1]] |> 
      (\(.){
        ifelse(
          . == "0",
          strsplit(convert_to_36_bits(as.numeric(sub(".*\\[(\\d+)\\].*", "\\1", input[i]))),"")[[1]],
          ifelse(. == "X",
                 "X",
                 "1")
        )
      })() |> 
      paste(collapse = "") |> 
      get_possibilities() |> 
      lapply(function(.x){
        mem[as.character(convert_to_int(.x))] <<-
          convert_to_36_bits(as.numeric(gsub(".* = ","",input[i])))
      })
    
  }
}

solution2 <-
  lapply(mem,convert_to_int) |> 
  unlist() |> 
  sum()

