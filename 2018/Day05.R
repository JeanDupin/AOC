# Input ----

input <-
  httr2::request("https://adventofcode.com/2018/day/5/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

reactions <-
  c(paste0(letters,LETTERS),
    paste0(LETTERS,letters)) |> 
  paste(collapse = "|")

sortie <- input
while(grepl(reactions, sortie)){
  sortie <- gsub(reactions,"",sortie)
}

solution1 <-
  nchar(sortie)

# Partie 2 ----


solution2 <-
  lapply(
    letters,
    function(.x){
      reactions <-
        c(paste0(letters,LETTERS),
          paste0(LETTERS,letters)) |> 
        paste(collapse = "|")
      
      sortie <- gsub(paste(.x,toupper(.x),sep = "|"),"",input)
      while(grepl(reactions, sortie)){
        sortie <- gsub(reactions,"",sortie)
      }
      nchar(sortie)
    }
  ) |> 
  unlist() |> 
  min()
