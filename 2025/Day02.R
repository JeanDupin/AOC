# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/2/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  (\(.){strsplit(.,",")[[1]]})()

# Partie 1 ----

index <-
  lapply(
    input,
    function(.x){
      strsplit(.x,"-")[[1]] |> 
        (\(.){
          if(nchar(.[1]) %% 2 == 1 & nchar(.[2]) %% 2 == 1){
            NULL
          } else {
            as.numeric(.[1]):as.numeric(.[2])
          }
        })()
    }
  ) |> 
  unlist()


solution1 <-
  index |> 
  (\(.){
    .[substr(.,1,nchar(.)/2) == substr(.,(nchar(.)/2)+1,nchar(.))]
  })() |> 
  sum()

# Partie 2 ----
  
index <-
  lapply(
    input,
    function(.x){
      strsplit(.x,"-")[[1]] |> 
        (\(.){
            as.numeric(.[1]):as.numeric(.[2])
        })()
    }
  ) |> 
  unlist()

index2 <-
  index |> 
  (\(.){
    substr(paste0(index,index),2,2*nchar(index)-1)
  })()

solution2 <-
  sapply(
    seq_along(index),
    function(.x){
      if(grepl(index[.x],index2[.x])){
        as.numeric(index[.x])
      } else {
        0
      }
    },
    USE.NAMES = F,
    simplify = T
  ) |> 
  sum()
