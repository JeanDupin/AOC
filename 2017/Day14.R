# Input ----

input <-
  get_input("https://adventofcode.com/2017/day/14/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

reverse2 <- function(.y, pos, len) {
  n <- length(.y)
  if(len <= 1){return(.y)}
  idx <- ((pos:(pos + len - 1)) %% n) + 1
  .y[idx] <- rev(.y[idx])
  .y
}

hash <- function(.x) {
  
  lengths <-
    c(as.integer(charToRaw(.x)), 17, 31, 73, 47, 23)

  n <- 256L
  sequence <- 0:(n - 1)
  position <- 0
  skip <- 0
  
  for(i in seq_len(64)){
    for(j in lengths){
      sequence <- reverse2(sequence, position, j)
      position <- (position + j + skip) %% n
      skip <- skip + 1
    }
  }
  
  
  dense <- vector("numeric",16)
  for(i in seq(0,15)){
    block <- sequence[(i * 16 + 1):(i * 16 + 16)]
    x <- block[1]
    if(length(block) > 1){
      for(j in block[-1]){
        x <- bitwXor(x, j)
        }
      }
    dense[i + 1] <- x
  }
  
  paste(sprintf("%02x", dense), collapse = "")
}


hex_to_bin <- function(.z){
  strtoi(.z, base = 16) |> 
    intToBits() |> 
    (\(.){rev(as.integer(.)[1:4])})() |> 
    paste(collapse = "")
}

res = 0

for(i in seq(0,127)){
  res <-
    hash(paste0(input,"-",i)) |> 
    (\(.){sapply(strsplit(.,"")[[1]],
                 function(.x){strsplit(hex_to_bin(.x),"")[[1]]},
                 USE.NAMES = F,
                 simplify = F)})() |> 
    unlist() |> 
    (\(.){sum(. == "1") + res})()
}



solution1 <-
  res
  
# Partie 2 ----

res = vector("character")

for(i in seq(0,127)){
  res <-
    hash(paste0(input,"-",i)) |> 
    (\(.){sapply(strsplit(.,"")[[1]],
                 function(.x){strsplit(hex_to_bin(.x),"")[[1]]},
                 USE.NAMES = F,
                 simplify = F)})() |> 
    (\(.){append(res,unlist(.))})()
}

matrice <-
  matrix(res, nrow = 128,
         byrow = T)

points <-
  matrice|> 
  (\(.){which(. == "1",arr.ind = T)})() |> 
  data.frame() |> 
  sf::st_as_sf(coords = c("col","row"))

solution2 <-
  points |> 
  sf::st_buffer(.52) |> 
  sf::st_union() |> 
  sf::st_cast("POLYGON") |> 
  length()
  
