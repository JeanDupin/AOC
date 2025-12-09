# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/9/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  strsplit(",") |> 
  (\(.){matrix(as.numeric(unlist(.)), ncol = 2, byrow = T)})()

# Partie 1 ----

aire = 0
for(i in seq_len(nrow(input))[-nrow(input)]){
  for(j in seq_len(nrow(input))[-seq_len(i)]){
    aire <-
      max(aire,
          (abs(input[i,1]-input[j,1])+1) * (abs(input[i,2]-input[j,2])+1))
  }
}


solution1 <-
  aire

# Partie 2 ----

poly <-
  rbind(input, input[1,]) |> 
  (\(.){sf::st_polygon(list(.))})() |> 
  sf::st_sfc() |>
  (\(.){sf::st_sf(geometry = .)})()

aire = 0
for(i in seq_len(nrow(input))[-nrow(input)]){
  for(j in seq_len(nrow(input))[-seq_len(i)]){
    p <- input[c(i,j),]
    if(length(unique(p[,1])) == 1 | length(unique(p[,2])) == 1){
      next
    }
    rect_poly <-
      sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(
            p[1,1], p[1,2],
            p[2,1], p[1,2],
            p[2,1], p[2,2],
            p[1,1], p[2,2],
            p[1,1], p[1,2]
          ), ncol = 2, byrow = TRUE)))
        )
      )
    
    if(length(sf::st_within(rect_poly, poly)[[1]]) == 0){
      next
    }
    aire <- max(aire,
                (abs(p[1,1]-p[2,1])+1) * (abs(p[1,2]-p[2,2])+1))
  }
}

solution2 <-
  aire
