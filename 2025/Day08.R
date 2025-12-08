# Input ----

input <-
  get_input("https://adventofcode.com/2025/day/8/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})() |> 
  strsplit(",") |> 
  (\(.){matrix(as.numeric(unlist(.)), ncol = 3, byrow = T)})()

# Partie 1 ----

d <- as.matrix(dist(input))
diag(d) <- Inf
pairs <- which(upper.tri(d), arr.ind = TRUE)
distances <- d[pairs]
ordered_pairs <- pairs[order(distances), ]
edges <- ordered_pairs[1:1000, ]
g <- igraph::graph_from_edgelist(edges, directed = FALSE)
comp <- igraph::components(g)

solution1 <-
  prod(sort(comp$csize, decreasing = T)[1:3])

# Partie 2 ----

i = 1001
while(length(unique(comp$csize)) > 1){
  edges <- ordered_pairs[1:i, ]
  g <- igraph::graph_from_edgelist(edges, directed = FALSE)
  comp <- igraph::components(g)
  i = i + 1
}
  
solution2 <-
  input[ordered_pairs[i-1, ][1], ][1] * input[ordered_pairs[i-1, ][2], ][1]
