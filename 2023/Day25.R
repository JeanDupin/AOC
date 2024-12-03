# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/25/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

edges <-
  lapply(
    input,
    function(.x){
      regmatches(.x,
                 gregexpr("\\w+",.x))[[1]] |> 
        (\(.){
          c(
            paste(.[1],
                  .[2:length(.)],
                  sep = "-")
          )
        })()
    }
  ) |> 
  unlist()

edges_split <-
  do.call(rbind, strsplit(edges, "-"))

graph <-
  igraph::graph_from_edgelist(edges_split, directed = FALSE)


points <-
  attr(igraph::V(graph),"names")

groupes <-
  igraph::cut_at(igraph::cluster_walktrap(graph,steps = 6),2)

groupes <-
  list(
    groupe1 = points[groupes == 1],
    groupe2 = points[groupes == 2]
  )

chemins <-
  expand.grid(
  groupes$groupe1,
  groupes$groupe2
) |> 
  rbind(
    expand.grid(
      groupes$groupe2,
      groupes$groupe1
    )
  ) |> 
  apply(1,function(x){paste(x, collapse = "-")})



edges_split <- do.call(rbind, strsplit(edges[!edges %in% chemins[chemins %in% edges]], "-"))

graph <- igraph::graph_from_edgelist(edges_split, directed = FALSE)

components <- igraph::components(graph)

groups <- split(igraph::V(graph)$name, components$membership)

solution1 <-
  length(groups[[1]]) * length(groups[[2]])



# Partie 2 ----

solution2 <-
  NA