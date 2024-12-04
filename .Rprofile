if(!"memoise" %in% utils::installed.packages()[,1]){
  install.packages("memoise")
}
if(!"httr2" %in% utils::installed.packages()[,1]){
  install.packages("httr2")
}

get_input <-
  memoise::memoise(function(url){
    httr2::request(url) |> 
      httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
      httr2::req_headers("User-Agent" = "github.com/JeanDupin/AOC") |> 
      httr2::req_perform() |> 
      httr2::resp_body_string()
  })
