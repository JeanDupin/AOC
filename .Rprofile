if(!"memoise" %in% utils::installed.packages()[,1]){
  install.packages("memoise")
}
if(!"httr2" %in% utils::installed.packages()[,1]){
  install.packages("httr2")
}
if(!"rvest" %in% utils::installed.packages()[,1]){
  install.packages("rvest")
}

get_input <-
  memoise::memoise(function(url){
    httr2::request(url) |> 
      httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
      httr2::req_user_agent(Sys.getenv("aoc_user_agent")) |> 
      httr2::req_perform() |> 
      httr2::resp_body_string()
  })


send_solution <- function(annee, jour, niveau, solution){
  res <-
    httr2::request(paste0("https://adventofcode.com/",annee,"/day/",jour,"/answer")) |> 
    httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
    httr2::req_user_agent(Sys.getenv("aoc_user_agent")) |> 
    httr2::req_body_form(level = niveau, answer = solution) |> 
    httr2::req_perform()

  httr2::resp_body_html(res) |> 
    rvest::html_elements("article") |> 
    rvest::html_text2()
}



