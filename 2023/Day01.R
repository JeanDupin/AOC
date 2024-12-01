# Input ----

input <-
  httr2::request("https://adventofcode.com/2023/day/1/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()


# Part 1 ----
solution_1 <-
  gsub("\\D","",
     input$V1) |> 
  (\(.){
    paste0(
      substr(.,1,1),
      substr(.,nchar(.),nchar(.))
    )
  })() |> 
  as.numeric() |> 
  sum(); solution_1


# Part 2 ----
mots = c(paste0(1:9),
         "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")


premier <-
  sapply(mots,
       FUN = function(.x){
         gregexpr(.x,input$V1) |> 
           sapply(FUN = function(.y){ifelse(min(.y) == -1,
                                            NA,
                                            min(.y))})
       }) |> 
  apply(MARGIN = 1,
        FUN = function(.x){
          c(1:9,1:9)[which.min(.x)]
        })

dernier <-
  sapply(mots,
         FUN = function(.x){
           gregexpr(.x,input$V1) |> 
             sapply(FUN = function(.y){ifelse(min(.y) == -1,
                                              NA,
                                              max(.y))})
         }) |> 
  apply(MARGIN = 1,
        FUN = function(.x){
          c(1:9,1:9)[which.max(.x)]
        })

solution_2 <-
  paste0(premier, dernier) |> 
  as.numeric() |> 
  sum(); solution_2

