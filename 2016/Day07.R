# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

# Partie 1 ----


lettres <-
  expand.grid(letters, letters) |> 
  (\(.){paste(.$Var1,.$Var2,sep = "")})() |> 
  (\(.){.[substr(.,1,1) != substr(.,2,2)]})() |> 
  sort() |> 
  (\(.){
    paste0(
      .,
      paste0(
        substr(.,2,2),
        substr(.,1,1)
      )
    )
  })()


solution1 <-
  sapply(input, USE.NAMES = F,
         function(.x) {
           
           if (sapply(lettres, USE.NAMES = F,
                      function(.y) {
                        grepl(.y, regmatches(.x,
                                             gregexpr("\\[[a-z]*\\]", .x))[[1]])
                      }) |>
               any()) {
             return(FALSE)
           } else {
             return(sapply(lettres, USE.NAMES = F,
                           function(.y) {
                             grepl(.y, strsplit(.x,
                                                "\\[[a-z]*\\]")[[1]])
                           }) |>
                      any())
           }
           
         }) |> 
  sum()

  
# Partie 2 ----



lettres <-
  expand.grid(letters, letters) |> 
  (\(.){paste(.$Var1,.$Var2,sep = "")})() |> 
  (\(.){.[substr(.,1,1) != substr(.,2,2)]})() |> 
  sort() |> 
  (\(.){
    paste0(
      .,
      substr(.,1,1)
    )
  })()


solution2 <-
sapply(input, USE.NAMES = F,
       function(.x) {
         
         ABA <-
           lettres[sapply(lettres, USE.NAMES = F,
                          function(.y) {
                            any(grepl(.y, strsplit(.x,
                                                   "\\[[a-z]*\\]")[[1]]))
                          })]
         
        
         if(length(ABA) == 0){
           return(FALSE)
         }
         
         BAB <-
           paste0(
             substr(ABA,2,2),
             substr(ABA,1,1),
             substr(ABA,2,2)
           )
         
         
         if(sapply(BAB, USE.NAMES = F,
                function(.y) {
                  grepl(.y, regmatches(.x,
                                       gregexpr("\\[[a-z]*\\]", .x))[[1]])
                }) |> 
           any()){
           return(TRUE)
         } else {
           return(FALSE)
         }
         
       }) |> 
  sum()
