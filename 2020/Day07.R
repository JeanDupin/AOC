# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

df <-
  lapply(
    input,
    function(.x){
      main <-
        strsplit(.x," contain")[[1]][1] |> 
        (\(.){gsub(" bag(s|)","",.)})()
      
      if(grepl("no other",strsplit(.x," contain")[[1]][2])){
        sec <- NA
      } else if(!grepl(",",strsplit(.x," contain")[[1]][2])){
        sec <- 
          strsplit(.x," contain")[[1]][2] |> 
          (\(.){gsub(" [0-9]+ ","",.)})() |> 
          (\(.){gsub(" bag(s|)\\.$","",.)})()
      } else {
        sec <-
          strsplit(.x," contain")[[1]][2] |> 
          (\(.){gsub(" [0-9]+ ","",.)})() |> 
          (\(.){gsub(" bag(s|)\\.$","",.)})() |> 
          (\(.){strsplit(.,",")[[1]]})() |> 
          unlist() |> 
          paste(collapse = "-")
      }
      return(
        data.frame(main,sec)
      )
    }
  ) |> 
  (\(.){Reduce(rbind,.)})()


checked <- vector("character")
tocheck <- df[grepl("shiny gold",df$sec),"main"]

while(length(tocheck) != 0){
  etape = tocheck[1]
  if(etape %in% checked){
    tocheck <- tocheck[-1]
  } else {
    checked = unique(c(checked,etape))
    tocheck <- tocheck[-1]
    tocheck <- unique(c(tocheck,df[grepl(etape,df$sec),"main"]))
  }

}

solution1 <-
  length(checked)

# Partie 2 ----

wanted <- 'shiny gold'

tailles = vector("numeric")


while(length(wanted) != 0){
  
  tailles <-
    append(tailles, length(wanted))
  
  
  wanted <-
    lapply(
      wanted,
      function(.x){
        rep <-
          input[grepl(paste0("^",.x),input)]

        if(grepl("no other",rep)){
          rep <- ""
        } else {
          rep <-
            gsub("^.* contain ","",rep) |> 
            (\(.){gsub(" bag(s|)(\\.|)","",.)})() |> 
            (\(.){strsplit(.,", ")[[1]]})() |> 
            lapply(function(.y){
              rep(gsub("^[0-9]+ ","",.y),
                  as.numeric(regmatches(.y,
                                        gregexpr("[0-9]+ ",.y))))
            }) |> 
            unlist()
        }
        
      }
    ) |> 
    unlist() |> 
    (\(.){.[. != ""]})()

  
}

solution2 <-
  sum(tailles)-1
