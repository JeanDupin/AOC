# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})() 

# Partie 1 ----

input <-
  input[regmatches(input,
                   gregexpr("(?<=\\[)[^]]+(?=\\])",input, perl = T)) |> 
          lapply(function(.x){as.POSIXct(.x,"%Y-%m-%d %H:%M", tz = "CET")}) |> 
          unlist() |> 
          order()]

periode <-
  input[c(1,length(input))] |> 
  (\(.){
    regmatches(.,
               gregexpr("1518-[0-9][0-9]-[0-9][0-9]",.))
  })() |> 
  (\(.){seq(as.Date(.[[1]]),as.Date(.[[2]]),by = "day")})()

gardes <-
  lapply(
  input,
  function(.x){
    regmatches(.x,
               gregexpr("Guard #[0-9]*",.x))[[1]]
  }
) |>
  unlist()

replace_na <-
  function(vec) {
    last_valid <- NA  
    
    for (i in seq_along(vec)) {
      if (!is.na(vec[i])) {
        last_valid <- vec[i]
      } else {
        vec[i] <- last_valid
      }
    }
    
    return(vec)
  }

records <-
  lapply(
    seq_along(periode),
    function(.x){
      
      instructions <-
        input[grepl(paste0("\\[",periode[.x]),input) & !grepl("Guard",input)]
      
      temps <- c(0,rep(NA,59))
      names(temps) <- c(paste0("0",0:9),10:59)
      
      if(length(instructions) != 0){
        for(i in 1:length(instructions)){
          if(grepl("falls",instructions[i])){temps[[sub(".*([0-9]{2})[^0-9]*$", "\\1", instructions[i])]] <- 1}
          if(grepl("wakes",instructions[i])){temps[[sub(".*([0-9]{2})[^0-9]*$", "\\1", instructions[i])]] <- 0}
        }
      }
      temps <- replace_na(temps)
      tot = sum(temps)
      names(temps) <- paste0("M",names(temps))
      temps |> 
        as.list() |> 
        as.data.frame() |> 
        (\(.){
          cbind(data.frame(jour = periode[.x], garde = gardes[.x]),.,data.frame(total = tot))
        })()
    }
  ) |> 
  (\(.){Reduce(`rbind`,.)})()


solution1 <-
  records |> 
  (\(.){aggregate(total ~ garde, data = ., sum)})() |> 
  (\(.){.$garde[which.max(.$total)]})() |> 
  (\(.){records[records$garde == .,grepl("M",colnames(records))]})() |> 
  colSums() |> 
  (\(.){which(. == max(.))[[1]]-1})() |> 
  (\(.){
    . * records |> 
      (\(.x){aggregate(total ~ garde, data = .x, sum)})() |> 
      (\(.x){.x$garde[which.max(.x$total)]})() |> 
      (\(.x){as.numeric(gsub("Guard #","",.x))})()
  })()

# Partie 2 ----

solution2 <-
  aggregate(. ~ garde,records[,!names(records) %in% c("jour","total")],sum) |> 
  (\(.){which(. == max(.[,2:ncol(.)]), arr.ind = T)})() |> 
  (\(.x){
    aggregate(. ~ garde,records[,!names(records) %in% c("jour","total")],sum)[.x[[1,1]],]
  })() |> 
  (\(.){
    as.numeric(gsub("Guard #","",.[1,"garde"])) * 
      (which(.[1,2:ncol(.)] == max(.[1,2:ncol(.)]))-1)
  })()