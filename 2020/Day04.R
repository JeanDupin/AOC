# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/4/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

fields <- c("byr","iyr","eyr","hgt","hcl","ecl","pid")

solution1 <-
  lapply(
    2:(length(which(input == ""))+2),
    function(.x){
      indices.min <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[1]
      indices.max <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[2]
      
      indices<-
        Map(":", indices.min, indices.max)[[1]]
      
      paste(input[indices], collapse = " ") |>
        (\(.){
          lapply(paste0(fields,":"),
                 function(.y){
                   grepl(.y,.)
                 }) |>
            unlist() |>
            all()
        })()
    }
  ) |> 
  unlist() |> 
  sum()

# Partie 2 ----

input <-
  lapply(
    2:(length(which(input == ""))+2),
    function(.x){
      indices.min <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[1]
      indices.max <-
        (c(0,which(input == ""),length(input)+1)[c(.x-1,.x)] + c(1,-1))[2]
      
      indices<-
        Map(":", indices.min, indices.max)[[1]]
      
      paste(input[indices], collapse = " ")}) |> 
  unlist()

solution2 <-
  lapply(
  input,
  function(.){
    indic <-
      lapply(paste0(fields,":"),
           function(.y){
             grepl(.y,.)
           }) |> 
      unlist() |> 
      all()
    
    if(!indic){
      return(F)
    }
   
    lapply(
      paste0(fields,":"),
      function(.y){
        
        if(.y == "byr:"){
          regmatches(.,
                     gregexpr("byr:[0-9]+",.))[[1]] |> 
            (\(.z){as.numeric(gsub("byr:","",.z))})() |> 
            (\(.z){ifelse(.z >= 1920 & .z <= 2002,T,F)})()
        } else if(.y == "iyr:"){
          regmatches(.,
                     gregexpr("iyr:[0-9]+",.))[[1]] |> 
            (\(.z){as.numeric(gsub("iyr:","",.z))})() |> 
            (\(.z){ifelse(.z >= 2010 & .z <= 2020,T,F)})()
        } else if(.y == "eyr:"){
          regmatches(.,
                     gregexpr("eyr:[0-9]+",.))[[1]] |> 
            (\(.z){as.numeric(gsub("eyr:","",.z))})() |> 
            (\(.z){ifelse(.z >= 2020 & .z <= 2030,T,F)})()
        } else if(.y == "hcl:"){
          regmatches(.,
                     gregexpr("hcl:#[a-f0-9]{6}( |$)",.))[[1]] |> 
            (\(.z){ifelse(length(.z) != 0,T,F)})()
        } else if(.y == "ecl:"){
          regmatches(.,
                     gregexpr("ecl:[a-z]+",.))[[1]] |> 
            (\(.z){gsub("ecl:","",.z)})() |> 
            (\(.z){ifelse(.z %in% c("amb","blu","brn","gry","grn","hzl","oth"),T,F)})()
        } else if(.y == "pid:"){
          regmatches(.,
                     gregexpr("pid:[0-9]{9}( |$)",.))[[1]] |> 
            (\(.z){ifelse(length(.z) != 0,T,F)})()
        } else if(.y == "hgt:"){
          if(grepl("hgt:[0-9]+cm",.)){
            regmatches(.,
                       gregexpr("hgt:[0-9]+cm",.))[[1]] |> 
              (\(.z){gsub("hgt:","",.z)})() |> 
              (\(.z){as.numeric(gsub("cm","",.z))})() |> 
              (\(.z){ifelse(.z >= 150 & .z <= 193,T,F)})()
          } else if(grepl("hgt:[0-9]+in",.)){
            regmatches(.,
                       gregexpr("hgt:[0-9]+in",.))[[1]] |> 
              (\(.z){gsub("hgt:","",.z)})() |> 
              (\(.z){as.numeric(gsub("in","",.z))})() |> 
              (\(.z){ifelse(.z >= 59 & .z <= 76,T,F)})()
          } else {
            F
          }
        }
        
      }
    ) |> 
      unlist() |> 
      all()
    
    
  }
) |> 
  unlist() |> 
  (\(.){sum(.)-1})()
