# Input ----

input <-
  readLines("2023/Inputs/Day07.txt")


# Partie 1 ----


mains <-
  substr(input,1,5)
bids <-
  as.numeric(gsub("^.* ","",input))

# Groupe de force :
# 7 : Cinq fois la même -> length = 1
# 6 : 4 fois la même carte -> length = 2 & max(table) = 4
# 5 : 3 fois la même carte & 2 fois la même autre -> length = 2 & max(table) = 3
# 4 : 3 fois la même carte -> length(table) = 3 & max(table) = 3
# 3 : 2 fois la même carte x2 -> length(table) = 3 & max(table) = 2
# 2 : 2 fois la même carte  -> length(table) = 4
# 1 : Plus haute carte  -> length(table) = 5
# Et on inverse pour le tri ensuite

forces <-
  sapply(mains, USE.NAMES = F,
       function(.x){
         lgt <-
           strsplit(.x,"") |> 
           table() |> 
           length()
         vmax <-
           strsplit(.x,"") |> 
           table() |> 
           max()
         
         if(lgt == 1){
           force = 7
         } else if(lgt == 2 & vmax == 4){
           force = 6
         } else if(lgt == 2 & vmax == 3){
           force = 5
         } else if(lgt == 3 & vmax == 3){
           force = 4
         } else if(lgt == 3 & vmax == 2){
           force = 3
         } else if(lgt == 4){
           force = 2
         } else {
           force = 1
         }
         
         return(c(7:1)[force])
       }) 
mains.tr <-
  sapply(mains, USE.NAMES = F,
         function(.x){
           return(chartr("AKQJT98765432",
                             paste(LETTERS[1:13],collapse = ""),
                             .x))
         }) 


solution1 <-
  cbind(mains, bids,
        forces, mains.tr) |> 
    data.frame() |>
    (\(.){
      .[order(.$forces,.$mains.tr),] |> 
        transform(value = (nrow(.):1))
    })() |> 
    (\(.){
      sum(as.numeric(.[,"bids"]) * .[,"value"])
    })()
  

# Partie 2 ----



forces <-
  sapply(mains, USE.NAMES = F,
         function(.x){
           # Cas spécial : main JJJJJ
           
           if(.x == "JJJJJ"){
             return(1)
           }
           
           if(!grepl("J",.x)){
             # Appliquer force standard
             lgt <-
               strsplit(.x,"") |> 
               table() |> 
               length()
             vmax <-
               strsplit(.x,"") |> 
               table() |> 
               max()
          
             if(lgt == 1){
               force = 7
             } else if(lgt == 2 & vmax == 4){
               force = 6
             } else if(lgt == 2 & vmax == 3){
               force = 5
             } else if(lgt == 3 & vmax == 3){
               force = 4
             } else if(lgt == 3 & vmax == 2){
               force = 3
             } else if(lgt == 4){
               force = 2
             } else {
               force = 1
             }
             
             return(c(7:1)[force])
           } else {
             # Transformer le J en la lettre la plus présente sauf elle
             lettre <-
               gsub("J","",.x) |> 
                 strsplit("") |> 
                 table() |> 
                 sort(decreasing = T) |> 
                 (\(.){names(.[1])})()
             
             lgt <-
               strsplit(gsub("J",lettre,.x),"") |> 
               table() |> 
               length()
             vmax <-
               strsplit(gsub("J",lettre,.x),"") |> 
               table() |> 
               max()
             
             if(lgt == 1){
               force = 7
             } else if(lgt == 2 & vmax == 4){
               force = 6
             } else if(lgt == 2 & vmax == 3){
               force = 5
             } else if(lgt == 3 & vmax == 3){
               force = 4
             } else if(lgt == 3 & vmax == 2){
               force = 3
             } else if(lgt == 4){
               force = 2
             } else {
               force = 1
             }
             
             return(c(7:1)[force])
             
             
           }
         })
mains.tr <-
  sapply(mains, USE.NAMES = F,
         function(.x){
           return(chartr("AKQT98765432J",
                         paste(LETTERS[1:13],collapse = ""),
                         .x))
         }) 

solution2 <-
  cbind(mains, bids,
        forces, mains.tr) |> 
  data.frame() |>
  (\(.){
    .[order(.$forces,.$mains.tr),] |> 
      transform(value = (nrow(.):1))
  })() |> 
  (\(.){
    sum(as.numeric(.[,"bids"]) * .[,"value"])
  })()


