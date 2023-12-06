# Input ----

input <-
  read.delim("2015/Inputs/Day02.txt",
             sep = "x",
             header = FALSE) |> 
  (\(.){`colnames<-`(.,c("L","W","H"))})()


# Partie 1 ----


paper <-
  function(l,w,h){
    2*(l*w + w*h + h*l) +
      min(l*w,w*h, h*l)
  }

solution1 <-
  mapply(paper,
         input$L,
         input$W,
         input$H) |> 
  sum()

# Partie 2 ----

ribbon <-
  function(l,w,h){
    min(2*(l+w),2*(l+h),2*(h+w)) +
      l*w*h
  }

solution2 <-
  mapply(ribbon,
         input$L,
         input$W,
         input$H) |> 
  sum()
