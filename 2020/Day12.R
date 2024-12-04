# Input ----

input <-
  get_input("https://adventofcode.com/2020/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

position = c(0,0)
direction = "E"

for(i in seq_along(input)){
  
  lettre = substr(input[i],1,1)
  value = as.numeric(gsub("[A-Z]","",input[i]))
  
  if(lettre == "F"){lettre <- direction}
  
  if(lettre == "N"){
    position = position + c(0,value)
  } else if(lettre == "S"){
    position = position - c(0,value)
  } else if(lettre == "E"){
    position = position + c(value,0)
  } else if(lettre == "W"){
    position = position - c(value,0)
  }
  
  if(lettre == "R"){
    if(value == 180){
      direction <-
        switch(
          direction,
          "N" = "S",
          "S" = "N",
          "E" = "W",
          "W" = "E"
        )
    } else if(value == 90){
      direction <-
        switch(
          direction,
          "N" = "E",
          "E" = "S",
          "S" = "W",
          "W" = "N"
        )
    } else if(value == 270){
      direction <-
        switch(
          direction,
          "N" = "W",
          "W" = "S",
          "S" = "E",
          "E" = "N"
        )
    }
  } else if(lettre == "L"){
    if(value == 180){
      direction <-
        switch(
          direction,
          "N" = "S",
          "S" = "N",
          "E" = "W",
          "W" = "E"
        )
    } else if(value == 90){
      direction <-
        switch(
          direction,
          "N" = "W",
          "W" = "S",
          "S" = "E",
          "E" = "N"
        )
    } else if(value == 270){
      direction <-
        switch(
          direction,
          "N" = "E",
          "E" = "S",
          "S" = "W",
          "W" = "N"
        )
    }
  }
  
}

solution1 <-
  sum(abs(position))

# Partie 2 ----

rotate_point <-
  function(p1, p2, degrees, direction = "clockwise") {
  radians <- switch(
    as.character(degrees),
    "90" = pi / 2,
    "180" = pi,
    "270" = 3 * pi / 2
  )
  
  if (direction == "clockwise") {
    radians <- -radians
  }
  
  x1 <- p1[1]
  y1 <- p1[2]
  x2 <- p2[1]
  y2 <- p2[2]
  
  x <- x1 - x2
  y <- y1 - y2
  
  x_rot <- x * cos(radians) - y * sin(radians)
  y_rot <- x * sin(radians) + y * cos(radians)
  
  x_new <- x_rot + x2
  y_new <- y_rot + y2
  
  c(x_new, y_new)
}


waypoint.position = c(10,1)
position = c(0,0)

for(i in seq_along(input)){
  
  lettre = substr(input[i],1,1)
  value = as.numeric(gsub("[A-Z]","",input[i]))
  
  if(lettre == "F"){
    position = position + value*waypoint.position
  }
  
  if(lettre == "N"){
    waypoint.position = waypoint.position + c(0,value)
  } else if(lettre == "S"){
    waypoint.position = waypoint.position - c(0,value)
  } else if(lettre == "E"){
    waypoint.position = waypoint.position + c(value,0)
  } else if(lettre == "W"){
    waypoint.position = waypoint.position - c(value,0)
  }
  
  if(lettre == "R"){
    waypoint.position = rotate_point(position+waypoint.position,position,value, "clockwise")-position
  } else if(lettre == "L"){
    waypoint.position = rotate_point(position+waypoint.position,position,value, "counterclockwise")-position
  }
  
}

solution2 <-
  sum(abs(position))
