# Input ----

input <-
  get_input("https://adventofcode.com/2018/day/7/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

input <-
  gsub("^S","s",input) |> 
  (\(.){
    regmatches(.,
               gregexpr("[A-Z]",.))
  })() |> 
  lapply(function(.x){data.frame(required = .x[1], step = .x[2])}) |> 
  (\(.){Reduce(rbind,.)})()

steps <-
  unique(input$required[!input$required %in% input$step]) |> 
  sort()

done <-
  vector("character")

while(length(steps) >= 1){
  
  etape <- steps[1]
  
  if(!etape %in% input$step){
    steps <-
      setdiff(steps, etape)
    steps <-
      sort(unique(c(steps, input[input$required == etape,"step"])))
    done <-
      unique(c(done, etape))
    next
  }
  
  required <- input[input$step == etape,"required"]
  if(!all(required %in% done)){
    steps <-
      c(setdiff(steps,etape), etape)
  } else {
    steps <-
      setdiff(steps, etape)
    steps <-
      sort(unique(c(steps, input[input$required == etape,"step"])))
    done <-
      unique(c(done, etape))
  }
  
  next
}; rm(etape, required,steps)

solution1 <-
  paste(done, collapse = "")

# Partie 2 ----

w1 = w2 = w3 = w4 = w5 = ""
w1t = w2t = w3t = w4t = w5t = 0

steps <-
  unique(input$required[!input$required %in% input$step]) |> 
  sort()

done <-
  vector("character")

time = 0
while(length(done) != length(unique(c(input$required,input$step)))){
  
  # Avancée des runners
  w1t = max(w1t - 1,0)
  w2t = max(w2t - 1,0)
  w3t = max(w3t - 1,0)
  w4t = max(w4t - 1,0)
  w5t = max(w5t - 1,0)
  
  # Si un runner a fini, on ajoute l'étape chez les done et on le libère
  if(w1t == 0 & w1 != ""){
    done <-
      c(done,w1)
    steps <-
      sort(unique(c(steps, input[input$required == w1,"step"])))
    w1 = ""
  }
  if(w2t == 0 & w2 != ""){
    done <-
      c(done,w2)
    steps <-
      sort(unique(c(steps, input[input$required == w2,"step"])))
    w2 = ""
  }
  if(w3t == 0 & w3 != ""){
    steps <-
      sort(unique(c(steps, input[input$required == w3,"step"])))
    done <-
      c(done,w3)
    w3 = ""
  }
  if(w4t == 0 & w4 != ""){
    steps <-
      sort(unique(c(steps, input[input$required == w4,"step"])))
    done <-
      c(done,w4)
    w4 = ""
  }
  if(w5t == 0 & w5 != ""){
    steps <-
      sort(unique(c(steps, input[input$required == w5,"step"])))
    done <-
      c(done,w5)
    w5 = ""
  }
  
  # Si tous les runners tournent, on incrémente le temps et on continue
  if(w1t != 0 & w2t != 0 & w3t != 0 & w4t != 0 & w5t != 0){
    time = time + 1
    next
  }
  
  # Si aucune étape en attente, go next
  if(length(steps) == 0){
    time = time + 1
    next
  }
  
  # On teste si une étape peut être réalisée
  bouclesteps <- steps
  for(i in 1:length(bouclesteps)){
    etape <- bouclesteps[i]
    required <- input[input$step == etape,"required"]
    if(!all(required %in% done)){
      
    } else {
      if(w1 == ""){
        steps <-
          setdiff(steps, etape)
        w1 = etape
        w1t = 60 + which(LETTERS == etape)
      } else if(w2 == ""){
        steps <-
          setdiff(steps, etape)
        w2 = etape
        w2t = 60 + which(LETTERS == etape)
      } else if(w3 == ""){
        steps <-
          setdiff(steps, etape)
        w3 = etape
        w3t = 60 + which(LETTERS == etape)
      } else if(w4 == ""){
        steps <-
          setdiff(steps, etape)
        w4 = etape
        w4t = 60 + which(LETTERS == etape)
      } else if(w5 == ""){
        steps <-
          setdiff(steps, etape)
        w5 = etape
        w5t = 60 + which(LETTERS == etape)
      }
      
    }
  }
  
  # print(list(w1,w2,w3,w4,w5))
  # if(time == 0){break}
  time = time + 1
  next
}


solution2 <-
  paste(done, collapse = "")
