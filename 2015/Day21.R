# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/21/input") |>
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

weapons <-
  data.frame(
    cost = c(8,10,25,40,74),
    att = c(4,5,6,7,8),
    def = 0
  )

armors <-
  data.frame(
    cost = c(13,31,53,75,102),
    att = 0,
    def = c(1,2,3,4,5)
  )

rings <-
  data.frame(
    cost = c(25,50,100,20,40,80),
    att = c(1,2,3,0,0,0),
    def = c(0,0,0,1,2,3)
  )



equipements <-
  cbind(
    expand.grid(
      weapons$cost,
      c(0,armors$cost),
      c(0,rings$cost),
      c(0,rings$cost)
    ),
    expand.grid(
      weapons$att,
      c(0,armors$att),
      c(0,rings$att),
      c(0,rings$att)
    ),
    expand.grid(
      weapons$def,
      c(0,armors$def),
      c(0,rings$def),
      c(0,rings$def)
    )
  ) |> 
  setNames(c(paste0("Cost",1:4),paste0("Att",1:4),paste0("Def",1:4))) |> 
  (\(.){.[!(.$Cost3 == .$Cost4 & .$Cost3 != 0),]})() 


res <- vector("numeric",nrow(equipements))
for(i in seq_len(nrow(equipements))){
  boss_health = as.numeric(gsub(".* ","",input[1]))
  my_health = 100
  boss_att = as.numeric(gsub(".* ","",input[2]))
  my_att = rowSums(equipements[i,paste0("Att",1:4)])[[1]]
  boss_def = as.numeric(gsub(".* ","",input[3]))
  my_def = rowSums(equipements[i,paste0("Def",1:4)])[[1]]
  cout = rowSums(equipements[i,paste0("Cost",1:4)])[[1]]
  
  
  while(boss_health > 0 & my_health > 0){
    boss_health = boss_health - max(my_att-boss_def,1)
    my_health = my_health - max(boss_att - my_def,1)
  }
  
  if(boss_health <= 0){
    res[i] = cout
  } else {
    res[i] = Inf
  }
}

solution1 <-
  min(res)

# Partie 2 ----
  
for(i in seq_len(nrow(equipements))){
  boss_health = as.numeric(gsub(".* ","",input[1]))
  my_health = 100
  boss_att = as.numeric(gsub(".* ","",input[2]))
  my_att = rowSums(equipements[i,paste0("Att",1:4)])[[1]]
  boss_def = as.numeric(gsub(".* ","",input[3]))
  my_def = rowSums(equipements[i,paste0("Def",1:4)])[[1]]
  cout = rowSums(equipements[i,paste0("Cost",1:4)])[[1]]
  
  
  while(boss_health > 0 & my_health > 0){
    boss_health = boss_health - max(my_att-boss_def,1)
    my_health = my_health - max(boss_att - my_def,1)
  }
  
  if(boss_health <= 0){
    res[i] = -Inf
  } else {
    res[i] = cout
  }
}


solution2 <-
  max(res)
