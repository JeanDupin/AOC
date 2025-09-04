# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/22/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

boss <-
  regmatches(input,
           gregexpr("\\d+",input)) |> 
  unlist() |> 
  as.numeric()

spells <-
  list(
    missile = list(cost = 53, dmg = 4, heal = 0, shield = 0, poison = 0, recharge = 0),
    drain   = list(cost = 73, dmg = 2, heal = 2, shield = 0, poison = 0, recharge = 0),
    shield  = list(cost = 113, dmg = 0, heal = 0, shield = 6, poison = 0, recharge = 0),
    poison  = list(cost = 173, dmg = 0, heal = 0, shield = 0, poison = 6, recharge = 0),
    recharge = list(cost = 229, dmg = 0, heal = 0, shield = 0, poison = 0, recharge = 5)
  )


state1 <-
  list(
    player_hp = 50, player_mana = 500, armor = 0,
    boss_hp = boss[1], boss_dmg = boss[2],
    shield_timer = 0, poison_timer = 0, recharge_timer = 0,
    mana_spent = 0
  )

effets <- function(state){
  if(state$shield_timer > 0){state$armor = 7}else{state$armor = 0}
  if(state$poison_timer > 0){state$boss_hp <- state$boss_hp - 3}
  if(state$recharge_timer > 0){state$player_mana <- state$player_mana + 101}
  
  state$shield_timer = max(0,state$shield_timer - 1)
  state$poison_timer = max(0,state$poison_timer - 1)
  state$recharge_timer = max(0,state$recharge_timer - 1)
  
  state
}


meilleur_mana = Inf
etats <-
  vector("list",10000)
etats[[1]] <-
  state1
hashes <- vector("character",10000)
id.hash <- 1

while(length(etats) >= 1){
  # État avec le moins de coût en mana
  id <-
    sapply(etats,function(.x){
      mana <- .x[["mana_spent"]]
      ifelse(is.null(mana),Inf,mana)
    }) |> 
    which.min()
  etat <- etats[[id]]
  etats[id] <- list(NULL)
  
  # Si plus de mana que notre meilleur score, on enlève
  if(etat$mana_spent >= meilleur_mana){next}
  
  # Début du tour, on applique les effets & si on tue le boss, nouveau meilleur cout ?
  etat <- effets(etat)
  if(etat$boss_hp <= 0){
    meilleur_mana <-
      min(meilleur_mana, etat$mana_spent)
    next
  }
  
  # On teste tous les sorts
  for(sort in spells){
    
    # Si pas assez de mana, sort suivant
    if(etat$player_mana < sort$cost){next}
    # Si effet en cours, suivant
    if(sort$shield > 0 && etat$shield_timer > 0){next}
    if(sort$poison > 0 && etat$poison_timer > 0){next}
    if(sort$recharge > 0 && etat$recharge_timer > 0){next}
    
    # Ensuite on crée le nouvel état - Tour du joueur
    etat2 <- etat
    etat2$player_mana <- etat2$player_mana - sort$cost # Cout du sort
    etat2$mana_spent <- etat2$mana_spent + sort$cost # Mana dépensé
    etat2$boss_hp <- etat2$boss_hp - sort$dmg # Dégâts au boss
    etat2$player_hp <- etat2$player_hp + sort$heal # Soins
    etat2$shield_timer <- etat2$shield_timer + sort$shield # Bouclier
    etat2$poison_timer <- etat2$poison_timer + sort$poison # Poison
    etat2$recharge_timer <- etat2$recharge_timer + sort$recharge # Recharge
    
    # Tour du boss
    etat2 <- effets(etat2)
    if(etat2$boss_hp <= 0){
      meilleur_mana <-
        min(meilleur_mana, etat2$mana_spent)
      next
    }
    etat2$player_hp <- etat2$player_hp - max(1,etat2$boss_dmg - etat2$armor)
    
    # Si toujours vivant, nouvel état possible dans la liste
    if(etat2$player_hp > 0){
      hash <- digest::digest(paste(etat2,collapse = ";"))
      if(hash %in% hashes){next}
      hashes[id.hash] <- hash
      id.hash <- id.hash + 1
      id2 <-
        sapply(etats,is.null) |> 
        (\(.){which(. == 1)[1]})()
      etats[[id2]] <- etat2
    }
    
  }
  
}


solution1 <-
  meilleur_mana

# Partie 2 ----

meilleur_mana = Inf
etats <-
  vector("list",10000)
etats[[1]] <-
  state1
hashes <- vector("character",10000)
id.hash <- 1

while(length(etats) >= 1){
  # État avec le moins de coût en mana
  id <-
    sapply(etats,function(.x){
      mana <- .x[["mana_spent"]]
      ifelse(is.null(mana),Inf,mana)
    }) |> 
    which.min()
  etat <- etats[[id]]
  etats[id] <- list(NULL)
  
  # Si plus de mana que notre meilleur score, on enlève
  if(etat$mana_spent >= meilleur_mana){next}
  
  # Partie 2 : on perd 1PV
  etat$player_hp <- etat$player_hp - 1
  
  if(etat$player_hp <= 0){next}
  
  # Début du tour, on applique les effets & si on tue le boss, nouveau meilleur cout ?
  etat <- effets(etat)
  if(etat$boss_hp <= 0){
    meilleur_mana <-
      min(meilleur_mana, etat$mana_spent)
    next
  }
  
  # On teste tous les sorts
  for(sort in spells){
    
    # Si pas assez de mana, sort suivant
    if(etat$player_mana < sort$cost){next}
    # Si effet en cours, suivant
    if(sort$shield > 0 && etat$shield_timer > 0){next}
    if(sort$poison > 0 && etat$poison_timer > 0){next}
    if(sort$recharge > 0 && etat$recharge_timer > 0){next}
    
    # Ensuite on crée le nouvel état - Tour du joueur
    etat2 <- etat
    etat2$player_mana <- etat2$player_mana - sort$cost # Cout du sort
    etat2$mana_spent <- etat2$mana_spent + sort$cost # Mana dépensé
    etat2$boss_hp <- etat2$boss_hp - sort$dmg # Dégâts au boss
    etat2$player_hp <- etat2$player_hp + sort$heal # Soins
    etat2$shield_timer <- etat2$shield_timer + sort$shield # Bouclier
    etat2$poison_timer <- etat2$poison_timer + sort$poison # Poison
    etat2$recharge_timer <- etat2$recharge_timer + sort$recharge # Recharge
    
    # Tour du boss
    etat2 <- effets(etat2)
    if(etat2$boss_hp <= 0){
      meilleur_mana <-
        min(meilleur_mana, etat2$mana_spent)
      next
    }
    etat2$player_hp <- etat2$player_hp - max(1,etat2$boss_dmg - etat2$armor)
    
    # Si toujours vivant, nouvel état possible dans la liste
    if(etat2$player_hp > 0){
      hash <- digest::digest(paste(etat2,collapse = ";"))
      if(hash %in% hashes){next}
      hashes[id.hash] <- hash
      id.hash <- id.hash + 1
      id2 <-
        sapply(etats,is.null) |> 
        (\(.){which(. == 1)[1]})()
      etats[[id2]] <- etat2
    }
    
  }
  
}


solution2 <-
  meilleur_mana