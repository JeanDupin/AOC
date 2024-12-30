# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/25/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

indices = seq(1, length(input), 8)

matrices = vector("list")
for(i in indices){
  matrices[[which(indices == i)]] <-
    input[i:(i+6)] |>
    strsplit("") |> 
    unlist() |>
    matrix(nrow = 7, byrow = T)
}

keys = vector("character")
locks = vector("character")

for(i in seq_along(matrices)){
  matrice = matrices[[i]]
  vecteur = vector("numeric",ncol(matrice))
  for(j in seq_len(ncol(matrice))){
    vecteur[j] = sum(matrice[,j] == "#")
                     }
  vecteur = paste(vecteur - 1, collapse = ",")
  if(matrice[1,1] =="."){
    keys[i] = vecteur
  } else {
    locks[i] = vecteur
  }
  }

keys = keys[!is.na(keys)]
locks = locks[!is.na(locks)]

res = vector("logical")
for(i in locks){
  for(j in keys){
    test = as.numeric(strsplit(i,",")[[1]]) + as.numeric(strsplit(j,",")[[1]])
    res = append(res, all(test <= 5))
  }
}

solution1 = sum(res)


# Partie 2 ----

solution2 <-
  NA