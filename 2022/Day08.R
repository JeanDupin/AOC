# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/8/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

matrice <-
  matrix(as.numeric(unlist(strsplit(input,""))), nrow = length(input), byrow = T)
res = vector()

for(i in seq_len(nrow(matrice))[-c(1,nrow(matrice))]){
  for(j in seq_len(ncol(matrice))[-c(1,ncol(matrice))]){

      height = matrice[i,j]
      trees = c(
        any(matrice[c(1:(i-1)),j] >= height),
        any(matrice[c((i+1):nrow(matrice)),j] >= height),
        any(matrice[i,c(1:(j-1))] >= height),
        any(matrice[i,c((j+1):ncol(matrice))] >= height))
      res = append(res,!all(trees))

  }
}

solution1 <-
  sum(res) + 2 * (ncol(matrice) + nrow(matrice)) - 4

# Partie 2 ----

res = vector()
for(i in seq_len(nrow(matrice))[-c(1,nrow(matrice))]){
  for(j in seq_len(ncol(matrice))[-c(1,ncol(matrice))]){

      height = paste0("(?<=[",matrice[i,j],"-9]).*$")
      trees = c(
        nchar(gsub(paste0(height,".*$"),"",paste(matrice[c((i-1):1),j],collapse = ""), perl = T)),
        nchar(gsub(paste0(height,".*$"),"",paste(matrice[c((i+1):nrow(matrice)),j],collapse = ""), perl = T)),
        nchar(gsub(paste0(height,".*$"),"",paste(matrice[i,c((j-1):1)],collapse = ""), perl = T)),
        nchar(gsub(paste0(height,".*$"),"",paste(matrice[i,c((j+1):ncol(matrice))],collapse = ""), perl = T))
      )
      res = append(res,prod(trees))

  }
}

solution2 <-
  max(res)
