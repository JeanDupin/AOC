# Input ----

input <-
  get_input("https://adventofcode.com/2019/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

wire1 <- strsplit(input[1],",")[[1]]
wire2 <- strsplit(input[2],",")[[1]]

X1 = Y1 = X2 = Y2 = 0
chemin1 = chemin2 = vector("character")

for(i in seq_along(wire1)){
  if(grepl("R",wire1[i])){
    chemin1 <-
      paste(X1:(X1+as.numeric(regmatches(wire1[i],
                                         gregexpr("\\d+",wire1[i]))[[1]])),Y1,sep = ";") |> 
      (\(.){append(chemin1,.)})()
    X1 <-
      regmatches(wire1[i],
                 gregexpr("\\d+",wire1[i]))[[1]] |>
      (\(.){as.numeric(.) + X1})()
  } else if(grepl("L",wire1[i])){
    chemin1 <-
      paste(X1:(X1-as.numeric(regmatches(wire1[i],
                                         gregexpr("\\d+",wire1[i]))[[1]])),Y1,sep = ";") |> 
      (\(.){append(chemin1,.)})()
    X1 <-
      regmatches(wire1[i],
                 gregexpr("\\d+",wire1[i]))[[1]] |>
      (\(.){-as.numeric(.) + X1})()
  } else if(grepl("U",wire1[i])){
    chemin1 <-
      paste(X1,Y1:(Y1+as.numeric(regmatches(wire1[i],
                                            gregexpr("\\d+",wire1[i]))[[1]])),sep = ";") |> 
      (\(.){append(chemin1,.)})()
    Y1 <-
      regmatches(wire1[i],
                 gregexpr("\\d+",wire1[i]))[[1]] |>
      (\(.){as.numeric(.) + Y1})()
  } else {
    chemin1 <-
      paste(X1,Y1:(Y1-as.numeric(regmatches(wire1[i],
                                            gregexpr("\\d+",wire1[i]))[[1]])),sep = ";") |> 
      (\(.){append(chemin1,.)})()
    Y1 <-
      regmatches(wire1[i],
                 gregexpr("\\d+",wire1[i]))[[1]] |>
      (\(.){-as.numeric(.) + Y1})()
  }
}

for(i in seq_along(wire2)){
  if(grepl("R",wire2[i])){
    chemin2 <-
      paste(X2:(X2+as.numeric(regmatches(wire2[i],
                                         gregexpr("\\d+",wire2[i]))[[1]])),Y2,sep = ";") |> 
      (\(.){append(chemin2,.)})()
    X2 <-
      regmatches(wire2[i],
                 gregexpr("\\d+",wire2[i]))[[1]] |>
      (\(.){as.numeric(.) + X2})()
  } else if(grepl("L",wire2[i])){
    chemin2 <-
      paste(X2:(X2-as.numeric(regmatches(wire2[i],
                                         gregexpr("\\d+",wire2[i]))[[1]])),Y2,sep = ";") |> 
      (\(.){append(chemin2,.)})()
    X2 <-
      regmatches(wire2[i],
                 gregexpr("\\d+",wire2[i]))[[1]] |>
      (\(.){-as.numeric(.) + X2})()
  } else if(grepl("U",wire2[i])){
    chemin2 <-
      paste(X2,Y2:(Y2+as.numeric(regmatches(wire2[i],
                                            gregexpr("\\d+",wire2[i]))[[1]])),sep = ";") |> 
      (\(.){append(chemin2,.)})()
    Y2 <-
      regmatches(wire2[i],
                 gregexpr("\\d+",wire2[i]))[[1]] |>
      (\(.){as.numeric(.) + Y2})()
  } else {
    chemin2 <-
      paste(X2,Y2:(Y2-as.numeric(regmatches(wire2[i],
                                            gregexpr("\\d+",wire2[i]))[[1]])),sep = ";") |> 
      (\(.){append(chemin2,.)})()
    Y2 <-
      regmatches(wire2[i],
                 gregexpr("\\d+",wire2[i]))[[1]] |>
      (\(.){-as.numeric(.) + Y2})()
  }
}

chemin1 = unique(chemin1[-1])
chemin2 = unique(chemin2[-1])

solution1 <-
  chemin1[chemin1 %in% chemin2] |> 
  (\(.){
    regmatches(.,
               gregexpr("\\d+",.))
  })() |> 
  lapply(function(.x){sum(abs(as.numeric(.x)))}) |> 
  unlist() |> 
  min()

# Partie 2 ----

solution2 <-
  lapply(chemin1[chemin1 %in% chemin2],
         function(.x){
           which(chemin1 == .x)-1 + which(chemin2 == .x)-1
         })  |> 
  unlist() |> 
  min()

