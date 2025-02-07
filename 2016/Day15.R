# Input ----

input <-
  get_input("https://adventofcode.com/2016/day/15/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

disks <-
  regmatches(input,
             gregexpr("\\d+",input)) |> 
  lapply(function(.x){
    as.numeric(.x[-3]) |> 
      (\(.){as.data.frame(t(.))})()
  }) |> 
  (\(.){do.call(rbind,.)})()

i = 0
res = F
while(!res){
  i = i + 1
  res = all((disks$V3 + disks$V1 + i) %% disks$V2 == 0)
}

solution1 <-
  i

# Partie 2 ----

disks <-
  rbind(
    disks,
    data.frame(V1 = nrow(disks) + 1, V2 = 11, V3 = 0)
  )

i = 0
res = F
while(!res){
  i = i + 1
  res = all((disks$V3 + disks$V1 + i) %% disks$V2 == 0)
}

solution2 <-
  i