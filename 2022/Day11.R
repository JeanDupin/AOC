# Input ----

input <-
  get_input("https://adventofcode.com/2022/day/11/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

monkeys <-
  regmatches(input[grepl("items: ",input)],
            gregexpr("\\d+",input[grepl("items: ",input)])) |> 
  lapply(as.integer)

operations <-
  input[grepl("Operation: ",input)] |> 
  (\(.){gsub(".* = ","",.)})()

tests <- 
  regmatches(input[grepl("Test: ",input)],
            gregexpr("\\d+",input[grepl("Test: ",input)])) |> 
  lapply(as.integer)

throw <-
  regmatches(input[grepl("If ",input)],
            gregexpr("\\d+",input[grepl("If ",input)])) |> 
  lapply(function(.x){as.integer(.x) + 1}) |> 
  (\(.){
    lapply(seq(1, length(.), by = 2), function(i){
        c(.[[i]], .[[i + 1]])
    })
  })()

totals <-
  as.list(rep(0,length(monkeys)))
            
for(z in seq_len(20)){
  for(i in seq_along(monkeys)){
    if(length(monkeys[[i]]) == 0){
      next
    }
    for(j in seq_along(monkeys[[i]])){
      worry <-
        gsub("old",monkeys[[i]][j],operations[i]) |> 
        (\(.){eval(parse(text = .))})() |> 
        (\(.){floor(./3)})()

      if(worry %% tests[[i]] == 0){
        monkeys[[throw[[i]][1]]] <- append(monkeys[[throw[[i]][1]]], worry)
      } else {
        monkeys[[throw[[i]][2]]] <- append(monkeys[[throw[[i]][2]]], worry)
      }

    }
    monkeys[[i]] <- monkeys[[i]][-c(seq_len(j))]
    totals[[i]] <- totals[[i]] + j
  }
}

solution1 <-
  prod(sort(unlist(totals),decreasing = T)[seq_len(2)])

# Partie 2 ----
