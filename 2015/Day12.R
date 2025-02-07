# Input ----

input <-
  get_input("https://adventofcode.com/2015/day/12/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  regmatches(input,
            gregexpr("(-|)\\d+",input))[[1]] |> 
  as.numeric() |> 
  sum()

# Partie 2 ----

gsub("(\\[[^\\{]+?)(red)","\\1blue",input) |> 
  (\(.){gsub("\\{.+?red.+?\\}","",.)})() |> 
    (\(.){
      regmatches(.,
        gregexpr("(-|)\\d+",.))[[1]]
    })() |> 
  as.numeric() |> 
  sum()


gsub("\\{[^\\[]+?red[^\\]]+?\\}","",input)|> 
  (\(.){
    regmatches(.,
      gregexpr("(-|)\\d+",.))[[1]]
  })() |> 
as.numeric() |> 
sum()


gsub("\\{[^}]*red[^}]*\\}", "", input) |> 
  (\(.){
    regmatches(.,
      gregexpr("(-|)\\d+",.))[[1]]
  })() |> 
as.numeric() |> 
sum()

solution2 <-
  NA
