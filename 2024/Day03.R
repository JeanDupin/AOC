# Input ----

input <-
  get_input("https://adventofcode.com/2024/day/3/input") |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

solution1 <-
  regmatches(input,
             gregexpr("mul\\(\\d+,\\d+\\)",input)) |> 
  unlist() |> 
  lapply(
    function(x){
      regmatches(x,
                 gregexpr("\\d+",x))[[1]] |> 
        as.numeric() |> 
        prod()
    }
  ) |> 
  unlist() |> 
  sum()


# Partie 2 ----

solution2 <-
  regmatches(input,
             gregexpr("(mul\\(\\d+,\\d+\\)|do\\(\\)|don\\'t\\(\\))",input)) |> 
  unlist() |> 
  paste(collapse = "") |> 
  (\(.){gsub("don\\'t\\(\\).*?(do\\(\\)|$)","",.)})() |> 
  (\(.){gsub("do\\(\\)","",.)})() |> 
  (\(.){
    regmatches(.,
               gregexpr("mul\\(\\d+,\\d+\\)",.))
  })() |> 
  unlist() |> 
  lapply(
    function(x){
      regmatches(x,
                 gregexpr("\\d+",x))[[1]] |> 
        as.numeric() |> 
        prod()
    }
  ) |> 
  unlist() |> 
  sum()
