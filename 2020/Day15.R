# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/15/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

numbers <-
  as.numeric(strsplit(input,",")[[1]])

i = length(numbers) + 1

while(i <= 2020){
  if(sum(numbers == numbers[i-1]) == 1){
    numbers[i] = 0
    i = i + 1
  } else {
    numbers[i] <-
      which(numbers == numbers[i-1]) |> 
      rev() |> 
      (\(.){.[c(1,2)]})() |> 
      (\(.){diff(rev(.))})()
    i = i + 1
  }
}


solution1 <-
  numbers[2020]

# Partie 2 ----

numbers <-
  as.numeric(strsplit(input,",")[[1]])

nextnumber = numbers[length(numbers)]
numbers <- numbers[-length(numbers)]
times <- c(1:length(numbers))

i = length(numbers) + 1

while(i <= 30000000){
  if(!nextnumber %in% numbers){
    numbers <- append(numbers, nextnumber)
    times <- append(times,i)
    nextnumber = 0
  } else {
    index = which(numbers == nextnumber)
    nextnumber = i-times[index]
    times[index] = i
  }
  i = i + 1
}

solution2 <-
  numbers[which(times == 30000000)]
