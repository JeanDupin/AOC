# Input ----

input <-
  httr2::request("https://adventofcode.com/2020/day/13/input") |> 
  httr2::req_cookies_set(session = Sys.getenv("aoc_cookie")) |> 
  httr2::req_perform() |> 
  httr2::resp_body_string() |> 
  (\(.){strsplit(.,"\\n")[[1]]})()

# Partie 1 ----

dep <- as.numeric(input[1])
bus <-
  regmatches(input[2],
             gregexpr("[0-9]+",input[2]))[[1]] |> 
  as.numeric()

solution1 <-
  lapply(bus,
         function(.x){
           i = 1
           depart = .x
           while(depart < dep){
             depart <- .x * i
             i = i + 1
           }
           depart
         }) |> 
  unlist() |> 
  (\(.){bus[which.min(abs(. - dep))] * (.[which.min(abs(. - dep))]-dep)})()


# Partie 2 ----

bus <-
  strsplit(input[2],",")[[1]]
time.off <-
  c(0:(length(bus)-1))[which(bus != "x")]
bus <-
  as.numeric(bus[which(bus != "x")])

N = prod(bus)

Ni = N/bus

xi = mapply(function(x,y){numbers::modinv(x,y)},Ni,bus)

solution2 <-
  sum((-time.off %% bus) * Ni * xi) %% N
