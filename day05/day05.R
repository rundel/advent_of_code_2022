library(tidyverse)

test1 = readLines(here::here("day05/test.txt"), n=3)
test2 = readLines(here::here("day05/test.txt")) %>%
  {.[6:length(.)]}

input1 = readLines(here::here("day05/input.txt"), n = 8)
input2 = readLines(here::here("day05/input.txt"))  %>%
  {.[11:length(.)]}

## Task 1

board  = tibble(input = test1) |>
  extract(
    input, into=paste0("x",1:3), 
    "(?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   )"
  ) |>
  as.matrix() %>%
  rbind("", "", "", "", "", .)
moves = tibble(input = test2) |>
  extract(input, into = c("n", "start","end"), "move (\\d+) from (\\d+) to (\\d+)", convert=TRUE)


#board  = tibble(input = input1) |>
#  extract(
#    input, into=paste0("x",1:9), 
#    "(?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   )"
#  ) |>
#  as.matrix() %>%
#  rbind("", "", "", "","", "","", "","", "","", "",
#        "", "", "", "","", "","", "","", "","", "",
#        "", "", "", "","", "","", "","", "","", "",
#        .)
#
#moves = tibble(input = input2) |>
#  extract(input, into = c("n", "start","end"), "move (\\d+) from (\\d+) to (\\d+)", convert=TRUE)


find_top = function(m, i) {
  for(j in seq_along(m[,i])) {
    if (m[j,i] != "")
      return(j)
  }
  return(j+1)
}

pwalk(
  moves,
  function(n, start, end) {
    for (i in seq_len(n)) {
      from_i = find_top(board, start)
      to_i   = find_top(board, end) - 1
      stopifnot(to_i != 0)
      
      val = board[from_i, start]
      board[from_i, start] <<- ""
      board[to_i, end] <<- val
      print(board)
    }
  }
)

apply(board, 2, function(x) x[which(x!="")[1]])


## Task 2

board  = tibble(input = test1) |>
  extract(
    input, into=paste0("x",1:3), 
    "(?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   )"
  ) |>
  as.matrix() %>%
  rbind("", "", "", "", "", .)
moves = tibble(input = test2) |>
  extract(input, into = c("n", "start","end"), "move (\\d+) from (\\d+) to (\\d+)", convert=TRUE)


board  = tibble(input = input1) |>
  extract(
    input, into=paste0("x",1:9), 
    "(?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   ) (?:\\[([A-Z])\\]|   )"
  ) |>
  as.matrix() %>%
  rbind("", "", "", "","", "","", "","", "","", "",
        "", "", "", "","", "","", "","", "","", "",
        "", "", "", "","", "","", "","", "","", "",
        .)

moves = tibble(input = input2) |>
  extract(input, into = c("n", "start","end"), "move (\\d+) from (\\d+) to (\\d+)", convert=TRUE)

pwalk(
  moves,
  function(n, start, end) {
      from_i = find_top(board, start)
      to_i   = find_top(board, end) - 1
      stopifnot(to_i != 0)
      
      val = board[from_i:from_i:(from_i+n-1), start]
      board[from_i:from_i:(from_i+n-1), start] <<- ""
      board[(to_i-n+1):to_i, end] <<- val
      print(board)
  }
)

apply(board, 2, function(x) x[which(x!="")[1]])

