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



## A non-stupid solution

input = readLines("day05/input.txt")

board = tibble(input = input) |>
  mutate(
    l = nchar(input)
  ) %>%
  slice(1:(which(l == 0) - 2)) %>%
  mutate(
    values = str_split(input, ""),
    test = map(
      values, 
      ~ .x[seq(2, length(.x), by = 4)] %>% 
          {setNames(., paste0("x",seq_along(.)))}
    )
  ) %>% 
  select(test) %>%
  unnest_wider(test) %>%
  map(~ .x[.x != " "])

moves = tibble(input = input) |>
  extract(
    input, into = c("n", "start","end"), 
    "move (\\d+) from (\\d+) to (\\d+)", 
    convert=TRUE
  ) |>
  filter(!is.na(n))

## Task 1

board_t1 = board

pwalk(
  moves,
  function(n, start, end) {
    for (i in seq_len(n)) {
      val = board_t1[[start]][1]
      board_t1[[start]] <<- board_t1[[start]][-1]
      board_t1[[end]] <<- c(val, board_t1[[end]])
    }
  }
)

map_chr(board_t1, 1) |>
  paste(collapse="")

## Task 2

board_t2 = board

pwalk(
  moves,
  function(n, start, end) {
      val = board_t2[[start]][1:n]
      board_t2[[start]] <<- board_t2[[start]][-(1:n)]
      board_t2[[end]] <<- c(val, board_t2[[end]])
  }
)

map_chr(board_t2, 1) |>
  paste(collapse="")