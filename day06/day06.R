library(tidyverse)

test = readLines(here::here("day06/test.txt"))
test2 = readLines(here::here("day06/test2.txt"))
test3 = readLines(here::here("day06/test3.txt"))
test4 = readLines(here::here("day06/test4.txt"))
input = readLines(here::here("day06/input.txt"))

## Task 1


tibble(input = input) |>
  mutate(
    letters = str_split(input, "")
  ) %>%
  unnest_longer(letters) %>%
  transmute(
    l0 = letters,
    l1 = lag(letters,1),
    l2 = lag(letters,2),
    l3 = lag(letters,3)
  ) %>%
  pmap_int(
    function(l0,l1,l2,l3) {
      c(l0,l1,l2,l3) %>%
        {length(unique(.)) == 4 & !any(is.na(.))}
    }
  ) %>%
  {min(which(. == 1))}
  
  
## Task 2


letters = input %>%
  str_split("") %>%
  .[[1]]

res = list(letters)
for (i in 1:14) {
  res[[i]] = lag(letters, i-1)
}

for(i in seq_along(res[[1]])) {
  vals = map_chr(res, i)
  check = length(unique(vals)) == 14 & all (!is.na(vals)) 
  
  if (check) {
    print(i)
    break 
  }
}



## Task 1 - take 2

make_lags = function(x,n) {
  l = lapply(seq_len(n), function(i) lag(x, i-1))
  map(seq_along(l[[1]]), ~ map_chr(l, .x))
}

tibble(input = input) |>
  mutate(
    letters = str_split(input, "")
  ) %>%
  select(letters) %>%
  unnest_longer(letters) %>%
  mutate(
    id = 1:n(),
    lags = letters %>% make_lags(4),
    status = map_lgl(lags, ~ length(unique(.x)) == 4 & all(!is.na(.x))) 
  ) %>%
  filter(status)

## Task 2

tibble(input = input) |>
  mutate(
    letters = str_split(input, "")
  ) %>%
  select(letters) %>%
  unnest_longer(letters) %>%
  mutate(
    id = 1:n(),
    lags = letters %>% make_lags(14),
    status = map_lgl(lags, ~ length(unique(.x)) == 14 & all(!is.na(.x))) 
  ) %>%
  filter(status)
  
## w/ slider

tibble(input = input) |>
  mutate(
    letters = str_split(input, "")
  ) %>%
  select(letters) %>%
  unnest_longer(letters) %>%
  mutate(
    id = 1:n(),
    vals = slider::slide(letters, ~.x, .before=3, .complete = TRUE),
    status = map_lgl(vals, ~ length(unique(.x)) == 4 & all(!is.na(.x))) 
  ) %>%
  filter(status)
