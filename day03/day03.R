library(tidyverse)

test = readLines(here::here("day03/test.txt"))
input = readLines(here::here("day03/input.txt"))

## Task 1

values = c(
  1:52
) %>%
  setNames(c(letters, LETTERS))

input |>
  str_split("") |>
  map(
    function(x) {
      n = length(x)
      intersect(x[1:n/2], x[(n/2+1):n])
    }
  ) |>
  unlist() %>%
  {values[.]} %>%
  sum()


## Task 2

d = input |>
  str_split("") 
  
map(
  seq(1,length(d), by=3),
  function(i) {
    intersect(d[[i]], d[[i+1]]) |>
      intersect(d[[i+2]])
  }
) |>
  unlist() %>%
  {values[.]} %>%
  sum()


