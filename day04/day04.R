library(tidyverse)

test = readLines(here::here("day04/test.txt"))
input = readLines(here::here("day04/input.txt"))

## Task 1

input |>
  str_split(",") |>
  map(
    function(x) {
      z = str_split(x, "-") |>
        map(
          function(y) {
             as.numeric(y) %>%
              as.list() %>%
              do.call(`:`, .)
          }
        )
      all(z[[1]] %in% z[[2]]) | all(z[[2]] %in% z[[1]])
    }
  ) |>
  unlist() |>
  sum()

## Task 2

input |>
  str_split(",") |>
  map(
    function(x) {
      z = str_split(x, "-") |>
        map(
          function(y) {
            as.numeric(y) %>%
              as.list() %>%
              do.call(`:`, .)
          }
        )
      length(intersect(z[[1]],z[[2]])) > 0
    }
  ) |>
  unlist() |>
  sum()
