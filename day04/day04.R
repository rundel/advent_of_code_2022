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


## Task 1&2 (dplyr)

tibble(input = input) |>
  separate(input, ",",into = c("x","y")) |>
  mutate(id = 1:n()) |>
  pivot_longer(x:y) |>
  separate(value, "-", into=c("start","stop")) |>
  mutate(values = map2(start, stop, seq)) |>
  select(id, name, values) |>
  pivot_wider(id, names_from =name, values_from = values) |>
  mutate(
    task1 = map2_lgl(x,y,~ all(.x %in% .y) | all(.y %in% .x)),
    task2 = map2_lgl(x,y,~ length(intersect(.x,.y)) > 0)
  ) |>
  summarize(
    sum(task1),
    sum(task2)
  )

