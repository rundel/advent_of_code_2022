library(tidyverse)

test = readLines(here::here("day01/test.txt"))
input = readLines(here::here("day01/input.txt"))

## Task 1
library(tidyverse)

tibble(
  x = input |>
    as.numeric()
) |>
  mutate(
    nas = is.na(x),
    ids = cumsum(nas)
  ) |>
  filter(!is.na(x)) |>
  group_by(ids) |>
  summarize(total=sum(x)) |>
  arrange(desc(total))


## Task 2

tibble(
  x = input |>
    as.numeric()
) |>
  mutate(
    nas = is.na(x),
    ids = cumsum(nas)
  ) |>
  filter(!is.na(x)) |>
  group_by(ids) |>
  summarize(total=sum(x)) |>
  top_n(3) |>
  pull(total) |>
  sum()


## Task 1 redo

readr::read_file("day01/input.txt") |>
  str_split("\n\n") |>
  unlist() |>
  str_split("\n") |>
  map(~ as.numeric(.x) |> sum(na.rm=TRUE)) |>
  unlist() |>
  sort()
