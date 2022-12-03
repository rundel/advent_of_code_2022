library(tidyverse)

test = readLines(here::here("day02/test.txt"))
input = readLines(here::here("day02/input.txt"))

## Task 1

scores = c(X=1,Y=2,Z=3)

res = c("A X" = 3,
  "A Y" = 6,
  "A Z" = 0,
  "B X" = 0,
  "B Y" = 3,
  "B Z" = 6,
  "C X" = 6,
  "C Y" = 0,
  "C Z" = 3)


d = tibble(
  game = input
) |>
  mutate(
    x = str_split(game," "),
    play = map_chr(x, 1),
    res = map_chr(x,2)
  )

sum(res[d$game])+sum(scores[d$res])

## Task 2

d2 = tibble(
  game = input
) |>
  mutate(
    x = str_split(game," "),
    play = map_chr(x, 1),
    out = map_chr(x, 2),
    res = case_when(
      out == "X" & play == "A" ~ "C",
      out == "X" & play == "B" ~ "A",
      out == "X" & play == "C" ~ "B",
      out == "Y" & play == "A" ~ "A",
      out == "Y" & play == "B" ~ "B",
      out == "Y" & play == "C" ~ "C",
      out == "Z" & play == "A" ~ "B",
      out == "Z" & play == "B" ~ "C",
      out == "Z" & play == "C" ~ "A"
    )
  )

scores = c(A=1,B=2,C=3)
out_score = c(X=0,Y=3,Z=6)

sum(scores[d2$res])+sum(out_score[d2$out])
