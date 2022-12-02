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


input

## Task 2

input