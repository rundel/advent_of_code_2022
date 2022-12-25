library(tidyverse)

test  = readLines(here::here("day25/test.txt")) %>% str_split("")
input = readLines(here::here("day25/input.txt")) %>% str_split("")

options(digits=16)

## Task 1

vals = c(`2` = 2, `1`=1, `0`=0, `-`=-1, `=`=-2)
rev_vals = c("=","-","0","1","2")

data=input


tot = map_dbl(
  data,
  ~ sum(vals[.x] * 5^(length(.x):1-1))
) %>%
  sum()

n = floor(log(tot,5))
poss = c(2,1,0,-1,-2)

res = c()

orig=tot
for (i in n:0) {
  try = tot - poss * (5^i)
  j = which.min(abs(try))
  tot = try[j]
  res = c(res, poss[j])
}

z = rev_vals[res+3]

sum(vals[z] * 5^(length(z):1-1))


