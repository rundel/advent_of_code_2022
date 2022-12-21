library(tidyverse)

test  = readLines(here::here("day21/test.txt"))
input = readLines(here::here("day21/input.txt"))

## Task 1

input %>%
  str_replace(":", " =") %>%
  paste(collapse=", ") %>%
  paste0(
    "f = function(", . ,") {root};f()" 
  ) %>%
  parse(text = .) %>%
  eval() %>%
  formatC(digits=16)
  
## Task 2

f = input %>%
  str_replace(":", " =") %>%
  str_replace("root = ([a-z]+) \\+ ([a-z]+)", "root = \\1 == \\2") %>% 
  str_replace("humn = \\d+", "humn") %>%
  paste(collapse=", ") %>%
  paste0(
    "function(", . ,") {
      (wrvq-vqfc)^2
    }" 
  ) %>%
  parse(text = .) %>%
  eval()
g = function(x) {
  f(humn=x)
}

optimize(g, c(10^12, 10^13))

