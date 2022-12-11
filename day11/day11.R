library(tidyverse)

test  = readLines(here::here("day11/test.txt"))
input = readLines(here::here("day11/input.txt"))

## Task 1


data = input

n_monkeys = str_detect(data,"^Monkey") %>%
  sum()

parse_monkey = function(x) {
  list(
    items = str_extract(x[2], "(\\d+)(, \\d+)*") %>%
      str_split(", ") %>%
      unlist() %>%
      as.numeric(),
    op = str_extract(x[3], "old [*+] (old|\\d+)"),
    div = str_extract(x[4], "\\d+") %>%
      as.numeric(),
    true = str_extract(x[5], "\\d+") %>%
      as.numeric() %>% `+`(1),
    false = str_extract(x[6], "\\d+") %>%
      as.numeric() %>% `+`(1),
    insp = 0
  )
}

monkeys = map(
  seq(1,length(data),7),
  ~ data[.x:(.x+5)]
) %>%
  map(
    parse_monkey
  )

for (i in 1:20) {
  for (j in 1:n_monkeys) {
    for (old in monkeys[[j]]$items) {
      new = eval(parse(text=monkeys[[j]]$op)) 
      new = floor(new/3)
      
      #print(c(new,monkeys[[j]]$div))
      if (new %% monkeys[[j]]$div == 0)
        target = monkeys[[j]]$true
      else
        target = monkeys[[j]]$false
      
      monkeys[[target]]$items = c(monkeys[[target]]$items, new)
    }
    
    monkeys[[j]]$insp = monkeys[[j]]$insp + length(monkeys[[j]]$items)
    monkeys[[j]]$items = c()
  }
}

str(monkeys)

monkeys %>%
  map_dbl("insp") %>%
  sort(decreasing = TRUE) %>%
  .[1:2] %>%
  prod()


## Task 2


monkeys = map(
  seq(1,length(input),7),
  ~ input[.x:(.x+5)]
) %>%
  map(
    parse_monkey
  )

divs = map_dbl(monkeys, "div")

monkeys = map(
  monkeys,
  function(m) {
    m$items = map(m$items, ~ (.x %% divs) %>% setNames(divs))
    m
  }
)

for (i in 1:10000) {
  for (j in 1:length(monkeys)) {
    for (old in monkeys[[j]]$items) {
      new = eval(parse(text=monkeys[[j]]$op)) 
      new = new %% divs
      
      if (new[as.character(monkeys[[j]]$div)] == 0) {
        target = monkeys[[j]]$true
      } else {
        target = monkeys[[j]]$false
      }
      
      monkeys[[target]]$items = c(monkeys[[target]]$items, list(new))
    }
    
    monkeys[[j]]$insp = monkeys[[j]]$insp + length(monkeys[[j]]$items)
    monkeys[[j]]$items = list()
  }
}


monkeys %>%
  map_dbl("insp")

monkeys %>%
  map_dbl("insp") %>%
  sort(decreasing = TRUE) %>%
  .[1:2] %>%
  prod()
