library(tidyverse)

test  = readLines(here::here("day10/test.txt"))
input = readLines(here::here("day10/input.txt"))

## Task 1

cycles = c("noop" = 1 , "addx" = 2)

d = tibble(input = input) %>%
  separate(input, into=c("cmd","val"), sep = " ",convert = TRUE) %>%
  mutate(
    val = ifelse(is.na(val), 0, val)
  )

value = rep(NA, 220)
value[1] = 1
i = 1
j = 1

while (i < 220) {
  print(c(i,j))
  cmd = d$cmd[j]
  
  if (cmd == "addx") {
    value[i+1] = value[i]
    value[i+2] = value[i] + d$val[j]
    i = i+2
  }
  else {
    value[i+1] = value[i]
    i = i+1
  }
  j = j+1
}

cyc = c(20,60,100,140,180,220)

(value[cyc] * cyc) %>% sum()

  
## Task 2


d = tibble(input = input) %>%
  separate(input, into=c("cmd","val"), sep = " ",convert = TRUE) %>%
  mutate(
    val = ifelse(is.na(val), 0, val)
  )

value = rep(NA, 240)
pixel = rep("", 240)
value[1] = 1
i = 1
j = 1

while (i <= 240) {
  cmd = d$cmd[j]
  
  pixel[i]   = if (((i-1) %% 40)  %in% (value[i] +c(-1,0,1))) "#" else "."
  
  print(c(i,j))
  
  if (cmd == "addx") {
    value[i+1] = value[i]
    
    pixel[i+1] = if ((i %% 40) %in% (value[i+1] +c(-1,0,1))) "#" else "."
    
    value[i+2] = value[i] + d$val[j]
    i = i+2
  }
  else {
    value[i+1] = value[i]
    i = i+1
  }
  j = j+1
  
  #
  #print(value[1:10])
  #print(pixel[1:10])
}

matrix(pixel, ncol = 40, byrow=TRUE) %>%
  apply(1,paste0, collapse="") %>%
  paste(collapse="\n") %>%
  cat()

