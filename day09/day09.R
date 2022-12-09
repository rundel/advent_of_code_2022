library(tidyverse)

test = readLines(here::here("day09/test.txt"))
test2 = readLines(here::here("day09/test2.txt"))
input = readLines(here::here("day09/input.txt"))

## Task 1



d = tibble(input = input) %>%
  extract(input, into = c("move", "n"), regex = "([UDLR]) (\\d+)") %>%
  transmute(
    move = map2(move, n, rep)
  ) %>%
  unnest_longer(move) %>%
  mutate(
    Hx = case_when(
      move == "R" ~ 1,
      move == "L" ~ -1,
      TRUE ~ 0
    ) %>% 
      cumsum(),
    Hy = case_when(
      move == "U" ~ 1,
      move == "D" ~ -1,
      TRUE ~ 0
    ) %>%
      cumsum(),
    Tx = NA,
    Ty = NA
  )

cT = c(0,0)
for (i in seq_len(nrow(d))) {
  cH=c(d$Hx[i], d$Hy[i])
  diff = cH - cT
  
  if (2 %in% abs(diff)) {
    
    move = case_when(
      all(diff == c( 2,0)) ~ c( 1,0),
      all(diff == c(-2,0)) ~ c(-1,0),
      all(diff == c(0, 2)) ~ c(0, 1),
      all(diff == c(0,-2)) ~ c(0,-1),
      
      all(diff == c( 1, 2)) ~ c(1,1),
      all(diff == c( 1,-2)) ~ c(1,-1),
      all(diff == c(-1, 2)) ~ c(-1,1),
      all(diff == c(-1,-2)) ~ c(-1,-1),
      all(diff == c( 2, 1)) ~ c(1,1),
      all(diff == c(-2, 1)) ~ c(-1,1), # Check 
      all(diff == c( 2,-1)) ~ c(1,-1), # Check
      all(diff == c(-2,-1)) ~ c(-1,-1)
    )
    cT = cT + move
  }
  
  d$Tx[i] = cT[1]
  d$Ty[i] = cT[2]
}

d %>%
  select(Tx,Ty) %>%
  distinct() %>%
  nrow()

## Task 2

d = tibble(input = input) %>%
  extract(input, into = c("move", "n"), regex = "([UDLR]) (\\d+)") %>%
  transmute(
    move = map2(move, n, rep)
  ) %>%
  unnest_longer(move) %>%
  mutate(
    Hx = case_when(
      move == "R" ~ 1,
      move == "L" ~ -1,
      TRUE ~ 0
    ) %>% 
      cumsum(),
    Hy = case_when(
      move == "U" ~ 1,
      move == "D" ~ -1,
      TRUE ~ 0
    ) %>%
      cumsum()
  )

for(i in 1:9) {
  
  cT = c(0,0)
  for (i in seq_len(nrow(d))) {
    cH=c(d$Hx[i], d$Hy[i])
    diff = cH - cT
    
    if (2 %in% abs(diff)) {
      
      move = case_when(
        all(diff == c( 2,0)) ~ c( 1,0),
        all(diff == c(-2,0)) ~ c(-1,0),
        all(diff == c(0, 2)) ~ c(0, 1),
        all(diff == c(0,-2)) ~ c(0,-1),
        
        all(diff == c( 1, 2)) ~ c(1,1),
        all(diff == c( 1,-2)) ~ c(1,-1),
        all(diff == c(-1, 2)) ~ c(-1,1),
        all(diff == c(-1,-2)) ~ c(-1,-1),
        all(diff == c( 2, 1)) ~ c(1,1),
        all(diff == c(-2, 1)) ~ c(-1,1), # Check 
        all(diff == c( 2,-1)) ~ c(1,-1), # Check
        all(diff == c(-2,-1)) ~ c(-1,-1),
        
        all(diff == c( 2, 2)) ~ c( 1, 1),
        all(diff == c( 2,-2)) ~ c( 1,-1),
        all(diff == c(-2, 2)) ~ c(-1, 1),
        all(diff == c(-2,-2)) ~ c(-1,-1),
      )
      cT = cT + move
    }
    
    d$Hx[i] = cT[1]
    d$Hy[i] = cT[2]
  }
}

d %>%
  select(Hx,Hy) %>%
  distinct()

  
