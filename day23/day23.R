library(tidyverse)

test  = readLines(here::here("day23/test.txt")) 
test2 = readLines(here::here("day23/test2.txt")) 
input = readLines(here::here("day23/input.txt"))

## Task 1

rotate = function(x) {
  c(x[-1], x[1])
}

priority = c("N","S","W","E")

dirs = list(
  N  = c(-1, 0),
  NE = c(-1, 1),
  E  = c( 0, 1),
  SE = c( 1, 1),
  S  = c( 1, 0),
  SW = c( 1,-1),
  W  = c( 0,-1),
  NW = c(-1,-1),
  Z  = c( 0, 0)
)



check = list(
  N = c("N","NE","NW"),
  S = c("S","SE","SW"),
  W = c("W","NW","SW"),
  E = c("E","NE","SE")
)

count_neighbors = function(x,y) {
  dist(cbind(x,y)) %>%
    as.matrix() %>%
    apply(1, function(x) sum(x <= sqrt(2))-1)
}

propose = function(x, y, n) {
  res = rep(NA, length(x))
  for (i in seq_along(x)) {
    if (n[i] == 0) {
      res[i] = "Z"
      next
    }
    
    res[i] = "Z"
    for (dir in priority) {
      clear = map_lgl(
        check[[dir]],
        function(cd) {
          new_y = y[i] + dirs[[cd]][1]
          new_x = x[i] + dirs[[cd]][2]
          
          !any(new_y == y & new_x == x)
        }
      )
      
      if (all(clear)) {
        res[i] = dir
        break
      }
    }
  }
  
  res
}

print_state = function(d) {
  plot(d$x, -d$y, pch=15, cex=5, asp=1)
}




d = input %>%
  str_split("") %>%
  do.call(rbind, .) %>%
  {which(. == "#", arr.ind=TRUE)} %>%
  as_tibble() %>%
  rename(y=row, x=col)

#print_state(d)

for (i in 1:10) {
  d = d %>%
    mutate(
      neigh = count_neighbors(x,y),
      prop = propose(x,y,neigh),
      delta = map(prop, ~ dirs[[.x]]),
      new_pos = map2(x,y, ~ c(.y,.x)) %>%
        map2(delta, ~.x + .y),
      conflict = duplicated(new_pos) | rev(duplicated(rev(new_pos)))
    ) %>% transmute(
      y = ifelse(conflict, y, map_dbl(new_pos, 1)),
      x = ifelse(conflict, x, map_dbl(new_pos, 2))
    )
  
  priority = rotate(priority)
}

#print_state(d)



(diff(range(d$y)) + 1) * (diff(range(d$x)) + 1) - nrow(d)
