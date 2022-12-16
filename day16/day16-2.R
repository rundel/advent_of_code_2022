library(tidyverse)

test  = readLines(here::here("day16/test.txt"))
input = readLines(here::here("day16/input.txt"))


## Task 2

data = input

d = tibble(input = data) %>%
  extract(
    col = input,into = c("name", "rate", "other"),
    regex = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
  ) %>%
  mutate(
    rate = as.integer(rate),
    other = str_split(other, ", ")
  )

rates = d %>% 
  select(name, rate) %>%
  filter(rate != 0) %>%
  {setNames(.$rate, .$name)}

g = d %>%
  select(-rate) %>%
  unnest_longer(other) %>%
  as.matrix() %>%
  igraph::graph_from_edgelist()

dm = igraph::distances(g) 

#plot(g, edge.arrow.size=0.5,vertex.size=0.5)




state_init = list(
  rates = rates,
  p = list(
    pos = "AA",
    path = "AA",
    time_left = 26,
    press_rel = 0
  ),
  e = list(
    pos = "AA",
    path = "AA",
    time_left = 26,
    press_rel = 0
  )
)


move2 = function(state = state_init,depth=1) {
  
  if (length(state$rates) == 0)
    return(state)
  
  if (state$p$time_left <= 0 & state$e$time_left <= 0)
    return(state)
  
  if (state$p$time_left >= state$e$time_left) {
    mover = "p"
  } else {
    mover = "e"
  }
  cur = state[[mover]]
  
  moves = names(state$rates)
  dists = dm[cur$pos, moves]
  
  if (all(dists > cur$time_left)) {
    state[[mover]]$time_left = 0
    return(move2(state, depth=depth+1))
  }
  
  values = (cur$time_left - (dists+1)) * state$rates
  
  moves  = moves[values > 0]
  values = values[values > 0]
  
  #o = order(values, decreasing = TRUE) 
  
  #if (depth >= 3)
  #  o = o %>% head(5)
  
  #cat("Moving: ", mover, "\n")
  #print(moves)
  #print(dists)
  #print(values)
  
  
  res = map2(
    moves, values,
    function(m, v) {
      new_state = state
      
      new_state$rates = new_state$rates[ names(new_state$rates) != m ]
      new_state[[mover]]$pos = m
      new_state[[mover]]$time_left = new_state[[mover]]$time_left - (dists[m]+1)
      new_state[[mover]]$press_rel = new_state[[mover]]$press_rel + v
      new_state[[mover]]$path = c(new_state[[mover]]$path, m)
      
      move2(new_state, depth=depth+1)
    }
  )

  if (depth < 4)
    cat("Finished depth: ", depth, "\n")
  
  if (length(res) == 0)
    return(state)
  
  i = map_dbl(
    res, 
    ~ .x$p$press_rel + .x$e$press_rel
  ) %>%
    which.max()
  
  return(res[[i]])
}


(res = move2())

res$p$press_rel + res$e$press_rel

