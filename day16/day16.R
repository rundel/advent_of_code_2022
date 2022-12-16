library(tidyverse)

test  = readLines(here::here("day16/test.txt"))
input = readLines(here::here("day16/input.txt"))


## Task 1

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


edges = d %>%
  select(-rate) %>%
  unnest_longer(other)

g = edges %>%
  as.matrix() %>%
  igraph::graph_from_edgelist()

dm = igraph::distances(g) 

plot(g, edge.arrow.size=0.5,vertex.size=0.5)

init_state = list(
  time_left=30,
  rates = rates,
  valves_on = integer(),
  press_rel = 0
)

move = function(pos = "AA", state=init_state) {
  
  dists = dm[pos, names(state$rates)]
  if (all(dists > state$time_left)) # no moves left
    return(state)
  
  moves = names(state$rates)
  values = (state$time_left - (dists+1)) * state$rates
  
  res = map2(
    moves, values,
    function(m, v) {
      i = which(names(state$rates) == m)
      
      new_state = state
      
      new_state$rates = new_state$rates[-i]
      new_state$time_left = new_state$time_left - (dists[i]+1)
      new_state$press_rel = new_state$press_rel + v
      
      move(pos=m, state=new_state)
    }
  )

  if (length(res) == 0)
    return(state)
  
  i = map_dbl(res, "press_rel") %>%
    which.max()
  
  return(res[[i]])
}


(move())
