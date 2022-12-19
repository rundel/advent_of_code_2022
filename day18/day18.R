library(tidyverse)

strip_col_names = function(x) {
  colnames(x) = NULL
  x
}

test  = read.csv(here::here("day18/test.txt"), header=FALSE) %>% as.matrix() %>% strip_col_names()
test2 = read.csv(here::here("day18/test2.txt"), header=FALSE) %>% as.matrix() %>% strip_col_names()
input = read.csv(here::here("day18/input.txt"), header=FALSE) %>% as.matrix() %>% strip_col_names()


## Task 1

data = input

dist(data) %>%
  as.matrix() %>%
  apply(1, function(x) 6-sum( abs(x-1) < 1e-6  )) %>%
  sum()

## Task 2

data = input

df = data %>%
  as.data.frame() %>%
  setNames(c("x","y","z")) #%>%

ranges = map(
  1:ncol(data),
  ~ range(data[,.x])
) %>%
  map(
    ~ seq(.x[1]-1,.x[2]+1)
  ) %>% 
  setNames(c("x","y","z"))


pts = ranges %>%
  do.call(expand.grid,.) %>%
  anti_join(df)

g = dist(pts) %>%
  as.matrix() %>%
  apply(1, function(x) abs(x-1) < 1e-6) %>%
  igraph::graph_from_adjacency_matrix() %>%
  igraph::decompose.graph()

int_idx = tibble(  
  n = map_int(g, ~ igraph::V(.x) %>% length()),
  v = map(g, ~ igraph::V(.x)$name %>% as.integer())
) %>% 
  mutate(id = seq_len(n())) %>%
  filter(n < 2000) %>%
  unnest_longer(v)

interior = pts[int_idx$v,]

find_sa = function(data) {
  dist(data) %>%
    as.matrix() %>%
    apply(1, function(x) 6-sum( abs(x-1) < 1e-6  )) %>%
    sum()
}

rbind(
  df, 
  interior
) %>%
  find_sa()

