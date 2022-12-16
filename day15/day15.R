library(tidyverse)

test  = readLines(here::here("day15/test.txt"))
input = readLines(here::here("day15/input.txt"))


## Task 1

data = test
py = 10
data = input
py = 2000000

d = tibble(input = data) %>%
  extract(
    input, into = c("x","y", "Bx","By"), 
    regex = "Sensor at x=(\\d+), y=(\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)",
    convert = TRUE
  ) %>%
  mutate(
    mx = abs(x-Bx),
    my = abs(y-By),
    m_tot = mx+my
  )

imp = d %>%
  filter(
    map2_lgl(y,m_tot, ~py %in% (.x-.y):(.x+.y))
  ) %>%
  mutate(
    moves_left = abs(m_tot - abs(y-py)),
    px = map2(
      x, moves_left,
      ~ c(.x + 0:.y, .x - 0:.y) %>% unique()
    )
  ) %>%
  pull(px) %>%
  unlist() %>%
  unique() %>%
  setdiff(
    d %>%
      filter(By == py) %>%
      pull(Bx) %>%
      unique()
  )
  
imp
length(imp)
  



## Task 2

data = test
py = 10
pos = 1:20

data = input
py = 2000000
pos = 1:4e6

d = tibble(input = data) %>%
  extract(
    input, into = c("x","y", "Bx","By"), 
    regex = "Sensor at x=(\\d+), y=(\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)",
    convert = TRUE
  ) %>%
  mutate(
    mx = abs(x-Bx),
    my = abs(y-By),
    m_tot = mx+my
  )


boundary = sf::st_polygon(
  list( matrix(
    c( 
      0,0,
      0,4e6,
      4e6,4e6,
      4e6,0,
      0,0
    ),
    ncol=2, byrow=TRUE
  ) )
) %>%
  sf::st_sfc()

g = d %>%
  select(x,y,m_tot) %>%
  pmap(
    function(x,y,m_tot) {
      list( matrix(
        c(
          x,y+m_tot,
          x+m_tot,y,
          x,y-m_tot,
          x-m_tot,y,
          x,y+m_tot
        ), 
        ncol=2, byrow=TRUE
      ) ) %>%
        sf::st_polygon()
    }
  ) %>%
  sf::st_sfc()

g2 = sf::st_union(g)

g3 = sf::st_difference(boundary, g2)

sf::st_cast(g3, "POLYGON") %>%
  sf::st_centroid() %>%
  sf::st_coordinates()
  
sum(c(3204400,3219131) * c(4e6,1)) %>%
  formatC(digits=20)

