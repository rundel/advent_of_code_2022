library(tidyverse)

test  = readLines(here::here("day22/test.txt"))
input = readLines(here::here("day22/input.txt"))

## Task 1

parse = function(x) {
  i = which(x == "")
  
  chars = x[1:(i-1)] %>%
    str_split("")
  
  l = map_int(chars, length)
  max_width = max(l)
  
  map = map2(chars, l, ~ c(.x, rep(" ", max_width-.y))) %>%
    do.call(rbind, .)
  
  
  list(
    map =  map,
    turns = str_split_1(x[i+1],"\\d+") %>%
      discard(~ .x == ""),
    moves = str_split_1(x[i+1],"L|R") %>% as.numeric()
  )
}

p = parse(input)

move = list(
  `<` = c(0,-1),
  `^` = c(-1,0),
  `>` = c(0,1),
  `v` = c(1,0)
)

rotate = list(
  L = c(`>`="^", `^`="<", `<`="v", `v`=">"),
  R = c(`>`="v", `^`=">", `<`="^", `v`="<")
)

facing_score = c(
  `>`=0, `v`=1, `<`=2, `^`=3
)


wrap = function(p) {
  c(
    ifelse(p[1] %% nrow(map) == 0, nrow(map), p[1] %% nrow(map)),
    ifelse(p[2] %% ncol(map) == 0, ncol(map), p[2] %% ncol(map))
  )
}

map = p$map
moves = p$moves
turns = p$turns

dir = ">"
pos = c(1, which(map[1,] == '.')[1])

repeat {
  if (length(moves) == 0)
    break

  m = moves[1]
  moves = moves[-1]
  
  for (i in seq_len(m)) {
    new = wrap(pos + move[[dir]])
    
    
    if (map[new[1], new[2]] == " ") {
      repeat {
        new = wrap(new + move[[dir]])
        if (map[new[1], new[2]] %in% c(".","#"))
          break
      }
    }
    
    if (map[new[1], new[2]] == "#")
      break
    
    #map[pos[1], pos[2]] = dir
    pos = new
  }
  
  if (length(turns) != 0) {
    dir = rotate[[ turns[1] ]][dir]
    turns = turns[-1]
  }
}

map[pos[1], pos[2]] = "*"

apply(map, 1, paste, collapse="") %>%
  walk(cat, "\n")

sum(c(1000, 4) * pos) + facing_score[dir]
