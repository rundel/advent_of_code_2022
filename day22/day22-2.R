library(tidyverse)

test  = readLines(here::here("day22/test.txt"))
input = readLines(here::here("day22/input.txt"))

## Task 2

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

map = p$map
moves = p$moves
turns = p$turns


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


wrap = function(p, dir) {
  if (p[1] == 0 & p[2] %in% 51:100 & dir == "^") { # A -> F
    list(c(p[2]+100, 1), ">")
  } else if (p[1] %in% 1:50 & p[2] == 50 & dir == "<") { # A -> E
    list(c(151-p[1], 1), ">")
  } else if (p[1] == 0 & p[2] %in% 101:150 & dir == "^") { # B -> F
    list(c(200, p[2]-100), "^")
  } else if (p[1] == 51 & p[2] %in% 101:150 & dir == "v") { # B -> C
    list(c(p[2]-50, 100), "<")
  } else if (p[1] %in% 1:50 & p[2] == 151 & dir == ">") { # B -> D
    list(c(151-p[1], 100), "<")
  } else if (p[1] %in% 51:100 & p[2] == 50 & dir == "<") { # C -> E
    list(c(101, p[1]-50), "v")
  } else if (p[1] %in% 51:100 & p[2] == 101 & dir == ">") { # C -> B
    list(c(50, p[1]+50), "^")
  } else if (p[1] %in% 101:150 & p[2] == 101 & dir == ">") { # D -> B
    list(c(151-p[1],150), "<")
  } else if (p[1] == 151 & p[2] %in% 51:100 & dir == "v") { # D -> F
    list(c(p[2]+100 ,50), "<")
  } else if (p[1] == 100 & p[2] %in% 1:50 & dir == "^") { # E -> C
    list(c(p[2]+50 ,51), ">")
  } else if (p[1] %in% 101:150 & p[2] == 0 & dir == "<") { # E -> A
    list(c(151-p[1] ,51), ">")
  } else if (p[1] == 201 & p[2] %in% 1:50 & dir == "v") { # F -> B
    list(c(1,p[2]+100), "v")
  } else if (p[1] %in% 151:200 & p[2] == 0 & dir == "<") { # F -> A
    list(c(1 ,p[1]-100), "v")
  } else if (p[1] %in% 151:200 & p[2] == 51 & dir == ">") { # F -> D
      list(c(150 ,p[1]-100), "^")
  } else {
    list(p, dir)
  }
}

get_region = function(p) {
  case_when(
    p[1] %in% 1:50    & p[2] %in% 51:100 ~"A",
    p[1] %in% 1:50    & p[2] %in% 101:150 ~"B",
    p[1] %in% 51:100  & p[2] %in% 51:100 ~"C",
    p[1] %in% 101:150 & p[2] %in% 51:100 ~"D",
    p[1] %in% 101:150 & p[2] %in% 1:50 ~"E",
    p[1] %in% 151:200 & p[2] %in% 1:50 ~"F",
    TRUE ~ NA_character_
  )
}

test_con = function(pos, dir, map) {
#  cat(pos,"\n")
#  cat(pos+move[[dir]],"\n")
  r = wrap(pos+move[[dir]], dir)
  new_p = r[[1]]
  new_d = r[[2]]
#  cat(new_p,"\n")
  paste(
    get_region(pos),
    "->",
    get_region(new_p)
  )
}

test_round_trip = function() {
  ind = which(map != " ", arr.ind = TRUE)
  
  for (dir in c("<","^",">","v")) {
    for (i in 1:nrow(ind)) {
      pos = ind[i,]
      start = pos
      
      new = wrap(pos+move[[dir]], dir)
      new_p = new[[1]]
      new_d = new[[2]]
      
      rot_d = rotate[["R"]][new_d]
      rot_d = rotate[["R"]][rot_d]
      
      end = wrap(new_p+move[[rot_d]], rot_d)
      
      stopifnot(all(start == end[[1]]))
      
      rot_d2 = rotate[["R"]][end[[2]]]
      rot_d2 = rotate[["R"]][rot_d2]
      
      stopifnot(rot_d2 == dir)
    }  
  }
  
  cat(start, dir, get_region(start), "\n")
  cat(new_p, new_d, rot_d, get_region(new_p), "\n")
  cat(end[[1]], end[[2]], get_region(end[[1]]), "\n")
}
#test_round_trip()



for (i in 1:50) {
  stopifnot( test_con(c(1, 50+i), "^") ==  "A -> F" )
  stopifnot( test_con(c(i, 51), "<")  ==  "A -> E" )
  
  stopifnot( test_con(c(1, 100+i), "^") ==  "B -> F" )
  stopifnot( test_con(c(i, 150), ">")  ==  "B -> D" )
  stopifnot( test_con(c(50, 100+i), "v") ==  "B -> C" )
  
  stopifnot( test_con(c(50+i, 51), "<")  ==  "C -> E" )
  stopifnot( test_con(c(50+i, 100), ">")  ==  "C -> B" )
  
  stopifnot( test_con(c(100+i, 100), ">")  ==  "D -> B")
  stopifnot( test_con(c(150, 50+i), "v")  ==  "D -> F")
  
  stopifnot( test_con(c(101, i), "^")  ==  "E -> C")
  stopifnot( test_con(c(100+i, 1), "<")  ==  "E -> A")
  
  stopifnot( test_con(c(200, i), "v")  ==  "F -> B")
  stopifnot( test_con(c(150+i, 1), "<")  ==  "F -> A")
  stopifnot( test_con(c(150+i, 50), ">")  ==  "F -> D")
}










dir = ">"
pos = c(1, which(map[1,] == '.')[1])

repeat {
  if (length(moves) == 0)
    break

  m = moves[1]
  moves = moves[-1]
  
  for (i in seq_len(m)) {
    new = wrap(pos + move[[dir]], dir)
    new_p = new[[1]]
    new_d = new[[2]]
    
    if (map[new_p[1], new_p[2]] == " ")
      stop("Opps - bad wrap")
    
    if (map[new_p[1], new_p[2]] == "#")
      break
    
    map[pos[1], pos[2]] = dir
    pos = new_p
    dir = new_d
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
