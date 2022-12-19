library(tidyverse)

test  = readLines(here::here("day17/test.txt")) %>%
  str_split("") %>%
  .[[1]]
input = readLines(here::here("day17/input.txt")) %>%
  str_split("") %>%
  .[[1]]


shapes = list(
  matrix(
    c(0,0, 1,0, 2,0, 3,0),
    byrow=TRUE, ncol=2
  ),
  matrix(
    c(
           1,2,
      0,1, 1,1, 2,1,
           1,0
    ), byrow=TRUE, ncol=2
  ),
  matrix(
    c( 
                2,2,
                2,1,
      0,0, 1,0, 2,0
    ),
    byrow=TRUE, ncol=2
  ),
  matrix(
    c(
      0,3,
      0,2,
      0,1,
      0,0
    ),
    byrow=TRUE, ncol=2
  ),
  matrix(
    c(
      0,1, 1,1,
      0,0, 1,0
    ),
    byrow=TRUE, ncol=2
  )
) %>%
  map(
    function(m) {
      m[,2] = -m[,2]
      m
    }
  )

find_stack_top = function(b) {
  sub = apply(b, 1, function(x) any(x != "."))
  min(which(sub)[1], nrow(b)+1, na.rm = TRUE)
}

trim_board = function(b) {
  top = find_stack_top(b) 
  b[top:nrow(b),]
}

move_shape = function(shp, x, y) {
  shp[,1] = shp[,1] + x
  shp[,2] = shp[,2] + y
  shp
}

set_vals = function(b, shp, val="#") {
  apply(shp, 1, function(x) b[x[2],x[1]] <<-val)
  b
}

get_vals = function(b, shp) {
  apply(shp, 1, function(x) b[x[2],x[1]])
}


blow_shp = function(shp, b, dir,x_min=1, x_max=7) {
  if (dir == ">") {
    new_shp = move_shape(shp, 1, 0)
  } else if (dir == "<") {
    new_shp = move_shape(shp, -1, 0)
  }
  
  if (all(new_shp[,1] %in% x_min:x_max)) {
    if (any(get_vals(b, new_shp) != ".")) {
      shp
    } else { 
      new_shp 
    }
  } else {
    shp
  }
}

drop_shp = function(shp, b) {
  new_shp = move_shape(shp, 0, 1)
  
  if (any(new_shp[,2] > nrow(b))) {
    shp
  } else if (any(get_vals(b, new_shp) != ".")) {
    shp
  } else {
    new_shp
  }
}




## Task 1

wind = input
wind = test

n = 2022 #length(wind) * length(shapes)

do_sim = function(wind, n) {
  p = progress_estimated(n)
  max_board_rows = ceiling(n/5) * 13
  board = matrix(".", nrow=max_board_rows, ncol=7)
  
  j = 1
  s = 1
  
  res = numeric()
  
  for (i in 1:n) {
    x = 3
    y = find_stack_top(board) - 4
    
    cur = move_shape(shapes[[s]], x, y)
    
    #print(trim_board(set_vals(board, cur, "@")))
    #cat("\n")
    
    repeat {
      cur = blow_shp(cur, board, wind[j])
      j = j+1
      if (j > length(wind))
        j = 1
      
      drop = drop_shp(cur, board)
      if (identical(drop, cur))
        break
      
      cur = drop
      
      #print(trim_board(set_vals(board, cur, "@")))
      #cat("\n")
    }
    
    board = set_vals(board, cur)
    
    s = s+1
    if (s > length(shapes))
      s=1
    
    res = c(res, trim_board(board) %>% nrow())
    p$tick()$print()
  }
  
  list(
    res = res,
    board = board
  )
}


d = do_sim(wind = input, n = 10000)

find_cycle = function(res, range=seq(5,2000,1)) {
  for (x in range) {
    n = res[seq(x,length(res), by=x)] %>%
      diff() %>%
      unique() %>%
      length()
    
    if (n == 1) {
      return(x)
    }
  }
} 

cyc = find_cycle(d$res)


inc = d$res[2*cyc] - d$res[cyc]

extra = 1000000000000 %% cyc

((floor(1000000000000/cyc) -1) * inc + d$res[cyc] + (d$res[cyc+extra]-d$res[cyc])-1) %>%
  formatC(digits=16)

