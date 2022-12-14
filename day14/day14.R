library(tidyverse)

test  = readLines(here::here("day14/test.txt"))
input = readLines(here::here("day14/input.txt"))


print_board = function(board) {
  apply(board, 1, paste, collapse="") %>%
    paste(collapse="\n") %>%
    cat("\n\n")
}

## Task 1

data = test

lines = str_split(data," -> ") %>%
  map(
    function(x) {
      str_split(x, ",") %>%
        map(as.numeric) %>%
        do.call(rbind, .)
    }
  )

find_min = function(x, index, func=min) {
  min_x = map_dbl(
    x,
    ~ func(.x[,index])
  ) %>% func()
}

min_x = find_min(lines, 1)
min_y = 0 #find_min(lines, 2) - 10

max_x = find_min(lines, 1, max) 
max_y = find_min(lines, 2, max)

board = matrix(".", ncol=max_x-min_x+1, nrow = max_y-min_y+1)

for (line in lines) {
  x = line - matrix(c(min_x-1, min_y-1), ncol=2, nrow=nrow(line), byrow=TRUE)
  
  for(i in 2:nrow(line)) {
    board[
      x[i-1,2]:x[i,2],
      x[i-1,1]:x[i,1]
    ] = "#"
  }
}

sand_x = 500 - min_x + 1
sand_y = 0 + 1

i = 0
repeat {
  x = sand_x
  y = sand_y
  
  repeat {
    if (board[y+1,x] != '.') {
      if (board[y+1,x-1] == '.') {
        x=x-1
      } else if (board[y+1,x+1] == '.') {
        x=x+1
      } else {
        board[y,x] = 'O'
        break
      }
    }
    
    y = y+1
  }
  i = i+1
}

# Error -> done

print_board(board)

sum(board == "O")

## Task 2

data = input

lines = str_split(data," -> ") %>%
  map(
    function(x) {
      str_split(x, ",") %>%
        map(as.numeric) %>%
        do.call(rbind, .)
    }
  )

find_min = function(x, index, func=min) {
  min_x = map_dbl(
    x,
    ~ func(.x[,index])
  ) %>% func()
}

min_x = find_min(lines, 1) - 150
min_y = 0 #find_min(lines, 2) - 10

max_x = find_min(lines, 1, max) + 100
max_y = find_min(lines, 2, max) + 2

board = matrix(".", ncol=max_x-min_x+1, nrow = max_y-min_y+1)

for (line in lines) {
  x = line - matrix(c(min_x-1, min_y-1), ncol=2, nrow=nrow(line), byrow=TRUE)
  
  for(i in 2:nrow(line)) {
    board[
      x[i-1,2]:x[i,2],
      x[i-1,1]:x[i,1]
    ] = "#"
  }
}

board[nrow(board),] = "#"




sand_x = 500 - min_x + 1
sand_y = 0 + 1

i = 0
repeat {
  x = sand_x
  y = sand_y
  
  repeat {
    if (y == nrow(board)) {
      board[y,x] = 'O'
      break
    }
    if (board[y+1,x] != '.') {
      if (x-1 >= 1 && board[y+1,x-1] == '.') {
        x=x-1
      } else if (x+1 <= ncol(board) && board[y+1,x+1] == '.') {
        x=x+1
      } else {
        if (x == sand_x & y == sand_y) {
          print_board(board)
          stop("Done")
        }
        
        board[y,x] = 'O'
        break
      }
    }
    
    y = y+1
    
  }
  i = i+1
  
  if (i %% 50 == 0) {
    print_board(board)
  }
  
  if (x < 1 | x > ncol(board))
    break
  
  if (y < 1 | y > nrow(board))
    break
}

# Error -> done

sum(board == "O")+1 # for the top

