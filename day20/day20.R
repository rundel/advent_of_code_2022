library(tidyverse)

test  = readLines(here::here("day20/test.txt")) %>% as.numeric()
test2  = readLines(here::here("day20/test2.txt")) %>% as.numeric()
input = readLines(here::here("day20/input.txt")) %>% as.numeric()


## Task 1


wrap_index = function(i, n) {
  ifelse(
    i %% n == 0,
    n,
    i %% n
  )
}

rotate_left = function(v, i) {
  stopifnot(i >= 0)
  
  i = i %% length(v)
  
  if (i == 0)
    v
  else
    c(v[(i+1):length(v)], v[1:i])
}

rotate_right = function(v, i) {
  stopifnot(i >= 0)
  
  n = length(v)
  i = i %% n
  
  if (i == 0)
    v
  else
    c(v[(n-i+1):n], v[1:(n-i)])
}

rotate = function(v, i) {
  if (i >= 0) {
    rotate_right(v, i)
  } else {
    rotate_left(v, abs(i))
  }
}


data = input

d = imap(data, ~ c(val=.x,ord=.y))

mix = function(d, n = 1) {
  for (j in seq_len(n)) {
    for (i in seq_along(data)) {
      cur = map_dbl(d, "ord") %>%
        {which(. == i)}
      
      d = rotate(d, -(cur-1))
      
      val = d[[1]]
      d = rotate(
        d[-1],
        -val[1]
      )
      
      d[[length(d)+1]] = val
      
      cat(j, i, "\n")
    }
  }
  
  cur = map_dbl(d, 1L) %>%
    {which(. == 0)}
  d = rotate(d, -(cur-1))
  
  x1 = rotate(d,-1000)[[1]]
  x2 = rotate(d,-2000)[[1]]
  x3 = rotate(d,-3000)[[1]]
  
  x1[1] + x2[1] + x3[1]
}

mix(d)


## Task 2

data = input

d = imap(
  data, 
  ~ c(val=.x * 811589153, ord=.y))

(z = mix(d, 10))

z %>% formatC(20)