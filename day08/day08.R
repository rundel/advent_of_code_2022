library(tidyverse)

test = readLines(here::here("day08/test.txt"))
input = readLines(here::here("day08/input.txt"))

## Task 1

m = str_split(input, "") %>%
  map(as.numeric) %>%
  do.call(rbind, .)

res = matrix(0, nrow(m), ncol(m))

vis = 0
for (i in 2:(nrow(m)-1)) {
  for (j in 2:(ncol(m)-1)) {
    if ( all(m[i,j] > m[1:(i-1),j]) | 
         all(m[i,j] > m[(i+1):nrow(m),j]) |
         all(m[i,j] > m[i,1:(j-1)]) |
         all(m[i,j] > m[i,(j+1):ncol(m)])
    ) {
      res[i,j] = 1
      vis = vis+1
    }
  }
}

vis + (2*nrow(m)) + 2*(ncol(m)-2)

## Task 2

m = str_split(input, "") %>%
  map(as.numeric) %>%
  do.call(rbind, .)

res = matrix(0, nrow(m), ncol(m))

find_big = function(cur, x) {
  f = which(x >= cur)
  if (length(f) == 0)
    length(x)
  else
    f[1]
}

vis = 0
for (i in 2:(nrow(m)-1)) {
  for (j in 2:(ncol(m)-1)) {
    cur = m[i,j]
    z = c(find_big(cur, rev(m[1:(i-1),j]))  ,
          find_big(cur, m[(i+1):nrow(m),j]) ,
          find_big(cur, rev(m[i,1:(j-1)]))  ,
          find_big(cur, m[i,(j+1):ncol(m)]) )
    
    res[i,j] = prod(z)
  }
}

max(res)
