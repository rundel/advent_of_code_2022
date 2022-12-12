library(tidyverse)

test  = readLines(here::here("day12/test.txt"))
input = readLines(here::here("day12/input.txt"))

## Task 1

m = str_split(input, "") %>%
  do.call(rbind, .)

check_letter_diff = function(x,y) {
  if (x == "S") {
    0
  } else if (y == "E") {
    which("z" == letters) - which(x == letters) 
  } else if (x == "E" | y == "S") {
    100
  } else {
    which(y == letters) - which(x == letters) 
  }
}

el = list()

for(i in 1:nrow(m)) {
  for(j in 1:ncol(m)) {
    if (i > 1) {
      if (check_letter_diff(m[i,j], m[i-1,j]) <= 1)
        el = c(el, list(c(paste(i,j), paste(i-1,j)))) 
    }
    if (j > 1) {
      if (check_letter_diff(m[i,j], m[i,j-1]) <= 1)
        el = c(el, list(c(paste(i,j), paste(i,j-1))))
    }
    if (i < nrow(m)) {
      if (check_letter_diff(m[i,j], m[i+1,j]) <= 1)
        el = c(el, list(c(paste(i,j), paste(i+1,j))))
    }
    if (j < ncol(m)) {
      if (check_letter_diff(m[i,j], m[i,j+1]) <= 1)
        el = c(el, list(c(paste(i,j), paste(i,j+1))))
    }
  }
}

start = which(m == "S", arr.ind = TRUE) %>%
  paste(collapse=" ")

end = which(m == "E", arr.ind = TRUE) %>%
  paste(collapse=" ")

el %>%
  do.call(rbind, .) %>%
  igraph::graph_from_edgelist(directed = TRUE) %>%
  igraph::shortest_paths(from = start, to = end) %>%
  {length(.$vpath[[1]])-1}


## Task 2

a_start = which(m == "a", arr.ind = TRUE) %>%
  apply(1, paste, collapse=" ") 
gr = el %>%
  do.call(rbind, .) %>%
  igraph::graph_from_edgelist(directed = TRUE)

z = imap_int(
  a_start,
  function(s, i) {
    cat(i, length(a_start), "\n")
    igraph::shortest_paths(gr, from = s, to = end) %>%
      {length(.$vpath[[1]])-1L}
  }
)


