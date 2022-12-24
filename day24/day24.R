library(tidyverse)

test  = readLines(here::here("day24/test.txt")) 
test2 = readLines(here::here("day24/test2.txt")) 
input = readLines(here::here("day24/input.txt"))


move = list(
  ">" = c( 0, 1),
  "v" = c( 1, 0),
  "<" = c( 0,-1),
  "^" = c(-1, 0)
)

update_state = function(s) {
  s %>%
    mutate(
      delta = move[dir],
      y = y + map_dbl(delta,1),
      x = x + map_dbl(delta,2)
    ) %>% 
    mutate(
      y = case_when(
        y == 0 ~ nr+0,
        y == nr+1 ~ 1,
        TRUE ~ y
      ),
      x = case_when(
        x == 0 ~ nc+0,
        x == nc+1 ~ 1,
        TRUE ~ x
      )
    ) %>%
    select(y,x, dir)
}


print_state = function(d) {
  plot(d$x, -d$y, pch=15, cex=5, asp=1)
}


## Task 1

data = input

board = data %>%
  str_split("") %>%
  do.call(rbind,.) %>%
  {.[-c(1,nrow(.)),]} %>%
  {.[,-c(1,ncol(.))]}

n = pracma::Lcm(nrow(board), ncol(board))

nr = nrow(board)
nc = ncol(board)

state = board %>%
  {which(. != ".", arr.ind=TRUE)} %>%
  as_tibble() %>%
  rename(y=row, x=col) %>%
  mutate(dir = map2_chr(x,y, ~ board[.y,.x]))



psbl = list(state)
for(i in seq_len(n)) {
  psbl[[i+1]] = update_state(psbl[[i]])
}

stopifnot(
  all(psbl[[1]]$y == psbl[[length(psbl)]]$y) &
  all(psbl[[1]]$x == psbl[[length(psbl)]]$x)
)

grid = expand_grid(
  y = 1:nr,
  x = 1:nc
)


wrap = function(x, n) {
  ifelse(x %% n == 0, n, x %% n)
}


edges = map_dfr(
  seq_len(n),
  function(i) {
    if (i %% 50 == 0)
      cat(i,'/',n,'\n')
    avail = map(
      wrap(i + 0:1, n),
      function(j) {
        anti_join(
          grid, psbl[[j]],
          by = c("x", "y")
        ) %>%
          rbind(list(y=c(0,nr+1), x=c(1,nc))) %>%
          mutate(t = j)
      }
    )
    
    avail[[1]] %>%
      rowwise() %>%
      summarize(
        from = paste(y,x,t,sep=","),
        y = y + c(0, 1,-1, 0, 0),
        x = x + c(0, 0, 0, 1,-1),
        t = wrap(t + 1,n)
      ) %>% 
      semi_join(avail[[2]], by=c("y","x","t")) %>%
      summarize(
        from = from,
        to = paste(y,x,t,sep=",")
      )
  }
)

g = edges %>%
  as.matrix() %>%
  igraph::graph_from_edgelist()

start = "0,1,1"
end = paste(nr+1, nc , seq_len(n), sep=",")


z = igraph::shortest_paths(
  g, from=start, to=end
)

mins = map_int(z$vpath, length)

min(mins)-1







## Task 2

i = which.min(mins)
start2 = tail(names(z$vpath[[i]]),1)
end2 = paste(0, 1 , seq_len(n), sep=",")

z2 = igraph::shortest_paths(
  g, from=start2, to=end2
)

mins2 = map_int(z2$vpath, length)
min(mins2)-1



i = which.min(mins2)
start3 = tail(names(z2$vpath[[i]]),1)
end3 = paste(nr+1, nc , seq_len(n), sep=",")

z3 = igraph::shortest_paths(
  g, from=start3, to=end3
)

mins3 = map_int(z3$vpath, length)
min(mins3)-1




min(mins)-1 + min(mins2)-1 + min(mins3)-1