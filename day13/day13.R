library(tidyverse)

test  = readLines(here::here("day13/test.txt"))
input = readLines(here::here("day13/input.txt"))

test2 = c("[[],[1,2,3]]", "[[],[1,2,4]]", "", "[[],[1,2,5]]", "[[],[1,2,4]]")

## Task 1

data = input

n = ceiling(length(data) / 3)

parse_obj = function(x) {
  str_replace_all(x, "\\[", "list(") %>%
    str_replace_all("\\]", ")") %>%
    parse(text=.) %>%
    eval()
}

d = map(
  seq(1, length(data), by=3),
  ~ data[.x + 0:1]
) %>%
  map(
    ~ list(
        s1 = parse_obj(.x[1]),
        s2 = parse_obj(.x[2])
    )
  )


comp = function(s1, s2) {
  
  #cat("s1:", str(s1), "\n")
  #cat("s2:", str(s2), "\n")
  
  for (i in seq_len(max(length(s1), length(s2)))) {
    res = "equal"
    if (i > length(s1)) {
      return("right")
    } else if (i > length(s2)) {
      return("wrong")
    } else if (is.numeric(s1[[i]]) & is.numeric(s2[[i]])) {
      cat("nn\n")
      cat("Compare ",s1[[i]], s2[[i]],"\n")
      if (s1[[i]] < s2[[i]]) {
        return("right")
      } else if (s1[[i]] > s2[[i]]) {
        return("wrong")
      }
    } else if (is.list(s1[[i]]) & is.list(s2[[i]])) {
      cat("ll\n")
      res = comp(s1[[i]], s2[[i]])
    } else if (is.list(s1[[i]])) {
      cat("ln\n")
      res = comp(s1[[i]], list(s2[[i]]))
    } else if (is.list(s2[[i]])) {
      cat("nl\n")
      res = comp(list(s1[[i]]), s2[[i]])
    } else {
      stop("opps - shouldnt be here")
    }
    
    if (res != "equal") {
      return(res)
    }
  }

  return("equal")
}

ans = imap_chr(
  d,
  ~ {cat("\n\nPair ",.y,"\n");comp(.x[[1]],.x[[2]])}
) 

ans %>%
  {which(. == "right")} %>%
  sum()


ans


## Task 2

data=input

d2 = map(
  seq(1, length(data), by=3),
  ~ data[.x + 0:1]
) %>%
  unlist() %>%
  c("[[2]]", "[[6]]") %>%
  map(parse_obj)

`[.advent` <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "advent"
  x
}

`>.advent` <- function(a,b) {
  if (comp(a[[1]],b[[1]]) == "right") {
    FALSE
  } else {
    TRUE
  }
}

`<.advent` <- function(a,b) {
  if (comp(a[[1]],b[[1]]) == "right") {
    TRUE
  } else {
    FALSE
  }
}

`==.advent` <- function(a,b) {
  FALSE
}

class(d2) = "advent"
sd2 = sort(d2)

map_lgl(sd2, identical, list(list(2))) %>% which() *
map_lgl(sd2, identical, list(list(6))) %>% which() 
