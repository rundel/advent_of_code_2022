library(tidyverse)

test = readLines(here::here("day07/test.txt"))
input = readLines(here::here("day07/input.txt"))

## Task 1

cur_dir = "root"
listing = FALSE

res = list()

for (line in input) {
  if (grepl("^\\$", line)) { # running command
    listing = FALSE
  }
  
  if (!listing) {
    if (grepl("\\$ cd", line)) {
      new_dir = str_match(line, "\\$ cd ([a-z]+|/|\\.\\.)")[,2]
      if (new_dir == "/") cur_dir = "root"
      else if (new_dir == "..") cur_dir = cur_dir[-length(cur_dir)]
      else cur_dir = c(cur_dir, new_dir)
      
    }
      
    else  if (grepl("\\$ ls",line)) {
      res[[cur_dir]] = list()
      listing = TRUE
    }
  } else {
    if (str_detect(line, "(\\d+) (.*)")) {
      file = str_match(line, "(\\d+) (.*)")
      res[[cur_dir]]$name_ = c(res[[cur_dir]]$name, file[,3])
      res[[cur_dir]]$size_ = c(res[[cur_dir]]$size, file[,2] |> as.numeric())
    } else {
      d = str_match(line, "dir (.*)")[,2]
      res[[cur_dir]]$dir_ = c(res[[cur_dir]]$dir, d)
    }
      
  }
  
  #print(cur_dir)
}

count = function(node, res = numeric(), path="root") {
  subnodes = names(node) %>%
    {.[!str_detect(.,"_")]}
  
  new_res = map(
    subnodes,
    function(n) {
      count(node[[n]], res, path = file.path(path, n))
    }
  )
  
  new_tots = map_dbl(new_res, "total")
  new_res = map(new_res, "res") %>%
    unlist()
  
  total = sum(node$size_) + sum(new_tots)
  new_res[path] = total
  
  list(
    total =  total,
    res = new_res
  )
}

totals = count(res$root)$res

totals %>%
   {.[. < 100000]} %>%
   sum()

## Task 2

totals[totals > (30000000 - (70000000 - totals["root"]))] %>%
  sort() %>%
  .[1:3]
