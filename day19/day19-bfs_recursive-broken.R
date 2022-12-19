library(tidyverse)

test  = readLines(here::here("day19/test.txt"))
input = readLines(here::here("day19/input.txt"))

## Task 1

fix_missing = function(x) {
  n = c("ore","clay","obs","geode")
  x[setdiff(n, names(x))] = 0
  x[n]
}

parse = function(x) {
  list(
    ore = str_match(x, "Each ore robot costs (\\d+) ore")[,2] %>% 
      as.numeric() %>% 
      setNames("ore"),
    clay = str_match(x, "Each clay robot costs (\\d+) ore")[,2] %>% 
      as.numeric() %>% 
      setNames("ore"),
    obs = str_match(x, "Each obsidian robot costs (\\d+) ore and (\\d+) clay")[,2:3] %>% 
      as.numeric() %>% 
      setNames(c("ore","clay")),
    geode = str_match(x, "Each geode robot costs (\\d+) ore and (\\d+) obsidian")[,2:3] %>% 
      as.numeric() %>% 
      setNames(c("ore","obs"))
  ) %>%
    map(fix_missing)
}





blueprints = test %>%
  map(parse)

can_build = function(need, have) {
  have = have[names(need)]
  
  all(have >= need)
}

max_req = function(bp) {
  bind_rows(bp) %>% 
    apply(2,max)
}



mine = function(
  m = 1,
  res = c(ore=0, clay=0, obs=0, geode=0),
  robot = c(ore=1, clay=0, obs=0, geode=0)
) {
  if (m > 24)
    return(res["geode"])
  
  poss = map_lgl(bp, can_build, have=res) %>%
    {.[.]} %>%
    names()
  
  if ("geode" %in% poss)
    poss = "geode"
  
  if (robot["ore"] >= mr["ore"])
    poss = setdiff(poss, "ore")

  if (robot["clay"] >= mr["clay"])
    poss = setdiff(poss, "clay")
  
  new_res = res + robot
  
  #cat("Minute:",m,"\n")
  #cat("Poss:",poss,"\n")
  
  opts = map_dbl(
    poss,
    function(build) {
      new_robot = robot
      new_robot[build] = new_robot[build]+1
      new_res = res + robot - bp[[build]]
    
      mine(m=m+1, res=new_res, robot = new_robot)
    }
  )

  opts[length(opts)+1] = mine(m=m+1, res=new_res, robot = robot) # don't do anything
  
  return(max(opts))
}

bp = blueprints[[1]]
( z = memoise::memoise(mine, cache = cachem::cache_mem(2048 * 1024^2))() )





### Implement BFS

queue = list(
  list(
    m = 1,
    res = c(ore=0, clay=0, obs=0, geode=0),
    robot = c(ore=1, clay=0, obs=0, geode=0)
  )
)

best = 0
max_size = 2000

bp = blueprints[[1]]

mr = max_req(bp)

j = 1
repeat {
  mins = map_dbl(queue, "m")
  n_geode = map_dbl(queue, c("res","geode"))
  
  best <<- max(best, n_geode[mins == 24])
  queue = queue[mins != 24]
  
  if (length(queue) == 0) {
    break
  }
  
  if (length(queue) > 1.5*max_size) {
    i = map(queue, "res") %>%
      bind_rows() %>%
      mutate(
        i = 1:n(),
        m = mins
      ) %>%
      arrange(mins, desc(geode), desc(obs), desc(clay), desc(ore)) %>%
      pull(i)
      
      order()
    
    queue = queue[ i[1:max_size] ]
  }
  
  
  if (j %% 500 == 0) {
    cat("Minutes:", unique(mins), "\n")
    cat("len(queue) = ", length(queue),"\n")
    print(head(queue,3) %>% str())
  }
  
  
  res = do.call(mine, queue[[1]])
  queue = c(queue[-1], res)
  #print(str(queue))
  #cat("\n\n")
  
  j = j+1
}

