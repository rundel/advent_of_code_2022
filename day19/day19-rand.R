library(tidyverse)

test  = readLines(here::here("day19/test.txt"))
input = readLines(here::here("day19/input.txt"))

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


can_build = function(need, have) {
  have = have[names(need)]
  
  all(have >= need)
}

max_req = function(bp) {
  bind_rows(bp) %>% 
    apply(2,max)
}



mine = function(
  bp, mr,
  res = c(ore=0, clay=0, obs=0, geode=0),
  robot = c(ore=1, clay=0, obs=0, geode=0),
  m = 24
) {
  for (i in 1:m) {
    poss = map_lgl(bp, can_build, have=res) %>%
      {.[.]} %>%
      names()
    
    if ("geode" %in% poss) {
      poss = "geode"
    } else {
      poss = c(poss,"no_move")
    }
    
    if (robot["ore"] >= mr["ore"])
      poss = setdiff(poss, "ore")
    
    if (robot["clay"] >= mr["clay"])
      poss = setdiff(poss, "clay")
    
    probs = c(2,1,2,8,16) %>%
      setNames(c( "no_move", "ore","clay","obs","geode"))
    
    poss = sample(poss, size = 1, prob = probs[ poss ])
    res = res + robot
    if (poss != "no_move") {
        robot[poss] = robot[poss]+1
        res = res - bp[[poss]]
    } 
  }
  
  res["geode"]
}

## Task 1

future::plan(future::multisession, workers = 8)

n_sim = 1e5

blueprints = input %>%
  map(parse)


(df = purrr::imap_dfr(
  blueprints,
  function(bp, i) {
    cat("\nBlueprint", i, "\n")
    mr = max_req(bp)
    z = furrr::future_map_dbl(
      seq_len(n_sim), 
      ~ mine(bp,mr), 
      .progress = TRUE,
      .options = furrr::furrr_options(seed=TRUE)
    )
    
    list(i = i, max = max(z))
  }
) )

df %>%
  summarize(sum(i*max))


## Task 2

future::plan(future::multisession, workers = 10)

n_sim = 3e5

blueprints = test %>%
  map(parse)

blueprints = input %>%
  map(parse) %>%
  .[1:3]

(df = purrr::imap_dfr(
  blueprints,
  function(bp, i) {
    cat("\nBlueprint", i, "\n")
    mr = max_req(bp)
    z = furrr::future_map_dbl(
      seq_len(n_sim), 
      ~ mine(bp,mr, m=32), 
      .progress = TRUE,
      .options = furrr::furrr_options(seed=TRUE)
    )
    
    list(i = i, max = max(z))
  }
) )

df %>%
  summarize(prod(max))


