library(tidyverse)

# Part 1
# Drop into a tibble just to separate rows easily
declarations <- tibble(declarations = read_file("./6/input.txt")) %>%
  separate_rows(declarations, sep = "\n\n") %>%
  pull(declarations) 

count_all_decs <- function(x){
  return(length(unique(unlist(str_split(x, "")))))
}

declarations %>%
  str_remove_all("\n") %>%
  map_int(count_all_decs) %>%
  sum()

# Part 2
count_shared_decs <-  function(x){
  x <- x[x!=""] # Blank from trailing line break
  party_size <- length(x)
  all_decs   <- table(unlist(str_split(paste(x, collapse=""), "")))
  sum(all_decs == party_size)
}

declarations %>%
  str_split("\n") %>%
  map_int(count_decs) %>%
  unlist() %>%
  sum()
