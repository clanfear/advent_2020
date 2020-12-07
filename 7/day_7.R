# Data prep
# This is somewhat crude
# Want to stick to base in general but tidy string manipulation so convenient here
library(tidyverse)
bags <- tibble(bags = readLines("./7/input.txt")) %>%
  extract(bags, c("bag", "contents"), "^(.*) bags contain (.*)$") %>%
  separate_rows(contents, sep = ", ") %>%
  mutate(n_bags   = parse_number(contents, na = "no other bags."),
         contents = str_remove_all(contents, "(^[0-9] )|( bag(s)?(\\.)?)")) %>%
  filter(!is.na(n_bags))

# Part 1
get_bags <- function(bag){
    return(bags$bag[bags$contents %in% bag])
}

input         <- "shiny gold"
growing_vec   <- character(0)
while(length(input) > 0) {
  output      <- get_bags(input)
  growing_vec <- c(output, growing_vec) # Growing vectors is bad
  input       <- output
}
length(unique(growing_vec))

# Part 2
# Recursive counting
get_bags_count <- function(bag){
  current_bags <- bags[bags$bag == bag,]
  bag_count    <- 1 + unlist(lapply(current_bags$contents, get_bags_count))
  return(sum(current_bags$n_bags * bag_count))
}
get_bags_count("shiny gold")
