library(tidyverse)

input <- read.delim("./2/input.txt", sep = " ", header = FALSE,
                    col.names = c("policy", "letter", "password")) %>%
  separate(policy, c("min", "max"), convert = TRUE) %>%
  mutate(letter = str_remove(letter, ":")) 

# Part 1
count_letter <- function(x){
  apply(x, 1, FUN = function(x){str_count(x["password"], x["letter"])})
}

input %>%
  mutate(valid = count_letter(.) >= min & count_letter(.) <= max) %>%
  count(valid)

# Part 2
check_positions <- function(x){
  apply(x, 1, function(x){
    return(xor(str_sub(x["password"], x["min"], x["min"]) == x["letter"], 
               str_sub(x["password"], x["max"], x["max"]) == x["letter"]))
  })
}
sum(check_positions(input))

