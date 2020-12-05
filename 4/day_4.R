input <- scan("./4/input.txt", 
     what = "raw",
     blank.lines.skip = FALSE)

pp_list <- scan(text = paste(ifelse(input=="", "\n", input), collapse=" "), 
                sep = "\n", what = "list") # Hah, PP list

# Part 1: Don't even need a real list
count_valid <- function(x){
  return(sum(stringr::str_count(x, "(byr|iyr|eyr|hgt|hcl|ecl|pid)")==7))
}
count_valid(pp_list)

# Part 2: Need a real list
## This is truly hideous, but it works.
library(tidyverse)
convert_entry <- function(x){
  split_entry <- unlist(strsplit(stringr::str_trim(x), " "))
  setNames(stringr::str_remove(split_entry, "^[a-z]*:"), 
           stringr::str_extract(split_entry, "^[a-z]*"))
}

pp_df <- map_df(pp_list, convert_entry) %>%
  mutate(hgt_unit = str_extract(hgt, "(cm|in)$"),
         hgt      = parse_number(hgt)) %>%
  mutate(val_byr  = str_length(byr)==4 & byr >= 1920 & byr <= 2002,
         val_iyr  = str_length(iyr)==4 & iyr >= 2010 & iyr <= 2020,
         val_eyr  = str_length(eyr)==4 & eyr >= 2020 & eyr <= 2030,
         val_hgt  = (hgt_unit=="cm" & hgt >= 150 & hgt <= 193) | 
                    (hgt_unit=="in" & hgt >= 59 & hgt <=  76),
         val_hcl = str_detect(hcl, "^#[0-9a-f]{6}$"),
         val_ecl = str_detect(ecl, "^(amb|blu|brn|gry|grn|hzl|oth)$"),
         val_pid = str_detect(pid, "^[0-9]{9}$")
  )  %>%
  mutate(incomplete = apply(., 1, function(x){any(is.na(x[names(x)!="cid"]))})) %>%
  mutate(across(starts_with("val"), ~ ifelse(is.na(.), FALSE, .))) %>%
  rowwise() %>%
  mutate(valid = all(c_across(starts_with("val"))) & !incomplete)

sum(pp_df$valid)
