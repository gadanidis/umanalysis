library(plyr)
library(tidyverse)
library(RJSONIO)

source("setup.R")

exp2_dat %>% group_by(voice, condition) %>% distinct(subject_id) %>% summarize(n = n())
# # A tibble: 6 x 3
# # Groups:   voice [2]
#   voice condition     n
#   <chr>     <dbl> <int>
# 1 p             1    23
# 2 p             2    23
# 3 p             3    23
# 4 r             1    24
# 5 r             2    23
# 6 r             3    22

# SCHEDULING

# [ ] r3
# [ ] r2
# [ ] r3
# [ ] p1
# [ ] p2
# [ ] p3

# [ ] p1
# [ ] p1
# [ ] p2
# [ ] p2
# [ ] p3
# [ ] p3

# [ ] r1
# [ ] r1
# [ ] r2
# [ ] r2
# [ ] r3
# [ ] r3

# DONE

# gender

exp2_dat %>%
    group_by(gender, voice) %>%
    distinct(subject_id) %>%
    summarize(n = n())

exp2_dat %>%
    group_by(gender) %>%
    distinct(subject_id) %>%
    summarize(n = n())

summary(dat$age)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   16.00   19.00   20.00   21.85   22.00   54.00 

dat %>%
    distinct(subject_id, .keep_all = T) %>%
    group_by(voice) %>%
    summarize(mean = mean(age),
              median = median(age),
              max = max(age),
              min = min(age)
              )
