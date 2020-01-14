library(plyr)
library(tidyverse)
library(RJSONIO)

exp1_dat <- readRDS("exp1_dat.rds")
exp2_dat <- readRDS("exp2_dat.rds")

# gender

exp2_dat %>%
    group_by(gender, voice) %>%
    distinct(subject_id) %>%
    summarize(n = n()) %>%
    spread(gender, n)

exp2_dat %>%
    group_by(gender) %>%
    distinct(subject_id) %>%
    summarize(n = n())

exp1_dat %>%
    mutate(age = as.numeric(age)) %>%
    summarize(mean = mean(age),
              median = median(age),
              max = max(age),
              min = min(age),
              sd = sd(age)
              )

exp2_dat %>%
    distinct(subject_id, .keep_all = T) %>%
    group_by(voice) %>%
    summarize(mean = mean(age),
              median = median(age),
              max = max(age),
              min = min(age),
              sd = sd(age)
              )

sd(exp2_dat$age)
# [1] 5.271388
