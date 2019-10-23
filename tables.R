source("setup.R")

# gender

dat %>%
    group_by(gender, voice) %>%
    distinct(subject_id) %>%
    summarize(n = n())

dat %>%
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
