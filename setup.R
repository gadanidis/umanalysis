library(plyr)
library(tidyverse)
library(RJSONIO)
library(jsonlite)
library(ordinal)
library(broom)

exp1_dat <- readRDS("../results/exp1.rds") %>%
    mutate(gender = fct_recode(gender, "woman" = "female", "man" = "male"),
           variant = condition,
           subject_id = subject,
           stim = item
           )

exp2_path <- "../results/exp2/"
exp2_files <- dir(exp2_path, pattern = "*.csv")

scales <- c( "feminine"
           , "masculine"
           , "young"
           , "queer"
           , "canadian"
           , "intelligent"
           , "hesitant"
           , "polite"
           , "casual"
           , "friendly"
           )

get_variant <- function (condition, stimulus) {
    ifelse(condition == 1,
        ifelse(stimulus == 1 | stimulus == 4, "uh",
            ifelse(stimulus == 2 | stimulus == 5, "um", "neither")
            ),
        ifelse(condition == 2,
            ifelse(stimulus == 1 | stimulus == 4, "neither",
                ifelse(stimulus == 2 | stimulus == 5, "uh", "um")
                ),
            ifelse(stimulus == 1 | stimulus == 4, "um",
                ifelse(stimulus == 2 | stimulus == 5, "neither", "uh")
            )
        )
    )
}


exp2_raw <- exp2_files %>%
    map(~ read_csv(file.path(exp2_path, .))) %>%
    reduce(rbind.fill) %>%
    mutate(voice = replace_na(voice, "r")) %>%
    as_tibble()

afterbrief_exp2 <- exp2_raw %>%
    select( subject_id
          , gender
          , age
          , ethnicity
          , responses
          , voice
          , condition
          , type
          , stim
          ) %>%
    filter(type == "afterbrief") %>%
    mutate(responses = map(responses, ~ fromJSON(.) %>% as_tibble())) %>%
    unnest(responses)

exp2_dat <- exp2_raw %>%
    select( subject_id
          , gender
          , age
          , ethnicity
          , responses
          , voice
          , condition
          , type
          , stim
          ) %>%
    filter(type == "quant-results", stim < 7) %>%
    mutate(responses = map(responses, ~ fromJSON(.) %>% as_tibble())) %>%
    unnest(responses) %>%
    mutate(variant = get_variant(condition, stim)) %>%
    rename(
          feminine    = Q0
        , masculine   = Q1
        , young       = Q2
        , queer       = Q3
        , canadian    = Q4
        , intelligent = Q5
        , hesitant    = Q6
        , polite      = Q7
        , casual      = Q8
        , friendly    = Q9
    ) %>%
    mutate(gender = gender %>% fct_recode( woman = "FALSE"
                                         , woman = "female"
                                         , woman = "Female"
                                         , woman = "female/woman"
                                         , man = "male"
                                         , man = "Male" )
    )

exp2_qual <- exp2_raw %>%
    select( subject_id
          , gender
          , age
          , ethnicity
          , responses
          , voice
          , condition
          , type
          , stim
          ) %>%
    filter(type == "qual-results", stim < 7) %>%
    mutate(responses = map(responses, ~ fromJSON(.) %>% as_tibble())) %>%
    unnest(responses) %>%
    mutate(variant = get_variant(condition, stim)) %>%
    mutate(gender = gender %>% fct_recode( woman = "FALSE"
                                         , woman = "female"
                                         , woman = "Female"
                                         , woman = "female/woman"
                                         , man = "male"
                                         , man = "Male" )
    ) %>%
    filter(Q0 != "")

# fix exp2

exp2_dat <- exp2_dat %>%
    mutate_if(is.integer, ~.+ 1)

# make new datasets for regression

exp1_reg <- exp1_dat %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.numeric, as.factor) %>%
    mutate(variant = fct_relevel(variant, "neither"))

exp2_reg <- exp2_dat %>%
    mutate_if(is.integer, ~.+ 1) %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.numeric, as.factor)

# save datasets

exp1_dat %>% saveRDS("exp1_dat.rds")
exp2_dat %>% saveRDS("exp2_dat.rds")
exp1_reg %>% saveRDS("exp1_reg.rds")
exp2_reg %>% saveRDS("exp2_reg.rds")
