library(plyr)
library(tidyverse)
library(RJSONIO)
library(jsonlite)
library(ordinal)

data_path <- "../results"   # path to the data
files <- dir(data_path, pattern = "*.csv")
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

raw <- files %>%
    map(~ read_csv(file.path(data_path, .))) %>%
    reduce(rbind.fill) %>%
    mutate(voice = replace_na(voice, "r")) %>%
    as_tibble()

afterbrief <- raw %>%
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

dat <- raw %>%
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
