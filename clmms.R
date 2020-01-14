# ordinal regression models with random effects
library(tidyverse)
library(ordinal)
library(xtable)

exp1_reg <- readRDS("exp1_reg.rds")
exp2_reg <- readRDS("exp2_reg.rds")

contr.simp <- function(predictor){
    contrasts(predictor) - (1 / length(levels(predictor)))
}

# my_clmm_2

my_clmm_1 <- function(depvar) {
    clmm(
        formula = depvar ~ variant +
            (1+variant|subject_id) + (1|stim),
        data = exp1_reg
    )
}


my_clmm_2 <- function(depvar) {
    clmm(
        formula = depvar ~ variant * voice +
            (1+variant+voice|subject_id) + (1|stim),
        contrasts = list(voice = contr.simp(exp2_reg$voice)),
        data = exp2_reg
    )
}

# map clmm
scales <- colnames(exp1_reg %>% select(feminine:friendly, -queer)) %>% set_names
clmms_exp1 <- scales %>% map(~my_clmm_1(exp1_reg[[.]]))
# separate out queer model and run without slope (doesn't converge w/ slope)
clmms_exp1$queer <- clmm(queer ~ variant + (1|subject_id) + (1|stim), data = exp1_reg)
summaries_exp1 <- clmms_exp1 %>% map(summary)
tidied_exp1 <- clmms_exp1 %>%
    map(~tidy(., conf.int = TRUE) %>%
        select(-coefficient_type) %>%
        select(term, estimate, conf.low, conf.high, everything()) %>%
        mutate(term = recode(term,
                "variantuh" = "variant = uh",
                "variantum" = "variant = um")
        )
    )

scales <- colnames(exp2_reg %>% select(feminine:friendly, -young)) %>% set_names
clmms_exp2 <- scales %>% map(~my_clmm_2(exp2_reg[[.]]))
# separate out young model and run without voice slope (doesn't converge w/ slope)
clmms_exp2$young <- clmm(young ~ variant * voice + (1+variant|subject_id) + (1|stim), data = exp2_reg)
summaries_exp2 <- clmms_exp2 %>% map(summary)
tidied_exp2 <- clmms_exp2 %>%
    map(~tidy(., conf.int = TRUE) %>%
        select(-coefficient_type) %>%
        select(term, estimate, conf.low, conf.high, everything()) %>%
        mutate(term = recode(term,
                             "variantuh" = "variant = uh",
                             "variantuh:voicer" = "uh x raven",
                             "variantum" = "variant = um",
                             "variantum:voicer" = "um x raven",
                             "voicer" = "voice = raven"),
                interaction = ifelse(grepl("x", term), "B", "A")
        ) %>%
        arrange(interaction) %>%
        select(-interaction)
    )

latexify <- function(scale) {
    xtbl1 = xtable(tidied_exp1[[scale]], 
                   caption = paste0("E1 model for the `", scale, "' scale."),
                   label = paste0("t:",scale,"1"))
    xtbl2 = xtable(tidied_exp2[[scale]],
                   caption = paste0("E2 model for the `", scale, "' scale."),
                   label = paste0("t:",scale,"2"))
    print(xtbl1, booktabs = TRUE, include.rownames = FALSE)
    print(xtbl2, booktabs = TRUE, include.rownames = FALSE)
}

# to print all tables
walk(scales, latexify)

scales %>% walk(~latexify(., tidied_exp1))
scales %>% walk(~latexify(., tidied_exp2))
