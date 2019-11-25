# ordinal regression models with random effects
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
            (1|subject_id) + (1|stim),
        contrasts = list(variant = contr.simp(exp2_reg$variant)),
        data = exp1_reg
    )
}


my_clmm_2 <- function(depvar) {
    clmm(
        formula = depvar ~ variant * voice +
            (1|subject_id) + (1|stim),
        contrasts = list(variant = contr.simp(exp2_reg$variant)),
        data = exp2_reg
    )
}

# map clmm
scales <- colnames(exp1_reg %>% select(feminine:friendly)) %>% set_names
clmms_exp1 <- scales %>% map(~my_clmm_1(exp1_reg[[.]]))
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

scales <- colnames(exp2_reg %>% select(feminine:friendly)) %>% set_names
clmms_exp2 <- scales %>% map(~my_clmm_2(exp2_reg[[.]]))
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
    xtbl1 = xtable(tidied_exp1[[scale]], caption = paste0("E1 model for the `", scale, "' scale."))
    xtbl2 = xtable(tidied_exp2[[scale]], caption = paste0("E2 model for the `", scale, "' scale."))
    print(xtbl1, booktabs = TRUE, include.rownames = FALSE)
    print(xtbl2, booktabs = TRUE, include.rownames = FALSE)
}

# to print all tables
walk(scales, latexify)

scales %>% walk(~latexify(., tidied_exp1))
scales %>% walk(~latexify(., tidied_exp2))
