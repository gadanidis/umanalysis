library(Gifi)
library(lme4)
library(lmerTest)

exp1_reg <- readRDS("exp1_reg.rds")
exp2_reg <- readRDS("exp2_reg.rds")

exp1_anal <- exp1_reg %>%
    select(feminine:friendly) %>%
    mutate_all(as.factor) %>%
    as.data.frame()

exp2_anal <- exp2_reg %>%
    select(feminine:friendly) %>%
    mutate_all(as.factor) %>%
    as.data.frame()

exp1_princal <- exp1_anal %>% princals(ndim = 3)
summary(exp1_princal)
plot(exp1_princal, plot.type = "screeplot")

exp2_princal <- exp2_anal %>% princals(ndim = 3)
summary(exp2_princal)
plot(exp2_princal, plot.type = "screeplot")

#
exp1_scores <- exp1_princal$objectscores %>% as_tibble
exp1_reg$variant <- fct_relevel(exp1_reg$variant, "neither", "uh", "um")
exp1_reg <- exp1_reg %>% cbind(exp1_scores) %>% as_tibble()

exp2_scores <- exp2_princal$objectscores %>% as_tibble
exp2_reg$variant <- fct_relevel(exp2_reg$variant, "neither", "uh", "um")
exp2_reg <- exp2_reg %>% cbind(exp2_scores) %>% as_tibble()

# factors as dependent vars

my_lmer <- function(component) {
    lmer(
        component ~ variant + (1|subject_id) + (1|stim),
        data = exp1_reg)
}

my_lmer_2 <- function(component) {
    lmer(
        component ~ variant * voice + (1|subject_id) + (1|stim),
        data = exp2_reg)
}

exp1_components <- colnames(exp1_reg %>% select(D1:D3)) %>% set_names
exp1_lmers <- exp1_components %>% map(~my_lmer(exp1_reg[[.]]))
exp1_summaries <- lmers %>% map(~summary(.))

exp2_components <- colnames(exp2_reg %>% select(D2:D3)) %>% set_names
exp2_lmers <- exp2_components %>% map(~my_lmer_2(exp2_reg[[.]]))
exp2_summaries <- lmers %>% map(~summary(.))
