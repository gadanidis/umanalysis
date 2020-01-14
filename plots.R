library(tidyverse)
library(RColorBrewer)
library(cowplot)

theme_set(theme_bw(base_size = 18, base_family = "EB Garamond") %+replace%
    theme( strip.background = element_rect(fill = "transparent")
         , panel.background = element_rect(fill = "transparent")
         , plot.background = element_rect(fill = "transparent", color = NA)
         , panel.grid.major = element_blank()
         , panel.grid.minor = element_blank()
         , legend.background = element_rect(fill = "transparent")
         , legend.box.background = element_rect(fill = "transparent")
         )
)

exp1_dat <- readRDS("exp1_dat.rds")
exp2_dat <- readRDS("exp2_dat.rds")

combined_dat <- bind_rows(exp1_dat %>% select(feminine:friendly, variant),
                          exp2_dat %>% select(feminine:friendly, variant, voice)) %>%
    mutate(type = replace_na(voice, "im")) %>%
    mutate(type = recode(type,
                         im = "IM",
                         p = "Penguin",
                         r = "Raven"))


myColors <- brewer.pal(3,"Set2")
names(myColors) <- levels(exp1_dat$variant)
colScale <- scale_colour_manual(name = "variant", values = myColors)
fillScale <- scale_fill_manual(name = "variant", values = myColors)
likertScale <- scale_fill_manual(values = brewer.pal(5, "BrBG"))

scales <- colnames(exp1_dat %>% select(feminine:friendly)) %>% set_names

my_hist <- function(scale, df, other_facet = NULL) {
    ggplot(df, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_bar(stat = "count", width = 0.8) +
        facet_grid(cols = vars(variant), rows = vars({{other_facet}})) +
        labs( y = "count"
            , x = scale
            ) +
        guides(fill = F) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        fillScale
}

my_dense <- function(scale, df, other_facet = NULL) {
    ggplot(df, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_density() +
        facet_grid(cols = vars(variant), rows = vars({{other_facet}})) +
        labs( y = "count"
            , x = scale
            ) +
        guides(fill = F) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        colScale +
        fillScale
}

my_diff <- function(scale, df, other_facet = NULL) {
    df %>% group_by(variant, !!sym(scale), {{other_facet}}) %>%
        summarize(n = n()) %>%
        spread(variant, n) %>%
        mutate(uh = uh - neither, um = um - neither) %>%
        select(uh, um, !!sym(scale)) %>%
        gather(uh, um, {{other_facet}}, key = "variant", value = "cases")# %>%
        #ggplot(aes(x = !!sym(scale) %>% as.numeric, y = cases, fill = variant)) +
        #geom_col(width = 0.8) +
        #geom_hline(yintercept = 0) +
        #facet_grid(cols = vars(variant), rows = vars({{other_facet}})) +
        #labs( y = "difference"
            #, x = scale
            #) +
        #guides(fill = F) +
        #ylim(c(-10,10)) +
        #fillScale
}

my_likert <- function(scale, df) {
    df %>%
        count(variant, !!sym(scale)) %>%
        group_by(variant) %>%
        mutate(pc = n / sum(n)) %>%
        ggplot(aes(x = variant, fill = as.factor(!!sym(scale)), y = pc)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        coord_flip() +
        likertScale +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(fill = scale, y = "proportion")
}
my_likert_voice <- function(scale, df) {
    df %>%
        count(variant, voice, !!sym(scale)) %>%
        group_by(variant, voice) %>%
        mutate(pc = n / sum(n)) %>%
        ggplot(aes(x = variant, fill = as.factor(!!sym(scale)), y = pc)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        facet_grid(rows = vars(voice)) +
        coord_flip() +
        likertScale +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(fill = scale, y = "proportion")
}
my_likert_combined <- function(scale, df) {
    df %>%
        count(variant, type, !!sym(scale)) %>%
        group_by(variant, type) %>%
        mutate(pc = n / sum(n)) %>%
        ggplot(aes(x = variant, fill = as.factor(!!sym(scale)), y = pc)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        facet_grid(rows = vars(type)) +
        coord_flip() +
        likertScale +
        geom_hline(yintercept = 0.5, linetype = "dashed") +
        labs(fill = scale, y = "proportion")
}

exp1_denses <- scales %>% map(my_dense, exp1_dat)
exp2_denses <- scales %>% map(my_dense, exp2_dat, voice)

exp1_hists <- scales %>% map(my_hist, exp1_dat)
exp2_hists <- scales %>% map(my_hist, exp2_dat, other_facet = voice)

exp1_liks <- scales %>% map(my_likert, exp1_dat)
exp2_liks <- scales %>% map(my_likert_voice, exp2_dat)
combined_liks <- scales %>% map(my_likert_combined, combined_dat)

scales %>% walk(~ggsave(
        filename = paste0("figures/exp1/", ., ".png"),
        plot = exp1_liks[[.]],
        width = 6,
        height = 3)
    )

scales %>% walk(~ggsave(
        filename = paste0("figures/exp2/", ., ".png"),
        plot = exp2_liks[[.]],
        width = 6,
        height = 4)
    )

scales %>% walk(~ggsave(
        filename = paste0("figures/combined/", ., ".png"),
        plot = combined_liks[[.]],
        width = 7,
        height = 4)
    )


combined <- map2(exp1_liks, exp2_liks, ~ plot_grid(.x, .y, labels = c('(a) E1', '(b) E2'), nrow = 2))

exp1_dat %>%
    gather("scale", "rating", feminine, masculine, intelligent, hesitant, polite) %>%
    group_by(scale, variant, subject_id) %>%
    summarize(mean_rating = mean(rating)) %>%
    ggplot(aes(x = variant, y = mean_rating)) +
    geom_boxplot() +
    facet_wrap(~scale)

exp2_dat %>%
    gather("scale", "rating", feminine, masculine, intelligent, hesitant, polite) %>%
    group_by(scale, voice, variant, subject_id) %>%
    summarize(mean_rating = mean(rating)) %>%
    ggplot(aes(x = variant, y = mean_rating, fill = voice)) +
    geom_boxplot() +
    facet_wrap(~scale)
