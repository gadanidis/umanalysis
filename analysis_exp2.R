source("setup.R")
library(xtable)

# plotting
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

library(RColorBrewer)
myColors <- brewer.pal(3,"Set2")
names(myColors) <- levels(exp2_dat$variant)
colScale <- scale_colour_manual(name = "variant", values = myColors)
fillScale <- scale_fill_manual(name = "variant", values = myColors)

scales <- colnames(exp2_dat %>% select(feminine:friendly)) %>% set_names

my_hist <- function(scale) {
    ggplot(exp2_dat, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_bar(stat = "count", width = 0.8) +
        facet_grid(voice~variant) +
        geom_vline( data = aggregate( eval(parse(text = scale)) ~ variant
                                    , data = exp2_dat
                                    , mean
                                    ) %>% set_names("variant", "the_mean")
                  , mapping = aes(xintercept = the_mean)
                  , colour = "#555555"
                  , linetype = "dashed"
                  , size = 0.5
                  ) +
        labs( y = "count"
            , x = scale
            ) +
        guides(fill = F) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        fillScale
}
exp2_hists <- scales %>% map(my_hist)

my_dense <- function(scale) {
    ggplot(exp2_dat, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_density() +
        facet_grid(~voice) +
        geom_vline( data = aggregate( eval(parse(text = scale)) ~ variant
                                    , data = exp2_dat
                                    , mean
                                    ) %>% set_names("variant", "the_mean")
                  , mapping = aes(xintercept = the_mean, colour = variant)
                  , size = 1
                  ) +
        labs( y = "count"
            , x = scale
            ) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        fillScale
}
exp2_denses <- scales %>% map(my_dense)

my_diff <- function(scale) {
    exp2_dat %>% group_by(variant, voice, !!sym(scale)) %>%
        summarize(n = n()) %>%
        spread(variant, n) %>%
        mutate(uh = uh - neither, um = um - neither) %>%
        select(uh, um, !!sym(scale), voice) %>%
        gather(uh, um, key = "variant", value = "cases") %>%
        ggplot(aes(x = !!sym(scale) %>% as.numeric, y = cases, fill = variant)) +
        geom_col(width = 0.8) +
        geom_hline(yintercept = 0) +
        facet_grid(voice~variant) +
        labs( y = "difference"
            , x = scale
            ) +
        guides(fill = F) +
        ylim(c(-10,10)) +
        fillScale
}
exp2_diffs <- scales %>% map(my_diff)

# ordinal regression models with random effects

helmert <- matrix(c(2/3, -1/3, -1/3, 0, -0.5, 0.5), ncol = 2)
dummy <- matrix(c(-0.5, 0.5, 0, -0.5, 0, 0.5), ncol = 2)
colnames(helmert) <- c("xVhm", "hVm")
colnames(dummy) <- c("xVh", "xVm")

contr.simp <- function(predictor){
    contrasts(predictor) - (1 / length(levels(predictor)))
}

# my_clmm

my_clmm <- function(depvar) {
    clmm(
        formula = depvar ~ variant * voice +
            (1|subject_id) + (1|stim),
        contrasts = list(variant = contr.simp(exp2_reg$variant)),
        data = exp2_reg
    )
}

# map clmm
scales <- colnames(exp2_reg %>% select(feminine:friendly)) %>% set_names
clmms_exp2 <- scales %>% map(~my_clmm(exp2_reg[[.]]))
summaries_exp2 <- clmms_exp2 %>% map(summary)
tidied_exp2 <- clmms_exp2 %>%
    map(~tidy(., conf.int = TRUE) %>%
        select(-coefficient_type) %>%
        select(term, estimate, conf.low, conf.high, everything()))

latexify <- function(scale, tbls) {
    xtbl = xtable(tbls[[scale]], caption = paste0("Model for the `", scale, "' scale."))
    print(xtbl, booktabs = TRUE, include.rownames = FALSE)
}

# to print all tables
scales %>% walk(~latexify(., tidied_exp2))
