source("setup.R")

# plotting
theme_set(theme_bw(base_size = 18, base_family = "Libertinus Serif") %+replace%
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
names(myColors) <- levels(dat$condition)
colScale <- scale_colour_manual(name = "condition", values = myColors)
fillScale <- scale_fill_manual(name = "condition", values = myColors)

scales <- colnames(dat %>% select(feminine:friendly)) %>% set_names

my_hist <- function(scale) {
    ggplot(dat, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_bar(stat = "count", width = 0.8) +
        facet_grid(voice~variant) +
        geom_vline( data = aggregate( eval(parse(text = scale)) ~ variant
                                    , data = dat
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
hists <- scales %>% map(my_hist)

my_diff <- function(scale) {
    dat %>% group_by(variant, voice, !!sym(scale)) %>%
        summarize(n = n()) %>%
        spread(variant, n) %>%
        mutate(uh = uh - neither, um = um - neither) %>%
        select(uh, um, !!sym(scale)) %>%
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
diffs <- scales %>% map(my_diff)

# ordinal regression models with random effects

helmert <- matrix(c(2/3, -1/3, -1/3, 0, -0.5, 0.5), ncol = 2)
dummy <- matrix(c(-0.5, 0.5, 0, -0.5, 0, 0.5), ncol = 2)
colnames(helmert) <- c("xVhm", "hVm")
colnames(dummy) <- c("xVh", "xVm")

contr.simp <- function(predictor){
    contrasts(predictor) - (1 / length(levels(predictor)))
}

# make new dataset for regression

ordinal_data <- dat %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.numeric, as.factor)

# my_clmm

my_clmm <- function(depvar) {
    clmm(
        formula = depvar ~ variant * voice +
            (1|subject_id) + (1|stim),
        contrasts = list(variant = contr.simp(ordinal_data$variant)),
        data = ordinal_data
    )
}

temp_clmm <- clmm(
    formula = feminine ~ voice * variant + (1|subject_id) + (1|stim),
    contrasts = list(variant = contr.simp(ordinal_data$variant)),
    data = ordinal_data
    )

# map clmm
scales <- colnames(ordinal_data %>% select(feminine:friendly)) %>% set_names
clmms <- scales %>% map(~my_clmm(ordinal_data[[.]]))
summaries <- clmms %>% map(~summary(.))
