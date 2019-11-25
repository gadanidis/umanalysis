source("setup.R")

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
names(myColors) <- levels(exp1_dat$variant)
colScale <- scale_colour_manual(name = "variant", values = myColors)
fillScale <- scale_fill_manual(name = "variant", values = myColors)

scales <- colnames(exp1_dat %>% select(feminine:friendly)) %>% set_names

my_hist <- function(scale) {
    ggplot(exp1_dat, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_bar(stat = "count", width = 0.8) +
        facet_grid(~variant) +
        geom_vline( data = aggregate( eval(parse(text = scale)) ~ variant
                                    , data = exp1_dat
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
exp1_hists <- scales %>% map(my_hist)

my_dense <- function(scale) {
    ggplot(exp1_dat, aes(x = !!sym(scale) %>% as.numeric, fill = variant)) +
        geom_density() +
        geom_vline( data = aggregate( eval(parse(text = scale)) ~ variant
                                    , data = exp1_dat
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
exp1_denses <- scales %>% map(my_dense)

my_diff <- function(scale) {
    exp1_dat %>% group_by(variant, !!sym(scale)) %>%
        summarize(n = n()) %>%
        spread(variant, n) %>%
        mutate(uh = uh - neither, um = um - neither) %>%
        select(uh, um, !!sym(scale)) %>%
        gather(uh, um, key = "variant", value = "cases") %>%
        ggplot(aes(x = !!sym(scale) %>% as.numeric, y = cases, fill = variant)) +
        geom_col(width = 0.8) +
        geom_hline(yintercept = 0) +
        facet_grid(~variant) +
        labs( y = "difference"
            , x = scale
            ) +
        guides(fill = F) +
        ylim(c(-10,10)) +
        fillScale
}
exp1_diffs <- scales %>% map(my_diff)

# ordinal regression models with random effects

contr.simp <- function(predictor){
    contrasts(predictor) - (1 / length(levels(predictor)))
}

# my_clmm

my_clmm <- function(depvar) {
    clmm(
        formula = depvar ~ variant +
            (1|subject_id) + (1|stim),
        contrasts = list(variant = contr.simp(exp1_reg$variant)),
        data = exp1_reg
    )
}

# map clmm
scales <- colnames(exp1_reg %>% select(feminine:friendly)) %>% set_names
clmms_exp1 <- scales %>% map(~my_clmm(exp1_reg[[.]]))
summaries_exp1 <- clmms_exp1 %>% map(summary)
tidied_exp1 <- clmms_exp1 %>%
    map(~tidy(., conf.int = TRUE) %>%
        select(-coefficient_type) %>%
        select(term, estimate, conf.low, conf.high, everything()))

latexify <- function(scale, tbls) {
    xtbl = xtable(tbls[[scale]], caption = paste0("Model for the `", scale, "' scale."))
    print(xtbl, booktabs = TRUE, include.rownames = FALSE)
}

# to print all tables
scales %>% walk(~latexify(., tidied_exp1))
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -3.09 & -3.67 & -2.51 & 0.30 & -10.46 & 0.00 \\ 
#   2$|$3 & -1.06 & -1.50 & -0.62 & 0.22 & -4.73 & 0.00 \\ 
#   3$|$4 & 0.91 & 0.48 & 1.35 & 0.22 & 4.10 & 0.00 \\ 
#   4$|$5 & 3.13 & 2.55 & 3.71 & 0.30 & 10.52 & 0.00 \\ 
#   variantuh & 0.02 & -0.39 & 0.43 & 0.21 & 0.09 & 0.93 \\ 
#   variantum & 0.71 & 0.29 & 1.13 & 0.21 & 3.33 & 0.00 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `feminine' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -3.75 & -4.47 & -3.04 & 0.37 & -10.27 & 0.00 \\ 
#   2$|$3 & -1.58 & -2.10 & -1.07 & 0.26 & -6.01 & 0.00 \\ 
#   3$|$4 & 0.93 & 0.43 & 1.43 & 0.26 & 3.63 & 0.00 \\ 
#   4$|$5 & 3.16 & 2.54 & 3.79 & 0.32 & 9.93 & 0.00 \\ 
#   variantuh & 0.32 & -0.10 & 0.74 & 0.22 & 1.47 & 0.14 \\ 
#   variantum & -0.70 & -1.13 & -0.27 & 0.22 & -3.19 & 0.00 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `masculine' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -3.70 & -4.49 & -2.91 & 0.40 & -9.15 & 0.00 \\ 
#   2$|$3 & -1.43 & -2.08 & -0.78 & 0.33 & -4.29 & 0.00 \\ 
#   3$|$4 & 0.16 & -0.48 & 0.80 & 0.33 & 0.50 & 0.62 \\ 
#   4$|$5 & 3.27 & 2.52 & 4.02 & 0.38 & 8.58 & 0.00 \\ 
#   variantuh & 0.59 & 0.17 & 1.02 & 0.22 & 2.76 & 0.01 \\ 
#   variantum & 0.49 & 0.06 & 0.91 & 0.22 & 2.26 & 0.02 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `young' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -2.26 & -3.03 & -1.49 & 0.39 & -5.73 & 0.00 \\ 
#   2$|$3 & -0.17 & -0.90 & 0.56 & 0.37 & -0.46 & 0.64 \\ 
#   3$|$4 & 6.17 & 5.10 & 7.24 & 0.55 & 11.31 & 0.00 \\ 
#   4$|$5 & 8.51 & 6.36 & 10.66 & 1.10 & 7.77 & 0.00 \\ 
#   variantuh & 0.43 & -0.12 & 0.97 & 0.28 & 1.54 & 0.12 \\ 
#   variantum & 0.26 & -0.28 & 0.80 & 0.28 & 0.96 & 0.34 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `queer' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -4.30 & -5.03 & -3.57 & 0.37 & -11.59 & 0.00 \\ 
#   2$|$3 & -2.22 & -2.73 & -1.71 & 0.26 & -8.54 & 0.00 \\ 
#   3$|$4 & 1.54 & 1.07 & 2.01 & 0.24 & 6.39 & 0.00 \\ 
#   4$|$5 & 4.37 & 3.63 & 5.12 & 0.38 & 11.48 & 0.00 \\ 
#   variantuh & -0.19 & -0.65 & 0.27 & 0.24 & -0.81 & 0.42 \\ 
#   variantum & -0.31 & -0.77 & 0.16 & 0.24 & -1.30 & 0.19 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `canadian' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -4.35 & -5.11 & -3.59 & 0.39 & -11.22 & 0.00 \\ 
#   2$|$3 & -2.26 & -2.75 & -1.78 & 0.25 & -9.20 & 0.00 \\ 
#   3$|$4 & 1.01 & 0.58 & 1.44 & 0.22 & 4.60 & 0.00 \\ 
#   4$|$5 & 4.11 & 3.40 & 4.82 & 0.36 & 11.33 & 0.00 \\ 
#   variantuh & -0.76 & -1.21 & -0.31 & 0.23 & -3.30 & 0.00 \\ 
#   variantum & -0.62 & -1.07 & -0.17 & 0.23 & -2.72 & 0.01 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `intelligent' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -2.41 & -3.16 & -1.66 & 0.38 & -6.33 & 0.00 \\ 
#   2$|$3 & -0.89 & -1.60 & -0.19 & 0.36 & -2.48 & 0.01 \\ 
#   3$|$4 & -0.11 & -0.81 & 0.59 & 0.36 & -0.31 & 0.75 \\ 
#   4$|$5 & 2.24 & 1.50 & 2.98 & 0.38 & 5.93 & 0.00 \\ 
#   variantuh & 1.16 & 0.74 & 1.59 & 0.22 & 5.38 & 0.00 \\ 
#   variantum & 0.95 & 0.53 & 1.38 & 0.22 & 4.42 & 0.00 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `hesitant' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -3.62 & -4.32 & -2.91 & 0.36 & -10.00 & 0.00 \\ 
#   2$|$3 & -1.47 & -2.03 & -0.91 & 0.29 & -5.15 & 0.00 \\ 
#   3$|$4 & 0.19 & -0.35 & 0.73 & 0.27 & 0.70 & 0.49 \\ 
#   4$|$5 & 2.62 & 2.01 & 3.23 & 0.31 & 8.39 & 0.00 \\ 
#   variantuh & -0.50 & -0.92 & -0.08 & 0.21 & -2.35 & 0.02 \\ 
#   variantum & -0.33 & -0.75 & 0.08 & 0.21 & -1.57 & 0.12 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `polite' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -4.18 & -5.02 & -3.34 & 0.43 & -9.75 & 0.00 \\ 
#   2$|$3 & -1.83 & -2.49 & -1.18 & 0.34 & -5.46 & 0.00 \\ 
#   3$|$4 & -0.36 & -0.99 & 0.27 & 0.32 & -1.12 & 0.26 \\ 
#   4$|$5 & 2.22 & 1.54 & 2.89 & 0.34 & 6.46 & 0.00 \\ 
#   variantuh & 0.23 & -0.19 & 0.65 & 0.21 & 1.10 & 0.27 \\ 
#   variantum & 0.11 & -0.31 & 0.54 & 0.22 & 0.53 & 0.60 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `casual' scale.} 
# \end{table}
# % latex table generated in R 3.6.1 by xtable 1.8-4 package
# % Tue Nov 12 11:24:47 2019
# \begin{table}[ht]
# \centering
# \begin{tabular}{lrrrrrr}
#   \toprule
# term & estimate & conf.low & conf.high & std.error & statistic & p.value \\ 
#   \midrule
# 1$|$2 & -4.31 & -5.19 & -3.42 & 0.45 & -9.52 & 0.00 \\ 
#   2$|$3 & -1.83 & -2.54 & -1.13 & 0.36 & -5.10 & 0.00 \\ 
#   3$|$4 & 0.62 & -0.06 & 1.30 & 0.35 & 1.80 & 0.07 \\ 
#   4$|$5 & 3.77 & 2.95 & 4.60 & 0.42 & 8.97 & 0.00 \\ 
#   variantuh & -0.35 & -0.78 & 0.08 & 0.22 & -1.60 & 0.11 \\ 
#   variantum & -0.51 & -0.94 & -0.08 & 0.22 & -2.31 & 0.02 \\ 
#    \bottomrule
# \end{tabular}
# \caption{Model for the `friendly' scale.} 
# \end{table}
#      feminine     masculine         young         queer      canadian   intelligent      hesitant        polite 
#    "feminine"   "masculine"       "young"       "queer"    "canadian" "intelligent"    "hesitant"      "polite" 
#        casual      friendly 
#      "casual"    "friendly" 
