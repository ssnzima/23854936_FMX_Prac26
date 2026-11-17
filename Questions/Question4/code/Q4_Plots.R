# Q4_Plots.R
# Plot functions for Question 4 - same identity as Questions 1-3 (theme_sleek), but
# using chart forms not used elsewhere: overlapping shaded areas, a shaded frontier
# gap, a constraint heatmap, donuts and violins.

Strat_Pal <- c("Max Sharpe" = "#E0245E", "Minimum variance" = "#1F3A5F", "Naive (equal weight)" = "#C9A227")
Group_Pal <- c("Local equity" = "#1F3A5F", "Global equity" = "#5FA8D3", "Local bonds" = "#2A9D8F",
               "Global bonds" = "#8AB17D", "Global commodities" = "#C9A227")

# --- 1) Bond return index built from yields --------------------------------------------------
Plot_Q4_Bonds <- function(BondTRI) {

    D <- BondTRI %>%
        mutate(date = as.Date(paste0(YM, "-01")), Name = recode(Name, "US_10Yr" = "US 10Y", "Germany_10Yr" = "German 10Y")) %>%
        group_by(Name) %>% arrange(date) %>% mutate(Growth = 100 * cumprod(1 + Ret)) %>% ungroup()
    Cols <- c("US 10Y" = "#1F3A5F", "German 10Y" = "#E0245E")

    g1 <- ggplot(D, aes(date, Yield, colour = Name)) +
        geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = Cols) + scale_x_years(Breaks = "4 years") + scale_y_pct(Acc = 1) +
        labs(title = "Input: 10-year yields", subtitle = "Month-end yield to maturity", x = NULL, y = NULL) +
        theme_sleek()

    g2 <- ggplot(D, aes(date, Growth, colour = Name, fill = Name)) +
        geom_area(position = "identity", alpha = 0.18, linewidth = 0) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = Cols) + scale_fill_manual(values = Cols) +
        scale_x_years(Breaks = "4 years") +
        coord_cartesian(ylim = c(80, NA)) +
        labs(title = "Output: bond return index", subtitle = "Growth of 100, constant-maturity 10-year bond", x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "none")

    cowplot::plot_grid(g1, g2, ncol = 2, align = "h", axis = "b")
}

# --- 2) Efficient frontiers: what the mandate costs ---------------------------------------------
Plot_Q4_Frontier <- function(Fr_Mandate, Fr_Free, Points, Assets) {

    Fr <- bind_rows(Fr_Mandate %>% mutate(Set = "With the mandate's constraints"),
                    Fr_Free %>% mutate(Set = "Long-only, no other constraints")) %>%
        mutate(across(c(Ret, Vol), ~ .x * c(Ret = 12, Vol = sqrt(12))[cur_column()]))

    # Shade the gap: interpolate both frontiers on a common volatility grid
    Grid <- seq(max(min(Fr_Mandate$Vol), min(Fr_Free$Vol)), min(max(Fr_Mandate$Vol), max(Fr_Free$Vol)), length.out = 100) * sqrt(12)
    Gap <- tibble(Vol = Grid,
                  Lo = approx(Fr_Mandate$Vol * sqrt(12), Fr_Mandate$Ret * 12, Grid, ties = mean)$y,
                  Hi = approx(Fr_Free$Vol * sqrt(12), Fr_Free$Ret * 12, Grid, ties = mean)$y)

    ggplot() +
        geom_ribbon(data = Gap, aes(Vol, ymin = Lo, ymax = Hi), fill = "#E0245E", alpha = 0.15) +
        geom_line(data = Fr, aes(Vol, Ret, linetype = Set), colour = "grey20", linewidth = 0.8) +
        geom_point(data = Assets, aes(Vol, Ret), colour = "grey55", size = 1.8) +
        ggrepel::geom_text_repel(data = Assets, aes(Vol, Ret, label = Asset), size = 2.5, colour = "grey45", seed = 1,
                                 max.overlaps = 20) +
        geom_point(data = Points, aes(Vol, Ret, colour = Strategy), size = 4) +
        scale_colour_manual(values = Strat_Pal) +
        scale_linetype_manual(values = c("With the mandate's constraints" = "solid", "Long-only, no other constraints" = "dashed")) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "What the mandate costs: the shaded gap between the two frontiers",
             subtitle = "Expected annual return vs volatility at the latest rebalance (shrunk estimates, data since 2010)",
             x = "Expected volatility", y = "Expected return", colour = NULL, linetype = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3), legend.box = "vertical") +
        guides(colour = guide_legend(nrow = 1, order = 1), linetype = guide_legend(nrow = 1, order = 2))
}

# --- 3) Which mandate constraints bind? -------------------------------------------------------------
Plot_Q4_Binding <- function(Binding) {

    D <- Binding %>%
        group_by(Strategy, Constraint) %>% filter(any(Binding)) %>% ungroup() %>%
        mutate(date = as.Date(paste0(YM, "-01")),
               Strategy = factor(Strategy, levels = names(Strat_Pal)),
               Constraint = fct_reorder(Constraint, Binding, .fun = mean))

    ggplot(D, aes(date, Constraint, fill = Binding)) +
        geom_tile(colour = "white", linewidth = 0.4, width = 80) +
        facet_wrap(~Strategy, ncol = 1, scales = "free_y") +
        scale_fill_manual(values = c("TRUE" = "#E0245E", "FALSE" = "grey92"),
                          labels = c("TRUE" = "Binding (shapes the portfolio)", "FALSE" = "Not binding")) +
        scale_x_years(Breaks = "1 year") +
        labs(title = "Which rules actually shape the portfolio?",
             subtitle = "Mandate constraints with a positive shadow price at each quarterly rebalance (quadprog Lagrange multipliers)",
             x = NULL, y = NULL, fill = NULL) +
        theme_sleek() + theme(panel.grid.major.y = element_blank())
}

# --- 4) Weights over time: overlapping shaded areas per asset group ---------------------------------
Plot_Q4_Weights <- function(Weights) {

    Weights %>%
        group_by(YM, Strategy, Group) %>% summarise(Weight = sum(Weight), .groups = "drop") %>%
        mutate(date = as.Date(paste0(YM, "-01")),
               Group = factor(Group, levels = names(Group_Pal)), Strategy = factor(Strategy, levels = names(Strat_Pal))) %>%
        ggplot(aes(date, Weight, colour = Strategy, fill = Strategy)) +
        geom_area(position = "identity", alpha = 0.18, linewidth = 0) +
        geom_step(linewidth = 0.7, direction = "hv") +
        facet_wrap(~Group, nrow = 1) +
        scale_colour_manual(values = Strat_Pal) + scale_fill_manual(values = Strat_Pal) +
        scale_x_years(Breaks = "3 years") + scale_y_pct() +
        labs(title = "How each strategy allocates across asset groups",
             subtitle = "Target weight at each quarterly rebalance (overlapping shaded areas)", x = NULL, y = NULL, colour = NULL, fill = NULL) +
        theme_sleek()
}

# --- 5) Latest allocation: donut charts ----------------------------------------------------------------
Plot_Q4_Donuts <- function(Weights) {

    D <- Weights %>%
        filter(YM == max(YM)) %>%
        group_by(Strategy, Group) %>% summarise(Weight = sum(Weight), .groups = "drop") %>%
        filter(Weight > 0.0005) %>%
        mutate(Group = factor(Group, levels = names(Group_Pal)), Strategy = factor(Strategy, levels = names(Strat_Pal))) %>%
        arrange(Strategy, Group) %>%
        group_by(Strategy) %>% mutate(Pos = 1 - (cumsum(Weight) - Weight / 2)) %>% ungroup()

    ggplot(D, aes(x = 2, y = Weight, fill = Group)) +
        geom_col(width = 1, colour = "white", linewidth = 0.8) +
        geom_text(aes(y = Weight, label = ifelse(Weight >= 0.04, scales::percent(Weight, accuracy = 1), "")),
                  position = position_stack(vjust = 0.5), colour = "white", size = 3.1, fontface = "bold") +
        coord_polar(theta = "y", direction = -1) +
        xlim(0.6, 2.5) +
        facet_wrap(~Strategy, nrow = 1) +
        scale_fill_manual(values = Group_Pal) +
        labs(title = paste0("Latest allocation (", format(as.Date(paste0(max(Weights$YM), "-01")), "%B %Y"), " rebalance)"),
             subtitle = "Weight by asset group", x = NULL, y = NULL, fill = NULL) +
        theme_sleek() + theme(axis.text = element_blank(), axis.line.x = element_blank(),
                              panel.grid.major.y = element_blank(), strip.text = element_text(hjust = 0.5))
}

# --- 6) Performance: growth + overlapping drawdown areas -----------------------------------------------
Plot_Q4_Performance <- function(Port) {

    D <- Port %>% group_by(Strategy) %>% arrange(date) %>%
        mutate(Growth = 100 * cumprod(1 + Ret), DD = Growth / cummax(Growth) - 1) %>% ungroup() %>%
        mutate(Strategy = factor(Strategy, levels = names(Strat_Pal)))

    g1 <- ggplot(D, aes(date, Growth, colour = Strategy)) +
        geom_line(linewidth = 0.8) +
        scale_colour_manual(values = Strat_Pal) + scale_x_years(Breaks = "1 year") +
        labs(title = "Out-of-sample performance of the quarterly-rebalanced fund",
             subtitle = "Growth of R100, January 2020 - September 2026", x = NULL, y = NULL, colour = NULL) +
        theme_sleek()

    g2 <- ggplot(D, aes(date, DD, fill = Strategy, colour = Strategy)) +
        geom_area(position = "identity", alpha = 0.22, linewidth = 0.4) +
        scale_colour_manual(values = Strat_Pal) + scale_fill_manual(values = Strat_Pal) +
        scale_x_years(Breaks = "1 year") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.03, 0.1))) +
        labs(subtitle = "Drawdown from previous peak (overlapping areas)", x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "none")

    cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(1.2, 1), align = "v", axis = "lr")
}

# --- 7) Distribution of monthly returns: violins ---------------------------------------------------------
Plot_Q4_Violins <- function(Port) {

    Port %>%
        mutate(Strategy = factor(Strategy, levels = names(Strat_Pal))) %>%
        ggplot(aes(Strategy, Ret, fill = Strategy, colour = Strategy)) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_violin(alpha = 0.25, linewidth = 0.6, trim = FALSE) +
        geom_jitter(width = 0.06, size = 1, alpha = 0.5) +
        # 5th-95th percentile range (bar) and median (white diamond)
        stat_summary(fun.data = function(x) data.frame(y = median(x), ymin = quantile(x, 0.05), ymax = quantile(x, 0.95)),
                     geom = "linerange", linewidth = 1.6, colour = "grey25") +
        stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white", colour = "grey25") +
        scale_fill_manual(values = Strat_Pal) + scale_colour_manual(values = Strat_Pal) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "The shape of monthly returns: minimum variance has the thinnest left tail",
             subtitle = "Distribution of monthly returns, 2020-2026 (bar = 5th to 95th percentile, diamond = median)",
             x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "none")
}

# --- 0) The universe: risk and return of each building block ------------------------------
# (idea from the R Graph Gallery: "scatterplot with automatic text repel")
Plot_Q4_Universe <- function(Univ) {

    D <- Univ %>%
        group_by(Asset, Class, Region) %>%
        summarise(Start = min(YM), AnnRet = prod(1 + Ret)^(12 / n()) - 1, AnnVol = sd(Ret) * sqrt(12), .groups = "drop") %>%
        mutate(Group = case_when(Class == "Commodities" ~ "Global commodities",
                                 TRUE ~ paste(Region, tolower(Class))),
               Group = factor(Group, levels = names(Group_Pal)),
               Lab = paste0(Asset, " (from ", substr(Start, 1, 4), ")"))

    ggplot(D, aes(AnnVol, AnnRet, colour = Group)) +
        geom_abline(slope = 0.5, intercept = 0, colour = "grey85", linetype = "dashed") +
        annotate("text", x = 0.205, y = 0.5 * 0.205, label = "return / risk = 0.5", angle = 21, size = 2.5,
                 colour = "grey60", vjust = -0.5) +
        geom_point(size = 3.2, alpha = 0.9) +
        ggrepel::geom_text_repel(aes(label = Lab), size = 2.6, seed = 1, box.padding = 0.45, show.legend = FALSE) +
        scale_colour_manual(values = Group_Pal) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "The building blocks: local bonds earn the most per unit of risk",
             subtitle = "Annualised return vs volatility of each asset over its own history (monthly returns; global assets in USD)",
             x = "Annualised volatility", y = "Annualised return") +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

