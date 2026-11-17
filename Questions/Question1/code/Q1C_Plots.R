# Q1C_Plots.R
# Plot functions for Q1 Part C - same style as Parts A and B.

# --- 1) Dispersion over time: weighted vs unweighted -------------------------------
Plot_CSV <- function(MonthlyCSV) {

    MonthlyCSV %>%
        select(date, `Weighted (Capped SWIX weights)` = CSV_W, `Unweighted` = CSV_EW) %>%
        pivot_longer(-date, names_to = "Measure", values_to = "CSV") %>%
        ggplot(aes(date, CSV, colour = Measure)) +
        geom_events(MinDate = min(MonthlyCSV$date)) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = c("Weighted (Capped SWIX weights)" = "#E0245E", "Unweighted" = "grey60")) +
        scale_x_years(Breaks = "1 year") + scale_y_pct(Acc = 0.1) +
        labs(title = "Dispersion fell until 2024 and has picked up again",
             subtitle = "Cross-sectional volatility of Capped SWIX stocks: average daily value per month",
             caption = Events_Caption(MinDate = min(MonthlyCSV$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- 2) Within- vs between-sector dispersion --------------------------------------
Plot_CSV_Decomp <- function(MonthlyCSV) {

    MonthlyCSV %>%
        transmute(date,
                  `Within sectors (stock picking)` = Within_Var / (Within_Var + Between_Var),
                  `Between sectors (sector allocation)` = Between_Var / (Within_Var + Between_Var)) %>%
        pivot_longer(-date, names_to = "Source", values_to = "Share") %>%
        mutate(Source = factor(Source, levels = c("Between sectors (sector allocation)", "Within sectors (stock picking)"))) %>%
        ggplot(aes(date, Share, fill = Source)) +
        geom_area(alpha = 0.9, colour = "white", linewidth = 0.2) +
        geom_hline(yintercept = 0.8, colour = "white", linetype = "dashed", linewidth = 0.4) +
        scale_fill_manual(values = c("Within sectors (stock picking)" = "#1F3A5F",
                                     "Between sectors (sector allocation)" = "#E0245E")) +
        scale_x_years(Breaks = "1 year") + scale_y_pct() +
        labs(title = "Sector calls matter more than they used to",
             subtitle = "Share of (weighted) cross-sectional variance coming from within vs between super-sectors",
             caption = "Dashed line: 80%. Variance decomposition: total = within-sector + between-sector (exact).",
             x = NULL, y = NULL) +
        theme_sleek()
}

# --- 3) Fan chart of pairwise correlations ------------------------------------------
Plot_PairCor <- function(PairCor) {

    PairCor %>%
        ggplot(aes(date)) +
        geom_events(MinDate = min(PairCor$date)) +
        geom_ribbon(aes(ymin = P25, ymax = P75, fill = "Middle 50% of stock pairs"), alpha = 0.35) +
        geom_line(aes(y = Median, colour = "Median pair"), linewidth = 0.8) +
        scale_fill_manual(values = c("Middle 50% of stock pairs" = "#5FA8D3"), name = NULL) +
        scale_colour_manual(values = c("Median pair" = "#1F3A5F"), name = NULL) +
        scale_x_years(Breaks = "1 year") +
        labs(title = "Stocks moved most in lockstep in 2022, least in 2024-25",
             subtitle = "Pairwise correlations of Large Cap stocks, rolling 63 trading days, month-ends",
             caption = Events_Caption(MinDate = min(PairCor$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- 4) Opportunity quadrant: dispersion vs correlation ------------------------------
Plot_Opp_Quadrant <- function(Opp) {

    mx <- median(Opp$CSV_W); my <- median(Opp$AvgCor)
    Lab <- Opp %>% slice_max(abs(Opportunity), n = 6)

    Opp %>%
        mutate(Year = factor(lubridate::year(date))) %>%
        ggplot(aes(CSV_W, AvgCor)) +
        annotate("rect", xmin = mx, xmax = Inf, ymin = -Inf, ymax = my, fill = "#2A9D8F", alpha = 0.08) +
        annotate("rect", xmin = -Inf, xmax = mx, ymin = my, ymax = Inf, fill = "#E0245E", alpha = 0.06) +
        geom_vline(xintercept = mx, colour = "grey60", linewidth = 0.3) +
        geom_hline(yintercept = my, colour = "grey60", linewidth = 0.3) +
        geom_point(aes(colour = Year), size = 2.6, alpha = 0.9) +
        ggrepel::geom_text_repel(data = Lab, aes(label = format(date, "%b %Y")), size = 2.6, colour = "grey25", seed = 1) +
        annotate("text", x = Inf, y = -Inf, label = "Stock-picker's market\n(high dispersion, low correlation)",
                 hjust = 1.05, vjust = -0.4, size = 3, colour = "#2A9D8F", fontface = "italic") +
        annotate("text", x = -Inf, y = Inf, label = "Macro-driven market\n(low dispersion, high correlation)",
                 hjust = -0.05, vjust = 1.3, size = 3, colour = "#E0245E", fontface = "italic") +
        scale_colour_manual(values = c("2020" = "#8E6C8A", "2021" = "#5FA8D3", "2022" = "#1F3A5F", "2023" = "#2A9D8F",
                                       "2024" = "#C9A227", "2025" = "#F4A261", "2026" = "#E0245E")) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        labs(title = "Mapping the alpha opportunity month by month",
             subtitle = "Monthly dispersion (x) vs average pairwise correlation (y); lines at the medians",
             x = "Dispersion (avg daily weighted CSV)", y = "Average pairwise correlation") +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3)) +
        guides(colour = guide_legend(nrow = 1))
}

# --- 5) Alpha Opportunity Index over time ------------------------------------------
Plot_Opp_Index <- function(Opp) {

    Opp %>%
        mutate(Roll = zoo::rollmeanr(Opportunity, 6, fill = NA)) %>%
        ggplot(aes(date)) +
        geom_events(MinDate = min(Opp$date)) +
        geom_col(aes(y = Opportunity, fill = Opportunity > 0), width = 25, alpha = 0.85) +
        geom_line(aes(y = Roll, colour = "6-month average"), linewidth = 0.8, na.rm = TRUE) +
        geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
        scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E0245E"),
                          labels = c("TRUE" = "Above-average opportunity", "FALSE" = "Below-average opportunity"), name = NULL) +
        scale_colour_manual(values = c("6-month average" = "grey15"), name = NULL) +
        scale_x_years(Breaks = "1 year") +
        labs(title = "Alpha opportunity has improved since 2025",
             subtitle = "Alpha Opportunity Index = z(dispersion) - z(average correlation), monthly",
             caption = Events_Caption(MinDate = min(Opp$date)), x = NULL, y = "Standard deviations") +
        theme_sleek()
}

# --- 6) Was the opportunity realised? -------------------------------------------------
Plot_Realised <- function(Opp, Spreads) {

    Opp %>%
        inner_join(Spreads, by = "date") %>%
        mutate(Regime = factor(ntile(Opportunity, 3), labels = c("Low", "Medium", "High"))) %>%
        group_by(Regime) %>%
        summarise(`Best minus worst stock quintile` = mean(Quintile_Spread),
                  `Best minus worst factor index` = mean(Factor_Spread), .groups = "drop") %>%
        pivot_longer(-Regime, names_to = "Spread", values_to = "Value") %>%
        ggplot(aes(Regime, Value, fill = Regime)) +
        geom_col(width = 0.65) +
        geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), vjust = -0.4, size = 3, colour = "grey25") +
        facet_wrap(~Spread, scales = "free_y") +
        scale_fill_manual(values = c("Low" = "#F4A3B8", "Medium" = "#E0245E", "High" = "#8A0F3C")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
        labs(title = "When the index signals opportunity, the payoff to picking well is larger",
             subtitle = "Average monthly return spread, by tercile of the Alpha Opportunity Index",
             x = "Alpha opportunity", y = NULL) +
        theme_sleek() + theme(legend.position = "none")
}

# --- 7) Lead-lag lollipop: macro vs dispersion / correlation / opportunity -----------
Plot_LeadLag <- function(LL) {

    LL %>%
        mutate(Measure = factor(Measure, levels = c("Dispersion", "Correlation", "Opportunity")),
               Var = fct_reorder(Var, Cor * (Measure == "Opportunity" & Timing == "Same month"), .fun = sum)) %>%
        ggplot(aes(Cor, Var)) +
        geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_line(aes(group = Var), colour = "grey75", linewidth = 1) +
        geom_point(aes(colour = Timing), size = 2.8) +
        facet_wrap(~Measure, nrow = 1) +
        scale_colour_manual(values = c("Same month" = "#1F3A5F", "Next month" = "#E0245E"),
                            breaks = c("Same month", "Next month")) +
        labs(title = "Bond and currency volatility signal a worse market for stock pickers",
             subtitle = "Correlation of each macro measure with the market measure in the same month and the next month",
             x = "Correlation", y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
                              panel.grid.major.y = element_line(colour = "grey95", linewidth = 0.3))
}

# --- 8) High vs low macro regimes (dumbbells) ----------------------------------------
Plot_Macro_Regimes <- function(Reg) {

    W <- Reg %>% pivot_wider(names_from = Regime, values_from = Value)

    Reg %>%
        mutate(Var = factor(Var, levels = rev(unique(Reg$Var))),
               Regime = factor(Regime, levels = c("Low", "High"),
                               labels = c("Low third of months", "High third of months"))) %>%
        ggplot(aes(Value, Var)) +
        geom_segment(data = W %>% mutate(Var = factor(Var, levels = rev(unique(Reg$Var)))),
                     aes(x = Low, xend = High, y = Var, yend = Var), colour = "grey75", linewidth = 1.3,
                     inherit.aes = FALSE) +
        geom_point(aes(colour = Regime), size = 3) +
        facet_wrap(~Measure, scales = "free_x") +
        scale_colour_manual(values = c("Low third of months" = "#5FA8D3", "High third of months" = "#E0245E")) +
        # dispersion (~2%) shown as a percentage, correlation (~0.2) as a plain number
        scale_x_continuous(labels = function(x) ifelse(abs(x) < 0.1, scales::percent(x, accuracy = 0.1), sprintf("%.2f", x))) +
        labs(title = "Macro stress raises correlation more reliably than dispersion",
             subtitle = "Average dispersion and pairwise correlation when each macro measure is in its lowest vs highest third",
             x = NULL, y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}
