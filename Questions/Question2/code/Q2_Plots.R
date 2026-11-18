# Q2_Plots.R
# Plot functions for Question 2 - same style as Question 1 (theme_sleek, Pal, event
# bands), but with chart types chosen to suit the size-index story.

Q2_Sector_Pal <- c("Resources" = "#E0245E", "Financials" = "#1F3A5F", "Industrials" = "#5FA8D3",
                   "Property" = "#C9A227", "Unclassified" = "grey75")

# --- 1) Sector mix over time --------------------------------------------------------
Plot_Q2_Sectors <- function(SectorMix) {

    SectorMix %>%
        mutate(Index = factor(Index, levels = Size_Lvls),
               Sector = factor(Sector, levels = names(Q2_Sector_Pal))) %>%
        ggplot(aes(date, Weight, fill = Sector)) +
        geom_area(alpha = 0.92, colour = "white", linewidth = 0.1) +
        facet_wrap(~Index, ncol = 2) +
        scale_fill_manual(values = Q2_Sector_Pal) +
        scale_x_years(Breaks = "4 years") + scale_y_pct() +
        labs(title = "Each size index is a very different bet on the economy",
             subtitle = "Sector weights, monthly, 2007-2026 (grey = no sector available, mainly older small caps)",
             x = NULL, y = NULL) +
        theme_sleek()
}

# --- 2) Lorenz curves (left) + active share vs tracking error map (right) -------------
Plot_Q2_Lorenz_ASTE <- function(Lorenz, ASTE, AS_Cut = 0.6, TE_Cut = 0.08) {

    # Legend labels carry the Gini coefficients, e.g. "Large Caps (Gini 0.55 -> 0.44)"
    Lab <- Lorenz %>% distinct(Index, Year, Gini) %>% arrange(Year) %>%
        group_by(Index) %>%
        summarise(Lab = paste0(first(Index), " (Gini ", paste(sprintf("%.2f", Gini), collapse = " -> "), ")"), .groups = "drop")
    Yrs <- sort(unique(Lorenz$Year))

    g1 <- bind_rows(Lorenz,
                    Lorenz %>% distinct(Index, Year) %>% mutate(CumStocks = 0, CumWeight = 0)) %>%  # start curves at (0, 0)
        mutate(Index = factor(Index, levels = intersect(Size_Lvls, unique(Index)))) %>%
        ggplot(aes(CumStocks, CumWeight, colour = Index, linetype = Year)) +
        geom_abline(slope = 1, intercept = 0, colour = "grey70", linewidth = 0.3) +
        geom_line(linewidth = 0.8) +
        annotate("text", x = 0.62, y = 0.70, label = "Equal weights", angle = 38, size = 2.7, colour = "grey50") +
        scale_colour_manual(values = Pal, labels = setNames(Lab$Lab, Lab$Index)) +
        scale_linetype_manual(values = setNames(c("dashed", "solid"), Yrs)) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        coord_equal() +
        labs(title = "Concentration", subtitle = "Lorenz curves (2007 dashed, 2026 solid)",
             x = "Cumulative share of stocks (smallest first)", y = "Cumulative share of weight", colour = NULL, linetype = NULL) +
        theme_sleek() + theme(legend.position = "bottom", legend.box = "vertical",
                              panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3)) +
        guides(colour = guide_legend(ncol = 1))

    # Zone labels: lower zones at the bottom, upper zones just above the 60% line
    Zones <- tibble(x = c(0.005, 0.005, 0.195, 0.195), y = c(0.02, AS_Cut + 0.02, 0.02, AS_Cut + 0.02),
                    hj = c(0, 0, 1, 1), vj = c(0, 0, 0, 0),
                    lab = c("Closet indexer", "Diversified stock picker", "Factor bet", "Concentrated stock picker"))

    g2 <- ASTE %>%
        mutate(Index = factor(Index, levels = Size_Lvls)) %>%
        arrange(Index, Year) %>%
        ggplot(aes(TrackingError, ActiveShare, colour = Index)) +
        geom_vline(xintercept = TE_Cut, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
        geom_hline(yintercept = AS_Cut, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
        geom_text(data = Zones, aes(x, y, label = lab, hjust = hj, vjust = vj), inherit.aes = FALSE,
                  size = 2.7, colour = "grey45", fontface = "italic") +
        geom_path(aes(group = Index), alpha = 0.4, linewidth = 0.5) +
        geom_point(size = 2.2) +
        ggrepel::geom_text_repel(data = ~ .x %>% group_by(Index) %>% filter(Year %in% c(min(Year), max(Year))),
                                 aes(label = Year), size = 2.5, seed = 1, show.legend = FALSE) +
        scale_colour_manual(values = Pal) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.2)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = "How different from the market?", subtitle = "Active share vs tracking error, per year",
             x = "Tracking error", y = "Active share", colour = NULL) +
        theme_sleek() + theme(legend.position = "bottom",
                              panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))

    cowplot::plot_grid(g1, g2, ncol = 2, rel_widths = c(1, 1.1), align = "h", axis = "b")
}

# --- 3) Constituent volatility fan chart ------------------------------------------------
Plot_Q2_Const_Vol <- function(ConstVol) {

    ConstVol %>%
        filter(Index != "Capped SWIX") %>%
        mutate(Index = factor(Index, levels = Size_Lvls)) %>%
        ggplot(aes(date)) +
        geom_events(MinDate = min(ConstVol$date)) +
        geom_ribbon(aes(ymin = P25, ymax = P75, fill = Index), alpha = 0.25) +
        geom_line(aes(y = Median, colour = Index), linewidth = 0.7) +
        facet_wrap(~Index, nrow = 1) +
        scale_colour_manual(values = Pal) + scale_fill_manual(values = Pal) +
        scale_x_years(Breaks = "3 years") + scale_y_pct() +
        labs(title = "The typical stock is similarly volatile across indexes, except in crises",
             subtitle = "6-month realised volatility of index constituents: median (line) and middle 50% (band)",
             caption = Events_Caption(MinDate = min(ConstVol$date)), x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "none")
}

# --- 4) Diversification: stock volatility vs index volatility --------------------------
# Each dot is one month. Dots on the steep ray behave like their stocks (no
# diversification); dots near the flat ray are much calmer than their stocks.
Plot_Q2_Div_Scatter <- function(Div) {

    D <- Div %>% filter(Index != "Capped SWIX") %>% mutate(Index = factor(Index, levels = Size_Lvls))
    xmax <- max(D$WtdVol) * 1.02
    Rays <- tibble(k = c(1, 0.5, 0.25), lab = c("Index as volatile as its stocks", "Half as volatile", "A quarter as volatile"))

    D %>%
        ggplot(aes(WtdVol, IdxVol, colour = Index)) +
        geom_abline(data = Rays, aes(slope = k, intercept = 0), colour = "grey70", linetype = "dashed", linewidth = 0.3) +
        geom_text(data = Rays %>% mutate(x = pmin(xmax, 0.95 / k), y = pmin(xmax, 0.95 / k) * k),
                  aes(x, y, label = lab), inherit.aes = FALSE, hjust = 1.02, vjust = -0.5, size = 2.6, colour = "grey45") +
        geom_point(size = 1.8, alpha = 0.65) +
        scale_colour_manual(values = Pal) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, xmax)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
        labs(title = "Small caps look calm because their stocks rarely move together",
             subtitle = "Each dot is a month: weighted average volatility of the stocks (x) vs volatility of the index (y), 6-month windows",
             x = "Average stock volatility", y = "Index volatility", colour = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 5) News impact curves --------------------------------------------------------------
Plot_Q2_News_Impact <- function(NI) {

    NI %>%
        mutate(Name = factor(Name, levels = Size_Lvls)) %>%
        ggplot(aes(Shock, Vol, colour = Name)) +
        geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
        geom_line(linewidth = 0.9) +
        annotate("text", x = -0.05, y = Inf, label = "Bad news", hjust = 0, vjust = 1.5, size = 3,
                 colour = "grey40", fontface = "italic") +
        annotate("text", x = 0.05, y = Inf, label = "Good news", hjust = 1, vjust = 1.5, size = 3,
                 colour = "grey40", fontface = "italic") +
        scale_colour_manual(values = Pal) +
        scale_x_continuous(breaks = seq(-0.05, 0.05, 0.025), labels = scales::percent_format(accuracy = 0.1)) +
        scale_y_pct() +
        labs(title = "Large caps fear bad news; small caps react to news either way",
             subtitle = "News impact curves: next-day volatility (annualised) after a daily shock (GJR-GARCH fits)",
             x = "Shock (unexpected daily return)", y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 6) Calendar-year leadership: bump chart of the four indexes' annual return ranks ------
# (idea from the R Graph Gallery: "bump chart")
# Each line is an index; its height is its rank among the four in that calendar year
# (1st = best), and each dot carries that year's return.
Plot_Q2_Bump <- function(Annual) {

    Lvls <- c("Large Caps", "Mid Caps", "Small Caps", "Capped SWIX")
    MaxYear <- max(Annual$Year)

    D <- Annual %>%
        group_by(Year) %>% mutate(Rank = rank(-Ret, ties.method = "first")) %>% ungroup() %>%
        mutate(Name = factor(Name, levels = Lvls), Lab = sprintf("%+.0f", Ret * 100))
    Ends <- D %>% filter(Year == max(Year))
    Starts <- D %>% filter(Year == min(Year))

    ggplot(D, aes(Year, Rank, colour = Name, group = Name)) +
        geom_line(linewidth = 2.4, alpha = 0.85, lineend = "round") +
        geom_point(size = 9.5, shape = 21, fill = "white", stroke = 2) +
        geom_text(aes(label = Lab), size = 2.9, fontface = "bold", show.legend = FALSE) +
        geom_text(data = Ends, aes(x = Year + 0.45, label = Name), hjust = 0, size = 3.6, fontface = "bold", show.legend = FALSE) +
        geom_text(data = Starts, aes(x = Year - 0.45, label = Name), hjust = 1, size = 3.6, fontface = "bold", show.legend = FALSE) +
        scale_colour_manual(values = Pal[Lvls], guide = "none") +
        scale_y_reverse(breaks = 1:4, labels = c("1st", "2nd", "3rd", "4th")) +
        scale_x_continuous(breaks = unique(D$Year), labels = function(x) ifelse(x == MaxYear, paste0(x, "*"), x),
                           expand = expansion(add = c(2.8, 2.8))) +
        labs(title = "Small caps led 2021-2024; large caps led most years before that",
             subtitle = "Rank of each index by calendar-year return (1st = best); dots show the year's return in %",
             caption = paste0("* ", MaxYear, " = year to September."), x = NULL, y = NULL) +
        theme_sleek() +
        theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank(),
              axis.text.y = element_text(face = "bold"))
}

# --- 7) Risk-return summary ---------------------------------------------------------------
Plot_Q2_Risk_Return <- function(RR) {

    RR %>%
        mutate(Name = factor(Name, levels = Size_Lvls),
               Lab = paste0(Name, "\nSharpe ", sprintf("%.2f", Sharpe), " | max DD ", scales::percent(MaxDD, accuracy = 1))) %>%
        ggplot(aes(AnnVol, AnnRet, colour = Name)) +
        geom_point(aes(size = -MaxDD), alpha = 0.85) +
        ggrepel::geom_text_repel(aes(label = Lab), size = 2.8, seed = 1, box.padding = 0.8, show.legend = FALSE) +
        scale_colour_manual(values = Pal) +
        scale_size_continuous(range = c(4, 10), guide = "none") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = 0.25)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = 0.25)) +
        labs(title = "Small caps: the best risk-adjusted return, but the deepest drawdown",
             subtitle = "Annualised return vs volatility, Jan 2015 - Sep 2026 (bubble size = maximum drawdown)",
             x = "Annualised volatility", y = "Annualised return") +
        theme_sleek() + theme(legend.position = "none",
                              panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 8) Stratification: double ridgeline of monthly returns, calm vs stressed ---------------
# (idea from the R Graph Gallery: "double ridgeline plot")
Plot_Q2_Strat_Ridges <- function(StratM) {

    Lvls <- rev(c("Capped SWIX", "Large Caps", "Mid Caps", "Small Caps"))
    Regs <- c("Low" = "Calmest 20% of months", "High" = "Most stressed 20% of months")
    Grid <- seq(-0.13, 0.17, length.out = 300)

    D <- StratM %>%
        mutate(Row = match(Name, Lvls), Regime = Regs[Regime],
               Stratifier = factor(Stratifier, levels = c("Rand volatility (SAVI)", "Global fear (VIX)")))

    # Kernel density of each group on a common grid; each ridge sits on its index's row
    Dens <- D %>%
        group_by(Stratifier, Name, Row, Regime) %>%
        reframe(x = Grid, h = density(Ret, from = min(Grid), to = max(Grid), n = length(Grid), bw = 0.012)$y) %>%
        mutate(h = h / max(h) * 0.9)

    Meds <- D %>% group_by(Stratifier, Name, Row, Regime) %>%
        summarise(Med = median(Ret), Fall = mean(Ret < 0), .groups = "drop")
    Lab <- Meds %>% filter(Regime == Regs["High"]) %>%
        mutate(Lab = paste0("down in ", round(Fall * 100), "% of stressed months"))

    ggplot() +
        geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
        geom_ribbon(data = Dens, aes(x, ymin = Row, ymax = Row + h, fill = Regime, group = interaction(Name, Regime)),
                    alpha = 0.45, colour = NA) +
        geom_line(data = Dens, aes(x, Row + h, colour = Regime, group = interaction(Name, Regime)), linewidth = 0.45) +
        geom_segment(data = Meds, aes(x = Med, xend = Med, y = Row, yend = Row + 0.55, colour = Regime), linewidth = 0.9) +
        geom_label(data = Lab, aes(x = 0.165, y = Row + 0.2, label = Lab), hjust = 1, size = 2.3, colour = "#8A0F3C",
                   fill = "white", label.size = 0, label.padding = unit(1, "pt")) +
        facet_wrap(~Stratifier, ncol = 2) +
        scale_y_continuous(breaks = seq_along(Lvls), labels = Lvls, expand = expansion(mult = c(0.02, 0.08))) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_fill_manual(values = c("Calmest 20% of months" = "#5FA8D3", "Most stressed 20% of months" = "#E0245E")) +
        scale_colour_manual(values = c("Calmest 20% of months" = "#1F3A5F", "Most stressed 20% of months" = "#8A0F3C")) +
        labs(title = "Rand stress drags most indexes down; mid caps hold up best",
             subtitle = "Distribution of monthly returns in the calmest vs most stressed fifth of months (vertical ticks = medians)",
             x = "Monthly return", y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
                              panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold", colour = "grey25"))
}

