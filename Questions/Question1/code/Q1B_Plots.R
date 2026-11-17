# Q1B_Plots.R
# Plot functions for Q1 Part B - same style as Part A (theme_sleek, Pal, event bands).

Sector_Pal <- c("Resources" = "#E0245E", "Financials" = "#1F3A5F", "Industrials" = "#5FA8D3",
                "Property" = "#C9A227", "Other" = "grey70")

# --- 0) Low Vol rebuild check ---------------------------------------------------------
Plot_LowVol_Check <- function(LV_Rebuilt, LV_Daily, Idx_Daily, From = as.Date("2024-01-01")) {

    bind_rows(LV_Rebuilt %>% mutate(Name = "Low Vol (rebuilt, quarterly)"),
              LV_Daily,
              Idx_Daily %>% filter(Tickers == "STXLVLJ433") %>% transmute(date, Name = "Low Vol (actual)", Returns)) %>%
        filter(date >= From) %>%
        group_by(Name) %>% arrange(date) %>% mutate(Growth = 100 * cumprod(1 + Returns)) %>% ungroup() %>%
        ggplot(aes(date, Growth, colour = Name, linetype = Name)) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = c("Low Vol (actual)" = "#1F3A5F", "Low Vol (rebuilt, quarterly)" = "#E0245E",
                                       "Low Vol (daily weights)" = "#F4A261")) +
        scale_linetype_manual(values = c("Low Vol (actual)" = "solid", "Low Vol (rebuilt, quarterly)" = "solid",
                                         "Low Vol (daily weights)" = "dashed")) +
        scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
        labs(title = "The rebuilt Low Vol index tracks the real one closely",
             subtitle = "Growth of R100 since Jan 2024 (the first date the actual index is available)",
             x = NULL, y = NULL) +
        theme_sleek()
}

# --- 1) Growth of R100 + drawdowns (two panels) ------------------------------------
Plot_Growth_DD <- function(GDD) {

    g1 <- GDD %>%
        ggplot(aes(date, Growth, colour = Name)) +
        geom_events(MinDate = min(GDD$date), YMin = min(GDD$Growth) * 0.95, YMax = max(GDD$Growth) * 1.05) +
        geom_line(linewidth = 0.55) +
        scale_colour_manual(values = Pal) +
        scale_x_years() + scale_y_log10() +
        labs(title = "SafeCore beat its benchmark, but fell just as hard in sell-offs",
             subtitle = "Growth of R100 (log scale; Low Vol rebased to the Capped SWIX at its Sep 2020 start)",
             x = NULL, y = NULL) +
        theme_sleek()

    # Drawdowns are <= 0 by definition (0 = at a new peak), so add a 0% reference line
    # and some headroom above it, otherwise the "at peak" stretches look clipped.
    g2 <- GDD %>%
        ggplot(aes(date, Drawdown, colour = Name)) +
        geom_events(MinDate = min(GDD$date)) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_line(linewidth = 0.5) +
        scale_colour_manual(values = Pal) +
        scale_x_years() +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.03, 0.10))) +
        labs(subtitle = "Drawdown from previous peak", caption = Events_Caption(MinDate = min(GDD$date)),
             x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "none")

    cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(1.25, 1), align = "v", axis = "lr")
}

# --- Rolling vol vs the benchmark: two lines with the gap between them filled ---------
# (idea from the R Graph Gallery: "line chart with filled intersection")
Plot_Vol_Ratio <- function(VR, Names = c("SafeCore", "Low Vol")) {

    D <- VR %>%
        filter(Name %in% Names) %>%
        mutate(Fund = RollVol * sqrt(252), Bench = BenchVol * sqrt(252),
               Name = factor(Name, levels = Names, labels = paste(Names, "vs Capped SWIX")))

    ggplot(D, aes(date)) +
        geom_ribbon(aes(ymin = Bench, ymax = pmax(Fund, Bench), fill = "More volatile than the benchmark"), alpha = 0.45) +
        geom_ribbon(aes(ymin = pmin(Fund, Bench), ymax = Bench, fill = "Less volatile than the benchmark"), alpha = 0.45) +
        geom_line(aes(y = Bench, colour = "Capped SWIX"), linewidth = 0.45) +
        geom_line(aes(y = Fund, colour = "Fund"), linewidth = 0.6) +
        facet_wrap(~Name, ncol = 1) +
        scale_fill_manual(values = c("More volatile than the benchmark" = "#E0245E", "Less volatile than the benchmark" = "#2A9D8F")) +
        scale_colour_manual(values = c("Capped SWIX" = "grey30", "Fund" = "#1F3A5F")) +
        scale_x_years() + scale_y_pct() +
        labs(title = "SafeCore is rarely smoother than its benchmark; Low Vol always is",
             subtitle = "Rolling 1-year volatility, annualised", x = NULL, y = NULL) +
        theme_sleek() + guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
}

# --- 3) Capture "quadrant" scatter ---------------------------------------------------
Plot_Capture <- function(Cap) {

    Lims <- range(c(Cap$UpCapture, Cap$DownCapture)) + c(-0.03, 0.03)

    Cap %>%
        ggplot(aes(DownCapture, UpCapture, colour = Name)) +
        annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "#2A9D8F", alpha = 0.07) +
        annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#E0245E", alpha = 0.05) +
        geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "dashed") +
        geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.3) +
        geom_hline(yintercept = 1, colour = "grey60", linewidth = 0.3) +
        geom_point(size = 2.6, alpha = 0.9) +
        ggrepel::geom_text_repel(aes(label = Year), size = 2.5, show.legend = FALSE, max.overlaps = 30, seed = 1) +
        annotate("text", x = Lims[1], y = Lims[2], label = "Ideal: more upside, less downside",
                 hjust = 0, vjust = 1, size = 3, colour = "#2A9D8F", fontface = "italic") +
        annotate("text", x = Lims[2], y = Lims[1], label = "Worst: less upside, more downside",
                 hjust = 1, vjust = 0, size = 3, colour = "#E0245E", fontface = "italic") +
        scale_colour_manual(values = Pal) +
        coord_equal(xlim = Lims, ylim = Lims) +
        labs(title = "SafeCore behaves like the market; Low Vol trades upside for protection",
             subtitle = "Up- and downside capture vs the Capped SWIX, per calendar year (daily returns)",
             x = "Downside capture", y = "Upside capture") +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 4) Stress-test dumbbell ---------------------------------------------------------
Plot_Stress <- function(Stress) {

    D <- Stress %>%
        mutate(Type = factor(Type, levels = c("Local", "Global", "High VIX"),
                             labels = c("SA political shocks", "Global shocks", "Highest-VIX months")),
               Label = fct_reorder(Label, Start, .desc = TRUE))

    Seg <- D %>% filter(Name %in% c("SafeCore", "Capped SWIX")) %>%
        select(Label, Type, Name, CumRet) %>% pivot_wider(names_from = Name, values_from = CumRet)

    ggplot(D, aes(CumRet, Label)) +
        geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
        geom_segment(data = Seg, aes(x = `Capped SWIX`, xend = SafeCore, y = Label, yend = Label),
                     colour = "grey70", linewidth = 1.4, inherit.aes = FALSE) +
        geom_point(aes(colour = Name), size = 3) +
        facet_grid(Type ~ ., scales = "free_y", space = "free_y") +
        scale_colour_manual(values = Pal) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "In stress episodes SafeCore is usually close to its benchmark",
             subtitle = "Cumulative return over each episode (Low Vol only from Sep 2020)",
             x = NULL, y = NULL) +
        theme_sleek() +
        theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 5) Excess return by VIX regime ---------------------------------------------------
Plot_VIX_Regimes <- function(Reg) {

    Reg %>%
        ggplot(aes(Regime, Excess, fill = Name)) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_col(position = position_dodge(width = 0.75), width = 0.7) +
        geom_text(aes(label = paste0(round(Hit * 100), "%"), vjust = ifelse(Excess >= 0, -0.4, 1.3)),
                  position = position_dodge(width = 0.75), size = 2.7, colour = "grey30") +
        scale_fill_manual(values = Pal) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = expansion(mult = 0.15)) +
        labs(title = "SafeCore's edge does not depend on the volatility regime",
             subtitle = "Average monthly return in excess of the Capped SWIX, by VIX tercile",
             caption = "Labels: share of months beating the Capped SWIX. Low Vol from Sep 2020 only.",
             x = NULL, y = NULL) +
        theme_sleek()
}

# --- 6) Sector risk contributions over time --------------------------------------------
Plot_Sector_Risk <- function(RC) {

    D <- RC %>%
        group_by(date, Sector) %>%
        summarise(RiskContrib = sum(RiskContrib), Weight = sum(weight), .groups = "drop") %>%
        mutate(Sector = factor(Sector, levels = names(Sector_Pal)))

    D %>%
        ggplot(aes(date)) +
        geom_area(aes(y = RiskContrib, fill = Sector), alpha = 0.9, colour = "white", linewidth = 0.15) +
        geom_line(data = D %>% filter(Sector == "Resources"), aes(y = Weight, linetype = "Resources weight"),
                  colour = "grey10", linewidth = 0.6) +
        scale_fill_manual(values = Sector_Pal, drop = TRUE) +
        scale_linetype_manual(values = c("Resources weight" = "dashed"), name = NULL) +
        scale_x_years() + scale_y_pct() +
        labs(title = "Resources drive most of SafeCore's risk",
             subtitle = "Share of portfolio variance by sector (Euler contributions, Ledoit-Wolf covariance, 1-year window)",
             x = NULL, y = NULL) +
        theme_sleek()
}

# --- 7) Weight vs risk contribution, top stocks at the latest date ------------------
Plot_Weight_vs_Risk <- function(RC, TopN = 10) {

    RC %>%
        filter(date == max(date)) %>%
        slice_max(RiskContrib, n = TopN) %>%
        mutate(Tickers = fct_reorder(Tickers, RiskContrib)) %>%
        ggplot(aes(y = Tickers)) +
        geom_segment(aes(x = weight, xend = RiskContrib, yend = Tickers), colour = "grey75", linewidth = 1.2) +
        geom_point(aes(x = weight, colour = "Portfolio weight"), size = 3) +
        geom_point(aes(x = RiskContrib, colour = "Share of risk"), size = 3) +
        scale_colour_manual(values = c("Portfolio weight" = "#5FA8D3", "Share of risk" = "#E0245E")) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Two gold miners carry almost half the fund's risk",
             subtitle = paste0("Top ", TopN, " risk contributors, ", format(max(RC$date), "%B %Y")),
             x = NULL, y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 8) Concentration + rand-hedge profile (two panels) ----------------------------
Plot_Conc_Hedge <- function(EffN, Hedge) {

    g1 <- EffN %>%
        ggplot(aes(date, EffN, colour = Name)) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = Pal) +
        scale_x_years(Breaks = "3 years") +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = "Concentration", subtitle = "Effective number of stocks (1 / sum of squared weights)",
             x = NULL, y = NULL) +
        theme_sleek()

    HLvls <- c("Pure Rand Hedge", "Rand Leverage", "Rand Neutral", "Rand Play", "Unclassified")
    g2 <- Hedge %>%
        mutate(hedge = factor(hedge, levels = rev(HLvls))) %>%
        ggplot(aes(Weight, Name, fill = hedge)) +
        geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
        geom_text(aes(label = ifelse(Weight > 0.06, paste0(round(Weight * 100), "%"), "")),
                  position = position_stack(vjust = 0.5), size = 2.7, colour = "white") +
        scale_fill_manual(values = c("Pure Rand Hedge" = "#1F3A5F", "Rand Leverage" = "#5FA8D3", "Rand Neutral" = "#C9A227",
                                     "Rand Play" = "#E0245E", "Unclassified" = "grey70"), breaks = HLvls) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Rand exposure", subtitle = "Weight by currency profile, latest holdings", x = NULL, y = NULL) +
        theme_sleek() + theme(legend.position = "bottom", legend.text = element_text(size = rel(0.75))) +
        guides(fill = guide_legend(nrow = 2))

    cowplot::plot_grid(g1, g2, ncol = 2, rel_widths = c(1, 1.15))
}

# --- 9) Own vs vendor scores: agreement over time + latest scatter -----------------
Plot_Score_Compare <- function(Agree, Own, Factor_Scores, SC_Holdings) {

    g1 <- Agree %>%
        ggplot(aes(date, RankCor, colour = Comparison)) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_line(linewidth = 0.7) +
        scale_colour_manual(values = c("#E0245E", "#2A9D8F")) +
        scale_x_years(Breaks = "1 year") +
        scale_y_continuous(limits = c(-0.5, 1)) +
        labs(title = "The own momentum score agrees with the vendor; the own low-vol score does not",
             subtitle = "Monthly rank (Spearman) correlation between the own scores and the vendor's",
             x = NULL, y = NULL) +
        theme_sleek() + guides(colour = guide_legend(nrow = 2))

    Last <- max(Own$date)
    Held <- SC_Holdings %>% filter(date == max(date)) %>% pull(Tickers)

    D <- Own %>% filter(date == Last) %>%
        inner_join(Factor_Scores %>% filter(date == Last) %>% pivot_wider(names_from = Factor, values_from = Factor_Value),
                   by = c("date", "Tickers")) %>%
        transmute(Tickers, Held = ifelse(Tickers %in% Held, "SafeCore holding", "Other stock"),
                  `Momentum: own (x) vs vendor (y)_x` = Momentum_Own, `Momentum: own (x) vs vendor (y)_y` = Momentum,
                  `Low Vol own (x) vs Vol. Sensitivity (y)_x` = LowVol_Own, `Low Vol own (x) vs Vol. Sensitivity (y)_y` = `Volatility Sensitivity`) %>%
        pivot_longer(-c(Tickers, Held), names_to = c("Panel", ".value"), names_sep = "_")

    Lab <- D %>% filter(Held == "SafeCore holding") %>% group_by(Panel) %>% slice_max(abs(x - y), n = 4)

    g2 <- D %>%
        ggplot(aes(x, y)) +
        geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "dashed") +
        geom_point(aes(colour = Held, size = Held), alpha = 0.75) +
        ggrepel::geom_text_repel(data = Lab, aes(label = Tickers), size = 2.6, colour = "#E0245E", seed = 1) +
        facet_wrap(~Panel) +
        scale_colour_manual(values = c("SafeCore holding" = "#E0245E", "Other stock" = "grey70")) +
        scale_size_manual(values = c("SafeCore holding" = 2.2, "Other stock" = 1.2)) +
        labs(subtitle = paste0("Scores at ", format(Last, "%B %Y"), " (labelled: SafeCore holdings where the two disagree most)"),
             x = "Own score (z)", y = "Vendor score") +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))

    cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(1, 1.15))
}

# --- Factor fingerprint: one radar per portfolio, SafeCore's shape overlaid -----------
# (idea from the R Graph Gallery: "radar chart" + "small multiples")
Plot_Fingerprint <- function(AE, Months = 12) {

    FLvls <- c("Momentum", "Momentum (own)", "Growth", "Value", "Earnings Quality", "Low Vol (own)", "Volatility Sensitivity")
    FLabs <- c("Momentum", "Momentum\n(own)", "Growth", "Value", "Quality", "Low vol\n(own)", "Vol.\nsensitivity")
    PLvls <- c("SafeCore", "Momentum", "Quality", "Value", "Low Vol")

    D <- AE %>%
        filter(date > max(date) %m-% months(Months), Factor %in% FLvls, Name %in% PLvls) %>%
        group_by(Name, Factor) %>% summarise(Active = mean(Active, na.rm = TRUE), .groups = "drop")

    # Polar -> cartesian by hand, so the polygons have straight edges
    Lim   <- ceiling(max(abs(D$Active)) * 10) / 10            # radial axis runs from -Lim (centre) to +Lim
    K     <- length(FLvls)
    Ang   <- tibble(Factor = FLvls, Lab = FLabs, Theta = 2 * pi * (seq_len(K) - 1) / K)
    XY    <- function(Val, Theta) tibble(x = (Val + Lim) * sin(Theta), y = (Val + Lim) * cos(Theta))

    Poly <- D %>% inner_join(Ang, by = "Factor") %>% arrange(Name, Theta) %>%
        bind_cols(XY(.$Active, .$Theta)) %>%
        mutate(Name = factor(Name, levels = PLvls))
    SC <- Poly %>% filter(Name == "SafeCore") %>% select(-Name)      # SafeCore's outline, repeated in every panel

    Rings <- expand_grid(Val = c(-Lim, -Lim / 2, 0, Lim / 2, Lim), Theta = c(Ang$Theta, 0)) %>%
        bind_cols(XY(.$Val, .$Theta))
    Spokes <- Ang %>% bind_cols(XY(Lim, .$Theta))
    Labs   <- Ang %>% bind_cols(XY(Lim * 1.42, .$Theta))
    RingLab <- tibble(Val = c(-Lim, 0, Lim)) %>% mutate(x = 0.04 * Lim, y = Val + Lim,
                                                         Lab = ifelse(Val == 0, "0 = benchmark", sprintf("%+.1f", Val)))

    ggplot() +
        geom_path(data = Rings, aes(x, y, group = Val, linewidth = Val == 0, colour = Val == 0), show.legend = FALSE) +
        geom_segment(data = Spokes, aes(x = 0, y = 0, xend = x, yend = y), colour = "grey88", linewidth = 0.3) +
        geom_polygon(data = Poly, aes(x, y, fill = Name, colour = Name), alpha = 0.35, linewidth = 0.7) +
        geom_polygon(data = SC, aes(x, y), fill = NA, colour = "#E0245E", linetype = "dashed", linewidth = 0.5) +
        geom_point(data = Poly, aes(x, y, colour = Name), size = 1.2) +
        geom_text(data = Labs, aes(x, y, label = Lab), size = 2.3, colour = "grey30", lineheight = 0.85) +
        geom_text(data = RingLab, aes(x, y, label = Lab), size = 2, colour = "grey45", hjust = 0, vjust = -0.3) +
        facet_wrap(~Name, ncol = 3) +
        scale_colour_manual(values = c(Pal, "TRUE" = "grey35", "FALSE" = "grey85"), guide = "none") +
        scale_linewidth_manual(values = c("TRUE" = 0.6, "FALSE" = 0.3)) +
        scale_fill_manual(values = Pal, guide = "none") +
        coord_equal(clip = "off") +
        labs(title = "SafeCore's factor fingerprint: quality and momentum, not low volatility",
             subtitle = paste0("Active exposure vs the Capped SWIX (weighted z-scores), average of the last ", Months,
                               " months.\nDashed pink outline = SafeCore, repeated in every panel for comparison"),
             caption = "Points outside the dark ring = more exposure than the benchmark. Own scores from price data; others from the third-party vendor.",
             x = NULL, y = NULL) +
        theme_sleek() +
        theme(axis.text = element_blank(), panel.grid.major.y = element_blank(), axis.line.x = element_blank(),
              strip.text = element_text(hjust = 0.5), panel.spacing = unit(22, "pt"))
}

# --- 11) Factor drift: SafeCore's active exposures over time --------------------------
Plot_Factor_Drift <- function(AE) {

    AE %>%
        filter(Name == "SafeCore") %>%
        ggplot(aes(date, Active)) +
        geom_events(MinDate = min(AE$date)) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_line(colour = "#E0245E", linewidth = 0.6) +
        facet_wrap(~Factor, ncol = 4) +
        scale_x_years(Breaks = "2 years") +
        labs(title = "How SafeCore's style has drifted",
             subtitle = "Active factor exposure vs the Capped SWIX, monthly",
             caption = Events_Caption(MinDate = min(AE$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- Weight vs risk: two waffle charts (100 squares each) ----------------------------
# (idea from the R Graph Gallery: "waffle chart")
Plot_Risk_Waffle <- function(RC, At = max(RC$date)) {

    # Each sector's share, rounded to whole squares out of 100 (largest-remainder rounding)
    Squares <- function(Share) {
        Share <- pmax(Share, 0) / sum(pmax(Share, 0)) * 100
        N <- floor(Share)
        Extra <- order(Share - N, decreasing = TRUE)[seq_len(100 - sum(N))]
        N[Extra] <- N[Extra] + 1
        N
    }

    S <- RC %>% filter(date == At) %>%
        group_by(Sector) %>% summarise(Weight = sum(weight), Risk = sum(RiskContrib), .groups = "drop") %>%
        mutate(Sector = factor(Sector, levels = names(Sector_Pal))) %>% arrange(Sector)

    W <- bind_rows(
        tibble(Panel = "Share of the fund's weight", Sector = rep(S$Sector, Squares(S$Weight))),
        tibble(Panel = "Share of the fund's risk",   Sector = rep(S$Sector, Squares(S$Risk)))) %>%
        group_by(Panel) %>% mutate(i = row_number() - 1, x = i %% 10, y = i %/% 10) %>% ungroup() %>%
        mutate(Panel = factor(Panel, levels = c("Share of the fund's weight", "Share of the fund's risk")))

    Res <- S %>% filter(Sector == "Resources")

    ggplot(W, aes(x, y, fill = Sector)) +
        geom_tile(colour = "white", linewidth = 1.2, width = 1, height = 1) +
        facet_wrap(~Panel) +
        scale_fill_manual(values = Sector_Pal, drop = TRUE) +
        coord_equal() +
        labs(title = paste0("Resources are ", round(Res$Weight * 100), "% of SafeCore, but ", round(Res$Risk * 100), "% of its risk"),
             subtitle = paste0("Each square = 1% of the fund, ", format(At, "%B %Y"),
                               " (risk = Euler contribution to portfolio variance)"),
             x = NULL, y = NULL) +
        theme_sleek() +
        theme(axis.text = element_blank(), panel.grid.major.y = element_blank(), axis.line.x = element_blank(),
              strip.text = element_text(hjust = 0.5, size = rel(1.05)))
}

