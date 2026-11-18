# Q1A_Plots.R
# Plot functions for Q1 Part A. Style comes from Theme_Events.R (theme_sleek + event bands).


# --- Rolling realised vol: small multiples, every other index greyed out for context ---
# (idea from the R Graph Gallery: "line chart with small multiples")
Plot_Roll_Vol <- function(RollVolDF) {

    Lvls <- c("Capped SWIX", "Top 40", "Mid Caps", "Small Caps",
              "MSCI ACWI", "MSCI EM", "S&P 500", "Euro Stoxx 50", "Nikkei 225")
    D <- RollVolDF %>% mutate(Name = factor(Name, levels = Lvls),
                              Panel = factor(paste0(Name, ifelse(Region == "Local (ZAR)", "  (local)", "  (global)")),
                                             levels = paste0(Lvls, rep(c("  (local)", "  (global)"), c(4, 5)))))

    ggplot(D, aes(date, RollVol)) +
        geom_events(MinDate = min(D$date)) +
        # every index in light grey in every panel (no Panel column, so it is repeated in each facet)
        geom_line(data = D %>% select(-Panel) %>% rename(Other = Name), aes(group = Other),
                  colour = "grey80", linewidth = 0.25) +
        geom_line(aes(colour = Name), linewidth = 0.6) +
        facet_wrap(~Panel, ncol = 3) +
        scale_colour_manual(values = Pal, guide = "none") +
        scale_x_years(Breaks = "3 years") + scale_y_pct() +
        labs(title = "Local and global volatility spike together",
             subtitle = "Rolling 3-month realised volatility, annualised. Each panel highlights one index; the others are in grey",
             caption = Events_Caption(MinDate = min(D$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- Calendar-year vol: heatmap of index x year ------------------------------------------
# (idea from the R Graph Gallery: "heatmap for time series")
Plot_Calendar_Vol <- function(CalVolDF) {

    Lvls <- c("Capped SWIX", "Top 40", "Mid Caps", "Small Caps",
              "MSCI EM", "MSCI ACWI", "S&P 500", "Euro Stoxx 50", "Nikkei 225")
    MaxYear <- max(CalVolDF$Year)

    CalVolDF %>%
        mutate(Name = factor(Name, levels = rev(Lvls)),
               Region = factor(Region, levels = c("Local (ZAR)", "Global (USD)")),
               Year = factor(ifelse(Year == MaxYear, paste0(Year, "*"), Year))) %>%
        ggplot(aes(Year, Name, fill = AnnVol)) +
        geom_tile(colour = "white", linewidth = 0.8) +
        geom_text(aes(label = round(AnnVol * 100), colour = AnnVol > 0.21), size = 2.7, show.legend = FALSE) +
        facet_grid(Region ~ ., scales = "free_y", space = "free_y") +
        scale_fill_gradientn(colours = c("#F7F4EF", "#F4C7A1", "#E76F51", "#9E2A2B", "#4A0E1E"),
                             values = scales::rescale(c(0.08, 0.15, 0.22, 0.32, 0.45)),
                             labels = scales::percent_format(accuracy = 1), name = "Volatility") +
        scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey20")) +
        labs(title = "In 2026 local volatility tracks emerging markets, not developed markets",
             subtitle = "Calendar-year realised volatility, annualised (%)",
             caption = paste0("* ", MaxYear, " = year to September."), x = NULL, y = NULL) +
        theme_sleek() +
        theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank(), legend.position = "right",
              legend.title = element_text(size = rel(0.8), colour = "grey35"), legend.key.height = unit(18, "pt"),
              strip.text.y = element_text(angle = 0, hjust = 0))
}

# --- GARCH conditional vol ------------------------------------------------------
Plot_Garch_Sigma <- function(SigmaDF) {

    SigmaDF %>%
        ggplot(aes(date, Sigma, colour = Name)) +
        geom_events(MinDate = min(SigmaDF$date)) +
        geom_line(linewidth = 0.45, alpha = 0.9) +
        scale_colour_manual(values = Pal) +
        scale_x_years() + scale_y_pct() +
        labs(title = "Conditional volatility: local vs global",
             subtitle = "GJR-GARCH(1,1), AR(1) mean, skewed-t errors, annualised",
             caption = Events_Caption(MinDate = min(SigmaDF$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- Rolling local-global correlation -------------------------------------------
Plot_Roll_Cor <- function(RollCorDF) {

    RollCorDF %>%
        ggplot(aes(date, RollCor, colour = Name)) +
        geom_events(MinDate = min(RollCorDF$date)) +
        geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
        geom_line(linewidth = 0.6) +
        scale_colour_manual(values = Pal) +
        scale_x_years() +
        scale_y_continuous(limits = c(0, 1)) +
        labs(title = "Correlations jump towards 1 in global sell-offs",
             subtitle = "Rolling 52-week correlation of Capped SWIX with global indexes (weekly returns)",
             caption = Events_Caption(MinDate = min(RollCorDF$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- Local vol vs offshore uncertainty (z-scores, small multiples) -------------
Plot_Drivers_Z <- function(Panel, LocalVol = "Capped SWIX Vol",
                           Drivers = c("VIX", "VXEEM", "Oil Vol", "MOVE (US bond vol)", "EPU Global", "SAVI USDZAR vol")) {

    Panel %>%
        mutate(across(-date, ~ as.numeric(scale(.x)))) %>%
        select(date, all_of(c(LocalVol, Drivers))) %>%
        pivot_longer(all_of(Drivers), names_to = "Driver", values_to = "Driver_Z") %>%
        mutate(Driver = factor(Driver, levels = Drivers)) %>%
        ggplot(aes(x = date)) +
        geom_events(MinDate = min(Panel$date), FacetVar = "Driver", FacetValue = Drivers[1], FacetLevels = Drivers) +
        geom_line(aes(y = .data[[LocalVol]], colour = "Capped SWIX volatility"), linewidth = 0.5) +
        geom_line(aes(y = Driver_Z, colour = "Uncertainty measure"), linewidth = 0.5) +
        facet_wrap(~Driver, ncol = 2) +
        scale_colour_manual(values = c("Capped SWIX volatility" = "#1F3A5F", "Uncertainty measure" = "#E76F51")) +
        scale_x_years(Breaks = "3 years") +
        labs(title = "Local volatility follows market fear and oil, not policy news",
             subtitle = "Monthly averages, standardised (z-scores)",
             caption = Events_Caption(MinDate = min(Panel$date)), x = NULL, y = NULL) +
        theme_sleek()
}

# --- Correlation heatmap (lower triangle only) ----------------------------------
Plot_Cor_Heatmap <- function(Panel) {

    CM <- cor(Panel %>% select(-date))
    Lvls <- colnames(CM)

    CM %>%
        as_tibble(rownames = "Var1") %>%
        pivot_longer(-Var1, names_to = "Var2", values_to = "Cor") %>%
        mutate(i = match(Var1, Lvls), j = match(Var2, Lvls)) %>%
        filter(i > j) %>%                                   # keep lower triangle only
        mutate(Var1 = factor(Var1, levels = rev(Lvls)), Var2 = factor(Var2, levels = Lvls)) %>%
        ggplot(aes(Var2, Var1, fill = Cor)) +
        geom_tile(colour = "white", linewidth = 0.6) +
        # White text on the dark (strong correlation) cells so the numbers stay readable
        geom_text(aes(label = sub("^-0[.]00$", "0.00", sprintf("%.2f", Cor)),
                      colour = abs(Cor) >= 0.6), size = 2.4, show.legend = FALSE) +
        scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey15")) +
        # Diverging scale: vivid blue (negative) - white (zero) - pink to deep raspberry (positive)
        scale_fill_gradientn(colours = c("#0057B8", "#7FB3E6", "#FFFFFF", "#FF8FAB", "#E0245E", "#8A0F3C"),
                             values = scales::rescale(c(-1, -0.5, 0, 0.35, 0.7, 1)), limits = c(-1, 1)) +
        labs(title = "How volatility and uncertainty measures move together",
             subtitle = "Correlation of monthly averages, 2015-2026", x = NULL, y = NULL) +
        theme_sleek() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.y = element_blank(),
              axis.line.x = element_blank(), legend.position = "right")
}

# --- Volatility regimes: arrows from pre-2020 to post-2020 average vol -----------------------
Plot_Vol_Regimes <- function(Regimes) {

    W <- Regimes %>%
        select(Sample, Region, Name, Period, AvgVol, HighVolDays) %>%
        pivot_wider(names_from = Period, values_from = c(AvgVol, HighVolDays)) %>%
        mutate(Sample = factor(Sample, levels = c("Full sample", "Excluding the COVID crash")),
               Name = fct_reorder(Name, `AvgVol_Post-2020` - `AvgVol_Pre-2020`),
               Lab = paste0(round(`HighVolDays_Pre-2020` * 100), "% -> ", round(`HighVolDays_Post-2020` * 100), "%"))

    ggplot(W, aes(y = Name)) +
        geom_segment(aes(x = `AvgVol_Pre-2020`, xend = `AvgVol_Post-2020`, yend = Name, colour = Region),
                     arrow = arrow(length = unit(0.18, "cm"), type = "closed"), linewidth = 0.9) +
        geom_point(aes(x = `AvgVol_Pre-2020`), colour = "grey55", size = 2) +
        geom_text(aes(x = pmax(`AvgVol_Pre-2020`, `AvgVol_Post-2020`), label = Lab), hjust = -0.08, size = 2.5, colour = "grey40") +
        facet_wrap(~Sample, nrow = 1) +
        scale_colour_manual(values = c("Local (ZAR)" = "#1F3A5F", "Global (USD)" = "#E76F51")) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.3))) +
        labs(title = "Outside COVID, local vol barely rose, but high-vol days became more common",
             subtitle = "Average rolling 3-month vol: pre-2020 (grey dot) to post-2020 (arrow head)",
             caption = "Labels: share of days above the index's own 80th percentile of vol, pre-2020 -> post-2020",
             x = "Average annualised volatility", y = NULL, colour = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- Event study: grouped bars of the rise in volatility -------------------------------------
Plot_Event_Vol <- function(EventLong) {

    EventLong %>%
        mutate(Event = fct_reorder(paste0(Event, " (", Type, ")"), Start, .desc = TRUE),
               Name = factor(Name, levels = c("Capped SWIX", "MSCI EM", "MSCI ACWI"))) %>%
        ggplot(aes(Rise, Event, fill = Name)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        geom_text(aes(label = sprintf("%.1f", Rise * 100)), position = position_dodge(width = 0.8),
                  hjust = -0.15, size = 2.6, colour = "grey30") +
        scale_fill_manual(values = Pal) +
        scale_x_continuous(labels = function(x) paste0(round(x * 100), "pp"), expand = expansion(mult = c(0, 0.12))) +
        labs(title = "Local shocks stay local and small; global shocks hit everyone",
             subtitle = "Rise in rolling 3-month volatility around each event (percentage points)",
             x = NULL, y = NULL, fill = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- DCC: local conditional volatility, coloured by the share that is imported -------------
# Each weekly point's height is the Capped SWIX's conditional volatility (from the DCC model's
# univariate GJR-GARCH), and its colour is the share of that week's conditional variance that
# moves with global markets (MSCI ACWI and MSCI EM together).
Plot_DCC_Imported <- function(Imported, Events = Q1_Events) {

    D <- Imported %>% arrange(date) %>% mutate(xend = lead(date), yend = lead(LocalVol)) %>% filter(!is.na(xend))
    Avg <- mean(Imported$Share)

    # Label the most imported and the most home-grown episodes around the shaded events
    Lab <- Events %>%
        mutate(Pt = map2(Start, End, ~ Imported %>% filter(date >= .x - 7, date <= .y + 30) %>%
                             summarise(Share = mean(Share), date = date[which.max(LocalVol)], LocalVol = max(LocalVol)))) %>%
        unnest(Pt) %>%
        mutate(Txt = paste0(Label, ": ", round(Share * 100), "% imported"))

    ggplot(D) +
        geom_events(MinDate = min(D$date)) +
        geom_segment(aes(x = date, y = LocalVol, xend = xend, yend = yend, colour = Share), linewidth = 1.1, lineend = "round") +
        ggrepel::geom_label_repel(data = Lab, aes(date, LocalVol, label = Txt), size = 2.7, seed = 4,
                                  fill = alpha("white", 0.9), label.size = 0, colour = "grey20",
                                  nudge_y = 0.06, min.segment.length = 0, segment.colour = "grey50") +
        scale_colour_gradientn(colours = c("#2A9D8F", "#8CC5BC", "#A9B2BC", "#F08FAE", "#E0245E", "#8A0F3C"),
                               values = scales::rescale(c(0.35, 0.45, Avg, 0.58, 0.64, 0.70)),
                               limits = c(0.34, 0.70), labels = scales::percent_format(accuracy = 1),
                               name = "Share of local variance\nexplained by global markets") +
        scale_x_years() + scale_y_pct() +
        labs(title = "About half of local volatility is imported; in global crashes, two-thirds",
             subtitle = "Capped SWIX DCC-GARCH conditional volatility (weekly, annualised), coloured by imported share",
             caption = Events_Caption(MinDate = min(D$date)), x = NULL, y = NULL) +
        theme_sleek() +
        theme(legend.position = "right", legend.title = element_text(size = rel(0.75), colour = "grey30"),
              legend.key.height = unit(26, "pt"))
}
