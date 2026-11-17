# Q1A_Plots.R
# Plot functions for Q1 Part A. Style comes from Theme_Events.R (theme_sleek + event bands).


# --- Rolling realised vol: local panel vs global panel --------------------------
Plot_Roll_Vol <- function(RollVolDF) {

    RegLvls <- c("Local (ZAR)", "Global (USD)")

    RollVolDF %>%
        mutate(Region = factor(Region, levels = RegLvls)) %>%
        ggplot(aes(date, RollVol, colour = Name)) +
        geom_events(MinDate = min(RollVolDF$date), FacetVar = "Region", FacetValue = "Local (ZAR)", FacetLevels = RegLvls) +
        geom_line(linewidth = 0.5) +
        facet_wrap(~Region, ncol = 1) +
        scale_colour_manual(values = Pal, breaks = names(Pal)) +
        scale_x_years() + scale_y_pct() +
        labs(title = "Local and global volatility spike together",
             subtitle = "Rolling 3-month realised volatility, annualised",
             caption = Events_Caption(MinDate = min(RollVolDF$date)), x = NULL, y = NULL) +
        theme_sleek() +
        guides(colour = guide_legend(nrow = 2, byrow = TRUE))
}

# --- Calendar-year vol bars -----------------------------------------------------
Plot_Calendar_Vol <- function(CalVolDF, Names = c("Capped SWIX", "MSCI EM", "MSCI ACWI", "S&P 500")) {

    CalVolDF %>%
        filter(Name %in% Names) %>%
        mutate(Name = factor(Name, levels = Names)) %>%
        ggplot(aes(factor(Year), AnnVol, fill = Name)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        scale_fill_manual(values = Pal) +
        scale_y_pct() +
        labs(title = "In 2026 local volatility tracks emerging markets, not developed markets",
             subtitle = "Calendar-year realised volatility, annualised (2026 = year to September)",
             x = NULL, y = NULL) +
        theme_sleek()
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
