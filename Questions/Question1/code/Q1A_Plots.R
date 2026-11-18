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

# --- Correlation network: hierarchical edge bundling --------------------------------------
# (idea from the R Graph Gallery: "hierarchical edge bundling", built with ggraph)
# Each measure sits on a circle, grouped by family; every pair with a monthly correlation of
# at least Threshold is joined by a curve bundled through the family hubs, so links between
# families read as cables. Links to local (Capped SWIX) volatility are highlighted.
Plot_Cor_Bundle <- function(Panel, Threshold = 0.5, Focus = "Capped SWIX Vol") {

    Fam <- tribble(
        ~name,                 ~Group,
        "Capped SWIX Vol",     "Local equity",
        "MSCI ACWI Vol",       "Global equity",
        "MSCI EM Vol",         "Global equity",
        "VIX",                 "Implied volatility",
        "V2X",                 "Implied volatility",
        "VXEEM",               "Implied volatility",
        "Oil Vol",             "Oil",
        "MOVE (US bond vol)",  "Rates and currencies",
        "JPM Global FX vol",   "Rates and currencies",
        "SAVI USDZAR vol",     "Rates and currencies",
        "EPU Global",          "Policy news",
        "EPU Europe",          "Policy news",
        "EPU US (daily news)", "Policy news") %>%
        filter(name %in% names(Panel))
    Fam_Pal <- c("Local equity" = "#E0245E", "Global equity" = "#1F3A5F", "Implied volatility" = "#5FA8D3",
                 "Oil" = "#C9A227", "Rates and currencies" = "#2A9D8F", "Policy news" = "#8E6C8A")

    # Hierarchy: origin -> family -> measure (this sets the circular layout and the bundling hubs)
    Hier  <- bind_rows(tibble(from = "origin", to = unique(Fam$Group)), Fam %>% transmute(from = Group, to = name))
    Verts <- tibble(name = unique(c(Hier$from, Hier$to))) %>%
        left_join(Fam, by = "name") %>%
        mutate(Group = coalesce(Group, name), Group = factor(Group, levels = names(Fam_Pal)))
    G <- igraph::graph_from_data_frame(Hier, vertices = Verts)

    # Connections: pairs with |correlation| >= Threshold
    CM <- cor(Panel %>% select(all_of(Fam$name)))
    Links <- as_tibble(CM, rownames = "from") %>%
        pivot_longer(-from, names_to = "to", values_to = "r") %>%
        filter(match(from, Fam$name) < match(to, Fam$name), abs(r) >= Threshold) %>%
        mutate(Link = ifelse(from == Focus | to == Focus, "Linked to local volatility", "Other links"))
    Con <- ggraph::get_con(from = match(Links$from, Verts$name), to = match(Links$to, Verts$name),
                           r = Links$r, Link = Links$Link)

    ggraph::ggraph(G, layout = "dendrogram", circular = TRUE) +
        ggraph::geom_conn_bundle(data = Con, aes(edge_colour = Link, edge_alpha = r, edge_width = Link), tension = 0.85) +
        ggraph::geom_node_point(aes(filter = leaf, colour = Group), size = 4.2) +
        ggraph::geom_node_text(aes(filter = leaf, x = x * 1.09, y = y * 1.09, label = name, colour = Group,
                                   angle = ifelse(abs(atan2(y, x)) > pi / 2, atan2(y, x) * 180 / pi + 180, atan2(y, x) * 180 / pi),
                                   hjust = ifelse(abs(atan2(y, x)) > pi / 2, 1, 0)),     # left-half labels read outwards too
                               size = 3.1, fontface = "bold", show.legend = FALSE) +
        ggraph::scale_edge_colour_manual(values = c("Linked to local volatility" = "#E0245E", "Other links" = "#7E8C9A"), name = NULL) +
        ggraph::scale_edge_width_manual(values = c("Linked to local volatility" = 1.3, "Other links" = 0.5), guide = "none") +
        ggraph::scale_edge_alpha(range = c(0.25, 0.95), name = "Correlation") +
        scale_colour_manual(values = Fam_Pal, name = NULL) +
        coord_equal(clip = "off") +
        expand_limits(x = c(-1.6, 1.6), y = c(-1.45, 1.62)) +
        labs(title = "Local volatility is wired to market fear and oil, not to policy news",
             subtitle = paste0("Monthly correlations of at least ", Threshold, " between volatility and uncertainty measures, bundled by family"),
             x = NULL, y = NULL) +
        theme_sleek() +
        theme(axis.text = element_blank(), axis.line.x = element_blank(), panel.grid.major.y = element_blank(),
              legend.position = "right", legend.box = "vertical") +
        guides(colour = guide_legend(order = 1, override.aes = list(size = 3)))
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
