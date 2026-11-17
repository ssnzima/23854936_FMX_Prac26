# Q2_Extension.R
# Beyond size: what is an investor really buying with each size index?
# Uses data the question does not ask for (factor scores, rand-hedge classification,
# MSCI global small & mid cap index) to look at the hidden style and currency bets.

# --- Factor exposures of each size index, relative to the Capped SWIX --------------
# Weighted average of the third-party factor scores of each index's holdings, averaged
# over the last 12 months, minus the Capped SWIX's exposure (the vendor scores are
# benchmark-relative, as found in Question 1).
Q2_Factor_Exposures <- function(Holdings, Factor_Scores, Months = 12, Bench = "Capped SWIX") {

    From <- max(Factor_Scores$date) %m-% months(Months - 1)

    E <- Holdings %>%
        filter(date >= lubridate::floor_date(From, "month")) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        inner_join(Factor_Scores %>% mutate(YM = format(date, "%Y-%m")) %>% select(-date),
                   by = c("YM", "Tickers"), relationship = "many-to-many") %>%
        group_by(Index, YM, Factor) %>%
        summarise(Exposure = sum(weight * Factor_Value) / sum(weight), .groups = "drop") %>%
        group_by(Index, Factor) %>% summarise(Exposure = mean(Exposure), .groups = "drop")

    E %>%
        filter(Index != Bench) %>%
        left_join(E %>% filter(Index == Bench) %>% select(Factor, BenchExp = Exposure), by = "Factor") %>%
        mutate(Active = Exposure - BenchExp)
}

# --- Rand-hedge profile of each index (latest holdings) ------------------------------
# The classification is only available at the latest date, so this is a snapshot.
Q2_Hedge_Profile <- function(Holdings, Hedge) {

    Hedge <- Hedge %>% mutate(hedge = ifelse(is.na(hedge) | hedge == "", "Unclassified", hedge))

    Holdings %>%
        filter(date == max(date)) %>%
        left_join(Hedge %>% select(Tickers, hedge), by = "Tickers") %>%
        mutate(hedge = replace_na(hedge, "Unclassified")) %>%
        group_by(Index, hedge) %>% summarise(Weight = sum(weight), .groups = "drop")
}

# --- Is the SA size effect a global one? -------------------------------------------------
# Monthly small-minus-large and mid-minus-large returns for SA (J202 / J201 minus J200)
# compared with the global equivalent (MSCI World small & mid caps minus MSCI World).
Q2_Global_Size <- function(Idx_Daily, MSCI) {

    To_Monthly <- function(df) df %>%
        mutate(date = lubridate::floor_date(date, "month")) %>%
        group_by(Name, date) %>% summarise(R = prod(1 + Returns) - 1, .groups = "drop")

    Local <- Idx_Daily %>% filter(Tickers %in% c("J200", "J201", "J202")) %>%
        transmute(date, Name = Tickers, Returns) %>% To_Monthly()

    Global <- MSCI %>% filter(Name %in% c("MSCI_SCMC", "MSCI_AllWorld")) %>%
        group_by(Name) %>% arrange(date) %>% mutate(Returns = TRI / lag(TRI) - 1) %>% ungroup() %>%
        filter(!is.na(Returns), date >= min(Local$date)) %>% select(date, Name, Returns) %>% To_Monthly()

    W <- bind_rows(Local, Global) %>%
        pivot_wider(names_from = Name, values_from = R) %>% drop_na() %>%
        transmute(date, SA_Small = J202 - J200, SA_Mid = J201 - J200, Global_SMID = MSCI_SCMC - MSCI_AllWorld)

    tibble(Spread = c("SA small minus large", "SA mid minus large", "Global small & mid minus world"),
           AnnPremium = c(mean(W$SA_Small), mean(W$SA_Mid), mean(W$Global_SMID)) * 12,
           CorWithGlobal = c(cor(W$SA_Small, W$Global_SMID), cor(W$SA_Mid, W$Global_SMID), 1))
}

# --- Plot: diverging factor bars (top) + rand-hedge waffle grids (bottom) ----------------
# Waffle: each index is drawn as 100 squares, coloured by the share of its weight in
# each rand-hedge group (rounded to whole squares with the largest-remainder method).
Plot_Q2_Hidden_Bets <- function(FE, HP) {

    Idx <- c("Large Caps", "Mid Caps", "Small Caps")

    g1 <- FE %>%
        mutate(Index = factor(Index, levels = Idx),
               Factor = fct_reorder(Factor, Active * (Index == "Small Caps"), .fun = sum)) %>%
        ggplot(aes(Active, Factor, fill = Active > 0)) +
        geom_col(width = 0.65) +
        geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
        geom_text(aes(label = sprintf("%+.2f", Active), hjust = ifelse(Active > 0, -0.15, 1.15)), size = 2.6, colour = "grey30") +
        facet_wrap(~Index, nrow = 1) +
        scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E0245E"), guide = "none") +
        scale_x_continuous(expand = expansion(mult = 0.35)) +
        labs(title = "Size is really a style bet: small caps are deep value with weak momentum",
             subtitle = "Factor exposure relative to the Capped SWIX (weighted vendor z-scores, last 12 months)",
             x = NULL, y = NULL) +
        theme_sleek() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))

    HLvls <- c("Pure Rand Hedge", "Rand Leverage", "Rand Neutral", "Rand Play", "Unclassified")
    Squares <- HP %>%
        filter(Index %in% Idx) %>%
        group_by(Index) %>%
        mutate(hedge = factor(hedge, levels = HLvls), Raw = Weight / sum(Weight) * 100, N = floor(Raw),
               N = N + (rank(-(Raw - N), ties.method = "first") <= (100 - sum(N)))) %>%   # largest remainder
        arrange(Index, hedge) %>%
        reframe(hedge = rep(hedge, N)) %>%                   # one row per square
        group_by(Index) %>% mutate(i = row_number() - 1, x = i %% 10, y = i %/% 10) %>% ungroup() %>%
        mutate(Index = factor(Index, levels = Idx))

    g2 <- Squares %>%
        ggplot(aes(x, y, fill = hedge)) +
        geom_tile(colour = "white", linewidth = 0.6) +
        facet_wrap(~Index, nrow = 1) +
        scale_fill_manual(values = c("Pure Rand Hedge" = "#1F3A5F", "Rand Leverage" = "#5FA8D3", "Rand Neutral" = "#C9A227",
                                     "Rand Play" = "#E0245E", "Unclassified" = "grey80"), drop = FALSE) +
        coord_equal() +
        labs(title = "...and small caps are the most rand-hedged, not the most rand-exposed",
             subtitle = "Rand-hedge profile of the latest holdings: each square = 1% of the index", x = NULL, y = NULL, fill = NULL) +
        theme_sleek() + theme(axis.text = element_blank(), axis.line.x = element_blank(),
                              panel.grid.major.y = element_blank(), legend.position = "bottom")

    cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(1, 1.15))
}
