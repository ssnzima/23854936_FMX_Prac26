# Q1C_Opportunity.R
# Rolling inter-constituent correlations, an alpha-opportunity index, and checks of
# whether the opportunity was actually realised in returns.

# --- Rolling pairwise correlations of the Large Caps --------------------------------
# Every month-end: correlation matrix of the past 63 trading days of daily returns,
# keep all pairwise correlations (upper triangle) and summarise their distribution.
# Using the distribution (median + inter-quartile range) rather than one average shows
# whether *all* stocks were moving together, or just some.
Q1C_Roll_PairCor <- function(Index_Constituents, IndexName = "Large Caps", Window = 63, MinObs = 50) {

    IC <- Index_Constituents %>% filter(Index == IndexName)
    Dates <- IC %>% distinct(date) %>% mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>% summarise(date = max(date), .groups = "drop") %>% pull(date)
    AllDays <- sort(unique(IC$date))

    map_dfr(Dates[Dates >= AllDays[Window]], function(d) {

        Members <- IC %>% filter(date == d) %>% pull(Tickers)          # current constituents
        Win <- AllDays[AllDays <= d] %>% tail(Window)

        M <- IC %>%
            filter(date %in% Win, Tickers %in% Members) %>%
            select(date, Tickers, Return) %>%
            pivot_wider(names_from = Tickers, values_from = Return) %>%
            select(-date) %>% as.matrix()
        M <- M[, colSums(!is.na(M)) >= MinObs, drop = FALSE]

        C <- cor(M, use = "pairwise.complete.obs")
        pc <- C[upper.tri(C)]

        tibble(date = lubridate::floor_date(d, "month"),
               Median = median(pc, na.rm = TRUE), Mean = mean(pc, na.rm = TRUE),
               P25 = quantile(pc, 0.25, na.rm = TRUE), P75 = quantile(pc, 0.75, na.rm = TRUE),
               Stocks = ncol(M))
    })
}

# --- Alpha Opportunity Index --------------------------------------------------------
# High dispersion and low correlation both mean more room for stock selection, so
# the index is z(dispersion) - z(correlation). Both are standardised over the sample,
# so 0 = an average month, and the units are standard deviations.
Q1C_Opportunity <- function(MonthlyCSV, PairCor) {

    MonthlyCSV %>%
        select(date, CSV_W) %>%
        inner_join(PairCor %>% select(date, AvgCor = Mean), by = "date") %>%
        mutate(Opportunity = as.numeric(scale(CSV_W)) - as.numeric(scale(AvgCor)))
}

# --- Was the opportunity realised? -----------------------------------------------------
# (1) Quintile spread: each month, average return of the best 20% of Capped SWIX
#     stocks minus the worst 20%. This is the payoff to a (perfect-foresight) stock
#     picker, so it measures how much there was to gain from picking the right stocks.
# (2) Factor spread: best minus worst monthly return across the Value, Momentum,
#     Quality and Low Vol indexes (built from their constituents) - the payoff to
#     choosing the right investment style.
Q1C_Realised_Spreads <- function(Index_Constituents) {

    Index_Constituents <- Q1C_Clean_IC(Index_Constituents)     # same cleaning as the dispersion

    Monthly_Stock <- Index_Constituents %>%
        filter(Index == "Capped SWIX") %>%
        mutate(date = lubridate::floor_date(date, "month")) %>%
        group_by(date, Tickers) %>%
        summarise(Ret = prod(1 + Return) - 1, Days = n(), .groups = "drop") %>%
        group_by(date) %>% filter(Days >= max(Days) - 2) %>% ungroup()   # full-month stocks only

    Quint <- Monthly_Stock %>%
        group_by(date) %>%
        mutate(Q = ntile(Ret, 5)) %>%
        summarise(Quintile_Spread = mean(Ret[Q == 5]) - mean(Ret[Q == 1]), .groups = "drop")

    Factor <- Index_Constituents %>%
        filter(Index %in% c("Value", "Momentum", "Quality", "LowVol")) %>%
        group_by(Index, date) %>%
        summarise(R = sum(weight * Return) / sum(weight), .groups = "drop") %>%
        mutate(date = lubridate::floor_date(date, "month")) %>%
        group_by(Index, date) %>% summarise(R = prod(1 + R) - 1, .groups = "drop") %>%
        group_by(date) %>%
        summarise(Factor_Spread = max(R) - min(R), .groups = "drop")

    inner_join(Quint, Factor, by = "date")
}
