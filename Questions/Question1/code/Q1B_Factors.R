# Q1B_Factors.R
# Own Momentum and Low-Volatility scores from price behaviour, comparison with the
# third-party scores, and factor exposures ("fingerprints") of SafeCore vs the
# benchmark and the factor (strategy) indexes.

# --- Helper: cross-sectional z-score, winsorised at +/-3 -------------------------
# Scores are standardised *across stocks* each month, so a score of +1 means
# "one standard deviation above the average stock that month". Capping at +/-3
# stops a few illiquid stocks with extreme moves from dominating.
Z_Win <- function(x, Cap = 3) {
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    z <- pmin(pmax(z, -Cap), Cap)
    (z - mean(z, na.rm = TRUE)) / sd(z, na.rm = TRUE)    # re-standardise after capping
}

# --- Own scores at each month-end -----------------------------------------------
# Momentum: return from 6 months ago to 1 month ago (126 to 21 trading days back).
#   The most recent month is skipped because very short-term returns tend to
#   reverse, which would add noise to a trend-following signal.
# Low Vol: minus the annualised volatility of the past 6 months (126 days), so
#   that a HIGH score = a LOW-volatility stock (same direction as the vendor:
#   higher score = more of the factor).
# Universe = stocks the third party scores that month, so the two are comparable.
Q1B_Own_Scores <- function(Stock_Rets, Factor_Scores, MinObs = 126) {   # need a full 6-month window

    Dates <- sort(unique(Factor_Scores$date))

    map_dfr(Dates, function(d) {

        Univ <- Factor_Scores %>% filter(date == d) %>% distinct(Tickers) %>% pull(Tickers)

        Stock_Rets %>%
            filter(Tickers %in% Univ, date <= d, date > d - 200) %>%
            group_by(Tickers) %>% arrange(date) %>%
            filter(n() >= MinObs) %>%
            summarise(Mom = prod(1 + tail(Returns, 126)[1:105]) - 1,       # skip last 21 days
                      Vol = sd(tail(Returns, 126)) * sqrt(252),
                      .groups = "drop") %>%
            mutate(date = d,
                   Momentum_Own = Z_Win(Mom),
                   LowVol_Own   = Z_Win(-Vol))
    })
}

# --- Agreement with the vendor, month by month -----------------------------------
# Spearman (rank) correlation: does the vendor rank stocks the same way we do?
Q1B_Score_Agreement <- function(Own, Factor_Scores) {

    Vendor <- Factor_Scores %>%
        filter(Factor %in% c("Momentum", "Volatility Sensitivity")) %>%
        pivot_wider(names_from = Factor, values_from = Factor_Value)

    Own %>%
        inner_join(Vendor, by = c("date", "Tickers")) %>%
        group_by(date) %>%
        summarise(`Momentum: own vs vendor` = cor(Momentum_Own, Momentum, method = "spearman"),
                  `Low Vol (own) vs vendor Vol. Sensitivity` = cor(LowVol_Own, `Volatility Sensitivity`, method = "spearman"),
                  .groups = "drop") %>%
        pivot_longer(-date, names_to = "Comparison", values_to = "RankCor")
}

# --- Long table of all scores (vendor + own) per stock-month -------------------------
Q1B_All_Scores <- function(Own, Factor_Scores) {

    bind_rows(
        Factor_Scores %>% select(date, Tickers, Factor, Score = Factor_Value),
        Own %>% select(date, Tickers, `Momentum (own)` = Momentum_Own, `Low Vol (own)` = LowVol_Own) %>%
            pivot_longer(-c(date, Tickers), names_to = "Factor", values_to = "Score")
    )
}

# --- Portfolio factor exposures over time ----------------------------------------
# Exposure = weighted average of the stock scores (weights rescaled to 100% over
# the stocks that have a score). Done for SafeCore, the Capped SWIX and the
# factor indexes, using month-end weights.
Q1B_Port_Exposures <- function(All_Scores, SC_Holdings, Index_Constituents,
                               Indexes = c("Capped SWIX", "Value", "Momentum", "LowVol", "Quality")) {

    IC_M <- Index_Constituents %>%
        filter(Index %in% Indexes) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Index, YM) %>% filter(date == max(date)) %>% ungroup() %>%
        transmute(YM, Name = recode(Index, "LowVol" = "Low Vol"), Tickers, weight)

    SC_M <- SC_Holdings %>% transmute(YM = format(date, "%Y-%m"), Name = "SafeCore", Tickers, weight)

    # many-to-many is intended: each holding gets all 7 factor scores, and each
    # stock-score can sit in several portfolios
    bind_rows(IC_M, SC_M) %>%
        inner_join(All_Scores %>% mutate(YM = format(date, "%Y-%m")) %>% select(-date),
                   by = c("YM", "Tickers"), relationship = "many-to-many") %>%
        filter(!is.na(Score)) %>%
        group_by(Name, YM, Factor) %>%
        summarise(Exposure = sum(weight * Score) / sum(weight), .groups = "drop") %>%
        mutate(date = as.Date(paste0(YM, "-01"))) %>%
        select(date, Name, Factor, Exposure)
}

# --- Active exposures: relative to the Capped SWIX --------------------------------
# The vendor's scores turn out to be benchmark-relative (the Capped SWIX scores ~0
# on every vendor factor), while our own scores are standardised across all stocks.
# Subtracting the Capped SWIX exposure puts everything on the same footing and
# answers the manager's real question: how does the fund differ from its benchmark?
Q1B_Active_Exposures <- function(Exposures, Bench = "Capped SWIX") {

    Exposures %>%
        filter(Name != Bench) %>%
        inner_join(Exposures %>% filter(Name == Bench) %>% select(date, Factor, BenchExp = Exposure),
                   by = c("date", "Factor")) %>%
        mutate(Active = Exposure - BenchExp)
}
