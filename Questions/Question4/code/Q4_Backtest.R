# Q4_Backtest.R
# Out-of-sample performance of the quarterly-rebalanced strategies, and turnover.

# --- Portfolio returns with drifting weights ---------------------------
# Weights set at a quarter-end apply from the next month (lag_weights = TRUE) and drift
# with returns until the next rebalance.
Q4_Backtest <- function(Weights, Univ) {

    R_xts <- Univ %>%
        filter(YM > min(Weights$YM)) %>%
        transmute(date = as.Date(paste0(YM, "-01")), Asset, Ret) %>%
        tbl2xts::tbl_xts(cols_to_xts = Ret, spread_by = Asset)
    R_xts[is.na(R_xts)] <- 0

    map_dfr(unique(Weights$Strategy), function(s) {
        W_xts <- Weights %>% filter(Strategy == s) %>%
            transmute(date = as.Date(paste0(YM, "-01")), Asset, Weight) %>%
            tbl2xts::tbl_xts(cols_to_xts = Weight, spread_by = Asset)
        W_xts[is.na(W_xts)] <- 0
        R <- R_xts[, colnames(W_xts)]
        P <- rmsfuns::Safe_Return.portfolio(R = R, weights = W_xts, lag_weights = TRUE, geometric = TRUE)
        tibble(date = as.Date(zoo::index(P)), Strategy = s, Ret = as.numeric(P))
    })
}

# --- Turnover at each rebalance ---------------------------------------------------------------------
# One-way turnover = half the sum of absolute weight changes (target vs previous target).
Q4_Turnover <- function(Weights) {
    Weights %>%
        select(YM, Strategy, Asset, Weight) %>%
        complete(YM, Strategy, Asset, fill = list(Weight = 0)) %>%
        group_by(Strategy, Asset) %>% arrange(YM) %>%
        mutate(Change = abs(Weight - lag(Weight))) %>%
        group_by(Strategy, YM) %>% summarise(Turnover = 0.5 * sum(Change, na.rm = TRUE), .groups = "drop") %>%
        filter(YM != min(YM))
}

# --- Summary statistics -----------------------------------------------------------------------------
Q4_Stats <- function(Port, Cash) {
    Port %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        left_join(Cash, by = "YM") %>%
        group_by(Strategy) %>% arrange(date) %>%
        summarise(`Ann. return` = prod(1 + Ret)^(12 / n()) - 1,
                  `Ann. volatility` = sd(Ret) * sqrt(12),
                  Sharpe = mean(Ret - Rf) / sd(Ret) * sqrt(12),
                  `Max drawdown` = min(cumprod(1 + Ret) / cummax(cumprod(1 + Ret)) - 1),
                  `Worst month` = min(Ret), .groups = "drop")
}
