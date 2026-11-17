# Q1B_Risk.R
# Where does SafeCore's volatility come from? Risk contributions (by stock and
# sector), concentration, and the fund's rand-hedge profile.

# --- Euler risk contributions at each month-end --------------------------------
# For weights w and covariance matrix S, portfolio variance is w'Sw. Each stock's
# contribution is w_i * (Sw)_i / (w'Sw), so contributions sum to 100% and show how
# much of the fund's risk each stock is responsible for - which can be very
# different from its weight (a volatile, highly correlated stock "punches above
# its weight").
# The covariance matrix uses the past year of daily returns and is shrunk towards
# a structured target (Ledoit-Wolf): with ~40 stocks and 252 days
# the sample covariance matrix is noisy, and shrinkage stabilises it.
Q1B_Risk_Contrib <- function(Holdings, Stock_Rets, Lookback = 252, MinObs = 200,
                             From = as.Date("2015-12-01")) {

    Dates <- Holdings %>% filter(date >= From) %>% distinct(date) %>% pull(date)

    map_dfr(Dates, function(d) {

        H <- Holdings %>% filter(date == d)

        R <- Stock_Rets %>%
            filter(Tickers %in% H$Tickers, date <= d, date > d - 400) %>%
            group_by(Tickers) %>% filter(n() >= MinObs) %>% ungroup() %>%
            select(date, Tickers, Returns) %>%
            pivot_wider(names_from = Tickers, values_from = Returns) %>%
            arrange(date) %>% tail(Lookback)

        Mat <- as.matrix(R[, -1]); Mat[is.na(Mat)] <- 0
        H <- H %>% filter(Tickers %in% colnames(Mat)) %>% mutate(weight = weight / sum(weight))
        Mat <- Mat[, H$Tickers]

        S <- RiskPortfolios::covEstimation(Mat, control = list(type = "lw"))
        w <- H$weight
        RC <- as.numeric(w * (S %*% w)) / as.numeric(t(w) %*% S %*% w)

        H %>% mutate(date = d, RiskContrib = RC, PortVol = sqrt(as.numeric(t(w) %*% S %*% w) * 252))
    })
}

# --- Effective number of stocks (concentration) --------------------------------
# 1 / sum(w^2): equals N for an equally weighted portfolio, and falls as the
# portfolio becomes more concentrated in a few names.
Q1B_Effective_N <- function(SC_Holdings, Index_Constituents, From = as.Date("2015-01-01")) {

    SC <- SC_Holdings %>%
        filter(date >= From) %>%
        group_by(date) %>% mutate(weight = weight / sum(weight)) %>%
        summarise(EffN = 1 / sum(weight^2), .groups = "drop") %>%
        mutate(Name = "SafeCore")

    # Capped SWIX: month-end weights from the constituent file
    J433 <- Index_Constituents %>%
        filter(Index == "Capped SWIX") %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>% filter(date == max(date)) %>% ungroup() %>%
        group_by(date) %>% mutate(weight = weight / sum(weight)) %>%
        summarise(EffN = 1 / sum(weight^2), .groups = "drop") %>%
        mutate(Name = "Capped SWIX")

    bind_rows(SC, J433)
}

# --- Rand-hedge profile: SafeCore vs Capped SWIX at the latest date -------------
# Note: the hedge classification is only given at the latest date, so this is a
# snapshot of the current portfolios, not a history.
Q1B_Hedge_Profile <- function(SC_Holdings, Index_Constituents, Hedge) {

    Hedge <- Hedge %>% mutate(hedge = ifelse(is.na(hedge) | hedge == "", "Unclassified", hedge))

    SC <- SC_Holdings %>% filter(date == max(date)) %>% mutate(Name = "SafeCore")
    J433 <- Index_Constituents %>%
        filter(Index == "Capped SWIX", date == max(date[format(date, "%Y-%m") == format(max(SC$date), "%Y-%m")])) %>%
        mutate(Name = "Capped SWIX")

    bind_rows(SC %>% select(Name, Tickers, weight), J433 %>% select(Name, Tickers, weight)) %>%
        left_join(Hedge %>% select(Tickers, hedge), by = "Tickers") %>%
        mutate(hedge = replace_na(hedge, "Unclassified")) %>%
        group_by(Name) %>% mutate(weight = weight / sum(weight)) %>%
        group_by(Name, hedge) %>% summarise(Weight = sum(weight), .groups = "drop")
}
