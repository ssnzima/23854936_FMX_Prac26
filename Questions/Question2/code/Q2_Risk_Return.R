# Q2_Risk_Return.R
# Risk (constituent and index volatility, diversification) and return (rolling
# returns, risk-return summary) of the size indexes.

# --- Realised volatility of the constituents, month by month -----------------------
# For every stock: rolling 126-day (6-month) annualised volatility of daily returns,
# sampled at month-ends. Each month, the stocks in each index are matched to these
# values, and I keep the median, inter-quartile range and weighted average.
# Daily returns above 100% in absolute value are dropped as data errors.
Q2_Constituent_Vol <- function(Holdings, Stock_Rets, Window = 126, MinObs = 100) {

    Stock_Vol <- Stock_Rets %>%
        filter(abs(Returns) <= 1) %>%
        group_by(Tickers) %>% arrange(date) %>%
        mutate(Vol = RcppRoll::roll_sd(Returns, n = Window, fill = NA, align = "right") * sqrt(252),
               N = row_number()) %>%
        ungroup() %>%
        filter(!is.na(Vol), N >= MinObs) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Tickers, YM) %>% filter(date == max(date)) %>% ungroup() %>%
        select(YM, Tickers, Vol)

    Holdings %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        inner_join(Stock_Vol, by = c("YM", "Tickers")) %>%
        group_by(date, Index) %>%
        summarise(Median = median(Vol), P25 = quantile(Vol, 0.25), P75 = quantile(Vol, 0.75),
                  WtdVol = sum(weight * Vol) / sum(weight), Coverage = sum(weight), .groups = "drop")
}

# --- Diversification ratio -------------------------------------------------------------
# Index volatility / weighted average volatility of its stocks (both over the last
# 6 months). A ratio well below 1 means the stocks' moves largely cancel out in the
# index (they are not very correlated) - so a calm index can hide volatile stocks.
Q2_Diversification <- function(ConstVol, Index_Rets, Window = 126) {

    Idx_Vol <- Index_Rets %>%
        group_by(Name) %>% arrange(date) %>%
        mutate(IdxVol = RcppRoll::roll_sd(Returns, n = Window, fill = NA, align = "right") * sqrt(252)) %>%
        ungroup() %>% filter(!is.na(IdxVol)) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Name, YM) %>% filter(date == max(date)) %>% ungroup() %>%
        select(YM, Index = Name, IdxVol)

    ConstVol %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        inner_join(Idx_Vol, by = c("YM", "Index")) %>%
        mutate(DivRatio = IdxVol / WtdVol)
}

# --- Rolling annualised returns ---------------------------------------------------------
# Built from monthly returns; e.g. the 3-year figure at each month-end is the
# annualised return over the previous 36 months.
Q2_Rolling_Returns <- function(Index_Rets, Years = c(1, 3), Bench = "Capped SWIX") {

    M <- Index_Rets %>%
        mutate(date = lubridate::floor_date(date, "month")) %>%
        group_by(Name, date) %>% summarise(R = prod(1 + Returns) - 1, .groups = "drop")

    Roll <- map_dfr(Years, function(y) {
        M %>%
            group_by(Name) %>% arrange(date) %>%
            mutate(RollRet = RcppRoll::roll_prod(1 + R, n = 12 * y, fill = NA, align = "right")^(1 / y) - 1,
                   Horizon = paste0(y, "-year")) %>%
            ungroup() %>% filter(!is.na(RollRet))
    })

    Roll %>%
        left_join(Roll %>% filter(Name == Bench) %>% select(date, Horizon, BenchRet = RollRet),
                  by = c("date", "Horizon")) %>%
        mutate(Relative = RollRet - BenchRet)
}

# --- Full-period risk-return summary -------------------------------------------------------
Q2_Risk_Return <- function(Index_Rets, STeFI) {

    From <- min(Index_Rets$date); To <- max(Index_Rets$date)
    Rf <- STeFI %>% filter(date >= From, date <= To) %>%
        summarise(Rf = prod(1 + Returns)^(252 / n()) - 1) %>% pull(Rf)

    Index_Rets %>%
        group_by(Name) %>% arrange(date) %>%
        summarise(AnnRet = prod(1 + Returns)^(252 / n()) - 1,
                  AnnVol = sd(Returns) * sqrt(252),
                  MaxDD  = min(cumprod(1 + Returns) / cummax(cumprod(1 + Returns)) - 1),
                  .groups = "drop") %>%
        mutate(Sharpe = (AnnRet - Rf) / AnnVol)
}

# --- News impact curves (from the fitted GARCH models) ---------------------------------
# How much does next-day volatility react to a shock (a surprise return) of a given
# size and sign? Evaluated on a common grid of daily shocks from -5% to +5% for all
# indexes, and shown as annualised volatility. An asymmetric (lopsided) curve means
# volatility jumps after falls but not after rises - the leverage effect.
Q2_News_Impact <- function(Fits, Shocks = seq(-0.05, 0.05, length.out = 201)) {

    imap_dfr(Fits, function(fit, n) {
        ni <- rugarch::newsimpact(object = fit, z = Shocks)
        tibble(Name = n, Shock = as.numeric(ni$zx), Vol = sqrt(as.numeric(ni$zy) * 252))
    })
}
