# Q2_Stratification.R
# Stratification (as in Practical 2's bonus): compare the size indexes in months of
# extreme stress vs extreme calm, where "extreme" = a month in the top or bottom
# quintile (20%) of a stratifying series. Two stratifiers are used:
#   * rand volatility (SAVI) - local currency stress
#   * the VIX               - global risk aversion
# Index returns are winsorised at their 1st and 99th percentiles first, so that a
# handful of extreme days do not drive the comparison.

Q2_Winsorise <- function(Index_Rets) {

    Index_Rets %>%
        group_by(Name) %>%
        mutate(Top = quantile(Returns, 0.99), Bot = quantile(Returns, 0.01),
               Returns = pmin(pmax(Returns, Bot), Top)) %>%
        ungroup() %>% select(-Top, -Bot)
}

# Months in the top / bottom quintile of a daily series (monthly average)
Q2_Strat_Months <- function(Series, Label) {

    Series %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>% summarise(Level = mean(Value), .groups = "drop") %>%
        mutate(Regime = case_when(Level >= quantile(Level, 0.8) ~ "High",
                                  Level <= quantile(Level, 0.2) ~ "Low",
                                  TRUE ~ "Middle"),
               Stratifier = Label) %>%
        filter(Regime != "Middle")
}

# Performance of each index in the stratified months, relative to its full sample
Q2_Stratify <- function(Index_Rets, StratMonths) {

    R <- Q2_Winsorise(Index_Rets) %>% mutate(YM = format(date, "%Y-%m"))

    Full <- R %>% group_by(Name) %>%
        summarise(FullVol = sd(Returns) * sqrt(252), FullRet = mean(Returns) * 21, .groups = "drop")

    R %>%
        inner_join(StratMonths %>% select(YM, Regime, Stratifier), by = "YM", relationship = "many-to-many") %>%
        group_by(Stratifier, Regime, Name) %>%
        summarise(Vol = sd(Returns) * sqrt(252),
                  AvgMonthlyRet = mean(Returns) * 21,           # average daily return x 21 trading days
                  Months = n_distinct(YM), .groups = "drop") %>%
        left_join(Full, by = "Name") %>%
        mutate(VolRatio = Vol / FullVol)
}

# Monthly returns of each index in the stratified months (from winsorised daily returns),
# so the whole distribution of outcomes can be compared, not only the averages.
Q2_Strat_Monthly <- function(Index_Rets, StratMonths) {

    Q2_Winsorise(Index_Rets) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Name, YM) %>% summarise(Ret = prod(1 + Returns) - 1, .groups = "drop") %>%
        inner_join(StratMonths %>% select(YM, Regime, Stratifier), by = "YM", relationship = "many-to-many")
}
