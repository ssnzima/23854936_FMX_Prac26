# Q3_Performance.R
# ABC Fund's performance relative to its peers over time, and a peer scorecard.

# --- Rolling 3-year return and ABC's percentile rank among peers ------------------------
# Percentile rank = share of peers ABC beat over the trailing 36 months (100 = best).
# Only peers with a full 36-month record at that date are ranked.
Q3_Rolling_Rank <- function(Funds, Window = 36) {

    Roll <- Funds %>%
        group_by(Name, Group) %>% arrange(YM) %>%
        mutate(RollRet = RcppRoll::roll_prod(1 + Ret, Window, fill = NA, align = "right")^(12 / Window) - 1,
               Span = as.numeric(date - lag(date, Window - 1)) / 30.44) %>%
        ungroup() %>%
        filter(!is.na(RollRet), Span < Window + 1)

    Roll %>%
        group_by(YM, date) %>%
        filter(any(Group == "ABC Fund")) %>%
        summarise(ABC = RollRet[Group == "ABC Fund"],
                  PeerMedian = median(RollRet[Group == "Peer"]),
                  Pctile = mean(RollRet[Group == "Peer"] < RollRet[Group == "ABC Fund"]) * 100,
                  Peers = sum(Group == "Peer"), .groups = "drop")
}

# --- Peer scorecard: where does ABC rank on each measure? ---------------------------------
# Common window = the benchmark period (2015 onwards). Peers need at least 5 years of data.
# For each measure the percentile is expressed as "better than x% of peers", so that a
# higher number is always better (lower volatility, smaller drawdown and lower beta count
# as better).
Q3_Scorecard <- function(Funds, Bench, Cash, MinMonths = 60) {

    D <- Funds %>%
        inner_join(Bench %>% select(YM, Mkt = Ret), by = "YM") %>%
        inner_join(Cash %>% select(YM, Rf = Ret), by = "YM") %>%
        group_by(Name, Group) %>% filter(n() >= MinMonths) %>% arrange(YM) %>%
        summarise(`Annual return` = prod(1 + Ret)^(12 / n()) - 1,
                  `Volatility` = sd(Ret) * sqrt(12),
                  `Sharpe ratio` = mean(Ret - Rf) / sd(Ret) * sqrt(12),
                  `Information ratio` = mean(Ret - Mkt) / sd(Ret - Mkt) * sqrt(12),
                  `Max drawdown` = min(cumprod(1 + Ret) / cummax(cumprod(1 + Ret)) - 1),
                  `Market beta` = cov(Ret, Mkt) / var(Mkt),
                  Months = n(), .groups = "drop") %>%
        pivot_longer(-c(Name, Group, Months), names_to = "Measure", values_to = "Value") %>%
        mutate(HigherBetter = !Measure %in% c("Volatility", "Market beta"))   # drawdown is negative: higher = better

    D %>%
        group_by(Measure) %>%
        mutate(Pctile = map2_dbl(Value, HigherBetter, function(v, hb) {
            Peer <- Value[Group == "Peer"]
            if (hb) mean(Peer < v) * 100 else mean(Peer > v) * 100
        })) %>%
        ungroup()
}
