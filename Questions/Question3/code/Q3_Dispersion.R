# Q3_Dispersion.R
# Testing the manager's claim: "when market dispersion is high, my fund does well".

# --- Monthly market dispersion ---------------------------------------------------------------
# Weighted cross-sectional volatility of the Capped SWIX constituents' monthly returns,
# using the index weights at the end of the previous month:
#   CSV_t = sqrt( sum_i w_i,t-1 (R_i,t - Rbar_t)^2 ),  Rbar_t = sum_i w_i,t-1 R_i,t
# Daily returns above 100% in absolute value (corporate-action artefacts, see Question 1)
# are removed before compounding to monthly returns.
Q3_Market_Dispersion <- function(Size_Idx, Stock_Rets) {

    Stock_M <- Stock_Rets %>%
        filter(abs(Returns) <= 1) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Tickers, YM) %>% summarise(R = prod(1 + Returns) - 1, .groups = "drop")

    Size_Idx %>%
        filter(Fund == "CappedSWIX") %>%
        mutate(YM = format(date %m+% months(1), "%Y-%m")) %>%              # weights apply to the next month
        inner_join(Stock_M, by = c("Tickers", "YM")) %>%
        group_by(YM) %>%
        mutate(w = Weight / sum(Weight)) %>%
        summarise(Dispersion = sqrt(sum(w * (R - sum(w * R))^2)), Coverage = sum(Weight), .groups = "drop")
}

# --- ABC's relative performance in each month ------------------------------------------------
# Excess vs benchmark = ABC - Capped SWIX; excess vs peers = ABC - median peer;
# Rank = share of peers ABC beat that month.
Q3_ABC_Monthly <- function(Funds, Bench, Disp) {

    Funds %>%
        group_by(YM) %>%
        filter(any(Group == "ABC Fund")) %>%
        summarise(ABC = Ret[Group == "ABC Fund"],
                  PeerMedian = median(Ret[Group == "Peer"]),
                  Rank = mean(Ret[Group == "Peer"] < Ret[Group == "ABC Fund"]) * 100,
                  PeerDispersion = sd(Ret[Group == "Peer"]), .groups = "drop") %>%
        inner_join(Bench %>% select(YM, Mkt = Ret), by = "YM") %>%
        inner_join(Disp %>% select(YM, Dispersion), by = "YM") %>%
        mutate(vs_Bench = ABC - Mkt, vs_Peers = ABC - PeerMedian,
               Regime = factor(ntile(Dispersion, 3), labels = c("Low dispersion", "Medium", "High dispersion")))
}

# --- Formal test: does ABC's excess return rise with dispersion? ------------------------------
# Regression of ABC's monthly excess return on (standardised) market dispersion, with
# Newey-West standard errors, which allow for autocorrelation and heteroskedasticity in
# the residuals (a limitation flagged in Question 1).
Q3_Dispersion_Test <- function(ABC_M) {

    tibble(Var = c("vs_Bench", "vs_Peers"), Label = c("vs the Capped SWIX", "vs the median peer")) %>%
        mutate(Res = map(Var, function(v) {
            m <- lm(ABC_M[[v]] ~ as.numeric(scale(ABC_M$Dispersion)))
            ct <- lmtest::coeftest(m, vcov. = sandwich::NeweyWest(m))
            tibble(Slope = ct[2, 1], SE = ct[2, 2], t = ct[2, 3], p = ct[2, 4], R2 = summary(m)$r.squared)
        })) %>%
        unnest(Res)
}
