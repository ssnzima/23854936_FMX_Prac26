# Q3_Alpha.R
# Skill or luck? CAPM (Jensen's) alpha for every fund, with Newey-West t-statistics.
#   R_fund - R_cash = alpha + beta (R_market - R_cash) + error
# Alpha is the return a fund earned beyond what its market exposure explains. With
# ~10 years of monthly data, a t-statistic above about 2 means the alpha is unlikely
# to be just luck. Newey-West standard errors allow for autocorrelation and changing
# volatility in the residuals. Peers need at least 5 years of data in the benchmark period.
Q3_Alpha <- function(Funds, Bench, Cash, MinMonths = 60) {

    Funds %>%
        inner_join(Bench %>% select(YM, Mkt = Ret), by = "YM") %>%
        inner_join(Cash %>% select(YM, Rf = Ret), by = "YM") %>%
        group_by(Name, Group) %>%
        filter(n() >= MinMonths) %>%
        group_modify(~ {
            m  <- lm(I(Ret - Rf) ~ I(Mkt - Rf), data = .x)
            ct <- lmtest::coeftest(m, vcov. = sandwich::NeweyWest(m))
            tibble(Alpha = ct[1, 1] * 12, t_Alpha = ct[1, 3], p_Alpha = ct[1, 4], Beta = ct[2, 1], Months = nrow(.x))
        }) %>%
        ungroup()
}
