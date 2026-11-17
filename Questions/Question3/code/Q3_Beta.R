# Q3_Beta.R
# How much of each fund's return is simply market beta?

# --- Rolling 3-year beta and R-squared against the Capped SWIX ---------------------------
# Beta = cov(fund, market) / var(market) over the past 36 months.
# R-squared = squared correlation = share of the fund's monthly variance explained by
# the market. A fund that adds "something other than market beta" has a lower R-squared.
# Rolling sums are used, so a window with any missing month is dropped (no partial windows).
Q3_Rolling_Beta <- function(Funds, Bench, Window = 36) {

    Funds %>%
        inner_join(Bench %>% select(YM, Mkt = Ret), by = "YM") %>%
        group_by(Name, Group) %>% arrange(YM) %>%
        filter(n() >= Window) %>%
        mutate(Sx = RcppRoll::roll_sum(Mkt, Window, fill = NA, align = "right"),
               Sy = RcppRoll::roll_sum(Ret, Window, fill = NA, align = "right"),
               Sxy = RcppRoll::roll_sum(Mkt * Ret, Window, fill = NA, align = "right"),
               Sxx = RcppRoll::roll_sum(Mkt^2, Window, fill = NA, align = "right"),
               Syy = RcppRoll::roll_sum(Ret^2, Window, fill = NA, align = "right"),
               Span = as.numeric(date - lag(date, Window - 1)) / 30.44) %>%     # months covered by the window
        ungroup() %>%
        filter(!is.na(Sx), Span < Window + 1) %>%                               # only consecutive months
        mutate(CovXY = Sxy / Window - (Sx / Window) * (Sy / Window),
               VarX = Sxx / Window - (Sx / Window)^2,
               VarY = Syy / Window - (Sy / Window)^2,
               Beta = CovXY / VarX,
               R2 = CovXY^2 / (VarX * VarY)) %>%
        select(YM, date, Name, Group, Beta, R2)
}

# Year-end snapshots (December, plus the latest month) for the box-and-scatter chart
Q3_Beta_Snapshots <- function(RollBeta) {

    Last <- max(RollBeta$YM)
    RollBeta %>%
        filter(substr(YM, 6, 7) == "12" | YM == Last) %>%
        mutate(Year = ifelse(YM == Last & substr(YM, 6, 7) != "12", paste0(substr(YM, 1, 4), " (", month.abb[as.integer(substr(YM, 6, 7))], ")"),
                             substr(YM, 1, 4)))
}
