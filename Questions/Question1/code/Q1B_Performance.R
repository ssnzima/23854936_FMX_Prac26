# Q1B_Performance.R
# How did SafeCore navigate volatility? Growth, drawdowns, rolling vol ratio,
# capture ratios, stress tests, VIX regimes and a summary scorecard.

# --- Growth of R100 and drawdowns ---------------------------------------------------
# Low Vol only starts in Sep 2020, so it is rebased to the Capped SWIX level on its
# first day - this keeps all lines comparable on one chart.
Q1B_Growth_DD <- function(Panel, RebaseTo = "Capped SWIX") {

    G <- Panel %>%
        group_by(Name) %>% arrange(date) %>%
        mutate(Growth = 100 * cumprod(1 + Returns),
               Drawdown = Growth / cummax(Growth) - 1) %>%
        ungroup()

    Base <- G %>% filter(Name == RebaseTo) %>% select(date, BaseGrowth = Growth)

    G %>%
        group_by(Name) %>%
        mutate(Start = min(date)) %>%
        left_join(Base %>% rename(Start = date, StartBase = BaseGrowth), by = "Start") %>%
        mutate(Growth = ifelse(Start > min(Panel$date), Growth / first(Growth) * StartBase, Growth)) %>%
        ungroup() %>%
        select(date, Name, Growth, Drawdown)
}

# --- Rolling vol ratio vs the benchmark ------------------------------------------
# Ratio < 1 = smoother than the Capped SWIX over the past year.
Q1B_Vol_Ratio <- function(Panel, Bench = "Capped SWIX", Window = 252) {

    RV <- Panel %>%
        group_by(Name) %>% arrange(date) %>%
        mutate(RollVol = RcppRoll::roll_sd(Returns, n = Window, fill = NA, align = "right")) %>%
        ungroup() %>% filter(!is.na(RollVol))

    RV %>%
        filter(Name != Bench) %>%
        inner_join(RV %>% filter(Name == Bench) %>% select(date, BenchVol = RollVol), by = "date") %>%
        mutate(Ratio = RollVol / BenchVol)
}

# --- Up / down capture per calendar year ------------------------------------------
# Upside capture   = avg fund return on days the benchmark is up   / avg benchmark return on those days
# Downside capture = avg fund return on days the benchmark is down / avg benchmark return on those days
# The ideal fund captures more of the upside (>1) and less of the downside (<1).
Q1B_Capture <- function(Panel, Bench = "Capped SWIX") {

    Panel %>%
        filter(Name != Bench) %>%
        inner_join(Panel %>% filter(Name == Bench) %>% select(date, B = Returns), by = "date") %>%
        mutate(Year = lubridate::year(date)) %>%
        group_by(Name, Year) %>%
        filter(n() > 120) %>%                                   # skip part-years
        summarise(UpCapture   = mean(Returns[B > 0]) / mean(B[B > 0]),
                  DownCapture = mean(Returns[B < 0]) / mean(B[B < 0]),
                  .groups = "drop")
}

# --- Stress tests: Part A events + the highest-VIX months --------------------------
# Cumulative return of each fund over each stress window.
Q1B_Stress_Windows <- function(Events, Implied_Vol, N_VIX = 5) {

    # Highest average-VIX months that are not already inside one of the events
    VIX_M <- Implied_Vol %>%
        filter(Name == "VIX", date >= as.Date("2015-01-01")) %>%
        mutate(Start = lubridate::floor_date(date, "month")) %>%
        group_by(Start) %>% summarise(VIX = mean(TRI), .groups = "drop") %>%
        mutate(End = lubridate::ceiling_date(Start, "month") - 1)

    In_Event <- function(s) any(s >= Events$Start - 31 & s <= Events$End)

    Top_VIX <- VIX_M %>%
        filter(!map_lgl(Start, In_Event)) %>%
        slice_max(VIX, n = N_VIX) %>%
        mutate(Label = paste0("High VIX: ", format(Start, "%b %Y")), Type = "High VIX")

    bind_rows(Events %>% select(Label, Start, End, Type), Top_VIX %>% select(Label, Start, End, Type))
}

Q1B_Stress_Returns <- function(Panel, Windows, Names = c("SafeCore", "Capped SWIX", "Low Vol")) {

    Windows %>%
        mutate(Res = map2(Start, End, function(s, e) {
            Panel %>%
                filter(Name %in% Names, date >= s, date <= e) %>%
                group_by(Name) %>%
                summarise(CumRet = prod(1 + Returns) - 1, N = n(), .groups = "drop")
        })) %>%
        unnest(Res) %>%
        filter(N >= 5)                                       # fund must exist in the window
}

# --- Performance by VIX regime ------------------------------------------------------
# Months are split into terciles of average VIX (calm / normal / stressed).
# Excess return = fund monthly return minus the Capped SWIX monthly return.
Q1B_VIX_Regimes <- function(Panel, Implied_Vol, Bench = "Capped SWIX") {

    Monthly <- Panel %>%
        mutate(YM = lubridate::floor_date(date, "month")) %>%
        group_by(Name, YM) %>% summarise(Ret = prod(1 + Returns) - 1, N = n(), .groups = "drop") %>%
        filter(N >= 15)                                      # drop part-months

    VIX <- Implied_Vol %>%
        filter(Name == "VIX") %>%
        mutate(YM = lubridate::floor_date(date, "month")) %>%
        group_by(YM) %>% summarise(VIX = mean(TRI), .groups = "drop") %>%
        filter(YM %in% Monthly$YM) %>%
        mutate(Regime = cut(VIX, quantile(VIX, c(0, 1/3, 2/3, 1)), include.lowest = TRUE,
                            labels = c("Calm", "Normal", "Stressed")))

    Monthly %>%
        filter(Name != Bench) %>%
        inner_join(Monthly %>% filter(Name == Bench) %>% select(YM, BenchRet = Ret), by = "YM") %>%
        inner_join(VIX, by = "YM") %>%
        group_by(Name, Regime) %>%
        summarise(Excess = mean(Ret - BenchRet), Hit = mean(Ret > BenchRet), Months = n(), .groups = "drop")
}

# --- Summary scorecard ----------------------------------------------------------------
# Computed over a common window so every fund is judged on the same period.
Q1B_Scorecard <- function(Panel, STeFI, From, To, Bench = "Capped SWIX") {

    P <- Panel %>% filter(date >= From, date <= To)
    B <- P %>% filter(Name == Bench) %>% select(date, B = Returns)
    Rf <- STeFI %>% filter(date >= From, date <= To) %>% summarise(Rf = prod(1 + Returns)^(252 / n()) - 1) %>% pull(Rf)
    BenchAnn <- prod(1 + B$B)^(252 / nrow(B)) - 1

    P %>%
        inner_join(B, by = "date") %>%
        group_by(Name) %>%
        summarise(`Ann. return`  = prod(1 + Returns)^(252 / n()) - 1,
                  `Ann. vol`     = sd(Returns) * sqrt(252),
                  `Max drawdown` = min(cumprod(1 + Returns) / cummax(cumprod(1 + Returns)) - 1),
                  Beta           = cov(Returns, B) / var(B),
                  `Tracking error` = sd(Returns - B) * sqrt(252),
                  .groups = "drop") %>%
        mutate(Sharpe = (`Ann. return` - Rf) / `Ann. vol`,
               `Info. ratio` = ifelse(Name == Bench, NA, (`Ann. return` - BenchAnn) / `Tracking error`)) %>%
        select(Fund = Name, `Ann. return`, `Ann. vol`, Sharpe, `Max drawdown`, Beta, `Tracking error`, `Info. ratio`)
}
