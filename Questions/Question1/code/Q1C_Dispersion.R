# Q1C_Dispersion.R
# Cross-sectional volatility (dispersion) of the local market and its split into
# within-sector and between-sector components.

# --- Data cleaning: corporate-action artefacts --------------------------------------
# Two daily "returns" above 100% are not real price moves: Richemont's shareholder
# warrants (CFRO) show +369,900% when they listed in Nov 2020, and Fortress B shows
# +106% around its A/B share restructure in Feb 2024. Even with a tiny weight, the
# warrant print alone lifts the weighted dispersion for Nov 2020 to ~6%, so these
# rows are dropped.
Q1C_Clean_IC <- function(Index_Constituents, MaxAbsRet = 1) {
    Index_Constituents %>% filter(abs(Return) <= MaxAbsRet)
}

# --- Constituent panel with previous-day weights ----------------------------------
# The weight a stock has at the start of day t is its weight at the close of t-1,
# so returns on day t are weighted with lagged weights (no look-ahead).
Q1C_Constituents <- function(Index_Constituents, IndexName = "Capped SWIX") {

    Index_Constituents %>%
        Q1C_Clean_IC() %>%
        filter(Index == IndexName) %>%
        group_by(Tickers) %>% arrange(date) %>%
        mutate(w = lag(weight)) %>%
        ungroup() %>%
        filter(!is.na(w), !is.na(Return)) %>%
        group_by(date) %>% mutate(w = w / sum(w)) %>% ungroup() %>%   # rescale to 100% each day
        select(date, Tickers, SuperSector, Return, w)
}

# --- Daily dispersion: weighted and unweighted --------------------------------------
# Weighted CSV   = sqrt( sum_i w_i (R_i - Rbar_w)^2 ), Rbar_w = weighted mean (the index return)
# Unweighted CSV = cross-sectional standard deviation of returns (all stocks count equally)
# The weighted version is decomposed exactly into:
#   within-sector  = sum_s W_s * sum_{i in s} (w_i / W_s) (R_i - Rbar_s)^2  (stock picking)
#   between-sector = sum_s W_s (Rbar_s - Rbar_w)^2                          (sector allocation)
# where W_s is sector s's weight and Rbar_s its weighted return.
Q1C_Daily_CSV <- function(Const) {

    Sec <- Const %>%
        group_by(date) %>% mutate(Rbar = sum(w * Return)) %>%
        group_by(date, SuperSector) %>%
        summarise(Ws = sum(w),
                  Rs = sum(w * Return) / sum(w),
                  Within = sum(w * (Return - Rs)^2),
                  Rbar = first(Rbar), .groups = "drop") %>%
        group_by(date) %>%
        summarise(Within_Var  = sum(Within),
                  Between_Var = sum(Ws * (Rs - Rbar)^2), .groups = "drop")

    Const %>%
        group_by(date) %>%
        summarise(CSV_W = sqrt(sum(w * (Return - sum(w * Return))^2)),
                  CSV_EW = sd(Return), N = n(), .groups = "drop") %>%
        left_join(Sec, by = "date")
}

# --- Monthly averages (daily CSV is very noisy) --------------------------------------
Q1C_Monthly_CSV <- function(DailyCSV) {

    DailyCSV %>%
        mutate(date = lubridate::floor_date(date, "month")) %>%
        group_by(date) %>%
        summarise(CSV_W = mean(CSV_W), CSV_EW = mean(CSV_EW),
                  Within_Var = mean(Within_Var), Between_Var = mean(Between_Var),
                  Days = n(), .groups = "drop") %>%
        filter(Days >= 10)                                 # drop part-months at the edges
}
