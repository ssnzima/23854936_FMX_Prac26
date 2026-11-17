# Q2_Structure.R
# What is inside each size index: sector mix, concentration and active share.

# --- Sector weights per index and month ----------------------------------------------
Q2_Sector_Mix <- function(Holdings) {

    Holdings %>%
        group_by(date, Index, Sector) %>%
        summarise(Weight = sum(weight), .groups = "drop")
}

# --- Concentration ------------------------------------------------------------------
# Effective number of stocks = 1 / sum(w^2) (= N for an equally weighted index), and
# the combined weight of the 10 largest holdings.
Q2_Concentration <- function(Holdings) {

    Holdings %>%
        group_by(date, Index) %>%
        summarise(`Effective number of stocks` = 1 / sum(weight^2),
                  `Top-10 weight` = sum(sort(weight, decreasing = TRUE)[1:min(10, n())]),
                  Stocks = n(), .groups = "drop")
}

# --- Active share vs the Capped SWIX -------------------------------------------------
# Active share = 1/2 * sum_i |w_index,i - w_SWIX,i| over the union of holdings.
# 0% = identical to the benchmark, 100% = no overlap at all. It shows how far each
# size index is from "the market" an investor would otherwise hold.
Q2_Active_Share <- function(Holdings, Bench = "Capped SWIX") {

    B <- Holdings %>% filter(Index == Bench) %>% select(date, Tickers, wb = weight)

    Holdings %>%
        filter(Index != Bench) %>%
        select(date, Index, Tickers, wp = weight) %>%
        group_by(Index) %>%
        group_modify(~ full_join(.x, B %>% filter(date %in% .x$date), by = c("date", "Tickers"))) %>%
        ungroup() %>%
        mutate(across(c(wp, wb), ~ replace_na(.x, 0))) %>%
        group_by(date, Index) %>%
        summarise(ActiveShare = 0.5 * sum(abs(wp - wb)), .groups = "drop")
}

# --- Lorenz curves and Gini coefficients ------------------------------------------------
# Stocks are sorted from smallest to largest weight; the Lorenz curve plots the
# cumulative share of stocks against the cumulative share of index weight. An equally
# weighted index lies on the diagonal; the further the curve bows below it, the more
# the index is dominated by a few large stocks. The Gini coefficient summarises this
# (0 = equal weights, 1 = one stock holds everything).
Q2_Lorenz <- function(Holdings, Indexes = c("Large Caps", "Mid Caps", "Small Caps")) {

    Holdings %>%
        filter(Index %in% Indexes, date %in% c(min(date), max(date))) %>%
        group_by(Index, date) %>% arrange(weight, .by_group = TRUE) %>%
        mutate(CumStocks = row_number() / n(), CumWeight = cumsum(weight) / sum(weight),
               Gini = 1 - sum((lag(CumWeight, default = 0) + CumWeight) / n())) %>%
        ungroup() %>%
        mutate(Year = format(date, "%Y"))
}

# --- Active share vs tracking error (Cremers & Petajisto, 2009) ------------------------
# Active share says how different the HOLDINGS are from the benchmark; tracking error
# says how different the RETURNS are. Together they classify a portfolio:
#   low AS,  low TE  -> closet indexer      high AS, low TE  -> diversified stock picker
#   low AS,  high TE -> factor bet          high AS, high TE -> concentrated stock picker
# using the usual cut-offs of 60% active share and 8% tracking error. Computed per
# calendar year (average monthly active share; tracking error from daily returns vs J433).
Q2_AS_TE <- function(Holdings, Index_Rets, Bench = "Capped SWIX") {

    AS <- Q2_Active_Share(Holdings) %>%
        mutate(Year = lubridate::year(date)) %>%
        group_by(Index, Year) %>% summarise(ActiveShare = mean(ActiveShare), .groups = "drop")

    TE <- Index_Rets %>%
        filter(Name != Bench) %>%
        inner_join(Index_Rets %>% filter(Name == Bench) %>% select(date, B = Returns), by = "date") %>%
        mutate(Year = lubridate::year(date)) %>%
        group_by(Index = Name, Year) %>%
        summarise(TrackingError = sd(Returns - B) * sqrt(252), .groups = "drop")

    inner_join(AS, TE, by = c("Index", "Year"))
}
