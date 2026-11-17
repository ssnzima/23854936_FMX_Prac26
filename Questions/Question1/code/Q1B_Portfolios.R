# Q1B_Portfolios.R
# Builds the portfolio return series used in Part B: SafeCore (from its monthly
# holdings) and a back-filled Low Vol index (from its constituents).

# --- Generic: holdings (date | Tickers | weight) x daily stock returns ----------
# Weights are rescaled to 100% over the stocks that actually have returns in the
# following month (a few small holdings have no return data). Weights are lagged
# by one day (lag_weights = TRUE), so month-end weights apply from the next day,
# and they then drift with prices until the next rebalance.
Build_Portfolio <- function(Holdings, Stock_Rets, PortName) {

    Has_Rets <- Stock_Rets %>%
        mutate(YM = lubridate::floor_date(date, "month")) %>%
        distinct(Tickers, YM)

    W <- Holdings %>%
        mutate(YM = lubridate::floor_date(date, "month") %m+% months(1)) %>%
        semi_join(Has_Rets, by = c("Tickers", "YM")) %>%
        group_by(date) %>%
        mutate(weight = weight / sum(weight)) %>%          # rescale to 100%
        ungroup() %>%
        select(date, Tickers, weight)

    W_xts <- W %>% tbl2xts::tbl_xts(cols_to_xts = weight, spread_by = Tickers)
    W_xts[is.na(W_xts)] <- 0

    R_xts <- Stock_Rets %>%
        filter(Tickers %in% unique(W$Tickers), date > min(W$date)) %>%
        select(date, Tickers, Returns) %>%
        tbl2xts::tbl_xts(cols_to_xts = Returns, spread_by = Tickers)
    R_xts[is.na(R_xts)] <- 0
    R_xts <- R_xts[, colnames(W_xts)]                      # same column order

    Port <- rmsfuns::Safe_Return.portfolio(R = R_xts, weights = W_xts, lag_weights = TRUE, geometric = TRUE)

    tibble(date = as.Date(zoo::index(Port)), Name = PortName, Returns = as.numeric(Port))
}

# --- SafeCore holdings: tidy up --------------------------------------------------
# Stock returns only start in 2015, so holdings from Dec 2014 onwards are used.
# A few holdings have no sector (e.g. Valterra Platinum, Pan African Resources);
# these are filled from the super-sector in the index constituent file, otherwise
# they would hide resource risk in an "Other" bucket.
Q1B_SafeCore_Holdings <- function(SafeCore, Index_Constituents, StartDate = as.Date("2014-12-01")) {

    Sector_Map <- Index_Constituents %>%
        filter(SuperSector != "", !is.na(SuperSector)) %>%
        group_by(Tickers) %>% filter(date == max(date)) %>% ungroup() %>%
        distinct(Tickers, Fill = SuperSector) %>%
        distinct(Tickers, .keep_all = TRUE)

    SafeCore %>%
        filter(date >= StartDate) %>%
        left_join(Sector_Map, by = "Tickers") %>%
        mutate(Sector = coalesce(Sector, Fill, "Other")) %>%
        select(date, Tickers, Sector, weight = Weight)
}

# --- Low Vol index back-fill (quarterly rebalance, as the paper suggests) ------
# Rebalance weights = constituent weights on the last trading day of Mar, Jun,
# Sep and Dec (plus the first available date to start the series).
Q1B_LowVol_Rebuild <- function(Index_Constituents, Stock_Rets, IndexName = "LowVol") {

    IC <- Index_Constituents %>% filter(Index == IndexName)

    Rebal_Dates <- IC %>%
        distinct(date) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>% filter(date == max(date)) %>% ungroup() %>%
        filter(lubridate::month(date) %in% c(3, 6, 9, 12)) %>%
        pull(date) %>%
        c(min(IC$date), .) %>% unique() %>% sort()

    IC %>%
        filter(date %in% Rebal_Dates) %>%
        select(date, Tickers, weight) %>%
        Build_Portfolio(Stock_Rets, PortName = "Low Vol")
}

# --- Low Vol using the daily (already drifted) weights - for validation only ---
Q1B_LowVol_Daily <- function(Index_Constituents, IndexName = "LowVol") {

    Index_Constituents %>%
        filter(Index == IndexName) %>%
        group_by(date) %>%
        summarise(Returns = sum(weight * Return) / sum(weight), .groups = "drop") %>%
        mutate(Name = "Low Vol (daily weights)")
}

# --- Return panel used for the comparisons -------------------------------------
Q1B_Return_Panel <- function(SafeCore_Rets, LowVol_Rets, Idx_Daily) {

    bind_rows(
        SafeCore_Rets,
        LowVol_Rets,
        Idx_Daily %>% filter(Tickers %in% c("J433", "J200")) %>% select(date, Name, Returns)
    ) %>%
        filter(date >= as.Date("2015-01-01"))
}
