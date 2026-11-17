# Q3_Data.R
# Monthly data for Question 3: peer funds, ABC Fund, the Capped SWIX benchmark and cash.
# All series are matched on calendar month (YM = "YYYY-MM").

# --- Daily series -> monthly returns ----------------------------------------------------
To_Monthly <- function(Daily, NameValue) {
    Daily %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>%
        summarise(Ret = prod(1 + Returns) - 1, Days = n(), .groups = "drop") %>%
        filter(Days >= 15) %>%                                  # drop part-months
        transmute(YM, Name = NameValue, Ret)
}

# Benchmark: Capped SWIX (J433), only available from January 2015
Q3_Bench <- function(Idx_Daily) {
    Idx_Daily %>% filter(Tickers == "J433") %>% To_Monthly("Capped SWIX")
}

# Cash: STeFI, used as the risk-free rate in Sharpe ratios
Q3_Cash <- function(STeFI) To_Monthly(STeFI, "STeFI")

# --- Peers + ABC Fund -------------------------------------------------------------------
# Peer returns are winsorised each month at the cross-sectional 1st and 99th percentiles.
# This limits the influence of a few extreme prints - e.g. fund Z182 reports -55% in its
# final month (April 2025) before disappearing from the data, which looks like a fund
# wind-down rather than an investment return.
Q3_Funds <- function(Active_Managers, ABC_Fund) {

    Peers <- Active_Managers %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(YM) %>%
        mutate(Ret = pmin(pmax(Returns, quantile(Returns, 0.01)), quantile(Returns, 0.99))) %>%
        ungroup() %>%
        transmute(YM, Name, Ret, Group = "Peer")

    ABC <- ABC_Fund %>%
        transmute(YM = format(date, "%Y-%m"), Name = "ABC Fund", Ret = Returns, Group = "ABC Fund")

    bind_rows(Peers, ABC) %>% mutate(date = as.Date(paste0(YM, "-01")))
}
