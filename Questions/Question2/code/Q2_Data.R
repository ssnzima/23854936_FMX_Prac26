# Q2_Data.R
# Data preparation for Question 2: monthly size index holdings and daily index returns.

# --- Monthly holdings of the size indexes -------------------------------------------
# * Fund labels are tidied (e.g. "LargeCaps" -> "Large Caps").
# * Missing sectors are filled from the same stock's other months, then from the
#   super-sector in the daily constituent file; the rest (mostly small caps) are
#   labelled "Unclassified" rather than dropped, so weights still add up.
# * Weights are rescaled to 100% per index and month (some months sum to ~85%).
Q2_Size_Holdings <- function(Size_Idx, Index_Constituents) {

    Own_Sec <- Size_Idx %>%
        filter(!is.na(Sector)) %>%
        count(Tickers, Sector) %>%
        group_by(Tickers) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
        select(Tickers, Sec_Own = Sector)

    IC_Sec <- Index_Constituents %>%
        filter(!is.na(SuperSector), SuperSector != "") %>%
        distinct(Tickers, Sec_IC = SuperSector) %>%
        distinct(Tickers, .keep_all = TRUE)

    Size_Idx %>%
        left_join(Own_Sec, by = "Tickers") %>%
        left_join(IC_Sec, by = "Tickers") %>%
        mutate(Sector = coalesce(Sector, Sec_Own, Sec_IC, "Unclassified"),
               Index = recode(Fund, "CappedSWIX" = "Capped SWIX", "LargeCaps" = "Large Caps",
                              "MidCaps" = "Mid Caps", "SmallCaps" = "Small Caps")) %>%
        group_by(Index, date) %>% mutate(weight = Weight / sum(Weight)) %>% ungroup() %>%
        select(date, Index, Tickers, Sector, weight)
}

# --- Daily index returns (actual index series, from 2015) --------------------------
# J200 (Top 40) is the large-cap index, J201 mid caps, J202 small caps.
Q2_Index_Returns <- function(Idx_Daily) {

    Idx_Daily %>%
        filter(Tickers %in% c("J200", "J201", "J202", "J433")) %>%
        mutate(Name = recode(Tickers, "J200" = "Large Caps", "J201" = "Mid Caps",
                             "J202" = "Small Caps", "J433" = "Capped SWIX")) %>%
        select(date, Name, Returns)
}

# --- Rand volatility (SAVI), cleaned as in Question 1 -------------------------------
# On ~109 days SAVI is quoted as a decimal (0.15) instead of a percentage (15).
Q2_SAVI <- function(LCL_Cncy_Vol) {

    LCL_Cncy_Vol %>%
        filter(!lubridate::wday(date) %in% c(1, 7)) %>%
        mutate(Value = ifelse(TRI < 1, TRI * 100, TRI)) %>%
        select(date, Value)
}
