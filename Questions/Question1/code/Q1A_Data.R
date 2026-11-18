# Q1A_Data.R
# Data wrangling for Question 1 Part A.
# Everything is returned in long (tidy) format: date | Name | Returns (or Value).

# --- Helper: price / total return index -> simple daily returns ----------------
TRI_to_Returns <- function(df) {

    df %>%
        group_by(Name) %>%
        arrange(date) %>%
        mutate(Returns = TRI / lag(TRI) - 1) %>%
        ungroup() %>%
        filter(!is.na(Returns)) %>%
        select(date, Name, Returns)
}

# --- Local + global equity index returns ---------------------------------------
# Local series are already returns (in ZAR). Global series are TRIs in USD, so
# they are converted. Note: the comparison is of the *pattern* of volatility across markets -
# the currency layer is picked up separately with the SAVIUSD (USDZAR vol) index.
Q1A_Equity_Returns <- function(Idx_Daily, MSCI, Global_Idx, StartDate = as.Date("2015-01-01")) {

    Local <-
        Idx_Daily %>%
        filter(Tickers %in% c("J433", "J200", "J201", "J202")) %>%
        mutate(Name = recode(Name, "MidCaps" = "Mid Caps", "SmallCaps" = "Small Caps")) %>%
        select(date, Name, Returns) %>%
        mutate(Region = "Local (ZAR)")

    Global <-
        bind_rows(
            MSCI %>% filter(Name %in% c("MSCI_ACWI", "MSCI_EM")),
            Global_Idx %>% filter(Name %in% c("SPXT", "Euro_Stoxx", "Nikkei_225"))
        ) %>%
        TRI_to_Returns() %>%
        # SPXT carries US holidays forward as flat days - these are not real trading days
        filter(Returns != 0) %>%
        mutate(Name = recode(Name,
                             "MSCI_ACWI"  = "MSCI ACWI",
                             "MSCI_EM"    = "MSCI EM",
                             "SPXT"       = "S&P 500",
                             "Euro_Stoxx" = "Euro Stoxx 50",
                             "Nikkei_225" = "Nikkei 225")) %>%
        mutate(Region = "Global (USD)")

    bind_rows(Local, Global) %>%
        filter(date >= StartDate)
}

# --- Uncertainty / risk-sentiment indicators (levels) --------------------------
# Implied vol, policy uncertainty, bond vol (MOVE), FX vol and Brent oil.
# Weekends are dropped: several of these series are padded onto weekends.
Q1A_Uncertainty_Levels <- function(Uncertainty_Indexes, Implied_Vol, Local_Cncy_Vol,
                                   Gl_Cncy_Vol, Commods, StartDate = as.Date("2015-01-01")) {

    bind_rows(
        Uncertainty_Indexes,
        Implied_Vol,
        # Data cleaning: on 109 days (mostly Mar-Jun 2021) SAVI is quoted as a decimal
        # (e.g. 0.15) instead of a percentage (15) - a units error, so rescale those.
        Local_Cncy_Vol %>% select(-Description) %>% mutate(TRI = ifelse(TRI < 1, TRI * 100, TRI)),
        Gl_Cncy_Vol,
        Commods %>% filter(Name == "Oil_Brent")
    ) %>%
        filter(!lubridate::wday(date) %in% c(1, 7), date >= StartDate) %>%
        mutate(Name = recode(Name,
                             "EPUCGLCP"   = "EPU Global",
                             "EPUCCEUM"   = "EPU Europe",
                             "EPUCNUSD"   = "EPU US (daily news)",
                             "MOVE_Index" = "MOVE (US bond vol)",
                             "SAVIUSD"    = "SAVI USDZAR vol",
                             "JPMVXYGL"   = "JPM Global FX vol",
                             "Oil_Brent"  = "Brent Oil")) %>%
        select(date, Name, Group, Value = TRI)
}
