# Q4_Data.R
# Monthly asset returns for the Global Balanced Index Fund.
# All series are matched on calendar month (YM = "YYYY-MM").

# --- Daily total return index -> monthly returns ------------------------------------------
TRI_Monthly <- function(df) {
    df %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Name, YM) %>% summarise(TRI = last(TRI), .groups = "drop") %>%
        group_by(Name) %>% arrange(YM) %>%
        mutate(Ret = TRI / lag(TRI) - 1) %>% ungroup() %>%
        filter(!is.na(Ret)) %>% select(YM, Name, Ret)
}

# Daily returns -> monthly returns (drops part-months at the start of a series)
Daily_Monthly <- function(df) {
    df %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Name, YM) %>% summarise(Ret = prod(1 + Returns) - 1, Days = n(), .groups = "drop") %>%
        filter(Days >= 15) %>% select(YM, Name, Ret)
}

# --- 10-year government bond return index from yields --------------------------------------
# Only yields (YTM) are provided, so a constant-maturity 10-year bond is simulated:
# at each month-end a 10-year par bond is bought (coupon = that month's yield); one
# month later it is repriced at the new yield with 10 - 1/12 years left, and a month of
# coupon is added. The bond is then sold and a new par bond bought.
#   Return_t = [ P(y_t; coupon = y_t-1, n = 119/12) + y_t-1 / 12 * 100 ] / 100 - 1
# Repricing the full cash-flow stream (rather than using duration alone) captures
# convexity. If the yield is unchanged the return is simply y/12, as it should be.
Bond_Price <- function(y, coupon, n) {
    annuity <- ifelse(abs(y) < 1e-8, n, (1 - (1 + y)^(-n)) / y)   # handles zero / negative yields
    coupon * 100 * annuity + 100 * (1 + y)^(-n)
}

Q4_Bond_TRI <- function(Bond_Yields, Names = c("US_10Yr", "Germany_10Yr")) {

    Bond_Yields %>%
        filter(Name %in% Names) %>%
        mutate(YM = format(date, "%Y-%m")) %>%
        group_by(Name, YM) %>% summarise(Yield = last(TRI) / 100, .groups = "drop") %>%  # month-end yield, in decimals
        group_by(Name) %>% arrange(YM) %>%
        mutate(Ret = (Bond_Price(Yield, lag(Yield), 10 - 1 / 12) + lag(Yield) / 12 * 100) / 100 - 1) %>%
        ungroup() %>%
        filter(!is.na(Ret))
}

# --- The investment universe --------------------------------------------------------------
# Asset groups drive the mandate's constraints:
#   Class  : Equity / Bonds / Commodities
#   Region : Local / Global (global exposure is limited to 60%)
# Note: global assets are in US dollars (no USD/ZAR rate is provided), so global exposure
# is effectively treated as currency-hedged.
Q4_Universe <- function(MSCI, Bond_Yields, Commods, LCL_Yield, Idx_Daily) {

    Global_Eq <- MSCI %>% filter(Name %in% c("MSCI_USA", "MSCI_EU", "MSCI_EM")) %>% TRI_Monthly()
    Commod    <- Commods %>% filter(Name %in% c("Gold", "Bcom_Index")) %>% TRI_Monthly()
    Glob_Bond <- Q4_Bond_TRI(Bond_Yields) %>% select(YM, Name, Ret)
    Local_Eq  <- Idx_Daily %>% filter(Tickers %in% c("J200", "J201", "STXVALJ433", "STXMOMJ433")) %>%
        select(date, Name = Tickers, Returns) %>% Daily_Monthly()
    Local_Yld <- LCL_Yield %>% transmute(YM = format(date, "%Y-%m"), Name, Ret = Returns)

    Map <- tribble(
        ~Name,                                            ~Asset,              ~Class,        ~Region,
        "J200",                                           "SA Large Caps",     "Equity",      "Local",
        "J201",                                           "SA Mid Caps",       "Equity",      "Local",
        "STXVALJ433",                                     "SA Value",          "Equity",      "Local",
        "STXMOMJ433",                                     "SA Momentum",       "Equity",      "Local",
        "FTSE/JSE SA Listed Property",                    "SA Property",       "Equity",      "Local",
        "FTSE/JSE All Bond",                              "SA Bonds (ALBI)",   "Bonds",       "Local",
        "FTSE/JSE SA Inflation-Linked Government Bonds",  "SA Inflation-Linked", "Bonds",     "Local",
        "MSCI_USA",                                       "MSCI USA",          "Equity",      "Global",
        "MSCI_EU",                                        "MSCI Europe",       "Equity",      "Global",
        "MSCI_EM",                                        "MSCI EM",           "Equity",      "Global",
        "US_10Yr",                                        "US 10Y Bond",       "Bonds",       "Global",
        "Germany_10Yr",                                   "German 10Y Bond",   "Bonds",       "Global",
        "Gold",                                           "Gold",              "Commodities", "Global",
        "Bcom_Index",                                     "Commodities (BCOM)", "Commodities", "Global")

    bind_rows(Global_Eq, Commod, Glob_Bond, Local_Eq, Local_Yld) %>%
        inner_join(Map, by = "Name") %>%
        mutate(Group = paste(Region, tolower(Class))) %>%
        select(YM, Asset, Class, Region, Group, Ret) %>%
        arrange(Asset, YM)
}

# Cash (STeFI), monthly - used for Sharpe ratios
Q4_Cash <- function(STeFI) STeFI %>% transmute(date, Name = "STeFI", Returns) %>% Daily_Monthly() %>% select(YM, Rf = Ret)
