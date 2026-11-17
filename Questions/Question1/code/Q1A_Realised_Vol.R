# Q1A_Realised_Vol.R
# Realised (historical) volatility measures used in Q1 Part A.

# --- Rolling annualised realised volatility ------------------------------------
# 63 trading days ~ 3 months: long enough to be stable, short enough to show regimes.
Roll_Vol <- function(df, Window = 63) {

    df %>%
        group_by(Name) %>%
        arrange(date) %>%
        mutate(RollVol = RcppRoll::roll_sd(Returns, n = Window, fill = NA, align = "right") * sqrt(252)) %>%
        ungroup() %>%
        filter(!is.na(RollVol))
}

# --- Calendar-year realised volatility -----------------------------------------
Calendar_Vol <- function(df) {

    df %>%
        mutate(Year = lubridate::year(date)) %>%
        group_by(Region, Name, Year) %>%
        summarise(AnnVol = sd(Returns) * sqrt(252), N = n(), .groups = "drop")
}

# --- Volatility regime table ---------------------------------------------------
# Compares average vol before and after a split date, and how often each index
# spends in a "high vol" state - defined as its own full-sample 80th percentile
# of rolling vol. This is a simple, intuitive way to say "vol has increased".
# Exclude: optional c(start, end) window to drop (e.g. the COVID crash) as a
# robustness check that the increase is not driven by one extreme episode.
Vol_Regime_Table <- function(RollVolDF, SplitDate = as.Date("2020-01-01"), Pctl = 0.8, Exclude = NULL) {

    if (!is.null(Exclude)) RollVolDF <- RollVolDF %>% filter(!(date >= Exclude[1] & date <= Exclude[2]))

    RollVolDF %>%
        group_by(Region, Name) %>%
        mutate(HighVolCut = quantile(RollVol, Pctl),
               Period = ifelse(date < SplitDate, "Pre", "Post")) %>%
        group_by(Region, Name, Period) %>%
        summarise(AvgVol = mean(RollVol),
                  HighVolDays = mean(RollVol > HighVolCut), .groups = "drop") %>%
        pivot_wider(names_from = Period, values_from = c(AvgVol, HighVolDays)) %>%
        mutate(VolChange = AvgVol_Post - AvgVol_Pre) %>%
        select(Region, Name, AvgVol_Pre, AvgVol_Post, VolChange, HighVolDays_Pre, HighVolDays_Post) %>%
        arrange(Region, desc(VolChange))
}

# --- Rolling local vs global correlation (weekly returns) ----------------------
# Weekly returns are used on purpose: the JSE closes before the US opens, so daily
# correlations with US-heavy indexes are understated (non-synchronous trading).
Roll_Cor_Weekly <- function(df, LocalName = "Capped SWIX",
                            GlobalNames = c("MSCI ACWI", "MSCI EM", "S&P 500"), Window = 52) {

    Wkly <-
        df %>%
        filter(Name %in% c(LocalName, GlobalNames)) %>%
        mutate(Week = lubridate::floor_date(date, "week", week_start = 1)) %>%
        group_by(Name, Week) %>%
        summarise(Returns = prod(1 + Returns) - 1, .groups = "drop")

    Lcl <- Wkly %>% filter(Name == LocalName) %>% select(Week, Local = Returns)

    Wkly %>%
        filter(Name %in% GlobalNames) %>%
        inner_join(Lcl, by = "Week") %>%
        group_by(Name) %>%
        arrange(Week) %>%
        mutate(RollCor = zoo::rollapplyr(cbind(Returns, Local), width = Window, by.column = FALSE,
                                         FUN = function(m) cor(m[, 1], m[, 2]), fill = NA)) %>%
        ungroup() %>%
        filter(!is.na(RollCor)) %>%
        rename(date = Week)
}

# --- Event study: how far did vol rise around each event? -----------------------
# Pre  = rolling vol on the last day before the event starts
# Peak = highest rolling vol from the start of the event to 30 days after it ends
# If local-only events lift local vol much more than global vol, while global events
# lift both, that tells us which shocks are home-grown and which are imported.
Event_Vol_Table <- function(RollVolDF, Events, Names = c("Capped SWIX", "MSCI EM", "MSCI ACWI")) {

    Events %>%
        mutate(Res = map2(Start, End, function(s, e) {
            RollVolDF %>%
                filter(Name %in% Names) %>%
                group_by(Name) %>%
                summarise(Pre  = last(RollVol[date < s]),
                          Peak = max(RollVol[date >= s & date <= e + 30]),
                          .groups = "drop") %>%
                mutate(Rise = Peak - Pre)
        })) %>%
        unnest(Res) %>%
        select(Event = Label, Type, Name, Rise) %>%
        pivot_wider(names_from = Name, values_from = Rise) %>%
        select(Event, Type, all_of(Names))
}
