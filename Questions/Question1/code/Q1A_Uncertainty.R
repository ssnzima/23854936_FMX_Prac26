# Q1A_Uncertainty.R
# Links local volatility to offshore uncertainty: VIX, policy uncertainty (EPU),
# bond vol (MOVE), oil vol and FX vol. Done at MONTHLY frequency because the
# global/Europe EPU series only update monthly.

# --- Monthly panel --------------------------------------------------------------
# SigmaDF : GARCH sigma output (date | Name | Sigma)
# UncLevels: output of Q1A_Uncertainty_Levels()
Q1A_Monthly_Panel <- function(SigmaDF, UncLevels, OilVolWindow = 21) {

    # Oil: the measure of interest is oil *volatility*, not the oil price level
    OilVol <-
        UncLevels %>%
        filter(Name == "Brent Oil") %>%
        arrange(date) %>%
        mutate(Ret = Value / lag(Value) - 1) %>%
        filter(!is.na(Ret), Ret != 0) %>%
        mutate(Value = RcppRoll::roll_sd(Ret, n = OilVolWindow, fill = NA, align = "right") * sqrt(252) * 100,
               Name = "Oil Vol") %>%
        filter(!is.na(Value)) %>%
        select(date, Name, Value)

    Levels <-
        bind_rows(UncLevels %>% filter(Name != "Brent Oil") %>% select(date, Name, Value), OilVol)

    # Local and global GARCH vol in % (same units as VIX)
    Vols <- SigmaDF %>% transmute(date, Name = paste0(Name, " Vol"), Value = Sigma * 100)

    bind_rows(Levels, Vols) %>%
        mutate(YM = lubridate::floor_date(date, "month")) %>%
        group_by(Name, YM) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = Name, values_from = Value) %>%
        rename(date = YM) %>%
        drop_na()
}

# --- Do offshore uncertainty measures explain local volatility? -----------------
# All variables are standardised (z-scores) so the coefficients are comparable:
# "a 1 sd rise in X is associated with a beta sd rise in local vol".
# Model 1 = offshore drivers only; Model 2 adds local currency vol (SAVI).
# Newey-West (HAC) standard errors, because the monthly volatility series are persistent
# and the residuals autocorrelated.
Q1A_Driver_Regressions <- function(Panel, LocalVol = "Capped SWIX Vol",
                                   Offshore = c("VIX", "EPU Global", "MOVE (US bond vol)", "Oil Vol"),
                                   Local = "SAVI USDZAR vol") {

    Z <- Panel %>% mutate(across(-date, ~ as.numeric(scale(.x))))
    Z <- Z %>% rename_with(~ make.names(.x))     # lm-friendly column names

    f1 <- as.formula(paste(make.names(LocalVol), "~", paste(make.names(Offshore), collapse = " + ")))
    f2 <- update(f1, paste(". ~ . +", make.names(Local)))

    m1 <- lm(f1, data = Z)
    m2 <- lm(f2, data = Z)

    Coefs <-
        bind_rows(broom::tidy(lmtest::coeftest(m1, vcov. = sandwich::NeweyWest(m1))) %>% mutate(Model = "Offshore only"),
                  broom::tidy(lmtest::coeftest(m2, vcov. = sandwich::NeweyWest(m2))) %>% mutate(Model = "Offshore + SAVI")) %>%
        filter(term != "(Intercept)")

    Fit <- tibble(Model = c("Offshore only", "Offshore + SAVI"),
                  R2 = c(summary(m1)$r.squared, summary(m2)$r.squared),
                  N = c(nobs(m1), nobs(m2)))

    list(Coefs = Coefs, Fit = Fit)
}
