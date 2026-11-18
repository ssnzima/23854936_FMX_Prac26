# Q1A_Garch.R
# Conditional volatility (GARCH) for Q1 Part A, in three steps:
# 1) test for ARCH effects, 2) compare candidate GARCH specs, 3) extract sigma.

# --- 1) ARCH effects: Ljung-Box on squared returns -----------------------------
# H0: no autocorrelation in squared returns (i.e. no volatility clustering).
ARCH_Test_Table <- function(df, Names, Lag = 12) {

    df %>%
        filter(Name %in% Names) %>%
        group_by(Name) %>%
        summarise(LB_Stat = Box.test(Returns^2, type = "Ljung-Box", lag = Lag)$statistic,
                  P_Value = Box.test(Returns^2, type = "Ljung-Box", lag = Lag)$p.value,
                  .groups = "drop")
}

# --- Helper: one GARCH spec -----------------------------------------------------
# AR(1) mean; skewed-t errors because equity returns are
# fat-tailed and negatively skewed.
Garch_Spec <- function(Model = "gjrGARCH", Dist = "sstd") {

    rugarch::ugarchspec(
        variance.model = list(model = Model, garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = Dist)
}

# --- Helper: one series as an xts (rugarch prefers xts / matrix input) ----------
Series_xts <- function(df, SeriesName) {

    df %>%
        filter(Name == SeriesName) %>%
        select(date, Returns) %>%
        tbl2xts::tbl_xts()
}

# --- 2) Compare sGARCH / gjrGARCH / eGARCH on information criteria --------------
Garch_Model_Compare <- function(df, Names, Models = c("sGARCH", "gjrGARCH", "eGARCH")) {

    expand_grid(Name = Names, Model = Models) %>%
        mutate(IC = map2(Name, Model, function(n, m) {
            fit <- rugarch::ugarchfit(spec = Garch_Spec(Model = m), data = Series_xts(df, n), solver = "hybrid")
            ic <- rugarch::infocriteria(fit)
            tibble(AIC = ic["Akaike", 1], BIC = ic["Bayes", 1])
        })) %>%
        unnest(IC)
}

# --- 3) Fit chosen model per series: sigma + key parameters --------------------
Garch_Fit_Sigma <- function(df, Names, Model = "gjrGARCH") {

    Fits <- set_names(Names) %>%
        map(~ rugarch::ugarchfit(spec = Garch_Spec(Model = Model), data = Series_xts(df, .x), solver = "hybrid"))

    # Annualised conditional volatility in long format
    Sigma <-
        imap_dfr(Fits, function(fit, n) {
            s <- rugarch::sigma(fit)
            tibble(date = as.Date(zoo::index(s)), Name = n, Sigma = as.numeric(s) * sqrt(252))
        })

    # Key parameters: alpha (shock), beta (memory), gamma (leverage), persistence
    Params <-
        imap_dfr(Fits, function(fit, n) {
            cf <- rugarch::coef(fit)
            tibble(Name = n,
                   alpha1 = cf["alpha1"], beta1 = cf["beta1"],
                   gamma1 = ifelse("gamma1" %in% names(cf), cf["gamma1"], NA_real_),
                   Persistence = rugarch::persistence(fit),
                   HalfLife_days = log(0.5) / log(rugarch::persistence(fit)))
        })

    list(Sigma = Sigma, Params = Params, Fits = Fits)
}

# --- 4) Diagnostics: has the model absorbed the volatility clustering? ------------------------
# Ljung-Box tests on the standardised residuals (z) and squared standardised residuals (z^2).
# A well-specified GARCH leaves no autocorrelation in z^2 (high p-values).
Garch_Diagnostics <- function(Fits, Lag = 12) {

    imap_dfr(Fits, function(fit, n) {
        z <- as.numeric(rugarch::residuals(fit, standardize = TRUE))
        tibble(Name = n,
               `LB p-value (z)`   = Box.test(z,   lag = Lag, type = "Ljung-Box")$p.value,
               `LB p-value (z^2)` = Box.test(z^2, lag = Lag, type = "Ljung-Box")$p.value)
    })
}
