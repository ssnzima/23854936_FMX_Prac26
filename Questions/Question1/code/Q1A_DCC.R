# Q1A_DCC.R
# Multivariate volatility for Q1 Part A: a DCC-GARCH model (Engle, 2002) of the Capped SWIX,
# MSCI ACWI and MSCI EM. Two questions:
#  1) Do correlations rise more after joint falls than after joint rallies? (DCC vs asymmetric aDCC)
#  2) How much of local volatility is imported? Each week, the share of the Capped SWIX's
#     conditional variance explained by the two global indexes, from the full conditional
#     covariance matrix H_t:  share_t = h_lg' H_gg^-1 h_lg / h_ll  (the R-squared of a regression
#     of local on global returns, using that week's covariances).

# --- Weekly returns, wide (Monday-start weeks) ---------------------------------------------
# Weekly data because the JSE closes before the US opens, which understates daily co-movement.
Q1A_Weekly <- function(df, Names = c("Capped SWIX", "MSCI ACWI", "MSCI EM")) {

    df %>%
        filter(Name %in% Names) %>%
        mutate(Week = lubridate::floor_date(date, "week", week_start = 1)) %>%
        group_by(Name, Week) %>%
        summarise(R = prod(1 + Returns) - 1, .groups = "drop") %>%
        pivot_wider(names_from = Name, values_from = R) %>%
        drop_na() %>%
        select(Week, all_of(Names))
}

# --- Fit DCC(1,1) or aDCC(1,1): GJR-GARCH(1,1) marginals with Student-t errors ---------------
Q1A_DCC_Fit <- function(Weekly, Model = "DCC") {

    X <- xts::xts(as.matrix(Weekly[, -1]) * 100, order.by = Weekly$Week)     # % returns help the optimiser
    uspec <- rugarch::ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                 distribution.model = "std")
    spec <- rmgarch::dccspec(rugarch::multispec(replicate(ncol(X), uspec)), dccOrder = c(1, 1),
                             model = Model, distribution = "mvt")
    rmgarch::dccfit(spec, data = X, solver = "solnp")
}

# --- Parameter table for DCC and aDCC ---------------------------------------------------------
Q1A_DCC_Table <- function(Fit_DCC, Fit_aDCC) {

    Row <- function(f, Lab) {
        cf <- coef(f)
        a <- cf["[Joint]dcca1"]; b <- cf["[Joint]dccb1"]; g <- ifelse("[Joint]dccg1" %in% names(cf), cf["[Joint]dccg1"], NA_real_)
        tibble(Model = Lab, `a (shock)` = a, `b (memory)` = b, `g (asymmetry)` = g,
               `Half-life (weeks)` = log(0.5) / log(a + b),
               `Log-likelihood` = rugarch::likelihood(f), AIC = rugarch::infocriteria(f)[1])
    }
    # Likelihood-ratio test of the aDCC against the DCC (one extra parameter)
    LR <- 2 * (rugarch::likelihood(Fit_aDCC) - rugarch::likelihood(Fit_DCC))
    bind_rows(Row(Fit_DCC, "DCC"), Row(Fit_aDCC, "aDCC (asymmetric)")) %>%
        mutate(`LR test p-value` = c(NA, 1 - pchisq(max(LR, 0), df = 1)))
}

# --- Imported share of local variance, week by week ----------------------------------------
Q1A_Imported_Share <- function(Fit, Weeks) {

    H <- rmgarch::rcov(Fit)                                              # k x k x T conditional covariances
    Share <- sapply(seq_len(dim(H)[3]), function(t) {
        S <- H[, , t]; h <- S[1, -1]
        as.numeric(t(h) %*% solve(S[-1, -1]) %*% h) / S[1, 1]
    })
    tibble(date = Weeks, LocalVol = sqrt(H[1, 1, ] * 52) / 100, Share = Share)
}
