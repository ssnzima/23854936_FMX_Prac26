# Q4_Optimise.R
# Constrained portfolio optimisation (quadprog) with the mandate's
# group constraints, quarterly rebalancing and an expanding estimation window.

# --- Mandate ---------------------------------------------------------------------------------
Q4_Limits <- list(Single = 0.35, Bonds = 0.25, Equity = 0.75, Commodities = 0.15, Global = 0.60)

# --- Covariance and mean estimates at a rebalance date ----------------------------------------
# Window: all months from January 2010 up to the rebalance date (expanding, so always at
# least 10 years once the backtest starts). Assets with a shorter history (e.g. local
# equity, which starts in 2015) use all their available months: correlations are computed
# pairwise, so the long histories of the other assets are not thrown away.
# Covariance: shrunk towards a constant-correlation target (Ledoit & Wolf, 2004),
#   Sigma = delta * F + (1 - delta) * S,
# with a fixed intensity (the optimal-intensity formula needs a complete data matrix),
# and forced to be positive definite.
# Means: historical means are very noisy, so they are shrunk halfway towards the
# cross-sectional average (a simple James-Stein-type correction).
Q4_Estimates <- function(Wide, Delta = 0.3, MeanShrink = 0.5) {

    S <- cov(Wide, use = "pairwise.complete.obs")
    vol <- sqrt(diag(S))
    R <- cov2cor(S)
    rbar <- mean(R[upper.tri(R)])
    F <- rbar * outer(vol, vol); diag(F) <- vol^2
    Sigma <- as.matrix(Matrix::nearPD(Delta * F + (1 - Delta) * S)$mat)
    dimnames(Sigma) <- dimnames(S)

    mu <- colMeans(Wide, na.rm = TRUE)
    mu <- MeanShrink * mean(mu) + (1 - MeanShrink) * mu

    list(Sigma = Sigma, mu = mu)
}

# --- Constraint matrix for quadprog::solve.QP ---------------------------------------------------
# solve.QP solves: min 1/2 w'Dw - d'w  subject to  A'w >= b (first meq rows are equalities).
Q4_Constraints <- function(Info, Limits = Q4_Limits, Mandate = TRUE) {

    n <- nrow(Info)
    A <- cbind(rep(1, n), diag(n))                      # fully invested; long only
    b <- c(1, rep(0, n)); lab <- c("Fully invested", paste("Long only:", Info$Asset))

    if (Mandate) {
        A <- cbind(A, -diag(n)); b <- c(b, rep(-Limits$Single, n))
        lab <- c(lab, paste("Single asset <= 35%:", Info$Asset))
        grp <- list(`Bonds <= 25%` = Info$Class == "Bonds", `Equities <= 75%` = Info$Class == "Equity",
                    `Commodities <= 15%` = Info$Class == "Commodities", `Global <= 60%` = Info$Region == "Global")
        lim <- c(Limits$Bonds, Limits$Equity, Limits$Commodities, Limits$Global)
        for (g in seq_along(grp)) { A <- cbind(A, -as.numeric(grp[[g]])); b <- c(b, -lim[g]); lab <- c(lab, names(grp)[g]) }
    }
    list(A = A, b = b, lab = lab, meq = 1)
}

# --- Solve one portfolio ------------------------------------------------------------------------
# Type = "MinVar": minimum variance.
# Type = "MV":     mean-variance utility  max w'mu - (lambda/2) w'Sigma w.
# Type = "Naive":  the feasible portfolio closest to equal weights (min ||w - 1/N||^2),
#                  i.e. equal weighting adjusted only as much as the mandate requires.
Q4_Solve <- function(Est, Info, Type = "MinVar", Lambda = 5, Mandate = TRUE) {

    n <- length(Est$mu); Con <- Q4_Constraints(Info, Mandate = Mandate)
    D <- switch(Type, MinVar = 2 * Est$Sigma, MV = Lambda * Est$Sigma, Naive = 2 * diag(n))
    d <- switch(Type, MinVar = rep(0, n), MV = Est$mu, Naive = 2 * rep(1 / n, n))
    sol <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = Con$A, bvec = Con$b, meq = Con$meq)
    w <- pmax(sol$solution, 0); w <- w / sum(w)
    list(w = setNames(w, Info$Asset), Lagrangian = setNames(sol$Lagrangian, Con$lab))
}

# --- Max-Sharpe portfolio: scan the constrained frontier ------------------------------------------
# The constrained max-Sharpe problem is not a simple quadratic programme, so the frontier
# is traced by varying the risk-aversion parameter, and the portfolio with the highest
# (excess return / volatility) is chosen.
Q4_Frontier <- function(Est, Info, Rf, Mandate = TRUE, Lambdas = exp(seq(log(0.5), log(500), length.out = 60))) {

    map_dfr(Lambdas, function(l) {
        s <- Q4_Solve(Est, Info, Type = "MV", Lambda = l, Mandate = Mandate)
        tibble(Lambda = l, Ret = sum(s$w * Est$mu), Vol = sqrt(drop(t(s$w) %*% Est$Sigma %*% s$w)),
               Sharpe = (Ret - Rf) / Vol, W = list(s$w), Lagr = list(s$Lagrangian))
    })
}

# --- Quarterly rebalancing backtest -----------------------------------------------------------------
# At each quarter-end: keep assets with at least 5 years of returns, estimate on data since
# 2010, and solve the three strategies. Returns the weights and constraint multipliers.
Q4_Rebalance <- function(Univ, Cash, First = "2019-12", MinMonths = 60, WindowStart = "2010-01") {

    YMs <- sort(unique(Univ$YM))
    Rebal <- YMs[YMs >= First & substr(YMs, 6, 7) %in% c("03", "06", "09", "12")]
    Rebal <- Rebal[Rebal < max(YMs)]                           # need at least one month after the last rebalance

    map_dfr(Rebal, function(r) {
        Hist <- Univ %>% filter(YM <= r)
        Elig <- Hist %>% count(Asset) %>% filter(n >= MinMonths) %>% pull(Asset)
        Info <- Univ %>% distinct(Asset, Class, Region) %>% filter(Asset %in% Elig) %>% arrange(Asset)
        Wide <- Hist %>% filter(YM >= WindowStart, Asset %in% Elig) %>%
            select(YM, Asset, Ret) %>% pivot_wider(names_from = Asset, values_from = Ret) %>%
            arrange(YM) %>% select(all_of(Info$Asset)) %>% as.matrix()
        Est <- Q4_Estimates(Wide)
        Rf  <- mean(Cash$Rf[Cash$YM >= WindowStart & Cash$YM <= r])

        Fr <- Q4_Frontier(Est, Info, Rf)
        MS <- Fr %>% slice_max(Sharpe, n = 1, with_ties = FALSE)
        MV <- Q4_Solve(Est, Info, "MinVar"); NV <- Q4_Solve(Est, Info, "Naive")

        bind_rows(
            tibble(Strategy = "Max Sharpe", W = MS$W, Lagr = MS$Lagr),
            tibble(Strategy = "Minimum variance", W = list(MV$w), Lagr = list(MV$Lagrangian)),
            tibble(Strategy = "Naive (equal weight)", W = list(NV$w), Lagr = list(NV$Lagrangian))
        ) %>% mutate(YM = r)
    })
}

# Long weights table: YM | Strategy | Asset | Weight
Q4_Weights <- function(Reb, Univ) {
    Info <- Univ %>% distinct(Asset, Class, Region, Group)
    Reb %>% mutate(W = map(W, ~ tibble(Asset = names(.x), Weight = .x))) %>%
        select(YM, Strategy, W) %>% unnest(W) %>% left_join(Info, by = "Asset")
}

# Binding mandate constraints: positive multiplier = the constraint is binding (it would
# improve the objective to relax it). Long-only and full-investment rows are excluded.
Q4_Binding <- function(Reb) {
    Reb %>% mutate(L = map(Lagr, ~ tibble(Constraint = names(.x), Multiplier = .x))) %>%
        select(YM, Strategy, L) %>% unnest(L) %>%
        filter(!grepl("^Long only|^Fully invested", Constraint)) %>%
        mutate(Binding = Multiplier > 1e-8,
               Constraint = sub("Single asset <= 35%: ", "Single asset cap: ", Constraint))
}

# --- Frontiers at the latest rebalance: with vs without the mandate --------------------------------
# Returns both frontiers, the strategies' ex-ante positions and each asset's own
# expected return and volatility (all annualised), for the "cost of the mandate" chart.
Q4_Latest_Frontier <- function(Univ, Cash, Weights, MinMonths = 60, WindowStart = "2010-01") {

    r <- max(Weights$YM)
    Elig <- Univ %>% filter(YM <= r) %>% count(Asset) %>% filter(n >= MinMonths) %>% pull(Asset)
    Info <- Univ %>% distinct(Asset, Class, Region) %>% filter(Asset %in% Elig) %>% arrange(Asset)
    Wide <- Univ %>% filter(YM >= WindowStart, YM <= r, Asset %in% Elig) %>%
        select(YM, Asset, Ret) %>% pivot_wider(names_from = Asset, values_from = Ret) %>%
        arrange(YM) %>% select(all_of(Info$Asset)) %>% as.matrix()
    Est <- Q4_Estimates(Wide)
    Rf  <- mean(Cash$Rf[Cash$YM >= WindowStart & Cash$YM <= r])

    Pts <- Weights %>% filter(YM == r) %>%
        group_by(Strategy) %>% summarise(w = list(setNames(Weight, Asset)), .groups = "drop") %>%
        mutate(Ret = map_dbl(w, ~ sum(.x[Info$Asset] * Est$mu) * 12),
               Vol = map_dbl(w, ~ sqrt(drop(t(.x[Info$Asset]) %*% Est$Sigma %*% .x[Info$Asset]) * 12)))

    list(Mandate = Q4_Frontier(Est, Info, Rf, Mandate = TRUE),
         Free = Q4_Frontier(Est, Info, Rf, Mandate = FALSE),
         Points = Pts,
         Assets = tibble(Asset = Info$Asset, Ret = Est$mu * 12, Vol = sqrt(diag(Est$Sigma) * 12)))
}
