# Q3_Drivers.R
# What drives each fund's returns? Principal component analysis
# on peer returns identifies the common "industry" return drivers; each fund is then
# described by how strongly it moves with those drivers.

# --- PCA on peer returns over a common window ---------------------------------------------
# Window: the last 5 years. Only peers with a complete history in the window are used
# (PCA needs a balanced panel). Returns are standardised (correlation-matrix PCA), so
# each fund counts equally regardless of how volatile it is. ABC Fund is NOT used to
# build the components - it is projected onto them afterwards, like any outsider.
Q3_PCA <- function(Funds, Months = 60) {

    YMs <- sort(unique(Funds$YM)) %>% tail(Months)

    Wide <- Funds %>%
        filter(YM %in% YMs) %>%
        group_by(Name) %>% filter(n() == Months) %>% ungroup() %>%
        select(YM, Name, Ret) %>%
        pivot_wider(names_from = Name, values_from = Ret) %>% arrange(YM)

    Peers <- Wide %>% select(-YM, -any_of("ABC Fund")) %>% as.matrix()
    PCA <- prcomp(Peers, center = TRUE, scale. = TRUE)
    Scores <- PCA$x[, 1:3]

    # For every fund (peers and ABC): correlation with each component, and the share of
    # its variance explained by the first component (R-squared of a regression on PC1).
    Funds_M <- Wide %>% select(-YM) %>% as.matrix()
    Fund_Stats <- tibble(Name = colnames(Funds_M)) %>%
        mutate(CorPC1 = map_dbl(Name, ~ cor(Funds_M[, .x], Scores[, 1])),
               CorPC2 = map_dbl(Name, ~ cor(Funds_M[, .x], Scores[, 2])),
               R2_PC1 = CorPC1^2,
               R2_PC12 = map_dbl(Name, ~ summary(lm(Funds_M[, .x] ~ Scores[, 1:2]))$r.squared),
               Group = ifelse(Name == "ABC Fund", "ABC Fund", "Peer"))

    list(VarShare = PCA$sdev^2 / sum(PCA$sdev^2),
         Scores = tibble(YM = Wide$YM, PC1 = Scores[, 1], PC2 = Scores[, 2]),
         Fund_Stats = Fund_Stats, N_Peers = ncol(Peers), Window = range(Wide$YM))
}

# --- PCA with FactoMineR, with ABC as a supplementary variable ----------------
# Same 5-year window and complete-history peers as above. ABC Fund enters as a
# "supplementary" variable: it is projected onto the components (its correlation with
# each component is computed) but plays no part in building them. This gives the
# standard scree plot and variable correlation circle.
Q3_PCA_Prac <- function(Funds, Months = 60) {

    YMs <- sort(unique(Funds$YM)) %>% tail(Months)

    W <- Funds %>%
        filter(YM %in% YMs) %>%
        group_by(Name) %>% filter(n() == Months) %>% ungroup() %>%
        select(YM, Name, Ret) %>%
        pivot_wider(names_from = Name, values_from = Ret) %>%
        select(-YM) %>% as.data.frame()

    FactoMineR::PCA(W, quanti.sup = which(names(W) == "ABC Fund"), scale.unit = TRUE, ncp = 10, graph = FALSE)
}
