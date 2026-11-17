# Q1C_Macro.R
# How has the alpha opportunity moved with global macro / uncertainty factors?
# Reuses the monthly uncertainty panel from Part A (Q1A_Monthly_Panel), so the same
# cleaned series (incl. the SAVI fix) are used throughout Question 1.

Q1C_Macro_Panel <- function(UncLevels, Opp, MonthlyCSV, PairCor,
                            Vars = c("VIX", "VXEEM", "Oil Vol", "MOVE (US bond vol)", "EPU Global",
                                     "JPM Global FX vol", "SAVI USDZAR vol")) {

    NoSigma <- tibble(date = as.Date(character()), Name = character(), Sigma = numeric())

    Q1A_Monthly_Panel(NoSigma, UncLevels) %>%
        select(date, all_of(Vars)) %>%
        inner_join(Opp %>% select(date, Opportunity), by = "date") %>%
        inner_join(MonthlyCSV %>% select(date, Dispersion = CSV_W), by = "date") %>%
        inner_join(PairCor %>% select(date, Correlation = Mean), by = "date")
}

# --- Lead-lag correlations ---------------------------------------------------------
# Same month: cor(macro_t, measure_t). Next month: cor(macro_t, measure_t+1).
# If the next-month correlation is as strong as the same-month one, macro stress is
# a useful *early warning* of changes in the alpha opportunity.
Q1C_LeadLag <- function(MacroPanel, Vars, Measures = c("Dispersion", "Correlation", "Opportunity")) {

    expand_grid(Var = Vars, Measure = Measures) %>%
        mutate(Same = map2_dbl(Var, Measure, ~ cor(MacroPanel[[.x]], MacroPanel[[.y]])),
               Next = map2_dbl(Var, Measure, ~ cor(head(MacroPanel[[.x]], -1), tail(MacroPanel[[.y]], -1)))) %>%
        pivot_longer(c(Same, Next), names_to = "Timing", values_to = "Cor") %>%
        mutate(Timing = recode(Timing, "Same" = "Same month", "Next" = "Next month"))
}

# --- High vs low macro regimes -------------------------------------------------------
# For each macro variable: average dispersion and correlation in its lowest-third
# months vs its highest-third months.
Q1C_Macro_Regimes <- function(MacroPanel, Vars) {

    map_dfr(Vars, function(v) {
        MacroPanel %>%
            mutate(T3 = ntile(.data[[v]], 3)) %>%
            filter(T3 %in% c(1, 3)) %>%
            mutate(Regime = ifelse(T3 == 1, "Low", "High")) %>%
            group_by(Regime) %>%
            summarise(Dispersion = mean(Dispersion), Correlation = mean(Correlation), .groups = "drop") %>%
            mutate(Var = v)
    }) %>%
        pivot_longer(c(Dispersion, Correlation), names_to = "Measure", values_to = "Value")
}
