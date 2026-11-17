# Q3_Plots.R
# Slide charts for Question 3. Same visual identity as Questions 1-2 (theme_sleek), but
# with larger text for slides, one message per chart, and ABC Fund always in raspberry
# against grey peers so it stands out immediately.

ABC_Col <- "#E0245E"; Peer_Col <- "grey65"; Bench_Col <- "#1F3A5F"
theme_slide <- function() theme_sleek(base_size = 15)

# --- 1) Rolling 3-year beta: box plots over scatter (as the manager asked) ---------------
Plot_Q3_Beta_Box <- function(Snap) {

    P <- Snap %>% filter(Group == "Peer")
    A <- Snap %>% filter(Group == "ABC Fund")

    ggplot(P, aes(Year, Beta)) +
        geom_hline(yintercept = 1, colour = "grey50", linetype = "dashed", linewidth = 0.4) +
        geom_jitter(width = 0.18, height = 0, colour = Peer_Col, alpha = 0.45, size = 1.3) +
        geom_boxplot(fill = NA, colour = "grey30", outlier.shape = NA, width = 0.55, linewidth = 0.5) +
        geom_line(data = A, aes(group = 1), colour = ABC_Col, linewidth = 1) +
        geom_point(data = A, colour = ABC_Col, fill = "white", shape = 23, size = 3.8, stroke = 1.4) +
        annotate("text", x = 0.6, y = 1.03, label = "Beta = 1: moves one-for-one with the market",
                 hjust = 0, vjust = 0, size = 3.6, colour = "grey40", fontface = "italic") +
        annotate("text", x = nrow(A) + 0.35, y = tail(A$Beta, 1), label = "ABC Fund", hjust = 0,
                 size = 4.2, colour = ABC_Col, fontface = "bold") +
        scale_x_discrete(expand = expansion(add = c(0.6, 1.3))) +
        labs(title = "Most peers are 80-90% market exposure. ABC is about half.",
             subtitle = "Rolling 3-year beta against the Capped SWIX at each year-end (dots = peer funds, boxes = middle 50%)",
             x = NULL, y = "Rolling 3-year beta") +
        theme_slide()
}

# --- 2) Beta vs R-squared: does the fund offer more than the market? ---------------------
Plot_Q3_Beta_R2 <- function(RollBeta) {

    L <- RollBeta %>% filter(YM == max(YM))
    P <- L %>% filter(Group == "Peer"); A <- L %>% filter(Group == "ABC Fund")

    ggplot(P, aes(Beta, R2)) +
        annotate("rect", xmin = -Inf, xmax = median(P$Beta), ymin = -Inf, ymax = median(P$R2), fill = "#2A9D8F", alpha = 0.07) +
        geom_vline(xintercept = median(P$Beta), colour = "grey70", linewidth = 0.3) +
        geom_hline(yintercept = median(P$R2), colour = "grey70", linewidth = 0.3) +
        geom_point(colour = Peer_Col, size = 2.4, alpha = 0.7) +
        geom_point(data = A, colour = ABC_Col, size = 6) +
        annotate("text", x = A$Beta, y = A$R2, label = "ABC Fund", hjust = -0.25, size = 4.5, colour = ABC_Col, fontface = "bold") +
        annotate("text", x = -Inf, y = -Inf, label = "Less market exposure,\nmore independent return", hjust = -0.08, vjust = -0.4,
                 size = 3.8, colour = "#2A9D8F", fontface = "italic") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Only 5% of peers are less driven by the market than ABC",
             subtitle = paste0("Each dot is a fund, last 3 years to ", format(max(L$date), "%b %Y"),
                               ". R-squared = share of a fund's monthly returns explained by the market"),
             x = "Market beta", y = "R-squared (market-driven share of returns)") +
        theme_slide() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 3) What drives returns: share explained by the industry's common driver ---------------
Plot_Q3_Drivers <- function(PCA) {

    FS <- PCA$Fund_Stats
    A <- FS %>% filter(Group == "ABC Fund")

    PeerMed <- median(FS$R2_PC1[FS$Group == "Peer"])

    FS %>% filter(Group == "Peer") %>%
        ggplot(aes(R2_PC1)) +
        geom_histogram(binwidth = 0.025, boundary = 0, fill = Peer_Col, colour = "white") +
        geom_vline(xintercept = PeerMed, colour = "grey30", linetype = "dashed", linewidth = 0.6) +
        geom_vline(xintercept = A$R2_PC1, colour = ABC_Col, linewidth = 1.3) +
        annotate("text", x = A$R2_PC1, y = Inf, vjust = 1.5, hjust = 1.08, label = paste0("ABC Fund: ", round(A$R2_PC1 * 100), "%"),
                 colour = ABC_Col, fontface = "bold", size = 4.6) +
        annotate("text", x = PeerMed, y = Inf, vjust = 1.5, hjust = 1.08,
                 label = paste0("Typical peer: ", round(PeerMed * 100), "%"), size = 4.2, colour = "grey30") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Peers largely move as one herd. ABC is driven by different things.",
             subtitle = "Share of each fund's returns explained by the industry's main common driver (last 5 years)",
             caption = paste0("This single driver (the first principal component of peer returns) explains ",
                              round(PCA$VarShare[1] * 100), "% of all variation in peer returns."),
             x = "Share of returns explained by the common industry driver", y = "Number of peer funds") +
        theme_slide()
}

# --- 4) Relative performance over time: rolling 3-year peer rank ---------------------------
Plot_Q3_Rank <- function(Rank) {

    Bands <- tibble(ymin = c(0, 25, 50, 75), ymax = c(25, 50, 75, 100),
                    Quartile = c("Bottom quartile", "Third quartile", "Second quartile", "Top quartile"),
                    fill = c("#E0245E", "#F4A3B8", "#A8DADC", "#2A9D8F"))
    TopShare <- round(mean(Rank$Pctile >= 75) * 100)

    ggplot(Rank, aes(date, Pctile)) +
        geom_rect(data = Bands, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax), fill = Bands$fill,
                  alpha = 0.18, inherit.aes = FALSE) +
        geom_text(data = Bands, aes(x = min(Rank$date), y = (ymin + ymax) / 2, label = Quartile), inherit.aes = FALSE,
                  hjust = 0, size = 3.6, colour = "grey35", fontface = "italic") +
        geom_line(colour = ABC_Col, linewidth = 1.3) +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), labels = function(x) paste0(x, "th")) +
        scale_x_years(Breaks = "1 year") +
        labs(title = paste0("ABC has been a top-quartile fund ", TopShare, "% of the time"),
             subtitle = "ABC's percentile rank among peers on rolling 3-year returns (100th = best fund)",
             x = NULL, y = "Peer percentile") +
        theme_slide() + theme(panel.grid.major.y = element_blank())
}

# --- 5) Peer scorecard: better than x% of peers --------------------------------------------
Plot_Q3_Scorecard <- function(SC) {

    A <- SC %>% filter(Group == "ABC Fund") %>%
        mutate(Fmt = case_when(Measure %in% c("Annual return", "Volatility", "Max drawdown") ~ scales::percent(Value, accuracy = 0.1),
                               TRUE ~ sprintf("%.2f", Value)),
               PeerMed = map_dbl(Measure, ~ median(SC$Value[SC$Group == "Peer" & SC$Measure == .x])),
               PeerFmt = case_when(Measure %in% c("Annual return", "Volatility", "Max drawdown") ~ scales::percent(PeerMed, accuracy = 0.1),
                                   TRUE ~ sprintf("%.2f", PeerMed)),
               Lab = paste0(Measure, "\nABC ", Fmt, " vs peers ", PeerFmt),
               Lab = fct_reorder(Lab, Pctile))

    ggplot(A, aes(Pctile, Lab)) +
        geom_segment(aes(x = 0, xend = 100, yend = Lab), colour = "grey88", linewidth = 4, lineend = "round") +
        geom_segment(aes(x = 0, xend = Pctile, yend = Lab), colour = ABC_Col, linewidth = 4, lineend = "round", alpha = 0.85) +
        geom_vline(xintercept = 50, colour = "grey50", linetype = "dashed", linewidth = 0.4) +
        geom_text(aes(x = 101, label = paste0(round(Pctile), "%")), hjust = 0, size = 4.4, fontface = "bold", colour = ABC_Col) +
        scale_x_continuous(limits = c(0, 112), breaks = c(0, 50, 100), labels = c("0%", "Median peer", "100%")) +
        labs(title = "ABC beats about 90% of peers on every key measure",
             subtitle = "Share of peers ABC does better than, Jan 2015 - Oct 2025 (lower volatility, drawdown and beta count as better)",
             x = NULL, y = NULL) +
        theme_slide() + theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(0.8), lineheight = 0.9))
}

# --- 6) The dispersion claim -----------------------------------------------------------------
Plot_Q3_Dispersion <- function(ABC_M) {

    ABC_M %>%
        group_by(Regime) %>%
        summarise(`vs the Capped SWIX` = mean(vs_Bench), `vs the median peer` = mean(vs_Peers), .groups = "drop") %>%
        pivot_longer(-Regime, names_to = "Comparison", values_to = "Excess") %>%
        ggplot(aes(Regime, Excess, fill = Comparison)) +
        geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
        geom_col(position = position_dodge(width = 0.75), width = 0.7) +
        geom_text(aes(label = scales::percent(Excess, accuracy = 0.1), vjust = ifelse(Excess >= 0, -0.4, 1.3)),
                  position = position_dodge(width = 0.75), size = 4, colour = "grey25") +
        scale_fill_manual(values = c("vs the Capped SWIX" = Bench_Col, "vs the median peer" = "#5FA8D3")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = expansion(mult = 0.2)) +
        labs(title = "The data does not show ABC doing better when dispersion is high",
             subtitle = "ABC's average monthly excess return, by level of stock-market dispersion (months split into thirds)",
             x = NULL, y = NULL, fill = NULL) +
        theme_slide()
}

# --- 7) What the data does show: ABC protects in falling markets ---------------------------
Plot_Q3_Market_Regimes <- function(ABC_M) {

    ABC_M %>%
        mutate(MktRegime = cut(Mkt, quantile(Mkt, c(0, 0.25, 0.75, 1)), include.lowest = TRUE,
                               labels = c("Worst 25% of months", "Middle 50%", "Best 25% of months"))) %>%
        group_by(MktRegime) %>%
        summarise(`Capped SWIX` = mean(Mkt), `Median peer` = mean(PeerMedian), `ABC Fund` = mean(ABC), .groups = "drop") %>%
        pivot_longer(-MktRegime, names_to = "Who", values_to = "Ret") %>%
        mutate(Who = factor(Who, levels = c("Capped SWIX", "Median peer", "ABC Fund"))) %>%
        ggplot(aes(MktRegime, Ret, fill = Who)) +
        geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
        geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        geom_text(aes(label = scales::percent(Ret, accuracy = 0.1), vjust = ifelse(Ret >= 0, -0.4, 1.3)),
                  position = position_dodge(width = 0.8), size = 3.8, colour = "grey25") +
        scale_fill_manual(values = c("Capped SWIX" = Bench_Col, "Median peer" = Peer_Col, "ABC Fund" = ABC_Col)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = 0.15)) +
        labs(title = "Where ABC really earns its place: when markets fall",
             subtitle = "Average monthly return in the market's worst, middle and best months (Jan 2015 - Oct 2025)",
             x = NULL, y = NULL, fill = NULL) +
        theme_slide()
}

# --- 8) Skill or luck? Alpha vs its t-statistic ----------------------------------------------
Plot_Q3_Alpha <- function(Alpha) {

    P <- Alpha %>% filter(Group == "Peer"); A <- Alpha %>% filter(Group == "ABC Fund")
    Share_Sig <- round(mean(P$t_Alpha > 2) * 100)

    ggplot(P, aes(Alpha, t_Alpha)) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2, ymax = Inf, fill = "#2A9D8F", alpha = 0.08) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -2, fill = "#E0245E", alpha = 0.06) +
        geom_hline(yintercept = c(-2, 2), colour = "grey55", linetype = "dashed", linewidth = 0.4) +
        geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
        geom_point(colour = Peer_Col, size = 2.4, alpha = 0.7) +
        geom_point(data = A, colour = ABC_Col, size = 6) +
        annotate("text", x = A$Alpha, y = A$t_Alpha, label = paste0("ABC Fund: ", scales::percent(A$Alpha, accuracy = 0.1), " a year"),
                 hjust = 1.1, vjust = -0.9, size = 4.4, colour = ABC_Col, fontface = "bold") +
        annotate("text", x = -Inf, y = Inf, label = paste0("Skill zone: consistent alpha (only ", Share_Sig, "% of peers)"),
                 hjust = -0.03, vjust = 1.4, size = 3.8, colour = "#2A9D8F", fontface = "italic") +
        annotate("text", x = -Inf, y = -Inf, label = "Consistently destroying value", hjust = -0.03, vjust = -0.8,
                 size = 3.8, colour = ABC_Col, fontface = "italic") +
        scale_x_continuous(breaks = seq(-0.08, 0.08, 0.02), labels = scales::percent_format(accuracy = 1)) +
        labs(title = "Skill or luck? ABC's alpha is higher than 98% of peers",
             subtitle = "Each dot is a fund: yearly alpha (x) and how reliably it was earned (t-statistic, y), 2015-2025",
             x = "Alpha (annual return beyond market exposure)", y = "t-statistic of alpha") +
        theme_slide() + theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3))
}

# --- 9) Technical appendix: scree plot + variable correlation circle -----
Plot_Q3_PCA_Prac <- function(PCA_F) {

    Eig <- tibble(Dim = 1:10, Share = PCA_F$eig[1:10, 2] / 100)
    g1 <- ggplot(Eig, aes(factor(Dim), Share)) +
        geom_col(fill = "#5FA8D3", width = 0.7) +
        geom_line(aes(group = 1), colour = "grey30") + geom_point(colour = "grey30") +
        geom_text(aes(label = scales::percent(Share, accuracy = 0.1)), vjust = -0.6, size = 3.2, colour = "grey25") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
        labs(title = "Scree plot", subtitle = "Share of peer return variation per component", x = "Component", y = NULL) +
        theme_slide()

    V <- as_tibble(PCA_F$var$coord[, 1:2], rownames = "Name") %>% setNames(c("Name", "D1", "D2"))
    S <- as_tibble(PCA_F$quanti.sup$coord[, 1:2, drop = FALSE], rownames = "Name") %>% setNames(c("Name", "D1", "D2"))
    Circle <- tibble(a = seq(0, 2 * pi, length.out = 200), x = cos(a), y = sin(a))
    Pct <- round(PCA_F$eig[1:2, 2], 1)

    g2 <- ggplot() +
        geom_path(data = Circle, aes(x, y), colour = "grey70") +
        geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
        geom_segment(data = V, aes(x = 0, y = 0, xend = D1, yend = D2), colour = Peer_Col, alpha = 0.5,
                     arrow = arrow(length = unit(0.12, "cm"))) +
        geom_segment(data = S, aes(x = 0, y = 0, xend = D1, yend = D2), colour = ABC_Col, linewidth = 1.3,
                     arrow = arrow(length = unit(0.25, "cm"))) +
        geom_text(data = S, aes(D1, D2, label = "ABC Fund (supplementary)"), colour = ABC_Col, fontface = "bold",
                  hjust = 1.05, vjust = -0.8, size = 3.8) +
        coord_equal(xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05)) +
        labs(title = "Variable correlation circle", subtitle = "Grey arrows = peer funds",
             x = paste0("Component 1 (", Pct[1], "%)"), y = paste0("Component 2 (", Pct[2], "%)")) +
        theme_slide()

    cowplot::plot_grid(g1, g2, ncol = 2, rel_widths = c(1, 1))
}
