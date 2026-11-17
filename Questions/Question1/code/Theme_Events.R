# Theme_Events.R
# My own plot style (used instead of the default fmxdat theme) plus shaded
# "event eras" so readers can link volatility spikes to what happened at the time.

# --- Palette: muted, modern colours. Local series = blues/purples/greens,
# global series = warm colours / greys, so local vs global is easy to spot.
Pal <- c("Capped SWIX"   = "#1F3A5F", "Top 40"     = "#5FA8D3", "Mid Caps"   = "#8E6C8A",
         "Small Caps"    = "#8AB17D", "MSCI ACWI"  = "#E76F51", "MSCI EM"    = "#F4A261",
         "S&P 500"       = "#6C7A89", "Euro Stoxx 50" = "#2A9D8F", "Nikkei 225" = "#C9A227")

# --- Theme ------------------------------------------------------------------------
theme_sleek <- function(base_size = 11) {

    theme_minimal(base_size = base_size, base_family = "sans") +
        theme(
            plot.title          = element_text(face = "bold", size = rel(1.15), colour = "grey10", margin = margin(b = 4)),
            plot.subtitle       = element_text(size = rel(0.9), colour = "grey40", margin = margin(b = 8)),
            plot.caption        = element_text(size = rel(0.75), colour = "grey50", face = "italic", hjust = 0, margin = margin(t = 8)),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            panel.grid.major.x  = element_blank(),
            panel.grid.minor    = element_blank(),
            panel.grid.major.y  = element_line(colour = "grey90", linewidth = 0.3),
            axis.line.x         = element_line(colour = "grey70", linewidth = 0.3),
            axis.ticks          = element_blank(),
            axis.text           = element_text(size = rel(0.85), colour = "grey35"),
            axis.title          = element_text(size = rel(0.85), colour = "grey35"),
            legend.position     = "top",
            legend.justification = "left",
            legend.title        = element_blank(),
            legend.text         = element_text(size = rel(0.85), colour = "grey25"),
            legend.key.width    = unit(14, "pt"),
            legend.margin       = margin(0, 0, 0, 0),
            strip.text          = element_text(face = "bold", size = rel(0.95), colour = "grey20", hjust = 0, margin = margin(t = 6, b = 4)),
            panel.spacing       = unit(12, "pt"),
            plot.background     = element_rect(fill = "white", colour = NA),
            plot.margin         = margin(10, 14, 8, 8)
        )
}

# Common date axis + percentage y-axis
scale_x_years <- function(Breaks = "2 years") scale_x_date(date_breaks = Breaks, date_labels = "%Y", expand = expansion(mult = 0.01))
scale_y_pct   <- function(Acc = 1) scale_y_continuous(labels = scales::percent_format(accuracy = Acc))

# --- Event eras ---------------------------------------------------------------------
# Market-relevant episodes since 2015. Two local (SA-specific) political shocks and
# three global shocks - this split is used in the analysis to test whether local
# volatility is home-grown or imported.
Q1_Events <- tibble::tribble(
    ~Label,                     ~Start,        ~End,          ~Type,
    "Nenegate",                 "2015-12-09",  "2015-12-31",  "Local",
    "Cabinet reshuffle & junk", "2017-03-30",  "2017-04-28",  "Local",
    "COVID-19 crash",           "2020-02-20",  "2020-06-30",  "Global",
    "Ukraine war & oil shock",  "2022-02-24",  "2022-06-30",  "Global",
    "US tariff shock",          "2025-04-02",  "2025-04-30",  "Global"
) %>% mutate(across(c(Start, End), as.Date), ID = row_number())

# Caption naming the shaded bands in chronological order (left to right on the plot)
Events_Caption <- function(Events = Q1_Events, MinDate = as.Date("1900-01-01")) {
    Ev <- Events %>% filter(End >= MinDate) %>% arrange(Start)
    stringr::str_wrap(paste0("Shaded areas, left to right: ", paste(Ev$Label, collapse = ", "), "."), width = 140)
}

# Grey bands marking each event (named in the caption, so no labels on the plot itself).
# Add BEFORE the line layers so the lines sit on top.
# MinDate: drop events that end before the plotted data starts.
# Labels = TRUE adds a small number on top of each band (kept as an option, off by default);
# FacetVar / FacetValue / FacetLevels then print those numbers in one facet panel only.
geom_events <- function(Events = Q1_Events, Labels = FALSE, MinDate = as.Date("1900-01-01"),
                        FacetVar = NULL, FacetValue = NULL, FacetLevels = NULL) {

    Ev <- Events %>% filter(End >= MinDate) %>% mutate(Start = pmax(Start, MinDate))

    Bands <- annotate("rect", xmin = Ev$Start, xmax = Ev$End, ymin = -Inf, ymax = Inf,
                      fill = "grey55", alpha = 0.16)
    if (!Labels) return(list(Bands))

    LabDF <- Ev %>% mutate(Mid = Start + (End - Start) / 2)
    if (!is.null(FacetVar)) LabDF[[FacetVar]] <- factor(FacetValue, levels = FacetLevels)

    list(Bands,
         geom_text(data = LabDF, aes(x = Mid, y = Inf, label = ID), inherit.aes = FALSE,
                   vjust = 1.4, size = 2.8, fontface = "bold", colour = "grey35"))
}
