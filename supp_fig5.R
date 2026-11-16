# supp_fig5.R - Supplementary Figure 5
# Input (data/): Figure_S05a.csv, Figure_S05a_levels.csv, Figure_S05a_text.csv, Figure_S05a_settings.csv,
#  Figure_S05c.csv, Figure_S05c_levels.csv, Figure_S05c_text.csv, Figure_S05c_settings.csv,
#  Figure_S05d.csv, Figure_S05d_levels.csv, Figure_S05d_text.csv, Figure_S05d_settings.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig5.R

source("00_helpers.R")
suppressPackageStartupMessages(library(ggtext))

model_palette <- c("#355872", "#C07A2C")
stratum_palette <- c("#B44E4E", "#406783")

theme_s5 <- theme_fig_base + theme(
  text           = element_text(family = "Helvetica", size = 8.5, colour = "black"),
  axis.title     = element_text(size = 8),
  axis.text      = element_text(size = 7.6, colour = "black"),
  strip.text     = element_markdown(size = 8, margin = margin(t = 2, b = 4)),
  legend.title   = element_text(size = 7.6, face = "plain"),
  legend.text    = element_text(size = 7.2),
  plot.margin    = margin(4, 5, 4, 4)
)

panel_S05a <- function() {
  d <- read_source("Figure_S05a.csv", cols = c("assessment", "series", "model", "hr", "lower", "upper"))
  lv <- read_levels("Figure_S05a")
  txt <- read_text("Figure_S05a")
  st <- read_settings("Figure_S05a")
  heads <- lv_labels(lv, "assessment")
  model_cols <- setNames(model_palette[seq_along(lv_codes(lv, "model"))], lv_codes(lv, "model"))
  model_labels <- lv_labels(lv, "model")
  stopifnot(setequal(d$assessment, names(heads)), setequal(d$model, names(model_cols)),
            all(d$series %in% names(series_labels)))
  d$facet <- factor(heads[d$assessment], levels = heads)
  d$series_f <- factor(series_labels[d$series], levels = rev(series_labels))
  d$model <- factor(d$model, levels = names(model_cols))
  ggplot(d, aes(hr, series_f, colour = model)) +
    geom_vline(xintercept = 1, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0, linewidth = 0.5,
                  position = position_dodge(0.55, reverse = TRUE)) +
    geom_point(size = 1.6, position = position_dodge(0.55, reverse = TRUE)) +
    facet_wrap(~facet, nrow = 1) +
    scale_x_continuous(limits = st$x_limits, breaks = st$x_breaks) +
    scale_colour_manual(values = model_cols, labels = model_labels, name = NULL) +
    labs(x = txt$x_title, y = NULL) +
    theme_s5 + theme(legend.position = "bottom", panel.spacing.x = unit(0.15, "in"))
}

panel_S05c <- function() {
  d <- read_source("Figure_S05c.csv", cols = c("assessment", "endpoint", "stratum", "hr", "lower", "upper"))
  lv <- read_levels("Figure_S05c")
  txt <- read_text("Figure_S05c")
  st <- read_settings("Figure_S05c")
  endpoints <- lv_codes(lv, "endpoint")
  heads <- lv_labels(lv, "assessment")
  stratum_cols <- setNames(stratum_palette[seq_along(lv_codes(lv, "stratum"))], lv_codes(lv, "stratum"))
  stopifnot(all(d$lower > 0), setequal(d$assessment, names(heads)), setequal(d$endpoint, endpoints),
            setequal(d$stratum, names(stratum_cols)))
  d$facet <- factor(heads[d$assessment], levels = heads)
  d$endpoint <- factor(d$endpoint, levels = rev(endpoints))
  d$stratum <- factor(d$stratum, levels = names(stratum_cols))
  xlim <- c(min(st$x_limit_min, min(d$lower) * 0.9), max(st$x_limit_max, max(d$upper) * 1.1))
  ggplot(d, aes(hr, endpoint, colour = stratum)) +
    geom_vline(xintercept = 1, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0, linewidth = 0.5,
                  position = position_dodge(0.6, reverse = TRUE)) +
    geom_point(size = 1.5, position = position_dodge(0.6, reverse = TRUE)) +
    facet_wrap(~facet, nrow = 1) +
    scale_x_log10(limits = xlim, breaks = st$x_breaks,
                  labels = as.character(st$x_breaks)) +
    scale_colour_manual(values = stratum_cols, name = NULL) +
    labs(x = txt$x_title, y = NULL) +
    theme_s5 + theme(legend.position = "bottom", panel.spacing.x = unit(0.12, "in"))
}

panel_S05d <- function() {
  d <- read_source("Figure_S05d.csv", cols = c("item", "stratum", "estimate", "lower", "upper", "estimate_both_strata"))
  lv <- read_levels("Figure_S05d")
  txt <- read_text("Figure_S05d")
  st <- read_settings("Figure_S05d")
  stratum_cols <- setNames(stratum_palette[seq_along(lv_codes(lv, "stratum"))], lv_codes(lv, "stratum"))
  stopifnot(min(d$lower) > st$x_limits[1], max(d$upper) < st$x_limits[2],
            setequal(d$stratum, names(stratum_cols)))
  both <- unique(d[, c("item", "estimate_both_strata")])
  d$item <- factor(d$item, levels = both$item[order(-both$estimate_both_strata)])
  d$stratum <- factor(d$stratum, levels = names(stratum_cols))
  ggplot(d, aes(estimate, item, colour = stratum)) +
    geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0, linewidth = 0.5,
                  position = position_dodge(0.6, reverse = TRUE)) +
    geom_point(size = 1.5, position = position_dodge(0.6, reverse = TRUE)) +
    scale_x_continuous(limits = st$x_limits, breaks = st$x_breaks) +
    scale_colour_manual(values = stratum_cols, guide = "none") +
    labs(x = txt$x_title, y = NULL) +
    theme_s5 + theme(plot.margin = margin(4, 8, 4, 4))
}

save_panel(panel_S05a(), "Figure_S05a", width_pt = 595, height_pt = 259)
save_panel(panel_S05c(), "Figure_S05c", width_pt = 403, height_pt = 256)
save_panel(panel_S05d(), "Figure_S05d", width_pt = 187, height_pt = 231)
