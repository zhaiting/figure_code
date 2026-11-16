# supp_fig3.R - Supplementary Figure 3
# Input (data/): Figure_S03a.csv, Figure_S03c.csv, Figure_S03d.csv,
#  Figure_S03a_text.csv, Figure_S03a_levels.csv, Figure_S03a_settings.csv,
#  Figure_S03c_text.csv, Figure_S03c_settings.csv, Figure_S03d_text.csv,
#  Figure_S03d_levels.csv, Figure_S03d_settings.csv, Labels_series_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig3.R

source("00_helpers.R")

stratum_palette <- c("#406783", "#B44E4E")
linetype_palette <- c("solid", "dashed")

strip_labeller <- as_labeller(function(x) {
  ifelse(x == series_labels[[first_code]], paste0("plain('", x, "')"), paste0("bold('", x, "')"))
}, default = label_parsed)

theme_s3 <- theme_fig_base + theme(
  text            = element_text(family = "Helvetica", size = 8.5),
  axis.title      = element_text(size = 9),
  axis.text       = element_text(size = 7.8, colour = "black"),
  strip.text      = element_text(size = 8.5),
  legend.text     = element_text(size = 8.5),
  legend.key.width = unit(0.3, "in"),
  plot.margin     = margin(6, 8, 4, 4)
)

one_decimal <- function(x) formatC(x + 0, format = "f", digits = 1)
panel_S03a <- function() {
  txt <- read_text("Figure_S03a")
  lv  <- read_levels("Figure_S03a")
  st  <- read_settings("Figure_S03a")
  column_labels <- lv_labels(lv, "assessment")
  stratum_cols <- setNames(stratum_palette[seq_along(lv_codes(lv, "stratum"))], lv_codes(lv, "stratum"))
  age_breaks <- function(lim) {
    if (min(lim) > st$x_breaks_threshold) st$x_breaks_high else st$x_breaks_low
  }
  d <- read_source("Figure_S03a.csv", cols = c("assessment", "series", "stratum", "age", "estimate", "lower", "upper"))
  stopifnot(setequal(d$assessment, names(column_labels)), setequal(d$stratum, names(stratum_cols)),
            setequal(d$series, series_codes))
  d$column <- factor(column_labels[d$assessment], levels = column_labels)
  d$row <- factor(series_labels[d$series], levels = series_labels)
  d$stratum <- factor(d$stratum, levels = names(stratum_cols))
  ggplot(d, aes(age, estimate, colour = stratum, fill = stratum)) +
    geom_hline(yintercept = 0, colour = "grey82", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.16, colour = NA) +
    geom_line(linewidth = 0.65) +
    facet_grid(row ~ column, scales = "free_x", switch = "y",
               labeller = labeller(row = strip_labeller)) +
    scale_colour_manual(values = stratum_cols, name = NULL) +
    scale_fill_manual(values = stratum_cols, name = NULL) +
    scale_x_continuous(breaks = age_breaks) +
    scale_y_continuous(breaks = st$y_breaks, labels = one_decimal) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s3 +
    theme(legend.position = "top", legend.margin = margin(0, 0, 0, 0),
          strip.placement = "outside", strip.text.y.left = element_text(angle = 0, size = 8.5),
          panel.spacing = unit(0.12, "in"))
}

age_difference_plot <- function(d, y_breaks, x_breaks, txt) {
  d$series_label <- factor(series_labels[d$series], levels = series_labels)
  ylim <- c(floor(min(d$lower) * 10) / 10, ceiling(max(d$upper) * 10) / 10)
  ggplot(d, aes(age, estimate, colour = series_label, fill = series_label)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
    facet_wrap(~series_label, nrow = 1, labeller = strip_labeller) +
    scale_colour_manual(values = setNames(series_colors, series_labels), guide = "none") +
    scale_fill_manual(values = setNames(series_colors, series_labels), guide = "none") +
    scale_x_continuous(breaks = x_breaks, expand = expansion(mult = 0.04)) +
    scale_y_continuous(limits = ylim, breaks = y_breaks, labels = one_decimal) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s3 +
    theme(panel.spacing = unit(0.1, "in"))
}

panel_S03c <- function() {
  txt <- read_text("Figure_S03c")
  st  <- read_settings("Figure_S03c")
  d <- read_source("Figure_S03c.csv", cols = c("assessment", "series", "age", "estimate", "lower", "upper"))
  stopifnot(setequal(d$series, series_codes))
  age_difference_plot(d, y_breaks = st$y_breaks, x_breaks = st$x_breaks, txt = txt) +
    geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.22) +
    geom_line(linewidth = 0.7)
}

panel_S03d <- function() {
  txt <- read_text("Figure_S03d")
  lv  <- read_levels("Figure_S03d")
  st  <- read_settings("Figure_S03d")
  periods <- lv_codes(lv, "assessment")
  d <- read_source("Figure_S03d.csv", cols = c("assessment", "series", "age", "estimate", "lower", "upper"))
  stopifnot(setequal(d$assessment, periods), setequal(d$series, series_codes))
  d$period <- factor(d$assessment, levels = periods, labels = lv_labels(lv, "assessment")[periods])
  age_difference_plot(d, y_breaks = st$y_breaks, x_breaks = st$x_breaks, txt = txt) +
    geom_ribbon(aes(ymin = lower, ymax = upper, group = period), colour = NA, alpha = 0.16) +
    geom_line(aes(linetype = period, group = period), linewidth = 0.7) +
    scale_linetype_manual(values = setNames(linetype_palette[seq_along(periods)],
                                            levels(d$period)),
                          name = txt$linetype_title) +
    guides(linetype = guide_legend(override.aes = list(colour = "#2B2B2B", fill = NA))) +
    theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-4, 0, 0, 0), legend.title = element_text(size = 8.5, face = "plain"),
          legend.key.width = unit(0.5, "in"))
}

save_panel(panel_S03a(), "Figure_S03a", width_pt = 545, height_pt = 290)
save_panel(panel_S03c(), "Figure_S03c", width_pt = 545, height_pt = 95)
save_panel(panel_S03d(), "Figure_S03d", width_pt = 545, height_pt = 120)
