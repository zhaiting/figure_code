# fig1.R - Figure 1
# Input (data/): Figure_1c.csv, Figure_1d.csv, Figure_1e.csv,
#  Figure_1c_text.csv, Figure_1c_settings.csv, Figure_1d_text.csv,
#  Figure_1d_settings.csv, Figure_1e_text.csv, Figure_1e_levels.csv,
#  Figure_1e_settings.csv, Labels_series_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript fig1.R

source("00_helpers.R")

age_font <- "Arial"

unnl <- function(x) setNames(gsub("\\n", "\n", x, fixed = TRUE), names(x))

theme_age <- theme_fig_base + theme(
  text         = element_text(family = age_font, size = 7, colour = "black"),
  axis.title   = element_text(size = 7),
  axis.text    = element_text(size = 7, colour = "black"),
  strip.text   = element_text(size = 7, face = "bold", margin = margin(b = 2.5)),
  legend.text  = element_text(size = 7),
  plot.margin  = margin(2, 3, 2, 1)
)

age_scale_y <- function(lim, breaks) {
  scale_y_continuous(limits = lim, breaks = breaks,
                     labels = function(x) formatC(round(x, 1) + 0, format = "f", digits = 1))
}

read_age <- function(file) {
  d <- read_source(file, cols = c("period", "series", "age", "estimate", "lower", "upper"))
  stopifnot(setequal(d$series, series_codes))
  d$series_f <- factor(d$series, levels = series_codes, labels = series_labels[series_codes])
  d
}

age_lines <- function(ribbon_alpha) {
  list(
    geom_hline(yintercept = 0, colour = "grey82", linewidth = 0.27),
    geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = ribbon_alpha),
    geom_line(aes(linewidth = series == first_code)),
    scale_linewidth_manual(values = c(`TRUE` = 0.93, `FALSE` = 0.53), guide = "none"),
    scale_colour_manual(values = setNames(series_colors, series_labels), name = NULL),
    scale_fill_manual(values = setNames(series_colors, series_labels), name = NULL)
  )
}

# Panel c
plot_fig1c <- function() {
  txt <- read_text("Figure_1c")
  st  <- read_settings("Figure_1c")
  d <- read_age("Figure_1c.csv")
  lim <- c(floor(min(d$lower) * 10) / 10, ceiling(max(d$upper) * 10) / 10)
  ggplot(d, aes(age, estimate, colour = series_f, fill = series_f)) +
    age_lines(ribbon_alpha = 0.14) +
    facet_wrap(~period, nrow = 1) +
    scale_x_continuous(breaks = st$x_breaks, expand = expansion(mult = 0.03)) +
    age_scale_y(lim, st$y_breaks) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_age +
    theme(legend.position = "none", panel.spacing = unit(9, "pt"))
}

# Panel d
plot_fig1d <- function() {
  txt <- read_text("Figure_1d")
  st  <- read_settings("Figure_1d")
  d <- read_age("Figure_1d.csv")
  lim <- c(floor(min(d$lower) * 10) / 10, ceiling(max(d$upper) * 10) / 10)
  ggplot(d, aes(age, estimate, colour = series_f, fill = series_f)) +
    age_lines(ribbon_alpha = 0.20) +
    facet_wrap(~period) +
    scale_x_continuous(breaks = st$x_breaks, limits = st$x_limits,
                       expand = expansion(mult = 0.02)) +
    age_scale_y(lim, st$y_breaks) +
    labs(x = txt$x_title, y = txt$y_title) +
    guides(colour = guide_legend(ncol = 2, byrow = TRUE,
                                 override.aes = list(linewidth = 0.7)),
           fill = guide_legend(ncol = 2, byrow = TRUE)) +
    theme_age +
    theme(legend.position = "inside",
          legend.position.inside = c(0.02, 0.02),
          legend.justification = c(0, 0),
          legend.key.width = unit(11.5, "pt"),
          legend.key.height = unit(6.6, "pt"),
          legend.spacing.y = unit(0, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          legend.background = element_blank())
}

# Panel e
plot_fig1e <- function() {
  txt <- read_text("Figure_1e")
  lv  <- read_levels("Figure_1e")
  st  <- read_settings("Figure_1e")
  d <- read_source("Figure_1e.csv",
                   cols = c("comparison", "period", "group", "series",
                            "estimate", "lower", "upper"))

  block_labels <- unnl(lv_labels(lv, "block"))
  d$block <- factor(block_labels[paste(d$comparison, d$period, sep = "|")],
                    levels = unique(block_labels))
  row_codes  <- lv_codes(lv, "row")
  row_labels <- lv_labels(lv, "row")
  d$row_f   <- factor(paste(d$group, d$period, sep = "|"), levels = rev(row_codes))
  d$series_f <- factor(d$series, levels = series_codes, labels = series_short[series_codes])
  d$ci_excludes_zero <- d$lower > 0 | d$upper < 0
  d$label <- sprintf("%.2f", round(d$estimate, 2) + 0)
  stopifnot(!anyNA(d$row_f), !anyNA(d$block), !anyNA(d$series_f))

  ggplot(d, aes(series_f, row_f, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_tile(data = d[d$ci_excludes_zero, ], fill = NA, colour = "#333333",
              linewidth = 0.4, width = 0.90, height = 0.88) +
    geom_text(aes(label = label, colour = abs(estimate) > 0.55),
              size = 2.5, family = "Helvetica") +
    scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"), guide = "none") +
    facet_grid(block ~ ., scales = "free_y", space = "free_y") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), labels = function(x) unname(row_labels[x])) +
    scale_fill_gradient2(low = col_rust, mid = "white", high = col_blue,
                         limits = st$fill_limits, breaks = st$fill_breaks,
                         name = txt$fill_title) +
    labs(x = NULL, y = NULL, caption = txt$caption) +
    guides(fill = guide_colourbar(title.position = "left")) +
    theme_fig_base +
    theme(text = element_text(family = "Helvetica", size = 8),
          axis.text.x = element_text(size = 7.5, face = "bold", colour = "black"),
          axis.text.y = element_text(size = 7.5, colour = "black"),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          strip.text.y = element_text(size = 7.1, angle = 0, hjust = 0, face = "bold"),
          panel.spacing.y = unit(4, "pt"),
          legend.position = "bottom",
          legend.key.height = unit(5, "pt"),
          legend.key.width = unit(17, "pt"),
          legend.title = element_text(size = 7.4, margin = margin(r = 8)),
          legend.text = element_text(size = 7.0),
          legend.margin = margin(1, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          plot.caption = element_text(size = 7.1, hjust = 0),
          plot.caption.position = "panel",
          plot.margin = margin(3, 3, 3, 2))
}

save_panel(plot_fig1c(), "Figure_1c", width_pt = 292, height_pt = 155)
save_panel(plot_fig1d(), "Figure_1d", width_pt = 182, height_pt = 150)
save_panel(plot_fig1e(), "Figure_1e", width_pt = 305, height_pt = 255)
