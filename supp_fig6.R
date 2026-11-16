# supp_fig6.R - Supplementary Figure 6
# Input (data/): Figure_S06a.csv, Figure_S06a_levels.csv, Figure_S06a_text.csv, Figure_S06a_settings.csv,
#  Figure_S06b.csv, Figure_S06b_levels.csv, Figure_S06b_text.csv,
#  Figure_S06c.csv, Figure_S06c_levels.csv, Figure_S06c_text.csv, Figure_S06c_settings.csv,
#  Figure_S06d.csv, Figure_S06d_levels.csv, Figure_S06d_text.csv, Figure_S06d_settings.csv,
#  Figure_S06e.csv, Figure_S06e_levels.csv, Figure_S06e_text.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig6.R

source("00_helpers.R")

series_order <- unname(series_labels)

theme_s6 <- theme_fig_base + theme(
  text         = element_text(family = "Arial"),
  axis.text    = element_text(size = 7.5, colour = "black"),
  axis.title   = element_text(size = 8.5, colour = "black"),
  strip.text   = element_text(size = 8, face = "bold", colour = "black"),
  legend.text  = element_text(size = 7.5, colour = "black"),
  legend.title = element_text(size = 8, colour = "black", face = "plain")
)

batch_factor <- function(x, lv) {
  codes <- c(lv_codes(lv, "batch"), lv_codes(lv, "batch_pooled"))
  labels <- unname(c(lv_labels(lv, "batch"), lv_labels(lv, "batch_pooled")))
  f <- factor(labels[match(x, codes)], levels = labels)
  stopifnot(!anyNA(f))
  f
}

# Panel a
panel_s6a <- function() {
  h <- read_source("Figure_S06a.csv", cols = c("series", "batch", "inferential", "same_sign_share"))
  lv <- read_levels("Figure_S06a")
  txt <- read_text("Figure_S06a")
  st <- read_settings("Figure_S06a")
  stopifnot(setequal(h$series, series_order))
  h$series <- factor(h$series, levels = rev(series_order))
  h$batch <- batch_factor(h$batch, lv)
  ggplot(h, aes(batch, series)) +
    geom_tile(aes(fill = same_sign_share), colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.0f", 100 * same_sign_share)), size = 2.2, nudge_y = 0.22,
              family = "Arial") +
    geom_text(aes(label = inferential), size = 1.7, nudge_y = -0.25, family = "Arial") +
    scale_fill_gradient(low = "#EEF1F3", high = "#9EB7C5", limits = st$fill_limits,
                        labels = label_percent(), name = txt$legend_title) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = txt$x_title, y = NULL) +
    theme_fig_base +
    theme(text = element_text(family = "Arial"), axis.text = element_text(size = 7, colour = "black"),
          axis.title = element_text(size = 9), legend.text = element_text(size = 8),
          panel.border = element_blank(), axis.ticks = element_blank(),
          legend.position = "right", plot.margin = margin(5, 6, 5, 6))
}

# Panel b
panel_s6b <- function() {
  h <- read_source("Figure_S06b.csv", cols = c("series", "batch", "point_only"))
  lv <- read_levels("Figure_S06b")
  txt <- read_text("Figure_S06b")
  stopifnot(setequal(h$series, series_order))
  h$series <- factor(h$series, levels = rev(series_order))
  h$batch <- batch_factor(h$batch, lv)
  ggplot(h, aes(batch, series)) +
    geom_tile(aes(fill = point_only > 0), colour = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(point_only > 0, point_only, "")), size = 2, family = "Arial") +
    scale_fill_manual(values = c("FALSE" = "#F4F4F4", "TRUE" = "#D9C9A8"),
                      labels = c(txt$legend_none, txt$legend_point_only), name = NULL) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = txt$x_title, y = NULL) +
    theme_fig_base +
    theme(text = element_text(family = "Arial"), panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text = element_text(size = 6, colour = "black"), axis.title = element_text(size = 7.5),
          legend.text = element_text(size = 6.5), legend.key.size = unit(0.09, "in"),
          legend.position = "bottom", legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(0, "pt"), plot.margin = margin(2, 2, 2, 2))
}

# Panel c
panel_s6c <- function() {
  om <- read_source("Figure_S06c.csv", cols = c("batch", "selection"))
  lv <- read_levels("Figure_S06c")
  txt <- read_text("Figure_S06c")
  st <- read_settings("Figure_S06c")
  types <- lv_codes(lv, "selection")
  pooled <- unname(lv_labels(lv, "batch_pooled"))
  n_batchs <- length(lv_codes(lv, "batch"))
  stopifnot(setequal(om$selection, types))
  om$batch <- batch_factor(om$batch, lv)
  om$selection <- factor(om$selection, levels = types)
  om$lab <- rep("", nrow(om))
  ggplot(om, aes(batch, 100 * share, colour = selection, shape = selection, group = selection)) +
    geom_vline(xintercept = n_batchs + 0.5, colour = "grey85", linewidth = 0.4) +
    geom_line(data = subset(om, !batch %in% pooled), linewidth = 0.35) +
    geom_point(size = 1.7) +
    geom_text(aes(label = lab), vjust = -1.0, size = 1.75, colour = "black", show.legend = FALSE,
              family = "Arial") +
    scale_colour_manual(values = setNames(c("#2E4B5E", "#9AA5AC")[seq_along(types)], types), name = NULL) +
    scale_shape_manual(values = setNames(c(16, 1)[seq_along(types)], types), name = NULL) +
    scale_y_continuous(limits = st$y_limits, breaks = st$y_breaks) +
    scale_x_discrete(labels = function(x) ifelse(x %in% pooled, x, sub("-", "-\n", x))) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s6 +
    theme(axis.text.x = element_text(size = 6, lineheight = 0.8), axis.title = element_text(size = 7.5),
          legend.position = "bottom", legend.text = element_text(size = 7),
          legend.margin = margin(0, 0, 0, 0), legend.key.width = unit(0.15, "in"))
}

# Panel d
panel_s6d <- function() {
  u <- read_source("Figure_S06d.csv", cols = c("feature", "year", "estimate", "lower", "upper"))
  lv <- read_levels("Figure_S06d")
  txt <- read_text("Figure_S06d")
  st <- read_settings("Figure_S06d")
  measures <- lv_codes(lv, "feature")
  batch_short <- unname(lv_labels(lv, "batch"))
  stopifnot(setequal(u$feature, measures))
  u$feature <- factor(u$feature, levels = measures)
  ggplot(u, aes(year, estimate)) +
    geom_hline(yintercept = 0, linetype = "22", colour = "grey60", linewidth = 0.25) +
    geom_linerange(aes(ymin = lower, ymax = upper), linewidth = 0.4, colour = col_blue) +
    geom_point(size = 1.2, colour = col_blue) +
    facet_wrap(~feature, ncol = 3, scales = "free_y") +
    scale_x_continuous(breaks = st$x_breaks, labels = batch_short[st$x_breaks],
                       limits = st$x_limits) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s6 +
    theme(axis.text = element_text(size = 6.5), panel.spacing = unit(0.1, "in"),
          strip.text = element_text(size = 7.5))
}

# Panel e
panel_s6e <- function() {
  e <- read_source("Figure_S06e.csv", cols = c("feature", "estimate_type", "period", "estimate",
                                               "lower", "upper"))
  lv <- read_levels("Figure_S06e")
  txt <- read_text("Figure_S06e")
  features <- lv_codes(lv, "feature")
  types <- lv_codes(lv, "estimate_type")
  batchs <- lv_codes(lv, "batch")
  batch_short <- unname(lv_labels(lv, "batch"))
  periods <- lv_codes(lv, "period")
  n_batch <- length(batchs)
  n_per <- length(periods)
  stopifnot(setequal(e$feature, features), setequal(e$estimate_type, types))
  e$row <- ifelse(e$estimate_type == types[1], n_batch + 4 - match(e$period, batchs),
                  n_per + 1 - match(e$period, periods) +
                    ifelse(e$estimate_type == types[3], -0.15, 0.15))
  stopifnot(!anyNA(e$row))
  e$feature <- factor(e$feature, levels = features)
  e$estimate_type <- factor(e$estimate_type, levels = types)
  ggplot(e, aes(estimate, row, colour = estimate_type, shape = estimate_type)) +
    geom_vline(xintercept = 0, linetype = "22", colour = "grey60", linewidth = 0.25) +
    geom_hline(yintercept = n_per + 1, colour = "grey82", linewidth = 0.3) +
    geom_segment(aes(x = lower, xend = upper, yend = row), linewidth = 0.35) +
    geom_point(size = 1.2) +
    facet_wrap(~feature, nrow = 1, scales = "free_x") +
    scale_y_continuous(breaks = c(rev(seq_len(n_batch)) + 3, rev(seq_len(n_per))),
                       labels = c(batch_short, unname(lv_labels(lv, "period")))) +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    scale_colour_manual(values = setNames(c(col_blue, col_blue, "#B5793F")[seq_along(types)], types), name = NULL) +
    scale_shape_manual(values = setNames(c(16, 1, 18)[seq_along(types)], types), name = NULL) +
    labs(x = txt$x_title, y = NULL) +
    theme_s6 +
    theme(legend.position = "bottom", axis.text = element_text(size = 7),
          legend.margin = margin(0, 0, 0, 0))
}

save_panel(panel_s6a(), "Figure_S06a", width_pt = 576, height_pt = 129.6)
save_panel(panel_s6b(), "Figure_S06b", width_pt = 288, height_pt = 136.8)
save_panel(panel_s6c(), "Figure_S06c", width_pt = 309.6, height_pt = 154.8)
save_panel(panel_s6d(), "Figure_S06d", width_pt = 612, height_pt = 259.2)
save_panel(panel_s6e(), "Figure_S06e", width_pt = 612, height_pt = 194.4)
