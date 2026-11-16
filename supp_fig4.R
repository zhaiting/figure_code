# supp_fig4.R - Supplementary Figure 4
# Input (data/): Figure_S04b.csv, Figure_S04b_levels.csv,
#  Figure_S04c.csv, Figure_S04c_levels.csv, Figure_S04c_text.csv,
#  Figure_S04e.csv, Figure_S04e_levels.csv, Figure_S04e_text.csv, Figure_S04e_settings.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig4.R

source("00_helpers.R")

theme_s4 <- theme_fig_base + theme(
  text         = element_text(family = "Helvetica", size = 8.5, colour = "black"),
  axis.title   = element_text(size = 8),
  axis.text    = element_text(size = 7.6, colour = "black"),
  strip.text   = element_text(size = 8, face = "bold", margin = margin(t = 2, b = 4)),
  legend.title = element_text(size = 7.6, face = "plain"),
  legend.text  = element_text(size = 7.2),
  plot.margin  = margin(4, 5, 4, 4)
)

category_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                    "#CC79A7", "#9B6D9E", "#66A69D", "#A08058", "#6B5A7F", "#B3B3B3")
tertile_palette <- c("#9DB9D3", "#4F86B5", "#1F4E79")

unescape_nl <- function(x) gsub("\\n", "\n", x, fixed = TRUE)

panel_S04b <- function() {
  d <- read_source("Figure_S04b.csv", cols = c("sample", "metric", "estimate", "lower", "upper"))
  lv <- read_levels("Figure_S04b")
  samples <- lv_codes(lv, "sample")
  wrapped <- unescape_nl(unname(lv_labels(lv, "sample")))
  metrics <- lv_codes(lv, "metric")
  stopifnot(setequal(d$sample, samples), setequal(d$metric, metrics))
  d$sample_f <- factor(wrapped[match(d$sample, samples)], levels = rev(wrapped))
  d$metric_f <- factor(d$metric, levels = metrics)
  stopifnot(!anyNA(d$sample_f), !anyNA(d$metric_f))
  blank <- data.frame(sample_f = factor(wrapped[length(wrapped)], levels = rev(wrapped)),
                      metric_f = factor(metrics[length(metrics)], levels = levels(d$metric_f)))
  ggplot(d, aes(estimate, sample_f)) +
    geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0, linewidth = 0.5, colour = "#1C4A5A") +
    geom_point(size = 1.4, colour = "#1C4A5A") +
    geom_blank(data = blank, aes(x = 0, y = sample_f), inherit.aes = FALSE) +
    facet_wrap(~metric_f, nrow = 1, scales = "free_x") +
    labs(x = NULL, y = NULL) +
    theme_s4 +
    theme(panel.spacing = unit(0.15, "in"),
          axis.text.y = element_text(hjust = 1, margin = margin(r = 3), size = 7, lineheight = 0.9))
}

panel_S04c <- function() {
  d <- read_source("Figure_S04c.csv", cols = c("measure", "tertile", "p10", "p25", "median", "p75", "p90"))
  lv <- read_levels("Figure_S04c")
  txt <- read_text("Figure_S04c")
  tertiles <- lv_codes(lv, "tertile")
  tertile_cols <- setNames(tertile_palette[seq_along(tertiles)], tertiles)
  stopifnot(setequal(d$measure, lv_codes(lv, "measure")), setequal(d$tertile, tertiles))
  d$measure <- factor(d$measure, levels = lv_codes(lv, "measure"))
  d$tertile <- factor(d$tertile, levels = tertiles)
  ggplot(d, aes(x = tertile, ymin = p10, lower = p25, middle = median, upper = p75, ymax = p90,
                colour = tertile, fill = tertile)) +
    geom_boxplot(stat = "identity", width = 0.32, alpha = 0.22, linewidth = 0.45) +
    facet_wrap(~measure, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = tertile_cols, guide = "none") +
    scale_fill_manual(values = tertile_cols, guide = "none") +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s4 + theme(panel.spacing = unit(0.1, "in"), axis.text.x = element_text(size = 6.8))
}

panel_S04e <- function() {
  d <- read_source("Figure_S04e.csv", cols = c("tertile", "category", "percent"))
  lv <- read_levels("Figure_S04e")
  txt <- read_text("Figure_S04e")
  st <- read_settings("Figure_S04e")
  tertiles <- lv_codes(lv, "tertile")
  category_cols <- setNames(category_palette[seq_along(lv_codes(lv, "category"))], lv_codes(lv, "category"))
  stopifnot(setequal(d$tertile, tertiles), all(d$category %in% names(category_cols)),
            all(abs(tapply(d$percent, d$tertile, sum) - 100) < 1e-4))
  d$tertile_f <- factor(d$tertile, levels = tertiles, labels = tertiles)
  shown <- intersect(names(category_cols), unique(d$category))
  d$category <- factor(d$category, levels = rev(shown))
  ggplot(d, aes(tertile_f, percent, fill = category)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = category_cols, name = NULL, breaks = shown) +
    scale_y_continuous(expand = c(0, 0), limits = st$y_limits) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s4 +
    theme(legend.key.size = unit(0.13, "in"), legend.text = element_text(size = 7),
          legend.spacing.y = unit(0.02, "in"), axis.text.x = element_text(size = 6.4, lineheight = 0.9),
          legend.margin = margin(0, 0, 0, 2))
}

save_panel(panel_S04b(), "Figure_S04b", width_pt = 358, height_pt = 150)
save_panel(panel_S04c(), "Figure_S04c", width_pt = 595, height_pt = 140)
save_panel(panel_S04e(), "Figure_S04e", width_pt = 198, height_pt = 259)
