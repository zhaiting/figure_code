# supp_fig7.R - Supplementary Figure 7
# Input (data/): Figure_S07.csv, Figure_S07_text.csv, Figure_S07_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig7.R

source("00_helpers.R")
suppressPackageStartupMessages({
  library(patchwork)
  library(ggtext)
})

theme_s7 <- theme_fig_base + theme(
  text         = element_text(family = "Arial"),
  axis.text    = element_text(size = 6.8, colour = "black"),
  axis.title   = element_text(size = 8.5, colour = "black"),
  strip.text   = element_markdown(size = 7.4, colour = "black", lineheight = 1.1),
  legend.text  = element_text(size = 7.5, colour = "black"),
  legend.title = element_text(size = 7.5, colour = "black", face = "plain"),
  plot.margin  = margin(3, 1, 3, 3)
)

txt <- read_text("Figure_S07")
lv <- read_levels("Figure_S07")
direction_codes <- lv_codes(lv, "direction")
direction_labels <- lv_labels(lv, "direction")

weight_plot <- function(w, bundle, x_title = FALSE) {
  v <- w[w$bundle == bundle, ]
  r <- v[1, ]
  fdr <- if (r$joint_fdr_q < 0.001) formatC(r$joint_fdr_q, format = "e", digits = 1) else sprintf("%.3f", r$joint_fdr_q)
  v$header <- sprintf(txt$header_template, bundle, r$joint_estimate, r$joint_lower, r$joint_upper, fdr)
  v$signed <- ifelse(v$direction == direction_codes[2], v$weight, -v$weight)
  v$component <- factor(v$component, levels = v$component[order(v$signed)])
  v$direction <- factor(direction_labels[v$direction], levels = direction_labels)
  ggplot(v, aes(signed, component, fill = direction)) +
    geom_vline(xintercept = 0, colour = "#777777", linewidth = 0.3) +
    geom_col(width = 0.7) +
    facet_wrap(~header) +
    scale_fill_manual(values = setNames(c(col_rust, col_blue), direction_labels), drop = FALSE,
                      name = txt$legend_title) +
    scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1),
                       labels = c("1", "0.5", "0", "0.5", "1")) +
    labs(x = if (x_title) txt$x_title else NULL, y = NULL) +
    theme_s7
}

panel_s7 <- function() {
  w <- read_source("Figure_S07.csv", cols = c("bundle", "component", "direction", "weight",
                                              "joint_estimate", "joint_lower", "joint_upper",
                                              "joint_fdr_q"))
  stopifnot(setequal(w$direction, direction_codes))
  sums <- tapply(w$weight, paste(w$bundle, w$direction), sum)
  stopifnot(max(abs(sums - 1)) < 1e-8)

  left <- lv_codes(lv, "bundle_left")
  right <- lv_codes(lv, "bundle_right")
  stopifnot(setequal(w$bundle, c(left, right)))
  k <- table(w$bundle)
  col_left <- wrap_plots(lapply(left, function(m) weight_plot(w, m, m == left[length(left)])), ncol = 1,
                         heights = pmax(as.numeric(k[left]), 5) + 1.6)
  col_right <- wrap_plots(lapply(right, function(m) weight_plot(w, m, m == right[length(right)])), ncol = 1,
                          heights = pmax(as.numeric(k[right]), 5) + 1.6)
  (col_left | col_right) + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

save_panel(panel_s7(), "Figure_S07", width_pt = 612, height_pt = 756)
