# supp_fig8.R - Supplementary Figure 8
# Input (data/): Figure_S08a_text.csv,
#  Figure_S08b.csv, Figure_S08b_levels.csv, Figure_S08b_text.csv, Figure_S08b_settings.csv,
#  Figure_S08c.csv, Figure_S08c_levels.csv, Figure_S08c_text.csv,
#  Figure_S08d.csv, Figure_S08d_levels.csv, Figure_S08d_text.csv, Figure_S08d_settings.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript supp_fig8.R

source("00_helpers.R")

theme_s8 <- theme_fig_base + theme(
  text              = element_text(family = "Helvetica"),
  axis.text         = element_text(size = 8.3, colour = "black"),
  axis.title        = element_text(size = 9),
  strip.text        = element_text(size = 9, face = "bold"),
  legend.text       = element_text(size = 8.2),
  plot.margin       = margin(3, 5, 3, 3),
  legend.position   = "bottom",
  legend.key.height = unit(9, "pt"),
  legend.key.width  = unit(18, "pt"),
  legend.margin     = margin(0, 0, 0, 0),
  panel.spacing     = unit(9, "pt")
)

# Panel a
panel_s8a <- function() {
  txt <- read_text("Figure_S08a")
  W <- 278
  H <- 155
  ink <- "#40515b"
  dash_col <- "#7e909b"
  vp <- viewport(xscale = c(0, W), yscale = c(H, 0))
  box <- function(x, y, w, h, lty = "solid") {
    grid.rect(x + w / 2, y + h / 2, w, h, default.units = "native", vp = vp,
              gp = gpar(col = ink, fill = "white", lwd = 0.7, lty = lty))
  }
  arrow_head <- arrow(length = unit(4.5, "pt"), angle = 25, type = "closed")
  edge <- function(x1, y1, x2, y2, dashed = FALSE) {
    col <- if (dashed) dash_col else ink
    grid.segments(x1, y1, x2, y2, default.units = "native", vp = vp, arrow = arrow_head,
                  gp = gpar(col = col, fill = col, lwd = 0.7, lty = if (dashed) "44" else "solid"))
  }
  label <- function(x, y, txt) {
    grid.text(txt, x, y, default.units = "native", vp = vp,
              gp = gpar(fontsize = 8, fontfamily = "Helvetica", col = "#111111"))
  }
  function() {
    box(26, 4, 93, 34)
    box(153, 4, 117, 34, lty = "44")
    box(3, 90, 65, 32)
    box(100, 90, 65, 32)
    box(197, 90, 78, 32)
    edge(60, 38, 36, 90); edge(81, 38, 132, 90); edge(105, 38, 235, 90)
    edge(68, 106, 100, 106); edge(165, 106, 197, 106)
    t <- seq(0, 1, length.out = 60)
    grid.lines((1 - t)^2 * 35 + 2 * (1 - t) * t * 136 + t^2 * 236,
               (1 - t)^2 * 122 + 2 * (1 - t) * t * 183 + t^2 * 122,
               default.units = "native", vp = vp, arrow = arrow_head,
               gp = gpar(col = ink, fill = ink, lwd = 0.7))
    edge(176, 38, 64, 90, TRUE); edge(201, 38, 146, 90, TRUE); edge(223, 38, 240, 90, TRUE)
    label(72.5, 13.5, txt$node_c_symbol); label(72.5, 26.5, txt$node_c_name)
    label(211.5, 13.5, txt$node_u_symbol); label(211.5, 26.5, txt$node_u_name)
    label(35.5, 100.5, txt$node_a_symbol); label(35.5, 112.5, txt$node_a_name)
    label(132.5, 100.5, txt$node_m_symbol); label(132.5, 112.5, txt$node_m_name)
    label(236, 100.5, txt$node_y_symbol); label(236, 112.5, txt$node_y_name)
  }
}

# Panel b
panel_s8b <- function() {
  s <- read_source("Figure_S08b.csv", cols = c("contrast", "model", "estimate", "lower", "upper"))
  lv <- read_levels("Figure_S08b")
  txt <- read_text("Figure_S08b")
  st <- read_settings("Figure_S08b")
  models <- lv_codes(lv, "model")
  stopifnot(setequal(s$contrast, lv_codes(lv, "contrast")), setequal(s$model, models))
  s$contrast <- factor(s$contrast, levels = lv_codes(lv, "contrast"))
  s$model <- factor(s$model, levels = models)
  ggplot(s, aes(estimate, contrast, colour = model)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y",
                  position = position_dodge(0.28), width = 0, linewidth = 0.6) +
    geom_point(position = position_dodge(0.28), size = 2) +
    scale_colour_manual(values = setNames(c("#9AA3AA", col_blue)[seq_along(models)], models), name = NULL) +
    scale_x_continuous(breaks = st$x_breaks) +
    labs(title = txt$title,
         x = txt$x_title, y = NULL) +
    theme_s8 +
    theme(plot.title = element_text(size = 10, face = "bold"), plot.subtitle = element_text(size = 8),
          plot.title.position = "plot", legend.margin = margin(3, 0, 0, 0))
}

# Panel c
panel_s8c <- function() {
  z <- read_source("Figure_S08c.csv", cols = c("series", "feature", "estimate", "lower", "upper",
                                               "fdr_supported"))
  lv <- read_levels("Figure_S08c")
  txt <- read_text("Figure_S08c")
  features <- lv_codes(lv, "feature")
  stopifnot(setequal(z$feature, features), all(z$series %in% series_labels))
  z$feature <- factor(z$feature, levels = rev(features))
  z$series <- factor(z$series, levels = unname(series_labels))
  z$fdr_supported <- as.logical(z$fdr_supported)
  z$higher_value <- z$estimate > 0
  p <- ggplot(z, aes(estimate, feature, colour = higher_value)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey65", linewidth = 0.3) +
    geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 0.4) +
    geom_point(aes(shape = fdr_supported), size = 1.8) +
    facet_wrap(~series, ncol = 3) +
    scale_colour_manual(values = c("FALSE" = col_blue, "TRUE" = col_rust), guide = "none") +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16), labels = c(txt$legend_not_supported, txt$legend_supported),
                       name = NULL) +
    labs(x = txt$x_title, y = NULL) +
    theme_s8
  stopifnot(nrow(ggplot_build(p)$data[[3]]) == nrow(z))
  p
}

# Panel d
panel_s8d <- function() {
  d <- read_source("Figure_S08d.csv", cols = c("measure_set", "path", "year", "hr", "lower", "upper"))
  lv <- read_levels("Figure_S08d")
  txt <- read_text("Figure_S08d")
  st <- read_settings("Figure_S08d")
  paths <- lv_codes(lv, "path")
  measures <- lv_codes(lv, "measure_set")
  stopifnot(setequal(d$path, paths), setequal(d$measure_set, measures))
  d$path <- factor(d$path, levels = paths)
  d$measure_set <- factor(d$measure_set, levels = measures)
  path_col <- setNames(c("#5E676E", "#A3ABB1", "#1A1A1A")[seq_along(paths)], paths)
  p <- ggplot(d, aes(year, hr, colour = path, fill = path, linetype = path)) +
    geom_hline(yintercept = 1, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lower, ymax = upper, alpha = path), colour = NA, show.legend = FALSE) +
    geom_line(aes(linewidth = path)) +
    facet_wrap(~measure_set, nrow = 2) +
    scale_alpha_manual(values = setNames(c(0.08, 0.08, 0.18)[seq_along(paths)], paths), guide = "none") +
    scale_linewidth_manual(values = setNames(c(0.4, 0.4, 0.55)[seq_along(paths)], paths), guide = "none") +
    scale_colour_manual(values = path_col, name = NULL) +
    scale_fill_manual(values = path_col, guide = "none") +
    scale_linetype_manual(values = setNames(c("solid", "longdash", "solid")[seq_along(paths)], paths), name = NULL) +
    scale_y_continuous(trans = "log10", breaks = st$y_breaks) +
    scale_x_continuous(breaks = st$x_breaks) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_s8
  stopifnot(nrow(ggplot_build(p)$data[[3]]) == nrow(d))
  p
}

save_panel(panel_s8a(), "Figure_S08a", width_pt = 278, height_pt = 155)
save_panel(panel_s8b(), "Figure_S08b", width_pt = 277.2, height_pt = 154.8)
save_panel(panel_s8c(), "Figure_S08c", width_pt = 565.2, height_pt = 219.6)
save_panel(panel_s8d(), "Figure_S08d", width_pt = 565.2, height_pt = 205.2)
