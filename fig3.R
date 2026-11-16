# fig3.R - Figure 3
# Input (data/): Figure_3a_histogram.csv, Figure_3a.csv, Figure_3a_text.csv,
#  Figure_3a_levels.csv, Figure_3a_settings.csv, Figure_3b.csv, Figure_3b_text.csv,
#  Figure_3b_levels.csv, Figure_3b_settings.csv, Figure_3c_attenuation.csv,
#  Figure_3c_curves.csv, Figure_3c_text.csv, Figure_3c_levels.csv,
#  Figure_3c_settings.csv, Figure_3d_km.csv, Figure_3d_text.csv,
#  Figure_3d_levels.csv, Figure_3d_settings.csv, Figure_3e.csv, Figure_3e_text.csv,
#  Figure_3e_levels.csv, Figure_3e_settings.csv, Figure_3f.csv, Figure_3f_levels.csv,
#  Labels_series_levels.csv, Labels_groups_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript fig3.R

source("00_helpers.R")
suppressPackageStartupMessages(library(patchwork))

theme_set(theme_fig)

outcome_a_palette <- c("#366B8C", "#BA7850")

nl <- function(x) setNames(gsub("\\n", "\n", x, fixed = TRUE), names(x))

save_panel_scaled <- function(plot, name, width_pt, height_pt, scale, png_dpi = 300) {
  w <- width_pt / 72
  h <- height_pt / 72
  svglite::svglite(file.path(fig_dir, paste0(name, ".svg")),
                   width = w, height = h, bg = "white", scaling = scale)
  print(plot)
  invisible(grDevices::dev.off())
  ragg::agg_png(file.path(fig_dir, paste0(name, ".png")),
                width = w, height = h, units = "in", res = png_dpi,
                background = "white", scaling = scale)
  print(plot)
  invisible(grDevices::dev.off())
}

stack_aligned <- function(top, bottom, heights) {
  g1 <- ggplotGrob(top)
  g2 <- ggplotGrob(bottom)
  widths <- unit.pmax(g1$widths, g2$widths)
  g1$widths <- widths
  g2$widths <- widths
  function() {
    pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(heights, "null"))))
    grid.draw(editGrob(g1, vp = viewport(layout.pos.row = 1)))
    grid.draw(editGrob(g2, vp = viewport(layout.pos.row = 2)))
    popViewport()
  }
}

panel_3a <- function() {
  curves <- read_source("Figure_3a.csv",
                        c("outcome", "series", "prevalence", "lower", "upper"))
  hist <- read_source("Figure_3a_histogram.csv", c("xmin", "xmax", "percent"))
  txt <- read_text("Figure_3a")
  lv <- read_levels("Figure_3a")
  st <- read_settings("Figure_3a")
  outcomes <- lv_codes(lv, "outcome")
  stopifnot(setequal(curves$outcome, outcomes))
  outcome_a_colors <- setNames(outcome_a_palette[seq_along(outcomes)], outcomes)
  curves$outcome <- factor(curves$outcome, levels = outcomes,
                           labels = lv_labels(lv, "outcome")[outcomes])
  names(outcome_a_colors) <- levels(curves$outcome)

  x_range <- range(curves$series)
  x_breaks <- c(seq(ceiling(x_range[1]), floor(x_range[2]), by = 1), st$extra_x_break)
  zero_line <- geom_vline(xintercept = 0, colour = "#777777",
                          linetype = "dashed", linewidth = 0.35)

  curve_plot <- ggplot(curves, aes(series, 100 * prevalence,
                                   colour = outcome, fill = outcome)) +
    zero_line +
    geom_ribbon(aes(ymin = 100 * lower, ymax = 100 * upper), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(values = outcome_a_colors) +
    scale_fill_manual(values = outcome_a_colors) +
    scale_x_continuous(breaks = x_breaks, labels = NULL, expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::breaks_pretty(4),
                       expand = expansion(mult = c(0, 0.02))) +
    coord_cartesian(xlim = x_range, ylim = c(0, 100 * max(curves$upper)), expand = FALSE) +
    labs(title = txt$title, subtitle = txt$subtitle,
         x = NULL, y = txt$y_curve, colour = NULL, fill = NULL) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 7.3),
          legend.key.width = unit(10, "pt"),
          legend.key.height = unit(6, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(4, 5, 6, 4),
          axis.ticks.x = element_blank())

  hist_plot <- ggplot(hist) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = percent),
              fill = "#AEB9BF", colour = "white", linewidth = 0.2) +
    zero_line +
    scale_x_continuous(breaks = x_breaks, expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::breaks_pretty(2), expand = c(0, 0)) +
    coord_cartesian(xlim = x_range, expand = FALSE) +
    labs(x = txt$x_hist, y = txt$y_hist, caption = txt$caption) +
    theme(axis.text = element_text(size = 7.6),
          axis.title = element_text(size = 7.6),
          plot.caption = element_text(size = 7, hjust = 0.5),
          plot.margin = margin(6, 5, 4, 4))

  save_panel(stack_aligned(curve_plot, hist_plot, st$stack_heights),
             "Figure_3a", 250, 225)
}

panel_3b <- function() {
  mo <- read_source("Figure_3b.csv",
                    c("endpoint", "endpoint_order", "series", "hr", "lower", "upper"))
  txt <- read_text("Figure_3b")
  lv <- read_levels("Figure_3b")
  st <- read_settings("Figure_3b")
  stopifnot(setequal(mo$series, series_codes))
  endpoints <- unique(mo$endpoint[order(mo$endpoint_order)])
  mo$endpoint <- factor(mo$endpoint, levels = rev(endpoints))
  mo$x <- match(mo$series, series_codes)
  blocks <- lv_codes(lv, "block")
  mo$block <- factor(ifelse(mo$endpoint %in% lv_codes(lv, "first_block_endpoint"),
                            blocks[1], blocks[2]), levels = blocks)
  mo$ci_excludes_1 <- mo$lower > 1 | mo$upper < 1
  mo$text_colour <- ifelse(abs(log(mo$hr)) > st$white_text_abs_log_hr, "white", "black")

  p <- ggplot(mo, aes(x, endpoint)) +
    geom_tile(aes(fill = log(hr)), colour = "white", linewidth = 0.5) +
    geom_tile(data = mo[mo$ci_excludes_1, ], fill = NA, colour = "#333333",
              linewidth = 0.4, width = 0.86, height = 0.82) +
    geom_text(aes(label = sprintf("%.2f", hr), colour = text_colour), size = 2.35) +
    scale_colour_identity() +
    facet_grid(block ~ ., scales = "free_y", space = "free_y") +
    scale_fill_gradient2(low = col_blue, mid = "white", high = col_rust,
                         limits = c(-log(st$hr_limit), log(st$hr_limit)),
                         breaks = log(st$hr_breaks),
                         labels = as.character(st$hr_breaks), name = txt$legend_title) +
    scale_x_continuous(position = "top", breaks = seq_along(series_codes),
                       labels = series_short,
                       limits = st$x_limits, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(title = txt$title, x = NULL, y = NULL, caption = txt$caption) +
    theme(panel.border = element_blank(),
          axis.ticks = element_blank(),
          strip.text.y = element_blank(),
          axis.text.y = element_text(size = 7.7),
          axis.text.x = element_text(size = 7.4, face = "bold"),
          panel.spacing.y = unit(4, "pt"),
          legend.position = "bottom",
          legend.key.height = unit(5, "pt"),
          legend.key.width = unit(19, "pt"),
          plot.caption = element_text(size = 7.1, hjust = 0),
          legend.margin = margin(0, 0, 0, 0))

  save_panel(p, "Figure_3b", 470, 225)
}

measure_palette <- c("#181818", "#A67500", "#3C5488", "#00856A", "#7B6D5D", "#0072B2",
                     "#56B4E9", "#D55E00", "#7B3294", "#648F00", "#CC79A7")
txt_3c <- read_text("Figure_3c")
lv_3c <- read_levels("Figure_3c")
st_3c <- read_settings("Figure_3c")
measure_codes <- lv_codes(lv_3c, "measure")
measure_labels <- nl(lv_labels(lv_3c, "measure"))
measure_colors <- setNames(measure_palette[seq_along(measure_codes)], measure_codes)
ref_measure <- measure_codes[1]

theme_3c <- theme_fig_base + theme(
  text = element_text(family = "Helvetica", size = 10.5, colour = "black"),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
  plot.title.position = "plot",
  plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 4)),
  axis.title = element_text(size = 10.5, colour = "black"),
  axis.text = element_text(size = 10, colour = "black"),
  legend.position = "none",
  plot.margin = margin(4, 5, 4, 4)
)

hr_curve_plot <- function(curves, sample, title, label_range) {
  z <- curves[curves$sample == sample, ]
  stopifnot(all(z$measure %in% measure_codes))

  ends <- z[z$year == max(z$year), ]
  ends <- ends[order(ends$hr), ]
  ends$label_y <- exp(seq(log(label_range[1]), log(label_range[2]),
                          length.out = nrow(ends)))
  ends$label <- unname(measure_labels[ends$measure])

  y_lim <- st_3c$y_limits
  x_lim <- st_3c$x_limits
  to_npc <- function(y) (log10(y) - log10(y_lim[1])) / diff(log10(y_lim))
  label_right <- unit(1, "npc") + unit(96, "pt")
  label_left <- label_right - stringWidth(ends$label)
  elbow <- unit(1, "npc") + unit(10, "pt")
  labels <- gTree(children = gList(
    segmentsGrob(x0 = unit(ends$year / x_lim[2], "npc"), x1 = elbow,
                 y0 = unit(to_npc(ends$hr), "npc"), y1 = unit(to_npc(ends$label_y), "npc"),
                 gp = gpar(col = measure_colors[ends$measure], lwd = 0.3 * .pt)),
    segmentsGrob(x0 = elbow, x1 = label_left - unit(3, "pt"),
                 y0 = unit(to_npc(ends$label_y), "npc"), y1 = unit(to_npc(ends$label_y), "npc"),
                 gp = gpar(col = measure_colors[ends$measure], lwd = 0.4 * .pt)),
    textGrob(ends$label, x = label_right, y = unit(to_npc(ends$label_y), "npc"),
             hjust = 1, vjust = 0.5)
  ), gp = gpar(fontsize = 9.3, fontfamily = "Helvetica", lineheight = 0.95))

  draw_order <- lv_codes(lv_3c, "draw_order")
  stopifnot(all(z$measure %in% draw_order))
  z$measure <- factor(z$measure, levels = intersect(draw_order, unique(z$measure)))
  ggplot(z, aes(year, hr, colour = measure, fill = measure, group = measure)) +
    geom_hline(yintercept = 1, colour = "grey65", linewidth = 0.3) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.04, colour = NA) +
    geom_line(data = z[z$measure != ref_measure, ], linewidth = 0.5) +
    geom_line(data = z[z$measure == ref_measure, ], linewidth = 0.75) +
    annotation_custom(labels, xmin = x_lim[1], xmax = x_lim[2],
                      ymin = y_lim[1], ymax = y_lim[2]) +
    scale_colour_manual(values = measure_colors) +
    scale_fill_manual(values = measure_colors) +
    scale_x_continuous(breaks = st_3c$x_breaks, expand = c(0, 0)) +
    scale_y_log10(breaks = st_3c$y_breaks, labels = as.character(st_3c$y_breaks)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE, clip = "off") +
    labs(title = title,
         subtitle = txt_3c$subtitle,
         x = txt_3c$x_curve, y = txt_3c$y_curve) +
    theme_3c +
    theme(plot.margin = margin(5, 100, 5, 5))
}

attenuation_plot <- function(att) {
  comparators <- lv_codes(lv_3c, "comparator")
  stopifnot(setequal(att$measure, c(ref_measure, comparators)))
  x_grid <- seq(st_3c$att_x_range[1], st_3c$att_x_range[2], length.out = st_3c$att_grid_n)
  density_of <- function(m) {
    s <- att[att$measure == m, ]
    dt((x_grid - s$estimate) / s$se, s$df) / s$se
  }
  peak <- max(sapply(att$measure, function(m) max(density_of(m))))

  shapes <- list()
  points <- list()
  for (i in seq_along(comparators)) {
    row <- length(comparators) + 1 - i
    for (m in c(ref_measure, comparators[i])) {
      side <- if (m == ref_measure) 1 else -1
      base <- row + side * 0.15
      d <- density_of(m)
      shapes[[length(shapes) + 1]] <- data.frame(
        x = c(x_grid, rev(x_grid)),
        y = c(base + side * 0.31 * d / peak, rep(base, length(x_grid))),
        measure = m, group = paste(i, m))
      s <- att[att$measure == m, ]
      points[[length(points) + 1]] <- data.frame(
        measure = m, estimate = s$estimate, lower = s$lower, upper = s$upper,
        y = row + side * 0.085)
    }
  }
  shapes <- do.call(rbind, shapes)
  points <- do.call(rbind, points)

  first_est <- att$estimate[att$measure == ref_measure]
  n_cmp <- length(comparators)
  inside_labels <- data.frame(
    measure = c(ref_measure, comparators),
    x = c(first_est, rep(st_3c$att_label_x, n_cmp)),
    y = st_3c$att_label_y,
    hjust = c(0.5, rep(1, n_cmp)),
    label = unname(measure_labels[c(ref_measure, comparators)]),
    face = c("bold", rep("plain", n_cmp)))

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey65") +
    geom_polygon(data = shapes, aes(x, y, group = group, fill = measure),
                 colour = NA, alpha = 0.16) +
    geom_segment(data = points, aes(x = lower, xend = upper, y = y, yend = y,
                                    colour = measure), linewidth = 0.45) +
    geom_point(data = points, aes(estimate, y, colour = measure), size = 1.65) +
    geom_text(data = inside_labels,
              aes(x, y, label = label, colour = measure, hjust = hjust, fontface = face),
              size = 3.1, family = "Helvetica", lineheight = 0.95) +
    scale_colour_manual(values = measure_colors) +
    scale_fill_manual(values = measure_colors) +
    scale_x_continuous(limits = range(x_grid), breaks = st_3c$att_x_breaks,
                       expand = c(0, 0)) +
    scale_y_continuous(limits = st_3c$att_y_limits, breaks = NULL, expand = c(0, 0)) +
    labs(title = txt_3c$title_attenuation, x = txt_3c$x_attenuation, y = NULL) +
    theme_3c +
    theme(plot.title.position = "panel",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
}

panel_3c <- function() {
  curves <- read_source("Figure_3c_curves.csv",
                        c("sample", "measure", "year", "hr", "lower", "upper"))
  att <- read_source("Figure_3c_attenuation.csv",
                     c("measure", "estimate", "se", "df", "lower", "upper"))
  samples <- lv_codes(lv_3c, "sample")
  sample_titles <- lv_labels(lv_3c, "sample")
  stopifnot(setequal(curves$sample, samples))
  left <- hr_curve_plot(curves, samples[1], sample_titles[[samples[1]]], st_3c$label_range_1)
  right <- hr_curve_plot(curves, samples[2], sample_titles[[samples[2]]],
                         st_3c$label_range_2) + labs(y = NULL)
  p <- left + attenuation_plot(att) + right + plot_layout(widths = c(1, 1.02, 1))
  save_panel_scaled(p, "Figure_3c", 720, 203, scale = 720 / 810)
}

panel_3d <- function() {
  km <- read_source("Figure_3d_km.csv", c("quartile", "time", "step_value"))
  txt <- read_text("Figure_3d")
  lv <- read_levels("Figure_3d")
  st <- read_settings("Figure_3d")

  quartile_colors <- c("#A45439", "#D5AD9F", "#B0C3D1", "#406783")
  quartile_codes <- lv_codes(lv, "quartile")
  quartile_labels <- unname(lv_labels(lv, "quartile")[as.character(quartile_codes)])
  stopifnot(setequal(as.character(km$quartile), as.character(quartile_codes)))
  km$q <- factor(km$quartile, levels = quartile_codes, labels = quartile_labels)

  theme_3d <- theme_fig_base + theme(
    text = element_text(family = "Helvetica", size = 8),
    axis.text = element_text(size = 7.5),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9, face = "bold"),
    legend.position = "none")
  x_scale <- scale_x_continuous(limits = st$x_limits, breaks = st$x_breaks,
                                expand = c(0, 0))

  key <- data.frame(x = st$key_x, y = st$key_y,
                    q = factor(quartile_labels, levels = quartile_labels))
  step_value_plot <- ggplot(km, aes(time, step_value, colour = q)) +
    geom_step(linewidth = 0.7) +
    geom_segment(data = key, aes(x = x, xend = x + 1.3, y = y, yend = y, colour = q),
                 inherit.aes = FALSE, linewidth = 0.8) +
    geom_text(data = key, aes(x = x + 1.8, y = y, label = q), inherit.aes = FALSE,
              hjust = 0, size = 2.55, family = "Helvetica") +
    x_scale +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_colour_manual(values = quartile_colors, drop = FALSE) +
    labs(title = txt$title, subtitle = txt$subtitle, x = txt$x, y = txt$y) +
    theme_3d +
    theme(plot.subtitle = element_text(size = 8),
          plot.margin = margin(4, 6, 0, 4),
          axis.title.x = element_text(margin = margin(t = 2)))

  steps <- ggplot_build(step_value_plot)$data[[1]]
  stopifnot(nrow(steps) == nrow(km), all(sort(steps$y) == sort(km$step_value)))

  save_panel(step_value_plot, "Figure_3d", 250, 150)
}

panel_3e <- function() {
  e <- read_source("Figure_3e.csv",
                   c("outcome_type", "outcome", "series", "estimate", "lower", "upper"))
  txt <- read_text("Figure_3e")
  lv <- read_levels("Figure_3e")
  st <- read_settings("Figure_3e")
  types <- lv_codes(lv, "outcome_type")
  type_titles <- lv_labels(lv, "outcome_type")
  stopifnot(setequal(e$outcome_type, types), all(e$series %in% series_codes))
  set_a <- e[e$outcome_type == types[1], ]
  set_b <- e[e$outcome_type == types[2], ]
  rows <- rev(series_codes)
  row_labels <- unname(series_labels[rows])
  set_a$row <- factor(set_a$series, levels = rows, labels = row_labels)
  set_b$row <- factor(set_b$series, levels = rows, labels = row_labels)
  outcomes <- lv_codes(lv, "outcome")
  outcome_labels <- lv_labels(lv, "outcome")
  stopifnot(setequal(set_a$outcome, outcomes))
  outcome_a_colors <- setNames(outcome_a_palette[seq_along(outcomes)], outcomes)
  set_a$outcome <- factor(set_a$outcome, levels = outcomes)

  small <- theme_fig_base + theme(
    text = element_text(family = "Helvetica", size = 7.5),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 7, hjust = 0.5, margin = margin(b = 2)),
    axis.text = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(hjust = 1, margin = margin(r = 2.5)),
    axis.title = element_text(size = 7.5),
    legend.position = "none",
    plot.margin = margin(3, 4, 1, 3))
  null_line <- geom_vline(xintercept = 1, linetype = "dashed", colour = "grey65",
                          linewidth = 0.3)
  rows_scale <- scale_y_discrete(drop = FALSE, expand = expansion(add = 0.55))

  outcome_a_plot <- ggplot(set_a, aes(estimate, row, colour = outcome)) +
    null_line +
    geom_linerange(aes(xmin = lower, xmax = upper), orientation = "y",
                   position = position_dodge(0.45), linewidth = 0.5) +
    geom_point(position = position_dodge(0.45), size = 1.35) +
    scale_colour_manual(values = outcome_a_colors) +
    scale_x_log10(breaks = st$outcome_a_breaks, limits = st$outcome_a_limits,
                  labels = as.character(st$outcome_a_breaks)) +
    rows_scale +
    labs(title = type_titles[[types[1]]], subtitle = txt$subtitle_outcome_a,
         x = txt$x_outcome_a, y = NULL) +
    small

  outcome_b_plot <- ggplot(set_b, aes(estimate, row)) +
    null_line +
    geom_linerange(aes(xmin = lower, xmax = upper), orientation = "y",
                   colour = "#536773", linewidth = 0.5) +
    geom_point(colour = "#536773", size = 1.45) +
    scale_x_log10(breaks = st$outcome_b_breaks, limits = st$outcome_b_limits,
                  labels = as.character(st$outcome_b_breaks)) +
    rows_scale +
    labs(title = type_titles[[types[2]]], subtitle = txt$subtitle_outcome_b,
         x = txt$x_outcome_b, y = NULL) +
    small +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  g1 <- ggplotGrob(outcome_a_plot)
  g2 <- ggplotGrob(outcome_b_plot)
  heights <- unit.pmax(g1$heights, g2$heights)
  g1$heights <- heights
  g2$heights <- heights

  key_item <- function(x, colour, label) {
    y <- unit(11, "pt")
    grid.segments(x0 = unit(x, "pt"), x1 = unit(x + 9, "pt"), y0 = y, y1 = y,
                  gp = gpar(col = colour, lwd = 1))
    grid.points(x = unit(x + 4.5, "pt"), y = y, pch = 16, size = unit(1.5, "mm"),
                gp = gpar(col = colour))
    grid.text(label, x = unit(x + 12, "pt"), y = y, just = "left",
              gp = gpar(fontsize = 6.8, fontfamily = "Helvetica"))
  }
  draw <- function() {
    pushViewport(viewport(layout = grid.layout(2, 2, widths = unit(c(150, 100), "pt"),
                                               heights = unit(c(137, 18), "pt"))))
    grid.draw(editGrob(g1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)))
    grid.draw(editGrob(g2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2)))
    pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
    grid.text(txt$key_title, x = unit(3, "pt"), y = unit(11, "pt"), just = "left",
              gp = gpar(fontsize = 6.8, fontfamily = "Helvetica"))
    for (i in seq_along(outcomes)) {
      key_item(st$key_x[i], outcome_a_colors[[outcomes[i]]], outcome_labels[[outcomes[i]]])
    }
    popViewport(2)
  }
  save_panel(draw, "Figure_3e", 250, 155)
}

panel_3f <- function() {
  d <- read_source("Figure_3f.csv",
                   c("sample", "measure", "c_index", "c_lower", "c_upper", "gain",
                     "gain_lower", "gain_upper", "diff", "diff_lower",
                     "diff_upper"))
  lv <- read_levels("Figure_3f")
  measures <- nl(lv_labels(lv, "measure"))
  samples <- nl(lv_labels(lv, "sample"))
  stopifnot(setequal(d$measure, names(measures)), setequal(d$sample, names(samples)))
  d$measure_f <- factor(d$measure, levels = rev(names(measures)), labels = rev(measures))
  d$sample_f <- factor(d$sample, levels = names(samples), labels = samples)
  d$is_first <- d$measure == names(measures)[1]

  metrics <- unname(nl(lv_labels(lv, "metric")))
  long <- rbind(
    transform(d, metric = metrics[1], value = c_index, lo = c_lower, hi = c_upper),
    transform(d, metric = metrics[2], value = gain, lo = gain_lower, hi = gain_upper),
    transform(d, metric = metrics[3], value = diff, lo = diff_lower,
              hi = diff_upper))
  long$metric <- factor(long$metric, levels = metrics)
  zero <- data.frame(metric = factor(metrics[2:3], levels = metrics), value = 0)

  p <- ggplot(long, aes(value, measure_f)) +
    geom_vline(data = zero, aes(xintercept = value), linetype = "dashed",
               colour = "grey65", linewidth = 0.35) +
    geom_linerange(aes(xmin = lo, xmax = hi), orientation = "y", linewidth = 0.5,
                   colour = "#69757C", na.rm = TRUE) +
    geom_point(aes(colour = is_first), size = 1.5, na.rm = TRUE) +
    scale_colour_manual(values = c("TRUE" = "#252525", "FALSE" = "#82959F"), guide = "none") +
    facet_grid(sample_f ~ metric, scales = "free", space = "free_y") +
    scale_x_continuous(breaks = scales::breaks_pretty(2), expand = expansion(mult = 0.12)) +
    labs(x = NULL, y = NULL) +
    theme_fig_base +
    theme(text = element_text(family = "Helvetica", size = 8),
          strip.text.x = element_text(size = 7.7, face = "bold"),
          strip.text.y = element_text(size = 7.2, angle = 0, face = "bold"),
          axis.text = element_text(size = 7.3),
          panel.spacing = unit(5, "pt"),
          plot.margin = margin(3, 3, 4, 3),
          legend.position = "none")

  save_panel(p, "Figure_3f", 425, 330)
}

panel_3a()
panel_3b()
panel_3c()
panel_3d()
panel_3e()
panel_3f()
