# fig4.R - Figure 4
# Input (data/): Figure_4a_bands.csv, Figure_4a_parts.csv, Figure_4a.csv,
#  Figure_4a_text.csv, Figure_4a_levels.csv, Figure_4a_settings.csv,
#  Figure_4b_agreement.csv, Figure_4b.csv, Figure_4b_text.csv, Figure_4b_levels.csv,
#  Figure_4b_settings.csv, Figure_4c.csv, Figure_4c_text.csv, Figure_4c_settings.csv,
#  Figure_4d.csv, Figure_4d_text.csv, Figure_4d_levels.csv, Figure_4d_settings.csv,
#  Figure_4e_curves.csv, Figure_4e_features.csv, Figure_4e_text.csv,
#  Figure_4e_settings.csv, Figure_4f.csv, Figure_4f_text.csv, Figure_4f_levels.csv,
#  Figure_4f_settings.csv, Labels_series_levels.csv, Labels_groups_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript fig4.R

source("00_helpers.R")
suppressPackageStartupMessages({
  library(patchwork)
  library(ggh4x)
})

group_levels <- lv_codes(group_lv, "group")
grey_ns <- "#C9CDCF"
part_levels <- unname(series_labels[part_codes])
part_short <- setNames(unname(series_short[part_codes]), part_levels)

theme_small <- theme_fig_base + theme(
  axis.text = element_text(size = 7.5, colour = "black"),
  axis.title = element_text(size = 8.5, colour = "black"),
  strip.text = element_text(size = 8, face = "bold", colour = "black"),
  legend.text = element_text(colour = "black"),
  plot.title = element_text(size = 11, face = "bold", colour = "black",
                            margin = margin(b = 4)),
  plot.title.position = "plot",
  plot.margin = margin(2, 4, 2, 4)
)

panel_4a <- function() {
  a <- read_source("Figure_4a.csv",
                   c("feature_id", "group", "position", "p", "signed_log10_p",
                     "fdr_supported", "joint_test"))
  strips <- read_source("Figure_4a_parts.csv",
                        c("feature_id", "part", "group", "position", "result"))
  bands <- read_source("Figure_4a_bands.csv",
                       c("group", "band", "label", "xmin", "xmax", "shaded",
                         "label_x", "label_y", "horizontal", "show_label"))
  txt <- read_text("Figure_4a")
  lv <- read_levels("Figure_4a")
  st <- read_settings("Figure_4a")
  stopifnot(all(a$group %in% group_levels), all(strips$group %in% group_levels),
            all(bands$group %in% group_levels), setequal(strips$part, part_levels))

  facet_titles <- lv_labels(lv, "facet_title")
  vertical_group <- lv_codes(lv, "vertical_title")
  vertical_title <- lv_labels(lv, "vertical_title")[[vertical_group]]
  as_facet <- function(g) factor(g, levels = group_levels)
  a$facet <- as_facet(a$group)
  strips$facet <- as_facet(strips$group)
  bands$facet <- as_facet(bands$group)

  counts <- table(a$facet)
  span <- data.frame(facet = as_facet(rep(names(counts), 2)),
                     x = c(rep(0.5, length(counts)), as.numeric(counts) + 0.5))
  fdr_line <- -log10(max(a$p[a$fdr_supported]))

  sig <- a[a$fdr_supported, ]
  ns <- a[!a$fdr_supported, ]
  y_top <- max(a$signed_log10_p) + 0.04 * diff(range(a$signed_log10_p))
  label_colour <- "#2F3337"
  label_pt <- 5.4
  vertical_labels <- bands[bands$show_label & !bands$horizontal, ]
  horizontal_labels <- bands[bands$show_label & bands$horizontal, ]
  repr_title <- data.frame(facet = as_facet(vertical_group),
                           x = mean(c(0.5, counts[[vertical_group]] + 0.5)), y = y_top,
                           label = vertical_title)

  screen <- ggplot(a, aes(position, signed_log10_p)) +
    geom_blank(data = span, aes(x = x, y = 0)) +
    geom_rect(data = bands[bands$shaded, ], aes(xmin = xmin, xmax = xmax),
              ymin = -Inf, ymax = Inf, inherit.aes = FALSE, fill = "#ECEEF1", colour = NA) +
    geom_hline(yintercept = 0, colour = "#777777", linewidth = 0.3) +
    geom_hline(yintercept = c(-fdr_line, fdr_line), colour = "#9AA3A8",
               linewidth = 0.25, linetype = "22") +
    geom_point(data = ns[!ns$joint_test, ], colour = grey_ns, size = 0.45, alpha = 0.6) +
    geom_point(data = ns[ns$joint_test, ], colour = grey_ns, size = 0.7, shape = 2,
               stroke = 0.35, alpha = 0.6) +
    geom_segment(data = sig[!sig$joint_test, ],
                 aes(xend = position, yend = 0, colour = group),
                 linewidth = 0.25, alpha = 0.4) +
    geom_point(data = sig[!sig$joint_test, ], aes(colour = group), size = 0.8, alpha = 0.75) +
    geom_point(data = sig[sig$joint_test, ], aes(colour = group), size = 1.05,
               shape = 17, alpha = 0.8) +
    geom_text(data = vertical_labels, aes(x = label_x, y = label_y, label = label),
              inherit.aes = FALSE, angle = 90, hjust = 1, vjust = 0.5,
              size = label_pt / .pt, lineheight = 0.85, colour = label_colour) +
    geom_text(data = horizontal_labels, aes(x = label_x, y = label_y, label = label),
              inherit.aes = FALSE, hjust = 0.5, vjust = 1,
              size = label_pt / .pt, colour = label_colour) +
    geom_text(data = repr_title, aes(x = x, y = y, label = label), inherit.aes = FALSE,
              angle = 90, hjust = -0.12, vjust = 0.5, size = 7.5 / .pt, fontface = "bold") +
    facet_grid(. ~ facet, scales = "free_x", space = "free_x",
               labeller = labeller(facet = facet_titles)) +
    scale_colour_manual(values = group_colors, guide = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(breaks = st$y_breaks, expand = expansion(mult = c(0.04, 0.04))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = txt$y) +
    theme_small +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text = element_text(size = 8, face = "bold", colour = "black"),
          panel.spacing.x = unit(0.05, "in"),
          axis.title.y = element_text(size = 8, margin = margin(r = 2)),
          axis.text.y = element_text(margin = margin(r = 2)),
          plot.margin = margin(2, 2, 0, 2))

  results <- lv_codes(lv, "result")
  result_colors <- setNames(c(col_rust, col_blue, "#5A5F63", "#EDEEEF")[seq_along(results)],
                            results)
  stopifnot(all(strips$result %in% results))
  strips$row <- factor(strips$part, levels = rev(part_levels))
  strips$result <- factor(strips$result, levels = results, labels = lv_labels(lv, "result")[results])
  names(result_colors) <- levels(strips$result)
  band_edges <- do.call(rbind, lapply(split(bands, bands$group), function(z) {
    if (nrow(z) > 1) data.frame(facet = z$facet[-1], x = z$xmin[-1])
  }))
  part_strip <- ggplot(strips, aes(position, row, fill = result)) +
    geom_blank(data = transform(span, row = factor(levels(strips$row)[1],
                                                   levels = levels(strips$row))),
               aes(x = x, y = row), inherit.aes = FALSE) +
    geom_tile(width = 1, height = 0.86) +
    geom_vline(data = band_edges, aes(xintercept = x), colour = "white", linewidth = 0.45) +
    facet_grid(. ~ facet, scales = "free_x", space = "free_x") +
    scale_fill_manual(values = result_colors, name = NULL) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(labels = part_short) +
    labs(x = NULL, y = NULL) +
    theme_small +
    theme(strip.text = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = 6.2, colour = "black", margin = margin(r = 2)),
          panel.spacing.x = unit(0.05, "in"),
          panel.border = element_rect(colour = "#555555", linewidth = 0.35, fill = NA),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.1, "in"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(2, "pt"),
          plot.margin = margin(0, 2, 2, 2))

  p <- screen / part_strip + plot_layout(heights = c(1.65, 0.75))
  save_panel(p, "Figure_4a", 612, 172.8)
}

panel_4b <- function() {
  b <- read_source("Figure_4b.csv",
                   c("feature_id", "group", "subgroup", "position", "batch", "ratio",
                     "estimate_only"))
  agree <- read_source("Figure_4b_agreement.csv", c("batch", "share"))
  txt <- read_text("Figure_4b")
  lv <- read_levels("Figure_4b")
  st <- read_settings("Figure_4b")
  batch_codes <- lv_codes(lv, "batch")
  pooled_code <- batch_codes[1]
  stopifnot(setequal(b$batch, batch_codes), all(agree$batch %in% batch_codes),
            all(b$group %in% group_levels))

  b$batch <- factor(b$batch, levels = rev(batch_codes))
  batch_label <- function(x) unname(lv_labels(lv, "batch")[x])
  b$group <- factor(b$group, levels = group_levels)
  facet_titles <- lv_labels(lv, "facet_title")
  divider_group <- lv_codes(lv, "divider_group")
  pooled <- b[b$batch == pooled_code, ]
  n_subgroup <- sum(pooled$group == divider_group &
                      pooled$subgroup == lv_codes(lv, "divider_subgroup"), na.rm = TRUE)
  divider <- data.frame(group = factor(divider_group, levels = group_levels),
                        x = n_subgroup + 0.5)

  base <- theme(axis.text = element_text(size = 7, colour = "black"),
                axis.title = element_text(size = 8),
                strip.text = element_text(size = 7, face = "bold"),
                legend.text = element_text(size = 7),
                legend.title = element_text(size = 7),
                legend.key.height = unit(3, "mm"),
                plot.margin = margin(3, 2, 2, 2))

  heat <- ggplot(b, aes(position, batch, fill = ratio)) +
    geom_tile() +
    geom_point(data = b[b$estimate_only, ], size = 0.25) +
    geom_vline(data = divider, aes(xintercept = x), linetype = 3, colour = "grey50",
               linewidth = 0.3) +
    facet_grid(. ~ group, scales = "free_x", space = "free_x",
               labeller = as_labeller(facet_titles)) +
    scale_fill_gradient2(low = col_rust, mid = "white", high = col_blue,
                         limits = st$ratio_limits, name = txt$legend_title) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(labels = batch_label, expand = c(0, 0)) +
    labs(x = txt$x_heat, y = NULL) +
    theme_fig_base + base +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.key.height = unit(4, "pt"),
          legend.key.width = unit(22, "pt"),
          axis.title.x = element_text(size = 7.2, margin = margin(t = 3)),
          plot.margin = margin(2, 2, 1, 2),
          panel.spacing.x = unit(4, "pt"))

  agree$y <- match(agree$batch, levels(b$batch))
  bars <- ggplot(agree, aes(100 * share, y)) +
    geom_col(fill = "#83949D", width = 0.65, orientation = "y") +
    geom_text(aes(label = sprintf("%.0f%%", 100 * share)), hjust = -0.1, size = 1.8) +
    scale_x_continuous(limits = st$bar_limits, breaks = st$bar_breaks) +
    scale_y_continuous(limits = c(0.5, nlevels(b$batch) + 0.5), expand = c(0, 0)) +
    labs(x = txt$x_bars, y = NULL) +
    theme_fig_base + base +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 7.2, margin = margin(t = 3)),
          plot.margin = margin(2, 3, 1, 2))

  p <- (heat + bars + plot_layout(widths = c(8.7, 1.3), guides = "collect")) &
    theme(legend.position = "bottom")
  save_panel(p, "Figure_4b", 540, 140)
}

panel_4c <- function() {
  cc <- read_source("Figure_4c.csv",
                    c("period", "feature_id", "group", "set_1", "set_2"))
  txt <- read_text("Figure_4c")
  st <- read_settings("Figure_4c")
  stopifnot(all(cc$group %in% group_levels))
  cc$group <- factor(cc$group, levels = group_levels)

  stats <- do.call(rbind, lapply(split(cc, cc$period), function(v) data.frame(
    period = v$period[1], n = nrow(v),
    agree = mean(sign(v$set_1) == sign(v$set_2)),
    r = cor(v$set_1, v$set_2))))
  stats$text <- sprintf(txt$stats_template, stats$n, 100 * stats$agree, stats$r)

  pl <- scales::pseudo_log_trans(sigma = 0.05, base = 10)
  data_range <- range(c(cc$set_1, cc$set_2))
  lim <- c(min(st$lim_floor, data_range[1] * 1.1), max(st$lim_ceiling, data_range[2] * 1.1))
  text_y <- pl$inverse(pl$transform(lim[2]) - 0.04 * diff(pl$transform(lim)))
  axis_breaks <- st$axis_breaks
  axis_labels <- as.character(axis_breaks)

  p <- ggplot(cc, aes(set_1, set_2, colour = group)) +
    geom_hline(yintercept = 0, colour = "grey82", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey82", linewidth = 0.3) +
    geom_abline(intercept = 0, slope = 1, linetype = "22", colour = "grey55", linewidth = 0.3) +
    geom_point(size = 0.75, alpha = 0.7) +
    geom_text(data = stats, aes(x = lim[1] * 0.95, y = text_y, label = text),
              inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.2, lineheight = 0.9,
              colour = "black") +
    facet_wrap(~period, nrow = 1) +
    scale_colour_manual(values = group_colors, name = NULL, drop = FALSE) +
    scale_x_continuous(trans = pl, breaks = axis_breaks, labels = axis_labels, limits = lim) +
    scale_y_continuous(trans = pl, breaks = axis_breaks, labels = axis_labels, limits = lim) +
    coord_equal() +
    guides(colour = guide_legend(ncol = 1, override.aes = list(size = 1.5, alpha = 1))) +
    labs(x = txt$x, y = txt$y) +
    theme_small +
    theme(legend.position = "right",
          legend.text = element_text(size = 5.9),
          legend.key.width = unit(0.07, "in"),
          legend.key.height = unit(0.12, "in"),
          legend.spacing.y = unit(0, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(1, "pt"),
          panel.spacing.x = unit(0.12, "in"),
          axis.text = element_text(size = 6.3),
          axis.title = element_text(size = 7.3),
          strip.text = element_text(size = 7.5),
          plot.margin = margin(16, 4, 2, 4))

  stopifnot(nrow(ggplot_build(p)$data[[4]]) == nrow(cc),
            all(cc$set_1 >= lim[1] & cc$set_1 <= lim[2]))
  save_panel(p, "Figure_4c", 396, 169.2)
}

panel_4d <- function() {
  mx <- read_source("Figure_4d.csv",
                    c("bundle", "components", "estimate", "lower", "upper", "q"))
  txt <- read_text("Figure_4d")
  lv <- read_levels("Figure_4d")
  st <- read_settings("Figure_4d")
  results <- lv_codes(lv, "result")
  mx$label <- sprintf("%s (%d)", mx$bundle, mx$components)
  mx <- mx[order(mx$estimate), ]
  mx$label <- factor(mx$label, levels = mx$label)
  mx$result <- ifelse(mx$q < st$fdr_threshold,
                      ifelse(mx$estimate > 0, results[2], results[1]), results[3])

  p <- ggplot(mx, aes(estimate, label, colour = result)) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey65", linewidth = 0.3) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0,
                  linewidth = 0.4) +
    geom_point(size = 1.3) +
    scale_colour_manual(values = setNames(c(col_rust, col_blue, "#8C959B"), results),
                        guide = "none") +
    scale_x_continuous(limits = st$x_limits, breaks = st$x_breaks,
                       labels = sprintf("%.2f", st$x_breaks)) +
    labs(x = txt$x, y = NULL) +
    theme_small +
    theme(axis.text.y = element_text(size = 6.9),
          axis.text.x = element_text(size = 6.9),
          axis.title.x = element_text(size = 6.9),
          plot.margin = margin(12, 6, 2, 2))

  save_panel(p, "Figure_4d", 222, 146)
}

panel_4e <- function() {
  cu <- read_source("Figure_4e_curves.csv",
                    c("feature_id", "feature", "unit", "group", "value", "estimate",
                      "lower", "upper"))
  ex <- read_source("Figure_4e_features.csv",
                    c("feature_id", "feature", "unit", "group", "log_scale", "median",
                      "p10", "p90", "nonlinear_q"))
  txt <- read_text("Figure_4e")
  st <- read_settings("Figure_4e")
  stopifnot(setequal(cu$feature_id, ex$feature_id), all(ex$group %in% group_levels))

  titles <- paste0(ex$feature, "\n", ex$unit)
  ex$panel <- factor(titles, levels = titles)
  cu$panel <- factor(titles[match(cu$feature_id, ex$feature_id)], levels = titles)
  ex$label_x <- tapply(cu$value, cu$feature_id, min)[ex$feature_id]
  ex$q_label <- paste0(txt$q_prefix,
                       ifelse(ex$nonlinear_q < 0.001,
                              formatC(ex$nonlinear_q, format = "e", digits = 1),
                              sprintf("%.3f", ex$nonlinear_q)))
  quantiles <- data.frame(panel = rep(ex$panel, 3), value = c(ex$p10, ex$median, ex$p90))

  log_breaks <- function(lim) {
    b <- sort(scales::breaks_log(n = 4)(lim))
    b <- b[b >= lim[1] & b <= lim[2]]
    keep <- b[1]
    for (v in b[-1]) if (log10(v) - log10(tail(keep, 1)) >= 0.3) keep <- c(keep, v)
    if (length(keep) < 2) {
      keep <- signif(10^seq(log10(lim[1]) + 0.1 * diff(log10(lim)),
                            log10(lim[2]) - 0.1 * diff(log10(lim)), length.out = 2), 2)
    }
    keep
  }
  linear_breaks <- function(id) st[[paste0("linear_breaks_", id)]]
  x_scales <- lapply(seq_len(nrow(ex)), function(i) {
    if (ex$log_scale[i]) {
      scale_x_log10(breaks = log_breaks, labels = scales::label_number(drop0trailing = TRUE))
    } else {
      scale_x_continuous(breaks = linear_breaks(ex$feature_id[i]))
    }
  })

  p <- ggplot(cu, aes(value, estimate)) +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.25) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2) +
    geom_line(aes(colour = group), linewidth = 0.6) +
    geom_point(data = ex, aes(x = median, y = 0), shape = 3, size = 1.2, colour = "black") +
    geom_rug(data = quantiles, aes(x = value), inherit.aes = FALSE, sides = "b",
             length = unit(0.055, "in"), linewidth = 0.4) +
    geom_text(data = ex, aes(x = label_x, y = Inf, label = q_label), inherit.aes = FALSE,
              hjust = -0.03, vjust = 1.3, size = 2.15, colour = "black") +
    facet_wrap(~panel, ncol = st$facet_ncol, scales = "free") +
    facetted_pos_scales(x = x_scales) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3),
                       expand = expansion(mult = c(0.07, 0.32))) +
    scale_colour_manual(values = group_colors, guide = "none") +
    scale_fill_manual(values = group_colors, guide = "none") +
    labs(x = txt$x, y = txt$y) +
    theme_fig_base +
    theme(text = element_text(family = "Helvetica"),
          axis.text = element_text(size = 7.2),
          axis.title = element_text(size = 9),
          strip.text = element_text(size = 8.1, face = "bold"),
          panel.spacing = unit(0.1, "in"),
          plot.margin = margin(2, 4, 2, 4))

  built <- ggplot_build(p)$data[[3]]
  stopifnot(nrow(built) == nrow(cu))
  save_panel(p, "Figure_4e", 648, 219.6)
}

panel_4f <- function() {
  ff <- read_source("Figure_4f.csv",
                    c("feature_id", "feature", "series", "group", "estimate", "lower",
                      "upper", "q"))
  txt <- read_text("Figure_4f")
  lv <- read_levels("Figure_4f")
  st <- read_settings("Figure_4f")
  first_code <- series_labels[[first_code]]
  stopifnot(setequal(ff$series, c(first_code, part_levels)))
  first <- ff[ff$series == first_code, ]
  parts <- ff[ff$series != first_code, ]
  rows <- first$feature
  first$row <- factor(first$feature, levels = rev(rows))
  directions <- lv_codes(lv, "direction")
  first$direction <- ifelse(first$estimate > 0, directions[1], directions[2])

  x_lim <- st$x_limits
  off_scale <- first$lower < x_lim[1]
  off_scale_row <- first[off_scale, ]
  off_scale_row$text <- sprintf(txt$off_scale_template, off_scale_row$estimate,
                                off_scale_row$lower, off_scale_row$upper)

  forest <- function(d, strip, x_title, row_labels, text = FALSE) {
    g <- ggplot(d, aes(estimate, row, colour = direction)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey65", linewidth = 0.3) +
      geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0,
                    linewidth = 0.45) +
      geom_point(size = 1.3) +
      facet_wrap(~"panel", labeller = as_labeller(function(x) strip)) +
      scale_y_discrete(limits = rev(row_labels),
                       expand = if (text) expansion(add = c(0.9, 0.6)) else waiver()) +
      scale_x_continuous(limits = x_lim, breaks = st$x_breaks,
                         labels = sprintf("%.2f", st$x_breaks)) +
      scale_colour_manual(values = setNames(c(col_blue, col_rust), directions),
                          guide = "none") +
      labs(x = x_title, y = NULL) +
      theme_small +
      theme(strip.text = element_text(size = 6.8),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 7.1),
            axis.text.x = element_text(size = 6.6),
            axis.title.x = element_text(size = 7))
    if (text) {
      g <- g + geom_text(data = off_scale_row, aes(x = st$off_scale_text_x, label = text),
                         hjust = 0, size = 2.0,
                         colour = "black", lineheight = 0.95)
    }
    g
  }
  in_left <- seq_len(st$left_rows)
  in_right <- setdiff(seq_along(rows), in_left)
  left <- forest(first[in_left, ], txt$strip_left, txt$x_left, rows[in_left])
  right <- forest(first[in_right, ][!off_scale[in_right], ],
                  txt$strip_right, txt$x_right, rows[in_right], text = TRUE) +
    theme(axis.text.y = element_text(size = 6.9))

  parts$row <- factor(parts$feature, levels = rev(rows))
  parts$part <- factor(parts$series, levels = part_levels)
  lim <- max(abs(parts$estimate))
  heat <- ggplot(parts, aes(part, row)) +
    geom_tile(aes(fill = estimate), colour = "white", linewidth = 0.25) +
    geom_tile(data = parts[parts$q < st$fdr_threshold, ], fill = NA, colour = "#303030",
              linewidth = 0.3, width = 0.84, height = 0.82) +
    scale_fill_gradient2(low = col_rust, mid = "white", high = col_blue,
                         limits = c(-lim, lim), trans = scales::pseudo_log_trans(sigma = 0.05),
                         name = txt$legend_title, breaks = st$heat_breaks,
                         labels = sprintf("%.1f", st$heat_breaks)) +
    scale_x_discrete(labels = part_short, position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_wrap(~"panel", labeller = as_labeller(function(x) txt$strip_heat)) +
    labs(x = NULL, y = NULL, caption = txt$caption) +
    theme_small +
    theme(axis.text.y = element_text(size = 5.8),
          axis.text.x = element_text(size = 6.5),
          axis.ticks = element_blank(),
          strip.text = element_text(size = 7.2),
          legend.position = "bottom",
          legend.justification = c(0, 1),
          legend.location = "plot",
          legend.title = element_text(size = 6.2, face = "bold"),
          legend.text = element_text(size = 5.8),
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(0, 2, 2, 2),
          strip.placement = "outside",
          legend.box.spacing = unit(0, "pt"),
          plot.caption = element_text(size = 6.2, hjust = 1, colour = "black",
                                      margin = margin(t = -9))) +
    guides(fill = guide_colorbar(barwidth = unit(0.85, "in"), barheight = unit(0.05, "in"),
                                 title.position = "left"))

  p <- wrap_elements(full = left) + wrap_elements(full = right) + wrap_elements(full = heat) +
    plot_layout(widths = c(1, 1.05, 1.1))
  save_panel(p, "Figure_4f", 540, 132)
}

panel_4a()
panel_4b()
panel_4c()
panel_4d()
panel_4e()
panel_4f()
