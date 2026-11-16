# fig5.R - Figure 5
# Input (data/): Figure_5a.csv, Figure_5b.csv, Figure_5c.csv, Figure_5d.csv,
#  Figure_5a_text.csv, Figure_5a_levels.csv, Figure_5a_settings.csv,
#  Figure_5b_text.csv, Figure_5b_levels.csv, Figure_5b_settings.csv,
#  Figure_5c_text.csv, Figure_5c_levels.csv, Figure_5c_settings.csv,
#  Figure_5d_text.csv, Figure_5d_levels.csv, Figure_5d_settings.csv,
#  Labels_series_levels.csv, Labels_groups_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript fig5.R

source("00_helpers.R")
suppressPackageStartupMessages(library(ggrepel))

pt <- function(x) x / .pt
txt_col <- "#1A1A1A"

series_order <- unname(series_labels)

theme_fig5 <- theme_fig_base + theme(
  text        = element_text(family = "Arial"),
  axis.text   = element_text(size = 8, colour = "black"),
  axis.title  = element_text(size = 8.5, colour = "black"),
  legend.text = element_text(size = 8, colour = "black"),
  plot.margin = margin(4, 6, 4, 4)
)

# Panel a

swarm_offsets <- function(x_in, d_in, cap_in, priority, seed) {
  set.seed(seed)
  n <- length(x_in)
  y <- rep(NA_real_, n)
  ord <- order(-priority, x_in)
  step <- min(d_in) / 3
  ks <- seq_len(floor(cap_in / step))
  candidates <- c(0, as.vector(rbind(ks * step, -ks * step)))
  placed <- integer(0)
  for (i in ord) {
    near <- placed[abs(x_in[placed] - x_in[i]) < (d_in[placed] + d_in[i]) / 2]
    if (!length(near)) {
      y[i] <- 0
      placed <- c(placed, i)
      next
    }
    min_d2 <- ((d_in[near] + d_in[i]) / 2)^2
    dx2 <- (x_in[near] - x_in[i])^2
    hit <- FALSE
    for (cc in candidates) {
      if (all(dx2 + (y[near] - cc)^2 >= min_d2 * 0.98)) {
        y[i] <- cc
        hit <- TRUE
        break
      }
    }
    if (!hit) y[i] <- stats::runif(1, -cap_in, cap_in)
    placed <- c(placed, i)
  }
  y
}

panel_5a <- function() {
  a <- read_source("Figure_5a.csv", cols = c("series", "feature", "feature_group", "estimate",
                                             "fdr_supported", "example"))
  txt <- read_text("Figure_5a")
  lv <- read_levels("Figure_5a")
  st <- read_settings("Figure_5a")
  stopifnot(setequal(a$series, series_order))
  a$fdr_supported <- as.logical(a$fdr_supported)
  a$example[is.na(a$example)] <- ""

  tr <- scales::pseudo_log_trans(sigma = st$pseudo_log_sigma, base = 10)
  a$x_t <- tr$transform(a$estimate)
  x_breaks <- st$x_breaks
  x_lim <- tr$transform(st$x_limits)
  stopifnot(all(a$x_t > x_lim[1] & a$x_t < x_lim[2]))

  row_y <- setNames(st$row_y, series_order)
  y_lim <- st$y_limits
  in_per_x <- 6.0 / diff(x_lim)
  in_per_y <- 2.5 / diff(y_lim)
  size_grey <- 0.6
  size_fdr <- 1.25
  cap_in <- 0.36 * in_per_y

  a$y <- NA_real_
  for (s in series_order) {
    k <- which(a$series == s)
    d_in <- ifelse(a$fdr_supported[k], size_fdr + 0.25, size_grey) / 25.4
    off <- swarm_offsets(a$x_t[k] * in_per_x, d_in, cap_in,
                         priority = as.integer(a$fdr_supported[k]),
                         seed = 101 + match(s, series_order))
    a$y[k] <- row_y[s] + off / in_per_y
  }

  n_inf <- as.integer(table(factor(a$series, levels = series_order)))
  n_fdr <- as.integer(tapply(a$fdr_supported, factor(a$series, levels = series_order), sum))
  count_lab <- sprintf(txt$count_template, n_fdr, n_inf)

  a$fill_key <- factor(ifelse(a$fdr_supported, a$feature_group, txt$not_supported),
                       levels = c(names(group_colors), txt$not_supported))
  grey_pts <- a[!a$fdr_supported, ]
  fdr_pts <- a[a$fdr_supported, ]
  ex_pts <- a[a$example != "", ]

  lab <- ex_pts[ex_pts$series == series_labels[[first_code]], ]
  lab <- lab[order(lab$x_t), ]
  lab$side <- rep(c("above", "below"), length.out = nrow(lab))
  lab$side[lab$example %in% lv_codes(lv, "label_below")] <- "below"
  lab$side[lab$example %in% lv_codes(lv, "label_above")] <- "above"

  repel <- function(d, ylim) {
    geom_text_repel(data = d, aes(x_t, y, label = example), size = 2.75, colour = txt_col,
                    family = "Arial", ylim = ylim, xlim = c(x_lim[1] + 0.05, x_lim[2] - 0.05),
                    direction = "x", segment.size = 0.25, segment.colour = "grey45",
                    min.segment.length = 0, box.padding = 0.35, point.padding = 0.15,
                    force = 6, max.iter = 20000, max.time = 2, seed = 7)
  }

  p <- ggplot() +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3, linetype = "dashed") +
    geom_hline(yintercept = unname(row_y), colour = "grey88", linewidth = 0.25) +
    geom_point(data = grey_pts, aes(x_t, y, fill = fill_key), shape = 21, colour = "transparent",
               stroke = 0, size = size_grey, alpha = 0.75) +
    geom_point(data = fdr_pts, aes(x_t, y, fill = fill_key), shape = 21, colour = "white",
               stroke = 0.18, size = size_fdr) +
    geom_point(data = ex_pts, aes(x_t, y, colour = txt$example), shape = 21, fill = NA,
               stroke = 0.45, size = 2.05) +
    repel(lab[lab$side == "above", ], st$label_ylim_above) +
    repel(lab[lab$side == "below", ], st$label_ylim_below) +
    scale_fill_manual(values = c(group_colors, setNames("#A9B0B6", txt$not_supported)), drop = FALSE,
                      name = NULL) +
    scale_colour_manual(values = setNames("black", txt$example), name = NULL) +
    scale_x_continuous(breaks = tr$transform(x_breaks), labels = as.character(x_breaks),
                       limits = x_lim, expand = c(0, 0)) +
    scale_y_continuous(breaks = unname(row_y), labels = series_order, limits = y_lim, expand = c(0, 0),
                       sec.axis = dup_axis(labels = count_lab, name = NULL)) +
    labs(x = txt$x_title, y = NULL) +
    guides(fill = guide_legend(order = 1, nrow = 1,
                               override.aes = list(size = 2.3, shape = 21, colour = "white",
                                                   stroke = 0.2, alpha = 1)),
           colour = guide_legend(order = 2, override.aes = list(size = 2.3, stroke = 0.5))) +
    theme_fig5 +
    theme(axis.ticks.y = element_blank(),
          axis.text.y.left = element_text(size = 8.5, colour = "black", margin = margin(r = 3)),
          axis.text.y.right = element_text(size = 8, colour = "black", hjust = 0, lineheight = 0.9,
                                           margin = margin(l = 3)),
          legend.position = "bottom", legend.box = "horizontal",
          legend.key.size = unit(0.12, "in"), legend.spacing.x = unit(0.03, "in"),
          legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(2, "pt"),
          legend.box.margin = margin(0, 0, 0, 0), plot.margin = margin(4, 6, 10, 4))

  b <- ggplot_build(p)
  stopifnot(nrow(b$data[[3]]) + nrow(b$data[[4]]) == nrow(a),
            nrow(b$data[[4]]) == sum(a$fdr_supported),
            isTRUE(all.equal(sort(b$data[[4]]$x), sort(fdr_pts$x_t))))
  p
}

# Panel b
panel_5b <- function() {
  b <- read_source("Figure_5b.csv", cols = c("layer", "from", "to", "estimate", "lower", "upper",
                                             "fdr_supported", "direction"))
  b$fdr_supported <- as.logical(b$fdr_supported)
  txt <- read_text("Figure_5b")
  lv <- read_levels("Figure_5b")
  st <- read_settings("Figure_5b")

  layers <- lv_codes(lv, "layer")
  stopifnot(setequal(b$layer, layers))
  features <- lv_codes(lv, "feature")
  parts <- unname(series_labels[part_codes])
  endpoints <- data.frame(label = lv_codes(lv, "endpoint"), pos = st$endpoint_position)
  stopifnot(nrow(endpoints) == length(st$endpoint_position))
  directions <- lv_codes(lv, "direction")
  direction_labels <- lv_labels(lv, "direction")
  stopifnot(setequal(b$direction, directions))

  y_top <- 44.35
  unit_y <- 66.6
  x_feat <- 277.3
  x_part <- 338.65
  x_endpoint <- 435.55
  exp_nodes <- data.frame(label = features, x = x_feat, y = y_top + (seq_along(features) - 1) * 22.2)
  part_nodes <- data.frame(label = parts, x = x_part, y = y_top + (seq_along(parts) - 1) * 33.3,
                          fill = unname(series_colors[part_codes]))
  endpoint_nodes <- data.frame(label = endpoints$label, x = x_endpoint, y = y_top + (1 - endpoints$pos) * unit_y)
  ny <- function(lbl) {
    c(setNames(exp_nodes$y, exp_nodes$label), setNames(part_nodes$y, part_nodes$label),
      setNames(endpoint_nodes$y, endpoint_nodes$label))[lbl]
  }
  edge_col <- setNames(c(col_rust, col_blue), directions)

  ed <- b[b$layer == layers[1], ]
  ed$y <- ny(ed$from)
  ed$yend <- ny(ed$to)
  dc <- b[b$layer == layers[2], ]
  dc$y <- ny(dc$from)
  dc$yend <- ny(dc$to)
  dc$width_pt <- 1.9 * pmax(0.12, 1.5 * abs(log(dc$estimate)))
  bp <- b[b$layer == layers[3], ]
  bp$y <- ny(bp$from)
  bp$yend <- ny(bp$to)
  stopifnot(!anyNA(c(ed$y, ed$yend, dc$y, dc$yend, bp$y, bp$yend)))

  m <- b[b$layer == layers[4], ]
  stopifnot(setequal(m$from, features), setequal(m$to, series_order))
  col_x <- setNames(st$matrix_column_x, series_order)
  m$x <- col_x[m$to]
  m$w <- ifelse(m$to == series_labels[[first_code]], st$matrix_column_width[1], st$matrix_column_width[2])
  m$y <- ny(m$from)
  m$label <- ifelse(m$to == series_labels[[first_code]],
                    sprintf("%+.1f (%+.1f, %+.1f)", m$estimate, m$lower, m$upper),
                    sprintf("%+.1f", m$estimate))
  lim <- max(abs(m$estimate))
  ramp <- scales::col_numeric(c("#91B0C5", "#FFFFFF", "#CF9986"), domain = c(-lim, lim))
  m$fill <- ramp(m$estimate)

  exp_lab <- exp_nodes

  key_y <- 195.5
  p <- ggplot() +
    geom_curve(data = bp, aes(x = x_feat, y = y, xend = x_endpoint - 1, yend = yend, colour = direction),
               curvature = 0.28, linewidth = pt(0.75), linetype = "22", alpha = 0.85, lineend = "round") +
    geom_segment(data = ed, aes(x = x_feat, y = y, xend = x_part, yend = yend, colour = direction),
                 linewidth = pt(0.85), alpha = 0.65, lineend = "round") +
    geom_segment(data = dc, aes(x = x_part, y = y, xend = x_endpoint - 1, yend = yend, colour = direction,
                                linewidth = pt(width_pt)), alpha = 0.6, lineend = "round") +
    geom_point(data = exp_nodes, aes(x, y), shape = 21, size = 1.45, fill = "#686868",
               colour = "#686868", stroke = pt(0.6)) +
    geom_text(data = exp_lab, aes(x - 4.8, y, label = label), hjust = 1, size = pt(8),
              lineheight = 0.95, family = "Arial") +
    geom_label(data = part_nodes, aes(x, y - 6.9, label = label), size = pt(7), family = "Arial",
               fill = scales::alpha("white", 0.85), border.colour = NA,
               label.padding = unit(0.6, "pt"), label.r = unit(1, "pt")) +
    geom_point(data = part_nodes, aes(x, y, fill = fill), shape = 21, size = 2.35,
               colour = "white", stroke = pt(0.6)) +
    geom_point(data = endpoint_nodes, aes(x, y), shape = 21, size = 2.15, fill = "#686868",
               colour = "#686868", stroke = pt(0.4), alpha = 0.7) +
    geom_text(data = endpoint_nodes, aes(x + 5, y, label = label), hjust = 0, size = pt(8),
              family = "Arial") +
    annotate("text", x = c(255.5, 338.65, 470), y = 27.2,
             label = c(txt$head_feature, txt$head_part, txt$head_endpoint),
             size = pt(8), fontface = "bold", family = "Arial") +
    geom_tile(data = m, aes(x, y, width = w, fill = fill), height = 16) +
    geom_tile(data = m[m$fdr_supported, ], aes(x, y, width = w - 1.3), height = 14.7,
              fill = NA, colour = "#303030", linewidth = pt(0.56)) +
    geom_text(data = m, aes(x, y, label = label), size = pt(7), family = "Arial") +
    annotate("text", x = 107.3, y = 25.1, label = txt$matrix_title, size = pt(8),
             family = "Arial") +
    annotate("text", x = col_x, y = 35.4,
             label = c(txt$matrix_first_column, unname(series_short[part_codes])),
             size = pt(7), family = "Arial") +
    annotate("text", x = 18.2, y = key_y, label = txt$key_border, hjust = 0, size = pt(6),
             family = "Arial") +
    annotate("segment", x = c(104, 172, 239), xend = c(124, 192, 259), y = key_y, yend = key_y,
             colour = c(col_rust, col_blue, "#6B7680"), linewidth = pt(c(1.3, 1.3, 0.95))) +
    annotate("segment", x = 304, xend = 324, y = key_y, yend = key_y, colour = "#6B7680",
             linewidth = pt(0.95), linetype = "22") +
    annotate("text", x = c(127.8, 195.8, 262.5, 327.8), y = key_y,
             label = c(unname(direction_labels), txt$key_association, txt$key_adjusted),
             hjust = 0, size = pt(7), family = "Arial") +
    scale_colour_manual(values = edge_col, guide = "none") +
    scale_fill_identity() +
    scale_linewidth_identity() +
    scale_x_continuous(limits = c(0, 540), expand = c(0, 0)) +
    scale_y_reverse(limits = c(205, 0), expand = c(0, 0)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0), legend.position = "none")

  bb <- suppressWarnings(ggplot_build(p))
  stopifnot(nrow(bb$data[[1]]) == nrow(bp), nrow(bb$data[[2]]) == nrow(ed), nrow(bb$data[[3]]) == nrow(dc))
  p
}

# Panel c
panel_5c <- function() {
  d <- read_source("Figure_5c.csv", cols = c("feature_panel", "comparison", "estimate", "lower", "upper"))
  txt <- read_text("Figure_5c")
  lv <- read_levels("Figure_5c")
  st <- read_settings("Figure_5c")
  panels <- lv_codes(lv, "bundle")
  stopifnot(setequal(d$feature_panel, panels))
  cmp <- lv_codes(lv, "comparison")
  stopifnot(setequal(d$comparison, cmp))
  d$feature_panel <- factor(d$feature_panel, levels = rev(panels))
  d$comparison <- factor(d$comparison, levels = cmp)
  cmp_col <- setNames(c("#2B2B2B", "#9AA3AA"), cmp)
  cmp_shape <- setNames(c(16, 21), cmp)

  p <- ggplot(d, aes(estimate, feature_panel, colour = comparison, shape = comparison,
                     group = factor(comparison, levels = rev(cmp)))) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.27, linetype = "dashed") +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", position = position_dodge(0.55),
                  width = 0.18, linewidth = 0.36) +
    geom_point(position = position_dodge(0.55), size = 1.36, fill = "white", stroke = 0.55) +
    scale_colour_manual(values = cmp_col, name = NULL, breaks = cmp) +
    scale_shape_manual(values = cmp_shape, name = NULL, breaks = cmp) +
    scale_x_continuous(breaks = st$x_breaks) +
    labs(x = txt$x_title, y = NULL) +
    guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    theme_fig5 +
    theme(axis.text = element_text(size = 7.27), axis.title = element_text(size = 7.27),
          legend.text = element_text(size = 7.27), legend.position = "bottom",
          axis.ticks.length = unit(-0.09, "cm"),
          legend.key.size = unit(0.11, "in"), legend.key.height = unit(8, "pt"),
          legend.key.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(2, "pt"), plot.margin = margin(18, 8, 3, 4))
  stopifnot(isTRUE(all.equal(sort(ggplot_build(p)$data[[3]]$x), sort(d$estimate))))
  p
}

# Panel d
panel_5d <- function() {
  d <- read_source("Figure_5d.csv", cols = c("measure_set", "path", "estimate", "lower", "upper"))
  txt <- read_text("Figure_5d")
  lv <- read_levels("Figure_5d")
  st <- read_settings("Figure_5d")
  measures <- lv_codes(lv, "measure")
  paths <- lv_codes(lv, "path")
  stopifnot(setequal(d$measure_set, measures), setequal(d$path, paths))
  d$measure_set <- factor(d$measure_set, levels = rev(measures))
  d$path <- factor(d$path, levels = paths)
  path_col <- setNames(c("#5E676E", "#A3ABB1", "#1A1A1A"), paths)
  path_shape <- setNames(c(15, 21, 18), paths)

  p <- ggplot(d, aes(estimate, measure_set, colour = path, shape = path,
                     group = factor(path, levels = rev(paths)))) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.27, linetype = "dashed") +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", position = position_dodge(0.66),
                  width = 0.2, linewidth = 0.36) +
    geom_point(position = position_dodge(0.66), size = 1.4, fill = "white", stroke = 0.55) +
    scale_colour_manual(values = path_col, name = NULL, breaks = paths) +
    scale_shape_manual(values = path_shape, name = NULL, breaks = paths) +
    scale_x_continuous(breaks = st$x_breaks) +
    labs(x = txt$x_title, y = NULL) +
    theme_fig5 +
    theme(axis.text = element_text(size = 7.27), axis.title = element_text(size = 7.27),
          legend.text = element_text(size = 7.27), legend.position = "bottom",
          axis.ticks.length = unit(-0.09, "cm"),
          legend.key.size = unit(0.11, "in"), legend.key.height = unit(8, "pt"),
          legend.key.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
          legend.box.spacing = unit(2, "pt"), plot.margin = margin(18, 8, 14, 4))
  stopifnot(isTRUE(all.equal(sort(ggplot_build(p)$data[[3]]$x), sort(d$estimate))))
  p
}

save_panel(panel_5a(), "Figure_5a", width_pt = 612, height_pt = 244.8)
save_panel(panel_5b(), "Figure_5b", width_pt = 540, height_pt = 205)
save_panel(panel_5c(), "Figure_5c", width_pt = 267, height_pt = 183)
save_panel(panel_5d(), "Figure_5d", width_pt = 272, height_pt = 178)
