# fig2.R - Figure 2
# Input (data/): Figure_2a.csv, Figure_2b.csv, Figure_2c.csv,
#  Figure_2d.csv, Figure_2e.csv, Figure_2a_text.csv, Figure_2a_levels.csv,
#  Figure_2a_settings.csv, Figure_2b_text.csv, Figure_2b_levels.csv,
#  Figure_2c_text.csv, Figure_2c_levels.csv, Figure_2c_settings.csv,
#  Figure_2d_text.csv, Figure_2d_levels.csv, Figure_2d_settings.csv,
#  Figure_2e_text.csv, Figure_2e_levels.csv, Figure_2e_settings.csv,
#  Labels_series_levels.csv
# Output (output/): one .svg and one .png per panel
# Run from the repository root: Rscript fig2.R

source("00_helpers.R")

theme_set(theme_fig)

col_gold   <- "#AD8642"
col_purple <- "#74618C"

pt_small <- 6
pt_text  <- 7
pt_head  <- 8

unnl <- function(x) setNames(gsub("\\n", "\n", x, fixed = TRUE), names(x))

# Panel a
plot_fig2a <- function() {
  txt <- read_text("Figure_2a")
  lv  <- read_levels("Figure_2a")
  st  <- read_settings("Figure_2a")
  item_codes  <- lv_codes(lv, "item")
  item_labels <- lv_labels(lv, "item")
  item_groups <- unnl(lv_labels(lv, "item_group"))
  z <- read_source("Figure_2a.csv",
                   cols = c("period", "item", "series", "estimate", "lower", "upper"))
  stopifnot(setequal(z$item, item_codes),
            setequal(names(item_groups), item_codes))

  z$group       <- factor(item_groups[z$item],
                          levels = unique(item_groups[item_codes]))
  z$item_f <- factor(z$item, levels = item_codes,
                          labels = item_labels[item_codes])
  z$series_f     <- factor(z$series, levels = rev(series_codes), labels = rev(series_labels[series_codes]))
  z$ci_excludes_zero <- z$lower > 0 | z$upper < 0

  ggplot(z, aes(item_f, series_f, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_tile(data = z[z$ci_excludes_zero, ], fill = NA, colour = "#333333",
              linewidth = 0.4, width = 0.9, height = 0.88) +
    geom_text(aes(label = sprintf("%.2f", estimate), colour = abs(estimate) > 0.55),
              size = 2.3, family = "Helvetica") +
    scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white"), guide = "none") +
    facet_grid(. ~ group, scales = "free_x", space = "free_x") +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_gradient2(low = col_rust, mid = "white", high = col_blue,
                         limits = st$fill_limits, breaks = st$fill_breaks,
                         name = txt$fill_title) +
    labs(title = sprintf(txt$title, period_label(unique(z$period))),
         x = NULL, y = NULL, caption = txt$caption) +
    theme_fig +
    theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 7.5),
          axis.text.y = element_text(size = 8, face = "bold"),
          strip.text = element_text(size = 7.2, face = "bold"),
          panel.spacing.x = unit(0.04, "in"),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          legend.key.width = unit(6, "pt"),
          legend.key.height = unit(12, "pt"),
          plot.caption = element_text(size = 7.3, hjust = 0),
          plot.caption.position = "panel")
}

# Panel b
plot_fig2b <- function() {
  txt <- read_text("Figure_2b")
  levels_a <- lv_codes(read_levels("Figure_2b"), "assessment")
  b <- read_source("Figure_2b.csv",
                   cols = c("assessment", "series", "estimate", "lower", "upper"))
  stopifnot(setequal(b$assessment, levels_a),
            !anyDuplicated(paste(b$assessment, b$series)))
  b$assessment_f <- factor(b$assessment, levels = levels_a)
  b$series_f      <- factor(b$series, levels = rev(series_codes), labels = rev(series_labels[series_codes]))

  dodge <- position_dodge(0.62, reverse = TRUE)
  ggplot(b, aes(estimate, series_f, colour = assessment_f, shape = assessment_f)) +
    geom_vline(xintercept = 0, colour = "grey65", linewidth = 0.28) +
    geom_errorbar(aes(xmin = lower, xmax = upper), orientation = "y", width = 0,
                  position = dodge, linewidth = 0.46) +
    geom_point(position = dodge, size = 1.5) +
    scale_colour_manual(values = c("#252525", "#787878", "#B0B0B0"), name = NULL) +
    scale_shape_manual(values = c(16, 17, 15), name = NULL) +
    labs(x = txt$x_title, y = NULL) +
    theme_grey() + theme_fig_base +
    theme(text = element_text(family = "Helvetica", size = pt_text, colour = "black"),
          axis.title = element_text(size = pt_text),
          axis.text = element_text(size = pt_text, colour = "black"),
          axis.text.y = element_text(hjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = pt_small),
          legend.key.height = unit(6.6, "pt"),
          legend.key.width = unit(12, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(9, 22, 12, 22))
}

# Panel c
plot_fig2c <- function() {
  txt <- read_text("Figure_2c")
  lv  <- read_levels("Figure_2c")
  st  <- read_settings("Figure_2c")
  z <- read_source("Figure_2c.csv",
                   cols = c("period", "cluster", "series", "estimate", "part_difference_p"))
  clusters <- lv_codes(lv, "cluster")
  cluster_labels <- unname(lv_labels(lv, "cluster")[clusters])
  stopifnot(setequal(z$cluster, clusters), setequal(z$series, series_codes))

  angle <- setNames(pi / 2 - 2 * pi * (seq_along(clusters) - 1) / length(clusters), clusters)
  offset <- st$offset
  z$neg_value <- -z$estimate
  stopifnot(min(z$neg_value) > -offset)
  z$radius  <- z$neg_value + offset
  z$x       <- z$radius * cos(angle[z$cluster])
  z$y       <- z$radius * sin(angle[z$cluster])
  z$series_f <- factor(z$series, levels = series_codes)

  paths <- do.call(rbind, lapply(split(z, z$series_f), function(d) {
    d <- d[match(clusters, d$cluster), ]
    rbind(d, d[1, ])
  }))
  theta <- seq(0, 2 * pi, length.out = 361)
  rings <- do.call(rbind, lapply(st$rings, function(r) {
    data.frame(r = r, x = (r + offset) * cos(theta), y = (r + offset) * sin(theta))
  }))
  spokes <- data.frame(cluster = clusters, angle = angle, label = cluster_labels)
  spokes$lx <- 1.03 * cos(spokes$angle)
  spokes$ly <- 1.03 * sin(spokes$angle)
  spokes$hj <- ifelse(spokes$lx > 0.1, 0, ifelse(spokes$lx < -0.1, 1, 0.5))
  p_by_cluster <- tapply(z$part_difference_p, z$cluster, unique)
  spokes$bold <- p_by_cluster[spokes$cluster] < 0.05

  ring_angle <- pi / 2 - 2 * pi * st$ring_label_position / length(clusters)
  ring_labels <- data.frame(x = (st$ring_label_values + offset) * cos(ring_angle),
                            y = (st$ring_label_values + offset) * sin(ring_angle),
                            lab = vapply(seq_along(st$ring_label_values),
                                         function(i) txt[[paste0("ring_label_", i)]], ""))

  ggplot() +
    geom_path(data = rings, aes(x, y, group = r, linetype = r == 0),
              colour = "grey78", linewidth = 0.35) +
    scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") +
    geom_segment(data = spokes, aes(x = 0, y = 0, xend = 0.9 * cos(angle), yend = 0.9 * sin(angle)),
                 colour = "grey85", linewidth = 0.3) +
    geom_label(data = ring_labels, aes(x, y, label = lab), size = 2.0, colour = "grey35",
               linewidth = 0, fill = "white", label.padding = unit(0.4, "pt"),
               family = "Helvetica") +
    geom_path(data = paths, aes(x, y, colour = series_f, group = series_f),
              linewidth = 0.65, alpha = 0.85) +
    geom_point(data = z, aes(x, y, colour = series_f), size = 1.25) +
    geom_text(data = spokes, aes(lx, ly, label = label, hjust = hj,
                                 fontface = ifelse(bold, "bold", "plain")),
              size = 2.35, family = "Helvetica") +
    scale_colour_manual(values = series_colors, labels = series_labels, name = NULL) +
    coord_equal(xlim = st$x_limits, ylim = st$y_limits, clip = "off") +
    labs(title = sprintf(txt$title, period_label(unique(z$period))),
         caption = txt$caption) +
    theme_void() +
    theme(text = element_text(family = "Helvetica"),
          plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
          plot.caption = element_text(size = 7, hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 7.1),
          legend.key.width = unit(10, "pt"),
          legend.key.height = unit(10, "pt"),
          plot.margin = margin(4, 3, 3, 3))
}

# Panel d
plot_fig2d <- function() {
  txt <- read_text("Figure_2d")
  lv  <- read_levels("Figure_2d")
  st  <- read_settings("Figure_2d")
  m <- read_source("Figure_2d.csv",
                   cols = c("sample", "measure", "series", "partial_r"))
  blocks   <- lv_codes(lv, "sample")
  measures <- lv_codes(lv, "measure")
  stopifnot(setequal(m$sample, blocks), all(m$measure %in% measures))

  m$block <- factor(m$sample, levels = blocks, labels = lv_labels(lv, "sample")[blocks])
  m$measure_f <- factor(m$measure, levels = rev(measures))
  m$series_f   <- factor(m$series, levels = series_codes, labels = series_short[series_codes])
  m$text_col  <- ifelse(abs(m$partial_r) > 0.5, "white", "black")

  ggplot(m, aes(series_f, measure_f, fill = partial_r)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", partial_r), colour = text_col),
              size = pt_text / .pt, family = "Helvetica") +
    scale_colour_identity() +
    facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_gradient2(low = col_gold, mid = "white", high = col_purple, midpoint = 0,
                         limits = st$fill_limits, breaks = st$fill_breaks,
                         name = txt$fill_title) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_grey() + theme_fig_base +
    theme(text = element_text(family = "Helvetica", size = pt_text, colour = "black"),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 90, size = pt_text, face = "bold"),
          axis.text.x.top = element_text(size = pt_head, face = "bold", colour = "black"),
          axis.text.y = element_text(size = pt_text, colour = "black", hjust = 1),
          panel.spacing.y = unit(0.05, "in"),
          legend.position = "bottom",
          legend.title = element_text(size = pt_head, face = "plain", vjust = 0.85),
          legend.text = element_text(size = pt_text),
          legend.key.height = unit(6, "pt"),
          legend.key.width = unit(20, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(4, 4, 4, 2))
}

# Panel e
plot_fig2e <- function() {
  txt <- read_text("Figure_2e")
  st  <- read_settings("Figure_2e")
  layers <- lv_codes(read_levels("Figure_2e"), "layer")
  e <- read_source("Figure_2e.csv",
                   cols = c("layer", "series", "x", "estimate", "lower", "upper"))
  stopifnot(setequal(e$layer, layers))
  curve <- e[e$layer == layers[1], ]
  corr  <- e[e$layer == layers[2], ]
  knots <- e[e$layer == layers[3], ]
  ylim  <- e[e$layer == layers[4], ]
  xlim  <- e[e$layer == layers[5], ]
  stopifnot(setequal(curve$series, series_codes), setequal(corr$series, series_codes),
            setequal(ylim$series, series_codes), nrow(xlim) == 1)

  corr$label <- sprintf(txt$corr_label, corr$estimate, corr$lower, corr$upper)
  rug  <- do.call(rbind, lapply(series_codes, function(s) data.frame(series = s, x = knots$x)))
  lims <- rbind(data.frame(series = ylim$series, y = ylim$lower),
                data.frame(series = ylim$series, y = ylim$upper))
  for (nm in c("curve", "corr", "rug", "lims")) {
    d <- get(nm)
    d$series_f <- factor(d$series, levels = series_codes, labels = series_labels[series_codes])
    assign(nm, d)
  }
  cols <- setNames(series_colors, series_labels)

  ggplot(curve, aes(x, estimate, colour = series_f)) +
    geom_blank(data = lims, aes(x = NULL, y = y), inherit.aes = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = series_f), colour = NA, alpha = 0.15) +
    geom_line(linewidth = 0.65) +
    geom_rug(data = rug, aes(x = x, colour = series_f), inherit.aes = FALSE, sides = "b",
             length = unit(2.3, "pt"), linewidth = 0.33) +
    geom_text(data = corr, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
              hjust = -0.07, vjust = 1.25, size = pt_text / .pt, family = "Helvetica") +
    facet_wrap(~series_f, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = st$x_breaks, limits = c(xlim$lower, xlim$upper),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 4), expand = c(0, 0)) +
    labs(x = txt$x_title, y = txt$y_title) +
    theme_grey() + theme_fig_base +
    theme(text = element_text(family = "Helvetica", size = pt_text, colour = "black"),
          legend.position = "none",
          strip.text = element_text(size = pt_text, face = "bold", margin = margin(t = 2, b = 3)),
          axis.title = element_text(size = pt_text),
          axis.text = element_text(size = pt_text, colour = "black"),
          axis.text.y = element_text(hjust = 1),
          panel.spacing = unit(0.08, "in"),
          plot.margin = margin(4, 4, 3, 3))
}

save_panel(plot_fig2a(), "Figure_2a", width_pt = 540, height_pt = 205)
save_panel(plot_fig2b(), "Figure_2b", width_pt = 255, height_pt = 177)
save_panel(plot_fig2c(), "Figure_2c", width_pt = 300, height_pt = 205)
save_panel(plot_fig2d(), "Figure_2d", width_pt = 255, height_pt = 335)
save_panel(plot_fig2e(), "Figure_2e", width_pt = 285, height_pt = 335)
