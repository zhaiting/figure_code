# 00_helpers.R - shared settings and functions for the figure scripts.

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop("A UTF-8 locale is required (for example, run with LC_ALL=en_US.UTF-8).",
         call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(scales)
  library(svglite)
  library(ragg)
})

grDevices::pdf(NULL)

data_dir <- "data"
fig_dir  <- "output"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

col_blue <- "#406783"
col_rust <- "#A45439"
series_palette <- c("#2B2B2B", "#8E6FB0", "#6FA86B", "#B5793F", "#2E9E8F", "#E8B33D",
                    "#5B6C8F", "#9C6B4E", "#4F8A8B", "#8A7A3C", "#7D5A8C", "#5E8C61")
group_palette <- c("#77618C", "#A97E45", "#B34EA0", "#3F8C7C", "#AC83A4", "#75A36C",
                   "#5B6C8F", "#9C6B4E", "#4F8A8B", "#8A7A3C", "#7D5A8C", "#5E8C61")

theme_fig_base <- theme(
  plot.background   = element_blank(),
  panel.background  = element_blank(),
  panel.border      = element_rect(colour = "black", linewidth = 0.5, fill = NA),
  panel.grid        = element_blank(),
  strip.background  = element_rect(colour = NA, fill = NA),
  strip.text        = element_text(colour = "black", size = 10),
  axis.line         = element_blank(),
  axis.ticks        = element_line(colour = "black", linewidth = 0.5),
  axis.ticks.length = unit(-0.1, "cm"),
  axis.text         = element_text(colour = "black", size = 8),
  axis.title        = element_text(colour = "black", size = 10),
  plot.title        = element_text(colour = "black", size = 11, face = "bold", hjust = 0),
  plot.subtitle     = element_text(colour = "black", size = 9, hjust = 0),
  plot.caption      = element_text(colour = "grey30", size = 7.5, hjust = 0,
                                   margin = margin(t = 6)),
  legend.background = element_blank(),
  legend.key        = element_blank(),
  legend.text       = element_text(colour = "black", size = 8),
  legend.title      = element_text(colour = "black", size = 9, face = "bold"),
  legend.position   = "right",
  panel.spacing     = unit(0.4, "lines")
)

theme_fig <- theme_fig_base + theme(
  text          = element_text(family = "Helvetica", size = 8.5, colour = "black"),
  plot.title    = element_text(size = 9, face = "bold"),
  plot.subtitle = element_text(size = 8),
  axis.title    = element_text(size = 8),
  axis.text     = element_text(size = 7.8, colour = "black"),
  strip.text    = element_text(size = 8, face = "bold"),
  legend.title  = element_text(size = 7.8),
  legend.text   = element_text(size = 7.5),
  plot.margin   = margin(5, 5, 5, 5)
)

require_input <- function(path, what = basename(path)) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Required input not found: %s\n  Expected at: %s",
      what, path), call. = FALSE)
  }
  invisible(path)
}

require_columns <- function(df, cols, what = "input") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("%s is missing required column(s): %s",
                 what, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

read_source <- function(file, cols = NULL) {
  path <- file.path(data_dir, file)
  require_input(path, file)
  df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        encoding = "UTF-8")
  if (!is.null(cols)) require_columns(df, cols, file)
  df
}

read_text <- function(name) {
  d <- read_source(paste0(name, "_text.csv"), cols = c("key", "value"))
  v <- gsub("\\n", "\n", d$value, fixed = TRUE)
  structure(setNames(v, d$key), class = c("text_table", "character"))
}

`[[.text_table` <- function(x, i) {
  if (!i %in% names(x)) stop(sprintf("Text key not found: %s", i), call. = FALSE)
  unname(unclass(x)[i])
}

`$.text_table` <- function(x, name) x[[name]]

read_levels <- function(name) {
  d <- read_source(paste0(name, "_levels.csv"), cols = c("variable", "code", "label"))
  d$label <- gsub("\\n", "\n", d$label, fixed = TRUE)
  d
}

lv_codes <- function(lv, variable) {
  z <- lv[lv$variable == variable, ]
  if (!nrow(z)) stop(sprintf("No levels for variable: %s", variable), call. = FALSE)
  z$code
}

lv_labels <- function(lv, variable) {
  z <- lv[lv$variable == variable, ]
  setNames(z$label, z$code)
}

read_settings <- function(name) {
  d <- read_source(paste0(name, "_settings.csv"), cols = c("key", "value"))
  setNames(lapply(strsplit(as.character(d$value), ";", fixed = TRUE), as.numeric), d$key)
}

series_lv     <- read_levels("Labels_series")
series_codes  <- lv_codes(series_lv, "series")
first_code    <- lv_codes(series_lv, "first")
part_codes    <- lv_codes(series_lv, "part")
series_labels <- lv_labels(series_lv, "series")
series_short  <- lv_labels(series_lv, "series_short")
series_colors <- setNames(series_palette[seq_along(series_codes)], series_codes)

group_lv     <- read_levels("Labels_groups")
group_colors <- setNames(group_palette[seq_along(lv_codes(group_lv, "group"))],
                         lv_codes(group_lv, "group"))

save_panel <- function(draw, name, width_pt, height_pt, png_dpi = 300) {
  w <- width_pt / 72
  h <- height_pt / 72
  render <- function() {
    grid.newpage()
    if (is.function(draw)) draw() else print(draw, newpage = FALSE)
  }
  svglite::svglite(file.path(fig_dir, paste0(name, ".svg")),
                   width = w, height = h, bg = "white")
  render()
  grDevices::dev.off()
  ragg::agg_png(file.path(fig_dir, paste0(name, ".png")),
                width = w, height = h, units = "in", res = png_dpi,
                background = "white")
  render()
  grDevices::dev.off()
  invisible(file.path(fig_dir, paste0(name, c(".svg", ".png"))))
}

period_label <- function(x) gsub("-", "–", x, fixed = TRUE)
