# =============================================================================
# utils_viz.R
# ggplot2 theme + reusable plot functions
# =============================================================================

library(tidyverse)
library(ggrepel)
library(patchwork)

# =============================================================================
# Brand colors
# =============================================================================
allianz_colors <- list(
  navy    = "#003781",
  blue    = "#0066CC",
  light   = "#66B5E8",
  teal    = "#00A9CE",
  orange  = "#FF6600",
  white   = "#FFFFFF",
  grey    = "#6B7280",
  bg      = "#F4F7FB",
  dark    = "#1A1A1A"
)

allianz_palette <- c(
  "#003781", "#0066CC", "#00A9CE", "#66B5E8",
  "#FF6600", "#6B7280", "#1A1A1A", "#FFFFFF"
)

# Named KPI palette
kpi_colors <- c(
  cost     = "#003781",
  speed    = "#0066CC",
  quality  = "#00A9CE",
  csat     = "#66B5E8",
  stability = "#FF6600"
)

# -----------------------------------------------------------------------------
# theme_allianz()
# Corporate ggplot2 theme
# -----------------------------------------------------------------------------
theme_allianz <- function(base_size = 11, grid = "both") {
  bg    <- "#F4F7FB"
  navy  <- "#003781"
  grey  <- "#6B7280"
  dark  <- "#1A1A1A"
  white <- "#FFFFFF"

  t <- theme_minimal(base_size = base_size) %+replace%
    theme(
      # Background
      plot.background      = element_rect(fill = bg, colour = NA),
      panel.background     = element_rect(fill = bg, colour = NA),
      panel.border         = element_blank(),
      # Grid
      panel.grid.major     = element_line(colour = white, linewidth = 0.4),
      panel.grid.minor     = element_blank(),
      # Axes
      axis.title           = element_text(colour = grey,  size = rel(0.85), face = "plain"),
      axis.text            = element_text(colour = grey,  size = rel(0.80)),
      axis.ticks           = element_blank(),
      axis.line            = element_blank(),
      # Titles
      plot.title           = element_text(colour = navy,  size = rel(1.2),  face = "bold",
                                           margin = margin(b = 6)),
      plot.subtitle        = element_text(colour = grey,  size = rel(0.9),
                                           margin = margin(b = 10)),
      plot.caption         = element_text(colour = grey,  size = rel(0.75), hjust = 1),
      plot.title.position  = "plot",
      # Facets
      strip.background     = element_rect(fill = navy, colour = NA),
      strip.text           = element_text(colour = white, face = "bold", size = rel(0.85)),
      # Legend
      legend.background    = element_rect(fill = bg, colour = NA),
      legend.key           = element_rect(fill = bg, colour = NA),
      legend.title         = element_text(colour = dark, face = "bold", size = rel(0.85)),
      legend.text          = element_text(colour = grey, size = rel(0.80)),
      # Margins
      plot.margin          = margin(12, 12, 8, 12)
    )

  if (grid == "x") {
    t <- t + theme(panel.grid.major.y = element_blank())
  } else if (grid == "y") {
    t <- t + theme(panel.grid.major.x = element_blank())
  } else if (grid == "none") {
    t <- t + theme(panel.grid.major = element_blank())
  }
  t
}

# -----------------------------------------------------------------------------
# scale_fill_allianz() / scale_color_allianz()
# -----------------------------------------------------------------------------
scale_fill_allianz <- function(discrete = TRUE, ...) {
  if (discrete) {
    scale_fill_manual(values = allianz_palette, ...)
  } else {
    scale_fill_gradient(low = allianz_colors$light, high = allianz_colors$navy, ...)
  }
}

scale_color_allianz <- function(discrete = TRUE, ...) {
  if (discrete) {
    scale_color_manual(values = allianz_palette, ...)
  } else {
    scale_color_gradient(low = allianz_colors$light, high = allianz_colors$navy, ...)
  }
}

# -----------------------------------------------------------------------------
# plot_partner_scorecard()
# Bubble chart: avg_cost (x) vs speed_index (y), sized by n_claims,
# colored by composite score
# -----------------------------------------------------------------------------
plot_partner_scorecard <- function(kpis_full, label_top_n = 8) {
  top_n_ids <- kpis_full |>
    slice_max(composite, n = label_top_n) |>
    pull(partner_id)

  ggplot(kpis_full, aes(x = avg_cost, y = speed_index,
                         size = n_claims, colour = composite)) +
    geom_point(alpha = 0.75, stroke = 0) +
    geom_label_repel(
      data = filter(kpis_full, partner_id %in% top_n_ids),
      aes(label = name),
      size = 2.8, colour = allianz_colors$navy,
      fill = allianz_colors$white, label.padding = 0.2,
      box.padding = 0.4, max.overlaps = 20
    ) +
    scale_size_continuous(range = c(2, 12), guide = guide_legend(title = "Claim Volume")) +
    scale_colour_gradient(
      low  = allianz_colors$orange,
      high = allianz_colors$teal,
      name = "Composite\nScore"
    ) +
    scale_x_continuous(labels = scales::label_number(prefix = "CHF ", big.mark = ",")) +
    labs(
      title    = "Partner Scorecard: Cost vs. Speed",
      subtitle = "Bubble size = claim volume · Colour = composite score (higher = better)",
      x        = "Average Repair Cost (CHF)",
      y        = "Speed Index (lower = faster)"
    ) +
    theme_allianz()
}

# -----------------------------------------------------------------------------
# plot_funnel()
# Funnel plot: O/E ratio (y) vs log(volume) (x) + CI bands
# Highlights outliers outside 95% / 99.8% control limits
# -----------------------------------------------------------------------------
plot_funnel <- function(oe_df, title = "Funnel Plot: Observed vs. Expected Cost") {
  # Expect columns: partner_id, name, n_claims, oe_ratio, oe_lower, oe_upper
  oe_df <- oe_df |>
    mutate(
      outlier_99 = oe_ratio < oe_lower | oe_ratio > oe_upper,
      label_txt  = if_else(outlier_99, name, NA_character_)
    )

  ggplot(oe_df, aes(x = n_claims, y = oe_ratio)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = allianz_colors$grey, linewidth = 0.6) +
    geom_ribbon(aes(ymin = oe_lower, ymax = oe_upper),
                fill = allianz_colors$light, alpha = 0.3) +
    geom_point(aes(colour = outlier_99), size = 2.5, alpha = 0.8) +
    geom_label_repel(
      aes(label = label_txt),
      size = 2.6, colour = allianz_colors$navy,
      fill  = "white", na.rm = TRUE,
      box.padding = 0.3, max.overlaps = 15
    ) +
    scale_colour_manual(
      values = c(`FALSE` = allianz_colors$blue, `TRUE` = allianz_colors$orange),
      labels = c("Within limits", "Outlier"),
      name   = NULL
    ) +
    scale_x_continuous(trans = "log10",
                       labels = scales::label_number(accuracy = 1)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    labs(
      title    = title,
      subtitle = "Shaded band = 95% control limits · Outliers labeled",
      x        = "Claim Volume (log scale)",
      y        = "O/E Cost Ratio (1 = expected)"
    ) +
    theme_allianz()
}

# -----------------------------------------------------------------------------
# plot_kpi_heatmap()
# Partners (rows) × KPI dimensions (cols), fill = z-score
# -----------------------------------------------------------------------------
plot_kpi_heatmap <- function(kpi_long_df) {
  # Expect columns: partner_id / name, kpi, z_score
  ggplot(kpi_long_df, aes(x = kpi, y = reorder(name, z_score), fill = z_score)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = round(z_score, 1)),
              colour = "white", size = 2.4, fontface = "bold") +
    scale_fill_gradient2(
      low      = allianz_colors$orange,
      mid      = allianz_colors$light,
      high     = allianz_colors$teal,
      midpoint = 0,
      name     = "Z-Score",
      guide    = guide_colorbar(barwidth = 0.8, barheight = 10)
    ) +
    labs(
      title    = "Partner KPI Heatmap",
      subtitle = "Z-scores relative to peer group mean",
      x        = NULL, y = NULL
    ) +
    theme_allianz(grid = "none") +
    theme(axis.text.y = element_text(size = 7))
}

# -----------------------------------------------------------------------------
# plot_lollipop()
# Lollipop chart for partner rankings
# -----------------------------------------------------------------------------
plot_lollipop <- function(df, x_var, y_var, title = NULL,
                           color_var = NULL, n_top = 20) {
  df <- df |> slice_max(!!sym(x_var), n = n_top)

  p <- ggplot(df, aes(x = !!sym(x_var),
                       y = reorder(!!sym(y_var), !!sym(x_var)))) +
    geom_segment(aes(xend = 0, yend = reorder(!!sym(y_var), !!sym(x_var))),
                 colour = allianz_colors$light, linewidth = 1) +
    labs(title = title, x = x_var, y = NULL) +
    theme_allianz(grid = "y")

  if (!is.null(color_var)) {
    p <- p + geom_point(aes(colour = !!sym(color_var)), size = 4) +
      scale_color_allianz()
  } else {
    p <- p + geom_point(colour = allianz_colors$navy, size = 4)
  }
  p
}
