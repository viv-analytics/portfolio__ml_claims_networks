# =============================================================================
# utils_kpi.R
# KPI calculation helpers for Motor Claims Partner Network Analysis
# =============================================================================

library(tidyverse)

# -----------------------------------------------------------------------------
# compute_kpis()
# Compute all KPIs for each partner aggregated from claims data
# Returns: tibble with partner_id + 8 KPI columns
# -----------------------------------------------------------------------------
compute_kpis <- function(claims, partners) {
  kpis <- claims |>
    filter(!is.na(partner_id), steering_flag == 1L) |>
    group_by(partner_id) |>
    summarise(
      n_claims         = n(),
      avg_cost         = mean(repair_cost),
      median_cost      = median(repair_cost),
      sd_cost          = sd(repair_cost),
      avg_duration     = mean(duration_days),
      median_duration  = median(duration_days),
      reopen_rate      = mean(reopening_flag),
      avg_csat         = mean(csat_score),
      sd_csat          = sd(csat_score),
      pct_glass        = mean(claim_type == "glass"),
      pct_body         = mean(claim_type == "body"),
      pct_engine       = mean(claim_type == "engine"),
      pct_total_loss   = mean(claim_type == "total_loss"),
      .groups          = "drop"
    ) |>
    left_join(partners |> select(partner_id, name, type, region, capacity_class,
                                  true_cost_factor, true_speed_factor, true_quality_factor),
              by = "partner_id")

  kpis
}

# -----------------------------------------------------------------------------
# cost_benchmark()
# Z-score normalised cost index relative to global mean
# Lower = better (lower cost than average)
# -----------------------------------------------------------------------------
cost_benchmark <- function(kpis) {
  global_mean <- mean(kpis$avg_cost, na.rm = TRUE)
  global_sd   <- sd(kpis$avg_cost,   na.rm = TRUE)
  kpis |>
    mutate(
      cost_index    = avg_cost / global_mean,
      cost_zscore   = (avg_cost - global_mean) / global_sd,
      cost_label    = case_when(
        cost_zscore < -1   ~ "Excellent",
        cost_zscore < -0.5 ~ "Good",
        cost_zscore <  0.5 ~ "Average",
        cost_zscore <  1   ~ "Below Average",
        TRUE               ~ "Poor"
      )
    )
}

# -----------------------------------------------------------------------------
# speed_benchmark()
# Duration index normalised by claim type mix (adjusts for case complexity)
# Lower = better (faster resolution)
# -----------------------------------------------------------------------------
speed_benchmark <- function(claims, partners) {
  # Compute type-specific benchmarks
  type_benchmarks <- claims |>
    filter(steering_flag == 1L) |>
    group_by(claim_type) |>
    summarise(type_avg_duration = mean(duration_days), .groups = "drop")

  partner_speed <- claims |>
    filter(!is.na(partner_id), steering_flag == 1L) |>
    left_join(type_benchmarks, by = "claim_type") |>
    mutate(duration_ratio = duration_days / type_avg_duration) |>
    group_by(partner_id) |>
    summarise(
      speed_index = mean(duration_ratio),
      .groups     = "drop"
    ) |>
    mutate(
      speed_label = case_when(
        speed_index < 0.80 ~ "Excellent",
        speed_index < 0.95 ~ "Good",
        speed_index < 1.05 ~ "Average",
        speed_index < 1.20 ~ "Below Average",
        TRUE               ~ "Poor"
      )
    )

  partner_speed
}

# -----------------------------------------------------------------------------
# quality_index()
# Composite quality score from csat + (1 - reopen_rate)
# Higher = better
# -----------------------------------------------------------------------------
quality_index <- function(kpis) {
  kpis |>
    mutate(
      csat_norm    = (avg_csat - 1) / 9,
      quality_idx  = 0.6 * csat_norm + 0.4 * (1 - reopen_rate),
      quality_label = case_when(
        quality_idx > 0.80 ~ "Excellent",
        quality_idx > 0.65 ~ "Good",
        quality_idx > 0.50 ~ "Average",
        quality_idx > 0.35 ~ "Below Average",
        TRUE               ~ "Poor"
      )
    )
}

# -----------------------------------------------------------------------------
# composite_score()
# Weighted composite KPI score: 0-100
# Weights: cost 30%, speed 25%, quality 25%, csat 10%, stability 10%
# Higher = better partner
# -----------------------------------------------------------------------------
composite_score <- function(kpis, speed_df) {
  df <- kpis |>
    left_join(speed_df |> select(partner_id, speed_index), by = "partner_id")

  # Normalise each component to 0-1 (min-max), then invert where lower = better
  df |>
    mutate(
      # Cost: lower is better → invert
      cost_norm    = 1 - (avg_cost    - min(avg_cost,    na.rm = TRUE)) /
                         (max(avg_cost,    na.rm = TRUE) - min(avg_cost,    na.rm = TRUE)),
      # Speed: lower index is better → invert
      speed_norm   = 1 - (speed_index - min(speed_index, na.rm = TRUE)) /
                         (max(speed_index, na.rm = TRUE) - min(speed_index, na.rm = TRUE)),
      # Quality: higher is better
      quality_norm = quality_idx,
      # CSAT: higher is better
      csat_norm2   = (avg_csat - min(avg_csat, na.rm = TRUE)) /
                     (max(avg_csat, na.rm = TRUE) - min(avg_csat, na.rm = TRUE)),
      # Stability: more claims → more reliable estimate; penalise small volume
      stability    = pmin(1, n_claims / 200),
      # Composite (0-100)
      composite    = round(100 * (
        0.30 * cost_norm +
        0.25 * speed_norm +
        0.25 * quality_norm +
        0.10 * csat_norm2 +
        0.10 * stability
      ), 1)
    )
}
