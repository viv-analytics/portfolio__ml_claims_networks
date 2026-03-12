# =============================================================================
# utils_models.R
# Modelling helpers: O/E ratios, shrinkage estimates, propensity scores
# =============================================================================

library(tidyverse)
library(lme4)

# -----------------------------------------------------------------------------
# observed_vs_expected()
# Compute O/E cost ratios from a fitted GLM
# model: fitted glm object (cost ~ covariates, no partner)
# claims: data frame with actual costs + partner_id
# Returns: tibble with partner_id, n_claims, observed, expected, oe_ratio,
#          oe_lower, oe_upper (95% Poisson-approx funnel limits)
# -----------------------------------------------------------------------------
observed_vs_expected <- function(model, claims_steered) {
  # Predicted "expected" cost from the case-mix model
  claims_steered <- claims_steered |>
    mutate(expected_cost = predict(model, newdata = claims_steered, type = "response"))

  # Sum observed and expected per partner
  oe <- claims_steered |>
    group_by(partner_id) |>
    summarise(
      n_claims = n(),
      observed = sum(repair_cost),
      expected = sum(expected_cost),
      .groups  = "drop"
    ) |>
    mutate(
      oe_ratio = observed / expected,
      # Approximate Poisson control limits for funnel plot
      # Based on Spiegelhalter (2005) method
      oe_lower = 1 - 1.96 * sqrt(1 / expected) * (observed / expected),
      oe_upper = 1 + 1.96 * sqrt(1 / expected) * (observed / expected)
    )

  oe
}

# -----------------------------------------------------------------------------
# shrinkage_estimate()
# Partial-pooling (empirical Bayes) shrinkage of partner-level cost ratios
# using lme4 random effects
# Returns: random effects tibble (partner_id, ranef, shrunk_oe)
# -----------------------------------------------------------------------------
shrinkage_estimate <- function(claims_steered) {
  # Fit mixed model: cost ~ case_mix + (1 | partner_id)
  df <- claims_steered |>
    mutate(log_cost = log(repair_cost + 1))

  m <- tryCatch(
    lmer(log_cost ~ claim_type + vehicle_class + severity_score +
           (1 | partner_id),
         data = df, REML = TRUE),
    error = function(e) {
      warning("lmer failed, returning NULL: ", e$message)
      NULL
    }
  )

  if (is.null(m)) return(NULL)

  re <- ranef(m)$partner_id |>
    as_tibble(rownames = "partner_id") |>
    rename(ranef = `(Intercept)`) |>
    mutate(
      shrunk_oe = exp(ranef),
      label     = case_when(
        ranef < -0.15 ~ "Significantly cheaper",
        ranef <  0.00 ~ "Slightly cheaper",
        ranef <  0.15 ~ "Slightly more expensive",
        TRUE          ~ "Significantly more expensive"
      )
    )
  re
}

# -----------------------------------------------------------------------------
# fit_propensity()
# Logistic regression propensity model for steering_flag
# Features: claim_type + vehicle_class + severity_score + region + vehicle_age
# Returns: list(model, claims_with_ps) where claims_with_ps has columns
#   ps (propensity score), ipw (inverse probability weight), trim_ipw
# -----------------------------------------------------------------------------
fit_propensity <- function(claims) {
  m <- glm(
    steering_flag ~ claim_type + vehicle_class + severity_score + region + vehicle_age,
    data   = claims,
    family = binomial()
  )

  ps <- predict(m, type = "response")

  claims_ps <- claims |>
    mutate(
      ps       = ps,
      # Stabilised IPW weights
      ipw      = ifelse(steering_flag == 1L,
                        mean(steering_flag) / ps,
                        mean(1 - steering_flag) / (1 - ps)),
      # Trim at 1st/99th percentile to reduce variance
      trim_ipw = pmin(ipw, quantile(ipw, 0.99))
    )

  list(model = m, data = claims_ps)
}

# -----------------------------------------------------------------------------
# aipw_ate()
# Augmented IPW (doubly robust) estimator for ATE of steering on log(cost)
# Returns: scalar ATE estimate (multiplicative effect on cost scale)
# -----------------------------------------------------------------------------
aipw_ate <- function(claims_ps) {
  d <- claims_ps |>
    mutate(log_cost = log(repair_cost))

  # Outcome model: E[Y | X, A]
  m1 <- lm(log_cost ~ claim_type + vehicle_class + severity_score + region +
              steering_flag,
            data = d)

  mu1 <- predict(m1, newdata = mutate(d, steering_flag = 1L))
  mu0 <- predict(m1, newdata = mutate(d, steering_flag = 0L))

  # AIPW estimator
  A    <- d$steering_flag
  Y    <- d$log_cost
  ps   <- d$ps
  ipw  <- d$trim_ipw

  phi1 <- A / ps   * (Y - mu1) + mu1
  phi0 <- (1-A)/(1-ps) * (Y - mu0) + mu0

  ate_log <- mean(phi1) - mean(phi0)
  ate_pct <- (exp(ate_log) - 1) * 100

  list(
    ate_log   = ate_log,
    ate_pct   = ate_pct,
    phi1      = phi1,
    phi0      = phi0
  )
}

# -----------------------------------------------------------------------------
# cate_by_segment()
# Conditional ATE by claim_type segment using subgroup outcome models
# Returns: tibble(claim_type, cate_pct)
# -----------------------------------------------------------------------------
cate_by_segment <- function(claims_ps) {
  types <- levels(claims_ps$claim_type)
  map_dfr(types, function(ct) {
    d <- filter(claims_ps, claim_type == ct) |>
      mutate(log_cost = log(repair_cost))

    if (n_distinct(d$steering_flag) < 2) {
      return(tibble(claim_type = ct, cate_pct = NA_real_, n = nrow(d)))
    }

    res <- tryCatch(
      aipw_ate(d),
      error = function(e) list(ate_pct = NA_real_)
    )
    tibble(claim_type = ct, cate_pct = res$ate_pct, n = nrow(d))
  })
}
