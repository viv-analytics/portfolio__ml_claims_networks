# =============================================================================
# 00_simulate_data.R
# Synthetic data simulation for Motor Claims Partner Network Analysis
# Uses mirai for parallel generation of claims data
# Run once: saves data/claims.rds, data/partners.rds, data/process_events.rds
# =============================================================================

library(mirai)
library(tidyverse)
library(lubridate)

set.seed(42)

# Create data directory if needed
if (!dir.exists("data")) dir.create("data")

cat("== Motor Claims Network: Synthetic Data Generation ==\n")
cat("Using mirai for parallel claim simulation...\n\n")

# =============================================================================
# 1. PARTNERS TABLE (n = 60)
# =============================================================================
cat("[1/3] Generating partners table...\n")

partner_types  <- c("Werkstatt", "Glaspartner", "Abschlepp", "Mietwagen", "Gutachter")
type_counts    <- c(25, 10, 8, 7, 10)  # sum = 60
swiss_regions  <- c("Zürich", "Bern", "Basel", "Luzern", "St.Gallen",
                    "Genf", "Lausanne", "Tessin")

# Fictitious workshop/partner names
ws_prefixes <- c("Auto", "Carrosserie", "Garage", "Zentrum", "Service",
                 "Repair", "Expert", "Total", "Quick", "Premium")
ws_suffixes <- c("AG", "GmbH", "Suisse", "Plus", "Pro", "24", "Direkt", "Swiss")

generate_name <- function(type, i) {
  if (type == "Glaspartner") return(paste0("GlasXpert ", i))
  if (type == "Abschlepp")   return(paste0("Pannenhilfe ", LETTERS[((i-1) %% 26)+1]))
  if (type == "Mietwagen")   return(paste0("MobilityFleet ", i))
  if (type == "Gutachter")   return(paste0("Sachverständigenbüro ", i))
  paste0(sample(ws_prefixes, 1), " ", sample(ws_suffixes, 1), " ", i)
}

partners <- tibble(
  partner_id = sprintf("P%03d", 1:60),
  type = rep(partner_types, type_counts),
  region = sample(swiss_regions, 60, replace = TRUE,
                  prob = c(0.25, 0.15, 0.12, 0.10, 0.10, 0.10, 0.10, 0.08)),
  capacity_class = sample(c("Small", "Medium", "Large"), 60, replace = TRUE,
                          prob = c(0.35, 0.45, 0.20)),
  # Latent quality factors (ground truth — not used in analysis directly)
  true_cost_factor    = pmax(0.55, pmin(1.45, rnorm(60, mean = 1.0, sd = 0.18))),
  true_speed_factor   = pmax(0.55, pmin(1.45, rnorm(60, mean = 1.0, sd = 0.20))),
  true_quality_factor = pmax(0.55, pmin(1.45, rnorm(60, mean = 1.0, sd = 0.15)))
) |>
  mutate(
    name = map2_chr(type, row_number(), generate_name),
    capacity_class = factor(capacity_class, levels = c("Small", "Medium", "Large")),
    type = factor(type, levels = partner_types)
  ) |>
  select(partner_id, name, type, region, capacity_class,
         true_cost_factor, true_speed_factor, true_quality_factor)

saveRDS(partners, "data/partners.rds")
cat("   partners.rds saved:", nrow(partners), "partners\n")

# =============================================================================
# 2. CLAIMS TABLE (n = 10,000) — parallel simulation via mirai
# =============================================================================
cat("[2/3] Simulating 10,000 claims in parallel (4 mirai workers)...\n")

# Parameters shared across workers
claim_types   <- c("glass", "body", "engine", "total_loss")
vehicle_classes <- c("economy", "mid", "premium")

# Cost baseline by claim type (CHF) — calibrated to Swiss market
# Sources: AXA Switzerland 2024 (avg collision ~CHF 3,400), Swiss Re, Carglass.ch
cost_base <- list(
  glass      = 500,    # avg chip repair + windshield replacement mix
  body       = 3400,   # matches AXA 2024 average for collision damage
  engine     = 4500,   # powertrain / mechanical
  total_loss = 20000   # vehicle replacement value minus salvage
)

# Steering ATE by claim type (multiplicative, ground truth)
steering_ate <- list(
  glass      = 0.80,  # -20%
  body       = 0.90,  # -10%
  engine     = 0.92,  # -8%
  total_loss = 1.00   # ~0%
)

# Vehicle class cost multiplier
vehicle_multiplier <- list(economy = 0.75, mid = 1.00, premium = 1.55)

# Steering propensity by claim type (baseline)
steering_prob <- list(glass = 0.75, body = 0.60, engine = 0.50, total_loss = 0.30)

# Workers: each generates n_per_worker claims, using a unique random seed
n_per_worker <- 2500L
n_workers    <- 4L

# Capture objects needed inside mirai workers
sim_params <- list(
  partners         = partners,
  swiss_regions    = swiss_regions,
  claim_types      = claim_types,
  vehicle_classes  = vehicle_classes,
  cost_base        = cost_base,
  steering_ate     = steering_ate,
  vehicle_multiplier = vehicle_multiplier,
  steering_prob    = steering_prob,
  n_per_worker     = n_per_worker
)

# Launch parallel workers
daemons(n_workers)

worker_results <- mirai_map(
  seq_len(n_workers),
  function(worker_id, params) {
    set.seed(worker_id * 1000L)
    n      <- params$n_per_worker
    p      <- params$partners
    offset <- (worker_id - 1L) * n

    # Claim date: uniform 2021-01-01 to 2024-12-31
    dates <- as.Date("2021-01-01") + sample(0:1460, n, replace = TRUE)

    # Claim characteristics
    cl_type    <- sample(params$claim_types, n, replace = TRUE,
                         prob = c(0.30, 0.40, 0.20, 0.10))
    veh_class  <- sample(params$vehicle_classes, n, replace = TRUE,
                         prob = c(0.40, 0.45, 0.15))
    veh_age    <- round(pmax(0, rnorm(n, mean = 5, sd = 3.5)), 1)
    region     <- sample(params$swiss_regions, n, replace = TRUE,
                         prob = c(0.25, 0.15, 0.12, 0.10, 0.10, 0.10, 0.10, 0.08))
    severity   <- pmax(0.01, pmin(0.99, rbeta(n, shape1 = 2, shape2 = 5)))

    # Steering flag: propensity depends on claim type + severity + region
    steer_prob_base <- vapply(cl_type, function(t) params$steering_prob[[t]], numeric(1))
    steer_prob      <- plogis(qlogis(steer_prob_base) +
                                0.5 * scale(severity)[,1] +
                                ifelse(region %in% c("Zürich", "Bern", "Basel"), 0.3, -0.1))
    steering_flag   <- rbinom(n, 1L, steer_prob)

    # Partner assignment for steered claims
    partner_id <- rep(NA_character_, n)
    for (i in which(steering_flag == 1L)) {
      # Prefer partners in same region, of matching type
      type_map <- list(
        glass      = "Glaspartner",
        body       = "Werkstatt",
        engine     = "Werkstatt",
        total_loss = "Gutachter"
      )
      ptype     <- type_map[[cl_type[i]]]
      eligible  <- p[p$type == ptype, ]
      if (nrow(eligible) == 0) eligible <- p
      # Weight by region match
      wts <- ifelse(eligible$region == region[i], 3, 1)
      partner_id[i] <- eligible$partner_id[sample.int(nrow(eligible), 1, prob = wts)]
    }

    # Compute repair cost with ground truth steering effect
    base_cost   <- vapply(cl_type, function(t) params$cost_base[[t]], numeric(1))
    veh_mult    <- vapply(veh_class, function(v) params$vehicle_multiplier[[v]], numeric(1))
    steer_mult  <- ifelse(
      steering_flag == 1L,
      vapply(cl_type, function(t) params$steering_ate[[t]], numeric(1)),
      1.0
    )
    # Partner-specific latent effect (if partner assigned)
    partner_cost_factor <- rep(1.0, n)
    for (i in which(!is.na(partner_id))) {
      idx <- which(p$partner_id == partner_id[i])
      if (length(idx) > 0) partner_cost_factor[i] <- p$true_cost_factor[idx]
    }

    repair_cost <- round(
      base_cost * veh_mult * (1 + 0.6 * severity) * steer_mult *
        partner_cost_factor * exp(rnorm(n, 0, 0.12)),
      0
    )

    # Duration in days
    base_duration <- c(glass = 3, body = 12, engine = 20, total_loss = 35)
    partner_speed_factor <- rep(1.0, n)
    for (i in which(!is.na(partner_id))) {
      idx <- which(p$partner_id == partner_id[i])
      if (length(idx) > 0) partner_speed_factor[i] <- p$true_speed_factor[idx]
    }
    duration_days <- round(pmax(1,
      vapply(cl_type, function(t) base_duration[[t]], numeric(1)) *
        partner_speed_factor *
        (0.8 - 0.2 * steering_flag) *        # steering speeds up by ~20%
        (1 + 0.3 * severity) *
        exp(rnorm(n, 0, 0.18))
    ), 0)

    # Reopening flag
    reopen_prob <- plogis(-3 + 0.8 * severity + 0.3 * (veh_age > 8) +
                            ifelse(steering_flag == 1L, -0.4, 0))
    reopening_flag <- rbinom(n, 1L, reopen_prob)

    # CSAT score (1-10)
    partner_quality_factor <- rep(1.0, n)
    for (i in which(!is.na(partner_id))) {
      idx <- which(p$partner_id == partner_id[i])
      if (length(idx) > 0) partner_quality_factor[i] <- p$true_quality_factor[idx]
    }
    csat_raw <- 7.5 +
      1.5 * (steering_flag == 1L) -
      2.0 * reopening_flag +
      (1 / partner_quality_factor - 1) * (-2) +
      rnorm(n, 0, 0.8)
    csat_score <- round(pmax(1, pmin(10, csat_raw)), 1)

    # ── Extended realistic variables ──────────────────────────────────────────
    # Coverage type (Teilkasko = partial casco; Vollkasko = comprehensive)
    # Premium vehicles and newer cars more likely to have Vollkasko
    vollkasko_prob <- plogis(-0.5 + 1.2*(veh_class == "premium") +
                               0.6*(veh_class == "mid") - 0.08*veh_age)
    coverage_type <- ifelse(rbinom(n, 1L, vollkasko_prob) == 1L,
                            "Vollkasko", "Teilkasko")

    # NCD / Bonus-Malus class (0 = highest risk/new driver, 5 = best)
    # Distribution skewed toward experienced: real Swiss NCD distribution
    ncd_class <- sample(0:5, n, replace = TRUE,
                        prob = c(0.05, 0.10, 0.15, 0.25, 0.25, 0.20))

    # Fault indicator
    fault_prob <- 0.50 + 0.10*(cl_type %in% c("engine","total_loss")) -
                  0.05*(ncd_class >= 4)
    fault_indicator <- ifelse(
      rbinom(n, 1L, fault_prob) == 1L, "at_fault",
      ifelse(rbinom(n, 1L, 0.3) == 1L, "third_party", "not_at_fault")
    )

    # Vehicle replacement value (CHF) — by class and age
    base_value <- c(economy = 18000, mid = 35000, premium = 75000)
    vehicle_value_chf <- round(
      vapply(veh_class, function(v) base_value[[v]], numeric(1)) *
        exp(-0.07 * veh_age) *  # depreciation ~7%/year
        exp(rnorm(n, 0, 0.12)), 0
    )

    # Deductible (CHF) — by vehicle class and coverage
    ded_base <- c(economy = 300, mid = 500, premium = 500)
    deductible_amount <- vapply(veh_class, function(v) ded_base[[v]], numeric(1)) +
      ifelse(coverage_type == "Vollkasko", 0, 200)

    # Notification delay (days between accident and report)
    # Log-normal; larger delays = fraud signal
    notification_delay_days <- round(pmax(0, pmin(30,
      rlnorm(n, meanlog = 0.3, sdlog = 0.9))))

    # Fraud flag — ~10% base rate matching SVV official estimate (1-in-10 claims)
    # Higher for: large notification delay, total loss, high severity, at-fault
    fraud_logit <- -2.2 +
      1.2 * (notification_delay_days > 7) +
      1.0 * (cl_type == "total_loss") +
      0.6 * (severity > 0.7) +
      0.5 * (fault_indicator == "at_fault") -
      0.4 * (ncd_class >= 4)
    fraud_flag <- rbinom(n, 1L, plogis(fraud_logit))

    data.frame(
      claim_id                = sprintf("C%06d", offset + seq_len(n)),
      date                    = dates,
      claim_type              = cl_type,
      vehicle_class           = veh_class,
      vehicle_age             = veh_age,
      region                  = region,
      severity_score          = round(severity, 4),
      coverage_type           = coverage_type,
      ncd_class               = ncd_class,
      fault_indicator         = fault_indicator,
      vehicle_value_chf       = vehicle_value_chf,
      deductible_amount       = deductible_amount,
      notification_delay_days = notification_delay_days,
      fraud_flag              = fraud_flag,
      steering_flag           = steering_flag,
      partner_id              = partner_id,
      repair_cost             = repair_cost,
      duration_days           = duration_days,
      reopening_flag          = reopening_flag,
      csat_score              = csat_score,
      stringsAsFactors = FALSE
    )
  },
  .args = list(params = sim_params)
)

# Collect results
claims_raw <- do.call(rbind, worker_results[])

claims <- claims_raw |>
  as_tibble() |>
  mutate(
    date            = as.Date(as.integer(date), origin = "1970-01-01"),
    claim_type      = factor(claim_type,    levels = c("glass","body","engine","total_loss")),
    vehicle_class   = factor(vehicle_class, levels = c("economy","mid","premium")),
    coverage_type   = factor(coverage_type, levels = c("Teilkasko","Vollkasko")),
    fault_indicator = factor(fault_indicator,
                             levels = c("at_fault","not_at_fault","third_party"))
  ) |>
  arrange(date, claim_id)

daemons(0)  # shut down workers

saveRDS(claims, "data/claims.rds")
cat("   claims.rds saved:", nrow(claims), "claims\n")
cat("   Steered claims:", sum(claims$steering_flag), sprintf("(%.1f%%)\n",
    100 * mean(claims$steering_flag)))
cat("   Avg repair cost (all):", round(mean(claims$repair_cost)), "CHF\n")
cat("   Avg repair cost (steered vs not):",
    round(mean(claims$repair_cost[claims$steering_flag == 1])), "vs",
    round(mean(claims$repair_cost[claims$steering_flag == 0])), "CHF\n")

# =============================================================================
# 3. PROCESS EVENTS TABLE (n ≈ 50,000)
# =============================================================================
cat("[3/3] Generating process events...\n")

event_types <- c("FNOL", "Inspection", "RepairStart", "RepairEnd", "Payment", "Closed")

# Each claim gets a sequence of events with realistic durations
generate_events <- function(claim_row) {
  cid      <- claim_row$claim_id
  d0       <- as.POSIXct(claim_row$date)
  dur      <- claim_row$duration_days
  is_glass <- claim_row$claim_type == "glass"

  # For glass: shorter pipeline
  if (is_glass) {
    offsets <- cumsum(c(0,
                        runif(1, 0.5, 2),     # Inspection
                        runif(1, 0.5, 1),     # RepairStart
                        runif(1, 1, 3),       # RepairEnd
                        runif(1, 0.5, 2),     # Payment
                        runif(1, 0.5, 1)))    # Closed
    offsets <- offsets * (dur / max(offsets))
  } else {
    offsets <- cumsum(c(0,
                        runif(1, 1, 4),
                        runif(1, 2, 6),
                        runif(1, 5, 15),
                        runif(1, 2, 7),
                        runif(1, 1, 3)))
    offsets <- offsets * (dur / max(offsets))
  }

  data.frame(
    claim_id   = cid,
    event_type = event_types,
    timestamp  = d0 + offsets * 86400,
    stringsAsFactors = FALSE
  )
}

set.seed(99)
# Use lapply for speed (events are lightweight)
events_list <- lapply(seq_len(nrow(claims)), function(i) generate_events(claims[i, ]))
process_events <- bind_rows(events_list) |>
  arrange(claim_id, timestamp)

saveRDS(process_events, "data/process_events.rds")
cat("   process_events.rds saved:", nrow(process_events), "events\n")

# =============================================================================
# 4. VERIFICATION
# =============================================================================
cat("\n== Verification ==\n")
cat("Files created:\n")
for (f in c("data/claims.rds", "data/partners.rds", "data/process_events.rds")) {
  cat(sprintf("  %s  (%.1f KB)\n", f, file.size(f) / 1024))
}

# Ground truth check: steering ATE by claim type
cat("\nGround truth steering effect by claim type:\n")
claims |>
  group_by(claim_type, steering_flag) |>
  summarise(avg_cost = mean(repair_cost), .groups = "drop") |>
  pivot_wider(names_from = steering_flag, values_from = avg_cost,
              names_prefix = "steered_") |>
  mutate(effect_pct = round((steered_1 / steered_0 - 1) * 100, 1)) |>
  print()

cat("\nData generation complete.\n")
