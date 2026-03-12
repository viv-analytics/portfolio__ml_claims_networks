# Motor Claims Partner Network — ML Analysis

> **Audience:** Leiter Geschäftspartner-Netzwerke, Allianz Suisse
> **Stack:** R · Quarto · mirai · tidyverse · lme4 · XGBoost · WeightIt · easystats

A portfolio-grade R/Quarto project demonstrating end-to-end machine learning analysis of a
business partner network in motor insurance claims processing.
No real data is used — a realistic synthetic simulation with embedded ground-truth effects
enables full reproducibility and face-validity checks.

---

## Analytical Pipeline

![Pipeline](assets/pipeline.png)

---

## Project Structure

```
ml_analysis_networks/
├── _quarto.yml                      ← Quarto website project config
├── index.qmd                        ← Executive summary / landing page
├── dashboard.qmd                    ← Interactive Quarto Dashboard (Shiny)
├── R/
│   ├── 00_simulate_data.R           ← Parallel data generation (mirai)
│   ├── utils_kpi.R                  ← KPI calculation helpers
│   ├── utils_viz.R                  ← ggplot2 Allianz theme + plot functions
│   └── utils_models.R               ← Modelling helpers (O/E, shrinkage, propensity)
├── analysis/
│   ├── 01_descriptive.qmd           ← Stage 1: KPI Dashboard & EDA
│   ├── 02_partner_comparison.qmd    ← Stage 2: Fair adjusted benchmarking
│   ├── 03_causal_steering.qmd       ← Stage 3: Causal effect of network steering
│   └── 04_partner_ranking.qmd       ← Stage 4: Composite scoring + XGBoost
└── assets/
    ├── allianz.scss                 ← Custom Quarto SCSS theme
    └── pipeline.png                 ← Pipeline diagram
```

---

## Four Analytical Stages

| Stage | File | Methods |
|-------|------|---------|
| 1. Descriptive KPIs | `01_descriptive.qmd` | Network overview, process funnel, KPI heatmap, regional maps |
| 2. Fair Comparison | `02_partner_comparison.qmd` | Case-mix GLM, O/E ratios, lme4 partial pooling, funnel plot |
| 3. Causal Steering | `03_causal_steering.qmd` | IPW/AIPW (doubly robust), CATE by segment, marginaleffects |
| 4. Partner Ranking | `04_partner_ranking.qmd` | Composite KPI score, XGBoost EV model, case-to-partner lookup |

---

## Key Findings (Ground Truth)

The synthetic data embeds the following effects that the analysis recovers:

- **Overall steering benefit**: −12% average repair cost (network vs. free choice)
- **Heterogeneous effects by claim type**:
  - Glass repairs: −20% cost reduction
  - Body damage: −10%
  - Engine damage: −8%
  - Total loss: ~0% (no benefit, commoditised valuation)
- **Top partners** correlate with latent `true_cost_factor` — Stage 4 ranking recovers this

---

## Parallel Execution with mirai

This project uses `mirai` for parallel execution at multiple points:

- **Data simulation**: 4 workers generate 2,500 claims each (embarrassingly parallel)
- **Stage 2**: GLMs fitted per claim type in parallel + 1,000-draw bootstrap CIs
- **Stage 3**: AIPW bootstrap (500 resamples) + CATE models per segment in parallel
- **Stage 4**: XGBoost 5-fold CV folds fitted in parallel

---

## How to Reproduce

```r
# 1. Install dependencies
renv::restore()   # or install.packages() for each listed package

# 2. Generate synthetic data (run once)
source("R/00_simulate_data.R")

# 3. Render the full Quarto website
quarto::quarto_render()
```

**Required R packages:** mirai, tidyverse, data.table, lubridate, lme4, WeightIt, MatchIt,
marginaleffects, easystats (performance, parameters, effectsize, see, report), ggeffects,
sjPlot, xgboost, tidymodels, ggplot2, plotly, gt, patchwork, ggrepel, leaflet, sf, igraph,
tidygraph, ggraph, shiny

---

## Data Schema

### `partners.rds` (n = 60)
| Column | Type | Description |
|--------|------|-------------|
| partner_id | chr | Unique ID (`P001`–`P060`) |
| name | chr | Fictitious partner name |
| type | fct | Werkstatt / Glaspartner / Abschlepp / Mietwagen / Gutachter |
| region | chr | One of 8 Swiss regions |
| capacity_class | fct | Small / Medium / Large |
| true_cost_factor | dbl | Latent cost multiplier (ground truth, not in analysis) |
| true_speed_factor | dbl | Latent speed multiplier |
| true_quality_factor | dbl | Latent quality multiplier |

### `claims.rds` (n = 10,000)
| Column | Type | Description |
|--------|------|-------------|
| claim_id | chr | Unique claim ID |
| date | date | Claim date (2021–2024) |
| claim_type | fct | glass / body / engine / total_loss |
| vehicle_class | fct | economy / mid / premium |
| vehicle_age | dbl | Vehicle age in years |
| region | chr | Swiss region |
| severity_score | dbl | Continuous complexity measure (0–1) |
| steering_flag | int | 1 = routed to network partner |
| partner_id | chr | Partner ID (NA if not steered) |
| repair_cost | dbl | Final claim cost in CHF |
| duration_days | dbl | Days from report to closure |
| reopening_flag | int | 1 = claim was reopened |
| csat_score | dbl | Customer satisfaction (1–10) |

### `process_events.rds` (n ≈ 50,000)
| Column | Type | Description |
|--------|------|-------------|
| claim_id | chr | Foreign key to claims |
| event_type | chr | FNOL / Inspection / RepairStart / RepairEnd / Payment / Closed |
| timestamp | POSIXct | Event timestamp |

---

*This project is for portfolio and analytical demonstration purposes only.*
*All data is fully synthetic; no real customer or partner information is used.*
