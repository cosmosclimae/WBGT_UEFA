# WBGT_UEFA

This repository contains the data processing scripts and derived indicators used in the study:

**“Heat stress as a structural constraint on the European football calendar”**

The project quantifies present and future heat-stress exposure at European professional football venues using the Wet Bulb Globe Temperature (WBGT) index and bias-corrected CMIP6 climate projections.

---

## 📌 Scientific context

Climate change is increasing heat-related risks for outdoor physical activity.
Professional football represents a unique case of repeated high-intensity exposure, tightly constrained by fixed seasonal calendars and scheduling rules.

This repository supports a system-level assessment of:
- cumulative heat-stress exposure during the European football season (July–May),
- differences between afternoon and evening match scheduling,
- seasonal redistribution of heat stress under future climate scenarios.

---

## 📊 Data sources

All input data are publicly available:

- **Climate data**
  - NASA NEX-GDDP-CMIP6 (0.25° daily resolution)
  - Variables: Tmax, Tmean, relative humidity, surface downward shortwave radiation
  - Scenarios: historical (1991–2020), SSP2–4.5, SSP5–8.5

- **Stadium locations**
  - Geographic coordinates compiled from publicly available sources
  - Primarily based on Wikipedia listings of professional clubs
  - Top 15 European first-division leagues (2024–2025 season)

---

## 🔬 Methods overview

- Heat stress is quantified using the **Wet Bulb Globe Temperature (WBGT)** index.
- WBGT is computed separately for:
  - **Afternoon conditions** (Tmax, peak radiation)
  - **Evening conditions** (Tmean, no direct solar radiation)
- Exposure metrics are expressed as:
  - cumulative number of days exceeding WBGT thresholds (28°C, 30°C, 32°C)
  - aggregated over an 11-month football period (July–May)

Detailed methodological descriptions are provided in the associated publication.

---

## 📁 Repository structure

```text
WBGT_UEFA/
│
├── data_raw/          # Raw input data (not tracked; see .gitignore)
├── data_processed/    # Processed indicators used in figures
├── scripts/           # R scripts for WBGT computation and analysis
│   ├── wbgt_utils.R
│   ├── compute_exceedance_days.R
│   ├── seasonal_aggregation.R
│   └── plotting_figures.R
│
├── figures/           # Generated figures for the manuscript
├── supplementary/     # Supplementary figures and tables
├── environment/       # Session info, package versions
│
├── LICENSE
├── CITATION.cff
└── README.md
