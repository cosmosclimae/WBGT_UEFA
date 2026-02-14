############################################################
# FIGURE 1 – WBGT > 30 °C – Europe – SSP5-8.5
# Continuous Europe maps (0.25° grid)
# Metric: mean number of days per football season (July–May)
# WBGT > 30 = ndays_30_32 + ndays_ge32
# Periods: baseline 1991–2020, future 2071–2100 (SSP5–8.5), and difference
# Averaging: 29 complete Jul–May seasons within each 30-year window
############################################################

library(terra)
library(ggplot2)
library(patchwork)
library(scales)

# -----------------------------
# Optional borders: sf + NaturalEarth (fallback to maps)
# -----------------------------
use_sf <- TRUE
ok_sf <- try(requireNamespace("sf", quietly = TRUE), silent = TRUE)
ok_ne <- try(requireNamespace("rnaturalearth", quietly = TRUE), silent = TRUE)

if (inherits(ok_sf, "try-error") || !isTRUE(ok_sf) || inherits(ok_ne, "try-error") || !isTRUE(ok_ne)) {
  use_sf <- FALSE
}

if (use_sf) {
  library(sf)
  library(rnaturalearth)
  europe_sf <- rnaturalearth::ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")
} else {
  if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")
  if (!requireNamespace("mapdata", quietly = TRUE)) install.packages("mapdata")
  library(maps)
  library(mapdata)
}

# -----------------------------
# Paths
# -----------------------------
iscen="ssp245"
base_dir <- "C:/Data/WBGT/ENSEMBLE_Foot"
f_hist <- file.path(base_dir, "WBGT_mon_baseline_1991-2020_ensmean.nc")
f_fut  <- file.path(base_dir, paste("WBGT_mon_",iscen,"_2071-2100_ensmean.nc",sep=""))

stopifnot(file.exists(f_hist), file.exists(f_fut))

# -----------------------------
# Europe extent (lon/lat)
# -----------------------------
ext_eu <- ext(-12, 40, 35, 72)

# -----------------------------
# Helper: align geometry of x to ref (fix "extents do not match")
# -----------------------------
align_to <- function(x, ref) {
  if (!terra::compareGeom(x, ref, stopOnError = FALSE)) {
    x2 <- try(terra::extend(x, ref), silent = TRUE)
    if (inherits(x2, "try-error")) x2 <- x
    x2 <- terra::resample(x2, ref, method = "near")  # counts => nearest
    return(x2)
  }
  x
}

# -----------------------------
# Helper: load a NetCDF variable by name (subdataset)
# -----------------------------
load_var <- function(ncfile, varname) {
  r <- terra::rast(ncfile, subds = varname)
  if (is.null(terra::time(r))) {
    stop(paste("No time axis for", varname, "in", ncfile))
  }
  r
}

# -----------------------------
# Compute mean seasonal (Jul–May) WBGT>30 days
# Season y = Jul–Dec(y) + Jan–May(y+1)
# WBGT>30 = (30–32) + (≥32)
# 30-year window => 29 complete seasons
# -----------------------------
compute_seasonmean_gt30 <- function(ncfile, y_start, y_end_inclusive, ext_eu) {

  aft30_32 <- terra::rotate(load_var(ncfile, "ndays_wbgt_aft_30_32"))
  aftge32  <- terra::rotate(load_var(ncfile, "ndays_wbgt_aft_ge32"))
  eve30_32 <- terra::rotate(load_var(ncfile, "ndays_wbgt_eve_30_32"))
  evege32  <- terra::rotate(load_var(ncfile, "ndays_wbgt_eve_ge32"))

  # Align all to one reference grid
  aftge32  <- align_to(aftge32,  aft30_32)
  eve30_32 <- align_to(eve30_32, aft30_32)
  evege32  <- align_to(evege32,  aft30_32)

  tt <- terra::time(aft30_32)  # should be monthly dates
  yy <- as.integer(format(tt, "%Y"))
  mm <- as.integer(format(tt, "%m"))

  seasons <- y_start:(y_end_inclusive - 1)  # e.g., 1991..2019 => 29 seasons

  sum_aft <- NULL
  sum_eve <- NULL
  n <- 0

  for (y in seasons) {
    idx <- c(which(yy == y   & mm %in% 7:12),
             which(yy == y+1 & mm %in% 1:5))

    if (length(idx) != 11) next

    sa <- sum((aft30_32[[idx]] + aftge32[[idx]]))
    se <- sum((eve30_32[[idx]] + evege32[[idx]]))

    if (is.null(sum_aft)) {
      sum_aft <- sa
      sum_eve <- se
    } else {
      sum_aft <- sum_aft + sa
      sum_eve <- sum_eve + se
    }
    n <- n + 1
  }

  if (n == 0) stop("No complete Jul–May seasons found in requested window.")

  list(
    aft = terra::crop(sum_aft / n, ext_eu),
    eve = terra::crop(sum_eve / n, ext_eu),
    nseasons = n
  )
}

# -----------------------------
# Plot function (sf preferred, fallback maps)
# -----------------------------
# -----------------------------

plot_map <- function(r, title, palette, limits = NULL, legend_title = "Days per football season") {

  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  val_col <- setdiff(names(df), c("x","y"))[1]
  names(df)[names(df) == val_col] <- "value"

  p <- ggplot(df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(
      colours = palette,
      limits = limits,
      oob = scales::squish,
      name = legend_title
    ) +
    labs(title = title) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      plot.title = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 10)
    )

  if (use_sf) {
    p <- p +
      geom_sf(data = europe_sf, fill = NA, color = "black",
              linewidth = 0.3, inherit.aes = FALSE) +
      coord_sf(xlim = xlim_eu, ylim = ylim_eu, expand = FALSE)
  } else {
    p <- p +
      borders("worldHires", xlim = xlim_eu, ylim = ylim_eu,
              colour = "black", linewidth = 0.3) +
      coord_cartesian(xlim = xlim_eu, ylim = ylim_eu, expand = FALSE)
  }

  p
}

# -----------------------------
# Compute baseline / future / difference
# -----------------------------
hist <- compute_seasonmean_gt30(f_hist, 1991, 2020, ext_eu)
fut  <- compute_seasonmean_gt30(f_fut,  2071, 2100, ext_eu)

message(sprintf("Baseline seasons averaged: %d", hist$nseasons))
message(sprintf("Future seasons averaged:    %d", fut$nseasons))

diff <- list(
  aft = fut$aft - hist$aft,
  eve = fut$eve - hist$eve
)

# -----------------------------
# Better Europe bbox (fills space more)
# -----------------------------
xlim_eu <- c(-25, 45)
ylim_eu <- c(34, 72)


# -----------------------------
# Compute common limits (same scale) for present+future
# per time-of-day (recommended)
# -----------------------------
max_aft <- max(global(hist$aft, "max", na.rm=TRUE)[1,1],
               global(fut$aft,  "max", na.rm=TRUE)[1,1])

max_eve <- max(global(hist$eve, "max", na.rm=TRUE)[1,1],
               global(fut$eve,  "max", na.rm=TRUE)[1,1])

lim_aft <- c(0, max_aft)
lim_eve <- c(0, max_eve)

# Difference: symmetric around 0
max_diff <- max(abs(global(diff$aft, "max", na.rm=TRUE)[1,1]),
                abs(global(diff$aft, "min", na.rm=TRUE)[1,1]),
                abs(global(diff$eve, "max", na.rm=TRUE)[1,1]),
                abs(global(diff$eve, "min", na.rm=TRUE)[1,1]))

lim_diff <- c(-max_diff, max_diff)

# -----------------------------
# Palettes
# -----------------------------
pal_abs  <- c("#ffffcc", "#fd8d3c", "#bd0026")
pal_diff <- c("#2c7bb6", "#f7f7f7", "#d7191c")

# -----------------------------
# Build panels with LOCKED scales
# -----------------------------
p1 <- plot_map(hist$aft, "Afternoon\n1991–2020", pal_abs, limits = lim_aft, legend_title = "Days per football season")
p2 <- plot_map(hist$eve, "Evening\n1991–2020",   pal_abs, limits = lim_eve, legend_title = "Days per football season")

p3 <- plot_map(fut$aft,  "Afternoon\n2071–2100 (SSP5–8.5)", pal_abs, limits = lim_aft, legend_title = "Days per football season")
p4 <- plot_map(fut$eve,  "Evening\n2071–2100 (SSP5–8.5)",   pal_abs, limits = lim_eve, legend_title = "Days per football season")

p5 <- plot_map(diff$aft, "Afternoon\nDifference", pal_diff, limits = lim_diff, legend_title = "Δ days per football season")
p6 <- plot_map(diff$eve, "Evening\nDifference",   pal_diff, limits = lim_diff, legend_title = "Δ days per football season")

# -----------------------------
# Layout: reduce whitespace, keep legends
# (If you want even less clutter: we can collect guides, but Aft/Eve have different scales)
# -----------------------------
fig1 <- (p1 | p2) / (p3 | p4) / (p5 | p6)

out_png <- paste("C:/Data/WBGT/Figures/Figure1_WBGTgt30_Europe_",iscen,".png")
ggsave(out_png, fig1, width = 12, height = 12, dpi = 300)
message(paste("Saved:", out_png))




############################################################
# FIGURES 2–4 (A+B) — Country-level stadium distributions
# Days per football season (Jul–May), stadium locations only
# For each threshold (28/30/32) and each SSP (126/245/585):
#   Panel A = Afternoon, Panel B = Evening
# Output PNGs ready for SI/publication
############################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)

# -----------------------------
# Inputs / outputs
# -----------------------------
in_csv  <- "C:/Data/WBGT/stadium_monthly_wbgt.csv"
out_dir <- "C:/Data/WBGT/Figures"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Settings
# -----------------------------
football_months <- c(7,8,9,10,11,12,1,2,3,4,5)
thresholds <- c(28, 30, 32)
scenarios  <- c("ssp126", "ssp245", "ssp585")   # baseline always included

# Colors (baseline grey + scenario colors)
col_map <- c(
  baseline = "grey70",
  ssp126   = "#4575b4",
  ssp245   = "#fdae61",
  ssp585   = "#d73027"
)

# Nice labels
scen_lab <- c(
  baseline = "1991–2020 (baseline)",
  ssp126   = "SSP1–2.6 (2071–2100)",
  ssp245   = "SSP2–4.5 (2071–2100)",
  ssp585   = "SSP5–8.5 (2071–2100)"
)
tod_lab <- c(aft = "Afternoon", eve = "Evening")

# -----------------------------
# Load
# -----------------------------
df <- readr::read_csv(in_csv, show_col_types = FALSE)

# sanity
stopifnot(all(c("stadium_id","country","scenario","tod","month","threshold","ndays_mean") %in% names(df)))

# -----------------------------
# Build season metric: sum Jul–May of monthly mean days (30y mean)
# 1 row = stadium × scenario × tod × threshold
# -----------------------------
df_season <- df %>%
  filter(month %in% football_months) %>%
  group_by(stadium_id, country, scenario, tod, threshold) %>%
  summarise(days_season = sum(ndays_mean, na.rm = TRUE), .groups = "drop")

# -----------------------------
# FIX a consistent country order (same across all figs)
# Recommended: order by baseline median at threshold=30, afternoon
# -----------------------------
country_order <- df_season %>%
  filter(scenario == "baseline", threshold == 30, tod == "aft") %>%
  group_by(country) %>%
  summarise(med = median(days_season, na.rm = TRUE), .groups = "drop") %>%
  arrange(med) %>%
  pull(country)

# fallback if something weird
if (length(country_order) == 0) country_order <- sort(unique(df_season$country))

df_season <- df_season %>%
  mutate(country = factor(country, levels = country_order),
         scenario = factor(scenario, levels = c("baseline","ssp126","ssp245","ssp585")),
         tod = factor(tod, levels = c("aft","eve")),
         threshold = as.integer(threshold))

# -----------------------------
# Plot function: one scenario comparison (baseline vs sspX) for one threshold & one tod
# -----------------------------
plot_box <- function(data, threshold_val, tod_val, scen_val) {

  dat <- data %>%
    filter(threshold == threshold_val,
           tod == tod_val,
           scenario %in% c("baseline", scen_val))

  # ensure both levels exist even if missing (rare)
  dat$scenario <- factor(dat$scenario, levels = c("baseline", scen_val))

  ggplot(dat, aes(x = country, y = days_season, fill = scenario)) +
    geom_boxplot(width = 0.65, outlier.size = 0.7, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = col_map[c("baseline", scen_val)],
                      labels = scen_lab[c("baseline", scen_val)],
                      drop = FALSE) +
    labs(
      x = NULL,
      y = "Days per football season (Jul–May)",
      fill = NULL,
      title = paste0(tod_lab[[as.character(tod_val)]])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(size = 13, face = "bold"),
      legend.position = "bottom"
    )
}

# -----------------------------
# Build + save Figures 2/3/4 for each SSP
# -----------------------------
for (scen in scenarios) {

  for (thr in thresholds) {

    pA <- plot_box(df_season, threshold_val = thr, tod_val = "aft", scen_val = scen)
    pB <- plot_box(df_season, threshold_val = thr, tod_val = "eve", scen_val = scen)

    # Combine A+B (same legend)
    fig <- pA + pB + plot_layout(guides = "collect") &
      theme(legend.position = "bottom")

    # Figure numbering: 2=28, 3=30, 4=32
    fig_no <- ifelse(thr == 28, 2, ifelse(thr == 30, 3, 4))

    # Add a global title (clean SI)
    fig <- fig + plot_annotation(
      title = paste0("Figure ", fig_no, " — WBGT \u2265 ", thr, "\u00B0C (", scen_lab[[scen]], ")"),
      theme = theme(plot.title = element_text(size = 14, face = "bold"))
    )

    out_png <- file.path(out_dir, paste0("Fig", fig_no, "_WBGTge", thr, "_", scen, "_AftEve.png"))
    ggsave(out_png, fig, width = 14, height = 7, dpi = 300)

    message("Saved: ", out_png)
  }
}

# Build season days per stadium for threshold 30
df_season30 <- df %>%
  filter(month %in% football_months, threshold == 30, tod %in% c("aft","eve")) %>%
  group_by(stadium_id, country, scenario, tod) %>%
  summarise(days_season = sum(ndays_mean, na.rm = TRUE), .groups = "drop")
df_season30 <- df_season30 %>%
  mutate(country = factor(country, levels = country_order))

# Compute Δ per stadium (needs both aft & eve)
df_delta <- df_season30 %>%
  tidyr::pivot_wider(names_from = tod, values_from = days_season) %>%
  mutate(delta = aft - eve) %>%
  filter(!is.na(delta))

plot_delta <- function(dat, scen) {
  d <- dat %>% filter(scenario %in% c("baseline", scen))
  d$scenario <- factor(d$scenario, levels = c("baseline", scen))

  ggplot(d, aes(x = country, y = delta, fill = scenario)) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_boxplot(width = 0.65, outlier.size = 0.7,
                 position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = col_map[c("baseline", scen)],
                      labels = scen_lab[c("baseline", scen)],
                      drop = FALSE) +
    labs(x = NULL,
         y = expression(Delta~"days per football season (Jul–May)"),
         fill = NULL,
         title = paste0("Benefit of evening scheduling (", scen_lab[[scen]], ")")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.title = element_text(size = 13, face = "bold"))
}

for (scen in scenarios) {
  p <- plot_delta(df_delta, scen)
  out_png <- file.path(out_dir, paste0("Fig5_DeltaAftMinusEve_WBGTge30_", scen, ".png"))
  ggsave(out_png, p, width = 14, height = 7, dpi = 300)
  message("Saved: ", out_png)
}

###Figure6
# ---- Settings ----
countries_sel <- c("The Netherlands","England","Germany","Italy","Spain")
football_months <- c(7,8,9,10,11,12,1,2,3,4,5)
month_levels <- football_months
month_labels <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May")

thresholds <- c(28,30,32)
ssps <- c("ssp126","ssp245","ssp585")
tods <- c("aft","eve")   # main: c("aft") ; SI: keep both if you want

tod_label <- c(aft = "Afternoon", eve = "Evening")
ssp_label <- c(ssp126="SSP1–2.6", ssp245="SSP2–4.5", ssp585="SSP5–8.5")

# Consistent colors across the paper
col_map <- c(
  baseline = "grey40",
  ssp126   = "#4575b4",  # blue
  ssp245   = "#fdae61",  # orange
  ssp585   = "#d73027"   # red
)

scen_labels <- c(
  baseline = "1991–2020",
  ssp126   = "SSP1–2.6 (2071–2100)",
  ssp245   = "SSP2–4.5 (2071–2100)",
  ssp585   = "SSP5–8.5 (2071–2100)"
)

# ---- Load ----
df <- readr::read_csv(in_csv, show_col_types = FALSE)
stopifnot(all(c("stadium_id","country","scenario","tod","month","threshold","ndays_mean") %in% names(df)))

# ---- Plot builder ----
make_fig6 <- function(df, ssp_use, thr_use, tod_use) {

  dat <- df %>%
    filter(country %in% countries_sel,
           threshold == thr_use,
           tod == tod_use,
           month %in% football_months,
           scenario %in% c("baseline", ssp_use)) %>%
    group_by(country, scenario, month) %>%
    summarise(stadium_days = sum(ndays_mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      month   = factor(month, levels = month_levels, labels = month_labels),
      country = factor(country, levels = countries_sel),
      scenario = factor(scenario, levels = c("baseline", ssp_use))
    )

  if (nrow(dat) == 0) return(NULL)

  ggplot(dat, aes(x = month, y = stadium_days, group = scenario, color = scenario)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    facet_wrap(~ country, ncol = 3, scales = "free_y") +
    scale_color_manual(
      values = col_map,
      breaks = c("baseline", ssp_use),
      labels = scen_labels[c("baseline", ssp_use)],
      name = NULL
    ) +
    labs(
      x = NULL,
      y = paste0("Total stadium-days with WBGT ≥ ", thr_use, "°C (per month)"),
      title = paste0("Monthly heat-stress exposure across professional football venues — ",
                     tod_label[[tod_use]], " — ", ssp_label[[ssp_use]]),
      subtitle = "Football period: July–May (monthly means over 30 years)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold")
    )
}

# ---- Loop + save ----
for (ssp_use in ssps) {
  for (thr_use in thresholds) {
    for (tod_use in tods) {

      p <- make_fig6(df, ssp_use, thr_use, tod_use)
      if (is.null(p)) {
        message("Skipping (no data): ", ssp_use, " thr=", thr_use, " tod=", tod_use)
        next
      }

      out_png <- file.path(out_dir,
                           paste0("Fig6_Monthly_StadiumDays_WBGTge", thr_use, "_", tod_use, "_", ssp_use, ".png"))
      ggsave(out_png, p, width = 14, height = 8.5, dpi = 300)
      message("Saved: ", out_png)
    }
  }
}
