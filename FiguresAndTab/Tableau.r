############################################################
# STADIUM EXTRACTION – Monthly WBGT exceedance days
# Output: stadium_monthly_wbgt.csv (tidy)
# Dimensions: stadium × scenario × tod × month × threshold
############################################################

library(terra)
library(dplyr)
library(tidyr)
library(readr)

# ---- Paths ----
base_dir <- "C:/Data/WBGT/ENSEMBLE_Foot"
stadium_csv <- "C:/Data/WBGT/stadiums_clean_latlon_with_country.csv"   # <-- adapte si besoin
out_csv <- "C:/Data/WBGT/stadium_monthly_wbgt.csv"

files <- list(
  baseline = file.path(base_dir, "WBGT_mon_baseline_1991-2020_ensmean.nc"),
  ssp126   = file.path(base_dir, "WBGT_mon_ssp126_2071-2100_ensmean.nc"),
  ssp245   = file.path(base_dir, "WBGT_mon_ssp245_2071-2100_ensmean.nc"),
  ssp585   = file.path(base_dir, "WBGT_mon_ssp585_2071-2100_ensmean.nc")
)
stopifnot(all(file.exists(unlist(files))))

# ---- Stadiums ----
st <- readr::read_csv(stadium_csv, show_col_types = FALSE)

# REQUIRED columns: lon, lat, country, stadium_id (or name)
# If you have "Stadium" as name column, create an id:
if (!("stadium_id" %in% names(st))) {
  nm <- if ("stadium" %in% names(st)) "stadium" else if ("Stadium" %in% names(st)) "Stadium" else names(st)[1]
  st <- st %>% mutate(stadium_id = .data[[nm]])
}

stopifnot(all(c("lon","lat","country","stadium_id") %in% names(st)))
st <- st %>% mutate(lon = as.numeric(lon), lat = as.numeric(lat))
stopifnot(!any(is.na(st$lon) | is.na(st$lat)))

st <- st %>% distinct(stadium_id, .keep_all = TRUE)

pts <- vect(st, geom = c("lon","lat"), crs = "EPSG:4326")

# ---- Helper: extract monthly mean across years for one var ----
# returns: stadium_id, month, mean_ndays
extract_monthly_mean <- function(ncfile, varname, pts, st_meta) {
  r <- rotate(rast(ncfile, subds = varname))

  tt <- terra::time(r)

  # Force proper Date conversion
  if (inherits(tt, "Date")) {
    tt_date <- tt
  } else if (inherits(tt, "POSIXct") || inherits(tt, "POSIXt")) {
    tt_date <- as.Date(tt)
  } else if (is.numeric(tt)) {
    tt_date <- as.Date(tt, origin = "1970-01-01")
  } else {
    tt_date <- as.Date(tt)
  }

  mm <- as.integer(format(tt_date, "%m"))
  if (length(mm) != nlyr(r)) stop("Time vector length != number of layers")
  if (all(is.na(mm))) stop("Month extraction failed: time axis not convertible to Date")

  ex <- terra::extract(r, pts)
  if (!("ID" %in% names(ex))) names(ex)[1] <- "ID"

  layer_cols <- setdiff(names(ex), "ID")
  mat <- as.matrix(ex[, layer_cols, drop = FALSE])
  nL <- ncol(mat)
  if (nL != nlyr(r)) stop(sprintf("Extracted %d layers but raster has %d layers", nL, nlyr(r)))

  df_long <- data.frame(
  ID = rep(ex$ID, times = nL),
  layer_index = rep(1:nL, each = nrow(ex)),
  ndays = as.vector(mat)
  )

  df_long$stadium_id <- st_meta$stadium_id[df_long$ID]
  df_long$country    <- st_meta$country[df_long$ID]
  df_long$month      <- mm[df_long$layer_index]

  df_long %>%
    dplyr::group_by(stadium_id, country, month) %>%
    dplyr::summarise(mean_ndays = mean(ndays, na.rm = TRUE), .groups = "drop")
}

# ---- Variables to extract (bins we need) ----
bins <- c("28_30","30_32","ge32")
tods <- c("aft","eve")

varname <- function(tod, bin) paste0("ndays_wbgt_", tod, "_", bin)

# ---- Main extraction loop ----
all_out <- list()

# Ensure IDs line up with extract() "ID" (1..n)
st_meta <- st %>% mutate(ID = row_number())

for (scen in names(files)) {
  f <- files[[scen]]

  message("Extracting scenario: ", scen)

  # extract bins for aft/eve
  tmp <- list()
  for (tod in tods) {
    for (bin in bins) {
      v <- varname(tod, bin)
      message("  ", v)
      df <- extract_monthly_mean(f, v, pts, st_meta) %>%
        mutate(scenario = scen, tod = tod, bin = bin)
      tmp[[paste(tod, bin, sep="_")]] <- df
    }
  }

  scen_df <- bind_rows(tmp)

  # build thresholds ≥28/≥30/≥32 from bins (per stadium-month)
  scen_thr <- scen_df %>%
    select(stadium_id, country, scenario, tod, month, bin, mean_ndays) %>%
    pivot_wider(names_from = bin, values_from = mean_ndays) %>%
    mutate(
      ge28 = `28_30` + `30_32` + ge32,
      ge30 = `30_32` + ge32,
      ge32 = ge32
    ) %>%
    select(stadium_id, country, scenario, tod, month, ge28, ge30, ge32) %>%
    pivot_longer(cols = starts_with("ge"), names_to = "threshold", values_to = "ndays_mean") %>%
    mutate(threshold = recode(threshold, ge28 = 28L, ge30 = 30L, ge32 = 32L))

  all_out[[scen]] <- scen_thr
}

df_final <- bind_rows(all_out) %>%
  arrange(scenario, tod, threshold, country, stadium_id, month)

write_csv(df_final, out_csv)
message("Saved: ", out_csv)
