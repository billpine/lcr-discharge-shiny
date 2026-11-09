# Little Colorado River Discharge Data - USGS Water Data API version
# Rim-readable revision with experimental forecast, November 2026.
# Requires: shiny, dataRetrieval, ggplot2, plotly, dplyr, lubridate, purrr,
#           scales, withr, tibble
# USGS parameter 00060 = discharge (cfs); daily statistic 00003 = mean.
#
# App folder must contain:
#   app.R                    this file
#   R/forecast_helpers.R     copied from the flood pipeline (Shiny loads R/ automatically)
#   model_cam_gp.rds         copied from the flood pipeline's app/ folder (step 06)
#   .Renviron                API_USGS_PAT=... (never committed to Git)
#
# Page layout, top to bottom:
#  0. "Message for field crews": one inReach-length line (160 characters)
#     with a Copy button, rebuilt every refresh. Above the sidebar so it is
#     first on phones too.
#  1. Gage Pool headline: flow now, trend, crew levels (600 / 3,000 cfs).
#  2. "What this means": plain-language paragraph built from the numbers.
#  3. EXPERIMENTAL forecast: Cameron peak -> Gage Pool and camp arrival.
#     Same logic as 14_live_forecast.R; shown only when Cameron has a flood;
#     collapses to one line once the forecast window has passed.
#  4. "About this forecast": static, collapsed methods note and disclaimer.
#  5. Gage table, upstream to downstream. Winslow (mainstem) and Moenkopi
#     Wash (tributary) are both upstream of the Cameron gage, so water from
#     either shows up at Cameron and then the Gage Pool.
#  6. Plots: continuous (with crew levels) and daily mean.
#
# 11/2/2026 (FWS correction): Moenkopi Wash joins the LCR ABOVE the Cameron
# gage, not below. Moenkopi floods pass Cameron and are covered by the
# Cameron forecast once they arrive there, same as Winslow floods.
#
# 11/2/2026: a second Cameron pulse on the falling limb of an earlier flood
# (2,715 cfs at 00:00, down to 607 at 09:00, back to 2,532 by 13:00) was
# not detected. New floods are now measured from the low AFTER the biggest
# peak in the window, and the rise threshold scales with flow.
#
# 11/3/2026: upstream gages are reported when HIGH, not only when rising.
# The record Winslow flood (13,400 cfs Mon 11/2) was falling slowly, so the
# page said "Nothing is rising upstream" while that water was still on its
# way to Cameron. Flag levels in upstream_flag are provisional until the
# wet/dry channel analysis of which Winslow floods reach Cameron.
#
# 11/3/2026 (evening, live event check with 14c_event_check.R):
#  - SUSTAINED state. Winslow water raised Cameron slowly (1,060 to 2,490 cfs
#    over 16 h) with no peak, so the "peak" was always the newest hour and
#    the Gage Pool window slid later every refresh. When Cameron is still
#    rising slowly (less than sustained_pct over the sustained_look_h hours
#    before its peak, and at least sustained_look_h hours after its base) and
#    the Gage Pool is already above 600 cfs, the page drops the peak window
#    and reports sustained high water plus Cameron -> Gage Pool travel time.
#    Checked against 11/2-11/3 hourly data: on only from Tue 11/3 12:00; never
#    on for the Mon 11/2 pulses. sustained_pct is PROVISIONAL.
#  - Displayed peaks use hourly means, as the model does. A single bad 15-min
#    Moenkopi reading (16,900 cfs between 9,290 and 10,500) was being shown
#    as the Moenkopi peak; the hourly peak was 11,700.
#  - Upstream plateau. Winslow sat near 13,400 cfs for about 24 h, so "peaked
#    at <time>" was arbitrary. A gage that stays within plateau_frac of its
#    peak for plateau_h or longer is described by that span instead.
#  - Camp table says "Gauge Pool", matching the rest of the page (11/6:
#    everything now says "Gage").
#  - 14_live_forecast.R has the same sustained rule (11/3 evening).
#
# 11/4/2026: "Message for field crews" box at the top. Wording above 600 cfs
# at the Gage Pool is "caution when crossing" (not "do not cross"); above
# 3,000 it also names the crew alert. Same wording page-wide (headline,
# "What this means", sustained table, sidebar, plot label). Banner and
# disclaimer now say "for information purposes only". Optional sentences in
# the crew message are dropped if the line would pass 160 characters.
#
# 11/4/2026 (evening, matches 14_live_forecast.R):
#  - walk_back_base(): the start of a rise is found by walking back from the
#    peak to the first earlier pulse. Once Monday's 18:00 peak left the 48-h
#    window, the plain lookback minimum jumped from Tue 01:00 to Mon 09:00
#    ("risen slowly for 57 h").
#  - CRESTED counts as peaked: Cameron max at least crest_h old, at or below
#    crest_drop of it, and falling over sustained_look_h. The Winslow flood
#    sat near its Wed 06:00 crest for 12+ h, so the page said "no peak yet"
#    while Cameron fell. Crew message and "What this means" say "crested".
#
# 11/6/2026 (11/5-6 Winslow flood):
#  - "Gage" throughout (was "Gauge"), matching the pipeline and report.
#  - Slow trends. The 3-h rule (10% and 20 cfs) needs ~380 cfs in 3 h at
#    3,800 cfs, so the Gage Pool read "steady" while it climbed 560 cfs in
#    16 h. When the 3-h rule says steady, a 12-h change of more than 5% and
#    100 cfs, with the 3-h change not against it, reads "Rising slowly" /
#    "Falling slowly". Only fast "Rising" counts as Cameron rising or as a
#    Moenkopi pulse; either kind of rise flags an upstream gage.
#  - Upstream note under the forecast gives each gage's trend and recent
#    peak, not "high or rising" (Winslow was falling).
#  - Sustained crew message ends with when Cameron's newest hour reaches the
#    Gage Pool (model travel time), e.g. "At GP ~10:15.", instead of "GP
#    stays high while Cameron holds."
#
# 11/9/2026 (text cleanup after the 11/8 Cameron crest; crew message unchanged):
#  - Window passed: "What this means" no longer says the flood "has passed"
#    with "no distinct rise". The Gage Pool was already ~7,500 cfs when the
#    window opened and peaked at 8,165 cfs at Sun 21:00, 2 h after the window
#    closed. The one-line forecast block now gives the highest Gage Pool flow
#    since the window opened and when, relative to the window
#    (gp_since_text). "What this means" covers Cameron now, how long the Gage
#    Pool stays high, and what starts a new forecast.
#  - Window passed: no "clears from the page at" sentence, and the new-
#    forecast rule is stated as a rise from the low since the crest
#    (rerise_sentence), not a fixed cfs that sat above the crest on a
#    recession. The fixed-cfs trigger_sentence still runs when there is no
#    current flood.
#  - Stay-high estimate for crested floods with the Gage Pool above 600
#    (stay_high_sentence): hours for Cameron to fall to the level that puts
#    the Gage Pool at 3,000 (or 600), at Cameron's 6-h rate of fall, scaled
#    by the current Gage Pool / Cameron ratio, plus model travel time. A
#    straight-line recession at today's rate is a lower bound ("at least").
#  - Upstream peaks older than the gage's longest travel time to Cameron
#    (max_lag_h in upstream_flag) are reported as having had time to reach
#    Cameron. The Wed 11/4 Winslow plateau was still written as on its way
#    on Mon 11/9.
#  - Trigger sentence (no-flood state) names the low it is measured from.

library(shiny)
library(dataRetrieval)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(purrr)

# ---- Settings -------------------------------------------------------------
refresh_minutes <- 15     # auto-refresh interval
days_back       <- 10     # plotting window
fetch_days      <- 21     # continuous pull; forecast needs 14-d prior volume
stale_hours     <- 3      # "Gage reporting: Delayed" if last reading older
trend_hours     <- 3      # trend = now vs. this many hours ago
trend_pct       <- 0.10   # change must exceed 10% ...
trend_min_cfs   <- 20     # ... and 20 cfs to count as Rising/Falling
trend_slow_h    <- 12     # slow trend: now vs. this many hours ago, ...
trend_slow_pct  <- 0.05   # ... change must exceed 5% ...
trend_slow_min_cfs <- 100 # ... and 100 cfs to count as Rising/Falling slowly
no_cross_cfs    <- 600    # crew level at Gage Pool: caution when crossing at camps
alert_cfs       <- 3000   # crew level at Gage Pool: alert

# Forecast settings, matching 00_setup.R and 14_live_forecast.R
cam_min_peak   <- 100     # peak_rules, Cameron
cam_min_rise   <- 50      # peak_rules, Cameron
cam_rel_rise   <- 0.10    # a rise must also be 10% of its starting low, so
                          # recession wiggles at high flow don't count
lookback_h     <- 48      # base flow search before the peak
event_h        <- 48      # current Cameron flood = highest hour in last 48 h
peaked_drop    <- 0.8     # peaked once flow < 80% of max ...
peaked_min_h   <- 2       # ... and the max is at least 2 h old
crest_h        <- 6       # OR crested: max at least 6 h old, ...
crest_drop     <- 0.95    # ... flow at or below 95% of it, and falling over
                          # sustained_look_h (broad floods). PROVISIONAL
season_months  <- 4:10    # model fit on April-October floods

# Sustained high water (PROVISIONAL, set from the 11/3/2026 event): Cameron
# not peaked, rose less than sustained_pct over the sustained_look_h hours
# before its peak, at least sustained_look_h hours after its base, and the
# Gage Pool above no_cross_cfs. A fast pulse fails the rate test; the start
# of any new pulse fails the base test.
sustained_look_h <- 6
sustained_pct    <- 0.25

# Upstream plateau: within plateau_frac of the peak for plateau_h or longer
plateau_frac <- 0.9
plateau_h    <- 6

# Rise needed above a low of q cfs to count as a flood
rise_needed <- function(q) pmax(cam_min_rise, cam_rel_rise * q)

# Upstream gages are reported when rising, or when their peak in the last
# look_h hours reached flag_cfs. look_h covers the pairing window to Cameron
# (00_setup.R pathways). flag_cfs is PROVISIONAL: which Winslow floods reach
# Cameron depends on channel wetness; to be set from the model.
# max_lag_h: longest usual travel time to Cameron (upper end of lag_txt).
# A peak older than this has had time to reach Cameron.
upstream_flag <- tibble::tribble(
  ~Site,      ~flag_cfs, ~look_h, ~max_lag_h, ~lag_txt,
  "Winslow",  1000,      120,     90,         "35 to 90 h (1.5 to 4 days)",
  "Moenkopi", 500,       48,      48,         "3 to 48 h (typically about 11 h)"
)

tz_az <- "US/Arizona"

m <- readRDS("model_cam_gp.rds")
ante_d <- if (is.null(m$ante_days)) 14 else m$ante_days

# ---- Gages (flow order), colors, line types -------------------------------
# Winslow (mainstem) and Moenkopi Wash (tributary) both enter above Cameron;
# Cameron -> Gage Pool on the mainstem.
gages <- data.frame(
  monitoring_location_id = paste0(
    "USGS-", c("09400350", "09401260", "09402000", "09402300")
  ),
  Site = c("Winslow", "Moenkopi", "Cameron", "LCR Gage Pool"),
  stringsAsFactors = FALSE
)

site_colors <- c("Winslow" = "#E69F00", "Moenkopi" = "#009E73",
                 "Cameron" = "#56B4E9", "LCR Gage Pool" = "#000000")
site_lines  <- c("Winslow" = "dashed", "Moenkopi" = "longdash",
                 "Cameron" = "dotdash", "LCR Gage Pool" = "solid")
site_shapes <- c("Winslow" = 16, "Moenkopi" = 15,
                 "Cameron" = 17, "LCR Gage Pool" = 18)

lcr_scales <- list(
  scale_color_manual(values = site_colors, name = "Gage site"),
  scale_linetype_manual(values = site_lines, name = "Gage site"),
  theme_minimal(base_size = 13)
)

# Crew levels drawn on the continuous plot. Labels sit at the left edge
# (x_min) because ggplotly does not place text at x = -Inf.
crew_lines <- function(x_min) list(
  geom_hline(yintercept = c(no_cross_cfs, alert_cfs),
             linetype = "dotted", colour = "grey40"),
  annotate("text", x = x_min, y = c(no_cross_cfs, alert_cfs),
           label = c(sprintf("Gage Pool %s: caution when crossing",
                             scales::comma(no_cross_cfs)),
                     sprintf("Gage Pool %s: crew alert",
                             scales::comma(alert_cfs))),
           hjust = 0, vjust = -0.4, size = 3, colour = "grey30")
)

clean_legend <- function(p) {
  p$x$data <- purrr::map(p$x$data, function(tr) {
    if (!is.null(tr$name)) tr$name <- sub("^\\(([^,]+),.*\\)$", "\\1", tr$name)
    tr
  })
  p
}

site_factor <- function(x) factor(x, levels = gages$Site)
fmt_cfs <- function(x) ifelse(is.na(x), "-", scales::comma(round(x)))
# Day name plus numeric date so "Fri" is never ambiguous, e.g. Fri 10/30/2026 15:00
fmt_t   <- function(x) ifelse(is.na(x), "-", format(x, "%a %m/%d/%Y %H:%M", tz = tz_az))

# ---- Empty templates ------------------------------------------------------
empty_daily <- function() {
  data.frame(Date = as.Date(character()), Discharge = numeric(),
             Site = site_factor(character()))
}
empty_continuous <- function() {
  data.frame(dateTime = as.POSIXct(character(), tz = tz_az),
             Discharge = numeric(), Site = site_factor(character()))
}

# ---- USGS fetchers --------------------------------------------------------
fetch_daily <- function(start_date, end_date) {
  x <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = gages$monitoring_location_id,
    parameter_code = "00060",
    statistic_id = "00003",
    time = c(as.character(start_date), as.character(end_date)),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_daily())

  x %>%
    dplyr::filter(.data$parameter_code == "00060",
                  .data$statistic_id == "00003") %>%
    dplyr::left_join(gages, by = "monitoring_location_id") %>%
    dplyr::transmute(
      Date = as.Date(.data$time),
      Discharge = suppressWarnings(as.numeric(as.character(.data$value))),
      Site = site_factor(.data$Site)
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge)) %>%
    dplyr::arrange(.data$Site, .data$Date)
}

fetch_continuous <- function(start_date, end_time) {
  start_time <- as.POSIXct(paste(start_date, "00:00:00"), tz = tz_az)
  x <- dataRetrieval::read_waterdata_continuous(
    monitoring_location_id = gages$monitoring_location_id,
    parameter_code = "00060",
    time = c(start_time, end_time),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_continuous())

  x %>%
    dplyr::filter(.data$parameter_code == "00060") %>%
    dplyr::left_join(gages, by = "monitoring_location_id") %>%
    dplyr::transmute(
      dateTime = lubridate::with_tz(as.POSIXct(.data$time, tz = "UTC"),
                                    tzone = tz_az),
      Discharge = suppressWarnings(as.numeric(as.character(.data$value))),
      Site = site_factor(.data$Site)
    ) %>%
    dplyr::filter(!is.na(.data$Site), !is.na(.data$Discharge),
                  !is.na(.data$dateTime)) %>%
    dplyr::arrange(.data$Site, .data$dateTime)
}

safe_daily      <- purrr::safely(fetch_daily,      otherwise = empty_daily())
safe_continuous <- purrr::safely(fetch_continuous, otherwise = empty_continuous())

# ---- Per-gage summary -----------------------------------------------------
# Hourly means for every gage. Displayed peaks use these (as the model
# does), so a single bad 15-min reading cannot become "the peak".
hourly_all <- function(continuous) {
  continuous %>%
    dplyr::mutate(time = lubridate::floor_date(dateTime, "hour")) %>%
    dplyr::group_by(Site, time) %>%
    dplyr::summarise(flow = mean(Discharge), .groups = "drop") %>%
    dplyr::arrange(Site, time)
}

# Fast trend over trend_hours; if that is steady, slow trend over
# trend_slow_h, as long as the last trend_hours are not moving the other way.
trend_label <- function(now, before, before_slow) {
  thresh <- pmax(trend_min_cfs, trend_pct * before)
  d      <- now - before
  d_slow <- now - before_slow
  slow   <- !is.na(d_slow) & abs(d_slow) > pmax(trend_slow_min_cfs, trend_slow_pct * before_slow)
  dplyr::case_when(
    is.na(now) | is.na(before)     ~ "-",
    d >  thresh                    ~ "Rising",
    d < -thresh                    ~ "Falling",
    slow & d_slow > 0 & d >= 0     ~ "Rising slowly",
    slow & d_slow < 0 & d <= 0     ~ "Falling slowly",
    TRUE                           ~ "Steady"
  )
}
is_rising <- function(trend) grepl("^Rising", trend)

value_hours_before <- function(t, q, hours) {
  target <- as.numeric(max(t)) - hours * 3600
  gap    <- abs(as.numeric(t) - target)
  if (min(gap) > 3600) NA_real_ else q[which.min(gap)]
}

site_summary <- function(continuous, pulled) {
  if (nrow(continuous) == 0L) {
    return(data.frame(Site = site_factor(character()),
                      last_time = as.POSIXct(character(), tz = tz_az),
                      now_cfs = numeric(), before_cfs = numeric(),
                      before_slow_cfs = numeric(), peak24_cfs = numeric(), hours_old = numeric(),
                      trend = character()))
  }
  pk24 <- hourly_all(continuous) %>%
    group_by(Site) %>%
    filter(time > max(time) - 24 * 3600) %>%
    summarise(peak24_cfs = max(flow), .groups = "drop")

  continuous %>%
    group_by(Site) %>%
    summarise(
      last_time  = max(dateTime),
      now_cfs    = Discharge[which.max(dateTime)],
      before_cfs = value_hours_before(dateTime, Discharge, trend_hours),
      before_slow_cfs = value_hours_before(dateTime, Discharge, trend_slow_h),
      .groups = "drop"
    ) %>%
    left_join(pk24, by = "Site") %>%
    mutate(hours_old = as.numeric(difftime(pulled, last_time, units = "hours")),
           trend     = trend_label(now_cfs, before_cfs, before_slow_cfs))
}

# ---- Forecast (same steps as 14_live_forecast.R) --------------------------
hourly <- function(continuous, site) {
  continuous %>%
    dplyr::filter(Site == site) %>%
    dplyr::mutate(time = lubridate::floor_date(dateTime, "hour")) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(flow = mean(Discharge), .groups = "drop") %>%
    dplyr::arrange(time)
}

# Find the current Cameron flood in the event window.
# 1. Biggest hour in the window (pk_max).
# 2. Lowest hour AFTER it (trough): the recession low a new pulse starts from.
#    Empty when the biggest hour is the latest hour (still rising).
# 3. If Cameron has since risen enough above that trough, the new pulse is
#    the flood, with the trough as its base. This covers both a second pulse
#    on the falling limb (11/2/2026) and an old tail at the window edge
#    masking a small new rise (11/1/2026).
# 4. Otherwise the flood is pk_max, with base from walk_back_base().

# Base (start of the rise) for a peak: walk back in time from the peak and
# stop where flow climbs back up by rise_needed() above the lowest flow seen
# so far (an earlier pulse); the base is the lowest hour before that point.
# A plain lookback minimum reached past the low between two pulses once the
# earlier peak left the event window (11/4/2026: Tue 01:00 -> Mon 09:00).
walk_back_base <- function(cam, pk_time) {
  seg <- cam %>%
    dplyr::filter(time <= pk_time, time > pk_time - lookback_h * 3600) %>%
    dplyr::arrange(dplyr::desc(time)) %>%
    dplyr::mutate(run_min = cummin(flow),
                  climb   = flow - run_min >= rise_needed(run_min))
  stop_i <- which(seg$climb)[1]                  # NA when no earlier pulse
  keep   <- if (is.na(stop_i)) seg else seg[seq_len(stop_i - 1), ]
  keep %>%
    dplyr::slice_min(flow, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(time, n = 1, with_ties = FALSE) %>%
    dplyr::select(time, flow)
}

find_cam_flood <- function(cam, last_t) {
  win    <- dplyr::filter(cam, time > last_t - event_h * 3600)
  pk_max <- dplyr::slice_max(win, flow, n = 1, with_ties = FALSE)

  trough <- win %>%
    dplyr::filter(time > pk_max$time) %>%
    dplyr::slice_min(flow, n = 1, with_ties = TRUE) %>%
    dplyr::slice_max(time, n = 1, with_ties = FALSE)

  pk_new <- if (nrow(trough) == 1L) {
    win %>%
      dplyr::filter(time >= trough$time) %>%
      dplyr::slice_max(flow, n = 1, with_ties = FALSE)
  } else win[0, ]

  new_flood <- nrow(pk_new) == 1L &&
    pk_new$flow >= cam_min_peak &&
    pk_new$flow - trough$flow >= rise_needed(trough$flow)

  pk   <- if (new_flood) pk_new else pk_max
  base <- if (new_flood) trough else walk_back_base(cam, pk$time)

  # Low a future rise would be measured from: the post-peak trough when
  # there is one, else the plain recent low.
  post_low <- nrow(trough) == 1L
  low_ref  <- if (post_low) trough$flow else
    min(cam$flow[cam$time > last_t - lookback_h * 3600])

  list(pk = pk, base = base, low_ref = low_ref, post_low = post_low)
}

run_forecast <- function(continuous, now) {
  cam <- hourly(continuous, "Cameron")
  gp  <- hourly(continuous, "LCR Gage Pool")
  if (nrow(cam) == 0L) return(list(status = "no_data"))

  last_t  <- max(cam$time)
  cam_now <- cam$flow[cam$time == last_t]

  # Latest 15-min Cameron reading (the hourly mean lags a sharp rise)
  cam_raw <- continuous %>%
    dplyr::filter(Site == "Cameron") %>%
    dplyr::slice_max(dateTime, n = 1, with_ties = FALSE) %>%
    dplyr::pull(Discharge)

  ev   <- find_cam_flood(cam, last_t)
  pk   <- ev$pk
  base <- ev$base

  out <- list(cam_now = cam_now, cam_raw = cam_raw, last_t = last_t, pk = pk, now = now,
              pk_age_h    = as.numeric(difftime(now, pk$time, units = "hours")),
              clears_at   = pk$time + event_h * 3600,   # old peak leaves the search window
              low_recent  = ev$low_ref,
              low_is_post = ev$post_low,
              trigger_cfs = max(cam_min_peak, ev$low_ref + rise_needed(ev$low_ref)))

  has_flood <- pk$flow >= cam_min_peak &&
    pk$flow - base$flow >= rise_needed(base$flow)
  if (!has_flood) return(c(out, status = "no_flood"))

  cam_at <- function(t) {
    v <- cam$flow[cam$time == t]
    if (length(v) == 1L) v else NA_real_
  }
  look           <- sustained_look_h * 3600
  out$cam_change <- cam_now - cam_at(last_t - look)

  # Peaked: below 80% of the max (pulses), or crested: max at least crest_h
  # old, now at or below crest_drop of it, and falling (broad Winslow floods
  # stayed above 80% for 12+ h on 11/4/2026).
  pk_old_h    <- as.numeric(difftime(last_t, pk$time, units = "hours"))
  dropped     <- cam_now < peaked_drop * pk$flow && pk_old_h >= peaked_min_h
  out$crested <- !dropped && pk_old_h >= crest_h && cam_now <= crest_drop * pk$flow &&
    isTRUE(out$cam_change < 0)
  out$peaked  <- dropped || out$crested

  # Sustained high water: slow rise with no peak, Gage Pool already high.
  pre_pk    <- cam_at(pk$time - look)
  gp_raw    <- continuous %>%
    dplyr::filter(Site == "LCR Gage Pool") %>%
    dplyr::slice_max(dateTime, n = 1, with_ties = FALSE) %>%
    dplyr::pull(Discharge)
  out$gp_now        <- if (length(gp_raw) == 1L) gp_raw else NA_real_
  out$cam_rising_h  <- as.numeric(difftime(last_t, base$time, units = "hours"))
  out$sustained <- !out$peaked &&
    as.numeric(difftime(pk$time, base$time, units = "secs")) >= look &&
    !is.na(pre_pk) && pre_pk > 0 &&
    (pk$flow - pre_pk) / pre_pk < sustained_pct &&
    isTRUE(out$gp_now > no_cross_cfs)

  vol <- prior_volume_af(cam$time, cam$flow, base$time, days = ante_d)
  if (is.na(vol)) return(c(out, status = "patchy"))

  fc <- withr::with_seed(1, forecast_gp(m, pk$flow, pk$time, vol))
  s  <- summarise_forecast(fc)

  # What the Gage Pool did inside the 80% forecast window. "Max since the
  # Cameron peak" is misleading when the Gage Pool is still receding from an
  # earlier, larger flood, so look for a rise within the window instead.
  gp_win <- if (is.na(s$arrive_early)) gp[0, ] else {
    dplyr::filter(gp, time >= s$arrive_early, time <= s$arrive_late)
  }
  # Gage Pool from the window opening to now (hourly means). Used once the
  # window has passed: a broad flood can peak at the Gage Pool after the
  # window closes (11/8/2026: window 15:00-19:00, peak 8,165 cfs at 21:00).
  gp_since <- if (is.na(s$arrive_early)) gp[0, ] else {
    dplyr::filter(gp, time >= s$arrive_early)
  }

  c(out, list(
    status        = if (out$sustained) "sustained" else "forecast",
    vol           = vol,
    s             = s,
    cf            = camp_forecast(m, fc, pk$time),
    gp_win        = gp_win,
    gp_since      = gp_since,
    extrap        = extrapolation_note(m, pk$flow),
    out_of_season = !lubridate::month(pk$time) %in% season_months
  ))
}

# Did the Gage Pool rise inside the forecast window? Uses the same
# rise threshold as the Trend column (10% and 20 cfs).
gp_window_check <- function(w) {
  start <- w[1, ]
  top   <- dplyr::slice_max(w, flow, n = 1, with_ties = FALSE)
  list(start = start, last = w[nrow(w), ], top = top,
       rose  = top$flow - start$flow > max(trend_min_cfs, trend_pct * start$flow))
}

gp_window_text <- function(w, s, now) {
  if (is.na(s$arrive_early))  return("-")
  if (now < s$arrive_early)   return("Window not open yet")
  if (nrow(w) == 0L)          return("No Gage Pool readings in the window")

  g <- gp_window_check(w)
  so_far <- if (now <= s$arrive_late) "So far: " else ""
  if (g$rose) {
    sprintf("%sROSE from %s to %s cfs, peak at %s",
            so_far, fmt_cfs(g$start$flow), fmt_cfs(g$top$flow), fmt_t(g$top$time))
  } else {
    sprintf("%sNo rise (%s to %s cfs)",
            so_far, fmt_cfs(g$start$flow), fmt_cfs(g$last$flow))
  }
}

# "A new forecast will be made once Cameron rises above X cfs ..."
trigger_sentence <- function(f) {
  if (is.null(f$trigger_cfs)) return("")
  low_txt <- if (isTRUE(f$low_is_post)) "Cameron's low since the last peak" else "Cameron's recent low"
  why <- if (f$trigger_cfs > cam_min_peak) {
    sprintf(" (%s cfs above %s, %s cfs)",
            fmt_cfs(f$trigger_cfs - f$low_recent), low_txt, fmt_cfs(f$low_recent))
  } else ""
  sprintf("A new forecast will be made once Cameron rises above %s cfs%s.",
          fmt_cfs(f$trigger_cfs), why)
}

# Window passed: highest Gage Pool flow (hourly mean) since the window
# opened, and whether it came inside or after the window. Replaces "No rise"
# / "no distinct rise", which read as nothing happening when the Gage Pool
# was already high and peaked after the window (11/8/2026).
gp_since_text <- function(f) {
  g <- f$gp_since
  if (is.null(g) || nrow(g) == 0L) {
    return("There are no Gage Pool readings since the forecast window opened.")
  }
  first <- g[1, ]
  top   <- dplyr::slice_max(g, flow, n = 1, with_ties = FALSE)
  if (top$time == first$time) {
    return(sprintf("The Gage Pool has not risen since the forecast window opened (%s cfs at %s).",
                   fmt_cfs(first$flow), fmt_t(first$time)))
  }
  when <- if (top$time > f$s$arrive_late) {
    sprintf(", %.0f h after it closed", as.numeric(difftime(top$time, f$s$arrive_late, units = "hours")))
  } else ", inside the window"
  sprintf("Highest Gage Pool flow since the window opened: %s cfs at %s%s.",
          fmt_cfs(top$flow), fmt_t(top$time), when)
}

# "Sun 11/08/2026 15:00 to 19:00"; full date on the end only when it differs
fmt_span <- function(a, b) {
  same_day <- as.Date(a, tz = tz_az) == as.Date(b, tz = tz_az)
  sprintf("%s to %s", fmt_t(a),
          if (isTRUE(same_day)) format(b, "%H:%M", tz = tz_az) else fmt_t(b))
}

# Window passed: what would start a new forecast. On a recession the low
# since the crest is about the current flow and keeps dropping, so a fixed
# cfs trigger reads as Cameron going back up to a number it just left
# (11/9/2026: "rises above 10,574 cfs ... low of 9,612"). State the rule.
rerise_sentence <- function(f) {
  if (is.null(f$low_recent) || is.na(f$low_recent)) return("")
  amt <- if (rise_needed(f$low_recent) > cam_min_rise) {
    sprintf("%d%%", round(100 * cam_rel_rise))
  } else sprintf("%s cfs", fmt_cfs(cam_min_rise))
  sprintf("A new forecast starts if Cameron rises again by %s or more from its lowest flow since the %s.",
          amt, if (isTRUE(f$crested)) "crest" else "peak")
}

# Crested flood, Gage Pool still above 600: how long it stays above the
# crew level it is over (3,000 if above it, else 600). The Gage Pool reaches
# that level when Cameron falls to level x Cameron / Gage Pool (today's
# ratio), about one travel time later. Hours to get there use Cameron's
# 6-h rate of fall as a straight line; recessions slow down, so this is a
# lower bound ("at least"). Blank for pulses, rising Cameron, or under 12 h.
stay_high_sentence <- function(f) {
  gp <- f$gp_now
  if (!isTRUE(f$crested) || !isTRUE(f$cam_change < 0) || !isTRUE(gp > no_cross_cfs)) return("")
  level   <- if (gp >= alert_cfs) alert_cfs else no_cross_cfs
  rate    <- -f$cam_change / sustained_look_h               # cfs per hour
  cam_for <- level * f$cam_now / gp
  h <- (f$cam_now - cam_for) / rate + dplyr::coalesce(f$s$lag_mid_h, 0)
  if (!is.finite(h) || h < 12) return("")
  dur <- if (h >= 36) {
    sprintf("at least %s days", format(floor(h / 12) / 2))
  } else sprintf("at least %.0f h", 12 * floor(h / 12))
  sprintf("At Cameron's current rate of fall, the Gage Pool is likely to stay above %s cfs for %s, and longer if Cameron's fall slows, as recessions usually do.",
          scales::comma(level), dur)
}

# Sustained-state helpers
signed_cfs <- function(x) {
  if (length(x) != 1L || is.na(x)) "-" else paste0(if (x >= 0) "+" else "", fmt_cfs(x))
}
# Name Winslow as the likely source when it is high upstream and Moenkopi is
# not rising (Moenkopi floods are short and sharp, not slow rises).
sustained_source <- function(up) {
  if ("Winslow" %in% up$Site && !any(up$Site == "Moenkopi" & up$trend == "Rising")) {
    ", most likely Winslow water"
  } else ""
}
travel_txt <- function(s) {
  if (is.na(s$lag_mid_h)) return("an unknown time")
  sprintf("about %.0f h (likely %.0f to %.0f h)", s$lag_mid_h, s$lag_early_h, s$lag_late_h)
}

# Note under the forecast when upstream gages are high or rising: each
# gage's own trend and recent peak, e.g. "Winslow is falling slowly (now
# 2,840 cfs; peak about 13,400 cfs on Mon 11/02)."
upstream_note <- function(up) {
  if (nrow(up) == 0L) return(NULL)
  each <- sprintf("%s is %s (now %s cfs; peak about %s cfs on %s).",
                  up$Site,
                  dplyr::if_else(up$trend %in% c("-", NA), "trend unknown", tolower(up$trend)),
                  fmt_cfs(up$now_cfs), fmt_cfs(up$peak_cfs),
                  format(up$peak_time, "%a %m/%d", tz = tz_az))
  tags$p(tags$strong("Note: "),
         paste(c(each, "Water still on its way shows up as a new or continued Cameron rise when it arrives."),
               collapse = " "))
}

# When the latest 15-min reading has passed the trigger but the hourly mean
# has not, say so instead of the plain trigger sentence.
pending_sentence <- function(f) {
  if (!is.null(f$cam_raw) && length(f$cam_raw) == 1L &&
      !is.null(f$trigger_cfs) && f$cam_raw >= f$trigger_cfs) {
    sprintf("Cameron's latest reading (%s cfs) has passed the %s cfs trigger; the forecast starts once the hourly average does, usually within the hour.",
            fmt_cfs(f$cam_raw), fmt_cfs(f$trigger_cfs))
  } else trigger_sentence(f)
}

# Upstream gages (both above Cameron) that are rising now, or peaked high
# recently (upstream_flag). One row per gage with its recent peak.
upstream_active <- function(continuous, summ) {
  if (nrow(continuous) == 0L || nrow(summ) == 0L) {
    return(tibble::tibble(Site = character(), peak_cfs = numeric(),
                          peak_time = as.POSIXct(character(), tz = tz_az),
                          flag_cfs = numeric(), max_lag_h = numeric(), lag_txt = character(),
                          now_cfs = numeric(), trend = character(),
                          last_time = as.POSIXct(character(), tz = tz_az)))
  }
  # Hourly means (as the model uses), so one bad 15-min reading is not the peak.
  # near_from / near_to: first and last hour within plateau_frac of the peak.
  hourly_all(continuous) %>%
    dplyr::mutate(Site = as.character(Site)) %>%
    dplyr::inner_join(upstream_flag, by = "Site") %>%
    dplyr::group_by(Site) %>%
    dplyr::filter(time > max(time) - look_h * 3600) %>%
    dplyr::summarise(peak_cfs  = max(flow),
                     peak_time = time[which.max(flow)],
                     near_from = min(time[flow >= plateau_frac * max(flow)]),
                     near_to   = max(time[flow >= plateau_frac * max(flow)]),
                     last_hour = max(time),
                     flag_cfs  = dplyr::first(flag_cfs),
                     max_lag_h = dplyr::first(max_lag_h),
                     lag_txt   = dplyr::first(lag_txt),
                     .groups   = "drop") %>%
    dplyr::mutate(plateau = as.numeric(difftime(near_to, near_from, units = "hours")) >= plateau_h,
                  on_plateau_now = plateau & near_to >= last_hour - 3600) %>%
    dplyr::left_join(dplyr::mutate(summ, Site = as.character(Site)) %>%
                       dplyr::select(Site, now_cfs, trend, last_time),
                     by = "Site") %>%
    dplyr::filter(is_rising(trend) | peak_cfs >= flag_cfs) %>%
    dplyr::arrange(match(Site, gages$Site))
}

# "Winslow peaked at 13,400 cfs (Mon 11/02/2026 12:00) and is now 11,900 cfs,
# falling. Winslow water usually peaks at Cameron 35 to 90 h ... later."
upstream_sentence <- function(up) {
  if (nrow(up) == 0L) return("")
  # Peaks are hourly means. A long, flat top (plateau) is described by its
  # span, because the single highest hour on it is arbitrary.
  # reached: the peak (end of a plateau) left this gage more than max_lag_h
  # ago, so that water has had time to reach Cameron (11/9/2026: the Wed
  # 11/4 Winslow plateau was still written as on its way on Mon 11/9).
  up <- up %>%
    dplyr::mutate(
      at_peak   = as.numeric(difftime(last_time, peak_time, units = "hours")) <= 1,
      trend_txt = dplyr::if_else(trend == "-", "trend unknown", tolower(trend)),
      top_end   = dplyr::if_else(plateau, near_to, peak_time),
      reached   = !at_peak &
        as.numeric(difftime(last_time, top_end, units = "hours")) > max_lag_h,
      peak_txt  = dplyr::if_else(
        plateau,
        sprintf("about %s cfs, %s to %s", fmt_cfs(peak_cfs), fmt_t(near_from), fmt_t(near_to)),
        sprintf("%s cfs, %s", fmt_cfs(peak_cfs), fmt_t(peak_time))),
      txt = dplyr::case_when(
        reached ~
          sprintf("%s water usually takes %s to reach Cameron, so the %s flood (%s) has had time to get there. %s is now %s cfs, %s.",
                  Site, lag_txt, Site, peak_txt, Site, fmt_cfs(now_cfs), trend_txt),
        at_peak & is_rising(trend) ~
          sprintf("%s is %s (%s cfs).", Site, tolower(trend), fmt_cfs(now_cfs)),
        on_plateau_now ~
          sprintf("%s has been near its peak of about %s cfs since %s and is now %s cfs, %s.",
                  Site, fmt_cfs(peak_cfs), fmt_t(near_from), fmt_cfs(now_cfs), trend_txt),
        plateau ~
          sprintf("%s rose to about %s cfs and stayed near that from %s to %s; it is now %s cfs, %s.",
                  Site, fmt_cfs(peak_cfs), fmt_t(near_from), fmt_t(near_to),
                  fmt_cfs(now_cfs), trend_txt),
        TRUE ~
          sprintf("%s peaked at %s cfs (%s) and is now %s cfs, %s.",
                  Site, fmt_cfs(peak_cfs), fmt_t(peak_time), fmt_cfs(now_cfs), trend_txt)),
      txt = dplyr::case_when(
        reached ~ txt,
        plateau ~ paste(txt, sprintf(
          "%s water usually takes %s to reach Cameron, so Cameron can stay high long after the %s peak.",
          Site, lag_txt, Site)),
        TRUE ~ paste(txt, sprintf(
          "%s water usually peaks at Cameron %s after the %s peak.",
          Site, lag_txt, Site))))
  c(up$txt,
    if (any(!up$reached)) "Upstream water enters the forecast once it reaches Cameron.") %>%
    paste(collapse = " ")
}

# ---- Message for field crews ----------------------------------------------
# One inReach line (max_chars) from the same numbers as the rest of the page:
# Gage Pool now and its level, then the forecast state. Examples:
# "LCR 04:00: GP 3,820 cfs rising slowly. ABOVE 3,000 alert, caution when
#  crossing. Cameron 4,138 cfs (+142 in 6h), no peak, Winslow water. At GP ~10:15."
# "LCR 07:30: GP 3,050 cfs rising. ABOVE 3,000 alert, caution when crossing.
#  Cameron peak 3,680 cfs Mon 18:00. GP peak Mon 23:30-Tue 04:30 (97% chance)."
# The last piece (`extra`) is dropped if the full line is too long.
crew_inreach <- function(summ, f, up, max_chars = 160) {
  gp <- dplyr::filter(summ, Site == "LCR Gage Pool")
  if (nrow(gp) == 0L) return("LCR: Gage Pool not reporting. Check upstream gages.")

  hm  <- function(x) format(x, "%H:%M", tz = tz_az)
  dhm <- function(x) format(x, "%a %H:%M", tz = tz_az)

  trend <- if (gp$trend == "-") "" else paste0(" ", tolower(gp$trend))
  lead  <- sprintf("LCR %s: GP %s cfs%s.", hm(gp$last_time), fmt_cfs(gp$now_cfs), trend)
  level <- if (gp$now_cfs >= alert_cfs) {
    sprintf("ABOVE %s alert, caution when crossing.", scales::comma(alert_cfs))
  } else if (gp$now_cfs > no_cross_cfs) "Caution when crossing." else ""

  win_open <- f$status == "forecast" && !is.na(f$s$arrive_late) && f$now <= f$s$arrive_late
  extra    <- ""
  main <- if (f$status == "sustained") {
    # When Cameron's newest hour reaches the Gage Pool (model travel time)
    extra <- if (is.na(f$s$lag_mid_h)) "" else
      sprintf("At GP ~%s.", hm(round_15(f$last_t + f$s$lag_mid_h * 3600)))
    sprintf("Cameron %s cfs (%s in %dh), no peak%s.",
            fmt_cfs(f$cam_now), signed_cfs(f$cam_change), sustained_look_h,
            if (nzchar(sustained_source(up))) ", Winslow water" else "")
  } else if (win_open && f$peaked) {
    sprintf("Cameron peak %s cfs %s. GP peak %s-%s (%d%% chance).",
            fmt_cfs(f$pk$flow), dhm(f$pk$time), dhm(f$s$arrive_early),
            dhm(f$s$arrive_late), round(100 * f$s$p_arrive))
  } else if (win_open) {
    sprintf("Cameron %s cfs and rising. GP peak est %s-%s, may be earlier.",
            fmt_cfs(f$pk$flow), dhm(f$s$arrive_early), dhm(f$s$arrive_late))
  } else if (f$status == "forecast" && isTRUE(f$peaked)) {
    # Peaked, forecast window already closed: say what Cameron is doing
    if ("Winslow" %in% up$Site) extra <- "Winslow still high."
    sprintf("Cameron %s %s cfs %s, now %s cfs (%s in %dh).",
            if (isTRUE(f$crested)) "crested" else "peaked", fmt_cfs(f$pk$flow),
            dhm(f$pk$time), fmt_cfs(f$cam_now), signed_cfs(f$cam_change), sustained_look_h)
  } else if (nrow(up) > 0L) {
    sprintf("%s high upstream, watch Cameron.", paste(up$Site, collapse = " and "))
  } else "Nothing high upstream."

  cands <- list(c(lead, level, main, extra), c(lead, level, main)) %>%
    purrr::map_chr(~ paste(.x[nzchar(.x)], collapse = " "))
  ok <- cands[nchar(cands) <= max_chars]
  if (length(ok) > 0L) ok[1] else cands[length(cands)]
}

level_phrase <- function(q) {
  dplyr::case_when(
    q >= alert_cfs   ~ sprintf("ABOVE the %s cfs crew alert level, caution when crossing", scales::comma(alert_cfs)),
    q > no_cross_cfs ~ sprintf("above %s cfs, caution when crossing", scales::comma(no_cross_cfs)),
    TRUE             ~ sprintf("below %s cfs", scales::comma(no_cross_cfs))
  )
}

# Plain-language "What this means" paragraph, rebuilt every refresh from the
# same numbers shown on the page. States facts only; no extra prediction.
interpretation <- function(summ, f, up) {
  pick <- function(site) dplyr::filter(summ, Site == site)
  gp  <- pick("LCR Gage Pool")
  cam <- pick("Cameron")
  cam_rising <- nrow(cam) == 1L && cam$trend == "Rising"

  # 1. Gage Pool now
  s1 <- if (nrow(gp) == 0L) {
    "The Gage Pool is not reporting."
  } else {
    sprintf("The Gage Pool is at %s cfs and %s, %s.",
            fmt_cfs(gp$now_cfs),
            if (gp$trend == "-") "its trend is unknown" else tolower(gp$trend),
            level_phrase(gp$now_cfs))
  }

  # 2. Upstream. Winslow and Moenkopi water both pass Cameron, so either one
  # enters the forecast once it raises Cameron. Reported when high or rising.
  up_txt <- upstream_sentence(up)
  s2 <- if (cam_rising) {
    trimws(paste(
      sprintf("Cameron is rising (%s cfs); a forecast appears or updates as it peaks.",
              fmt_cfs(cam$now_cfs)),
      up_txt))
  } else if (nrow(up) > 0L) {
    up_txt
  } else "Nothing is high or rising upstream."

  # 3. Forecast
  s3 <- switch(
    f$status,
    no_data  = "Cameron is not reporting, so no forecast is possible.",
    no_flood = paste("There is no Cameron flood forecast yet.",
                     pending_sentence(f)),
    patchy   = "A recent Cameron flood could not be forecast because of missing readings.",
    sustained = sprintf(
      "Cameron has risen slowly to %s cfs (%s cfs in the last %d h) with no peak yet. This looks like a long flood%s, not a single pulse, so no peak time is forecast. Water at Cameron takes %s to reach the Gage Pool, so the Gage Pool is likely to stay above %s cfs (caution when crossing) while Cameron rises or holds. A peak forecast returns once Cameron peaks.",
      fmt_cfs(f$cam_now), signed_cfs(f$cam_change), sustained_look_h,
      sustained_source(up), travel_txt(f$s), scales::comma(no_cross_cfs)),
    forecast = {
      s  <- f$s
      cf <- sprintf("The Cameron flood of %s cfs (%s)", fmt_cfs(f$pk$flow), fmt_t(f$pk$time))
      if (is.na(s$arrive_late)) {
        sprintf("%s is unlikely to reach the Gage Pool as a distinct peak (%d%% chance).",
                cf, round(100 * s$p_arrive))
      } else if (f$now > s$arrive_late) {
        # Window passed. Say what Cameron and the Gage Pool are doing, not
        # that the flood "has passed" (11/9/2026: Cameron was still at 92%
        # of its crest and the Gage Pool more than twice the alert level).
        cam_txt <- if (nrow(cam) == 1L) cam$now_cfs else f$cam_now
        trimws(paste(
          sprintf("Cameron %s %s cfs (%s) and is now %s cfs (%s cfs in the last %d h).",
                  if (isTRUE(f$crested)) "crested at" else "peaked at",
                  fmt_cfs(f$pk$flow), fmt_t(f$pk$time), fmt_cfs(cam_txt),
                  signed_cfs(f$cam_change), sustained_look_h),
          stay_high_sentence(f),
          rerise_sentence(f)))
      } else {
        when <- if (f$now >= s$arrive_early) "should be peaking at the Gage Pool now" else
          sprintf("is forecast to peak at the Gage Pool around %s", fmt_t(s$arrive_mid))
        paste0(sprintf("%s %s (likely %s to %s; %d%% chance it arrives).",
                       cf, when, fmt_t(s$arrive_early), fmt_t(s$arrive_late),
                       round(100 * s$p_arrive)),
               if (!f$peaked) " Cameron is still rising, so this is provisional and likely late." else "")
      }
    }
  )

  paste(s1, s2, s3)
}

# ---- About this forecast (static, collapsed) ------------------------------
# Lives in the UI, not a renderUI, so the 15-min refresh does not close it.
# Trigger numbers come from the settings above so the text stays in sync.
about_forecast <- tags$details(
  class = "about-forecast",
  tags$summary("About this forecast"),
  tags$p(
    "How it works: a statistical model (Bayesian, fit in R/brms) trained on ",
    "April to October floods at Cameron and the Gage Pool over the USGS period ",
    "of record. It does not route water through the channel. It uses the size of ",
    sprintf("the Cameron peak and how wet the channel has been (Cameron volume over the prior %d days) ",
            ante_d),
    "to estimate the chance the peak reaches the Gage Pool and when."
  ),
  tags$p(sprintf(
    "Trigger: Cameron must peak at %s cfs or more and rise above its low in the prior %d h by at least %s cfs or %d%% of that low, whichever is larger. A second pulse on the falling limb of an earlier flood counts as a new flood, measured from the low between them. Rises at Winslow or Moenkopi count only once they reach Cameron.",
    cam_min_peak, lookback_h, cam_min_rise, round(100 * cam_rel_rise))),
  tags$p(
    tags$strong("Upstream gages: "),
    "Winslow (mainstem) and Moenkopi Wash (tributary) both join above the Cameron ",
    "gage, so their floods pass Cameron before the Gage Pool. The forecast starts ",
    "when that water raises Cameron, not when it is first seen upstream, so a rise ",
    "at Winslow or Moenkopi is an early heads-up with no forecast attached yet. ",
    "When everything else is low, Moenkopi water can be followed from Moenkopi to ",
    "Cameron to the Gage Pool on the plots below."
  ),
  tags$p(sprintf(
    "Sustained high water: when Cameron rises slowly with no peak (less than %d%% over the %d h before its highest hour, at least %d h into the rise) and the Gage Pool is already above %s cfs, the page shows no peak time, because a peak time from a still-rising Cameron slides later with every update. It shows Cameron to Gage Pool travel time instead. This rule is provisional, set from the November 2026 Winslow flood. Cameron counts as peaked once it falls below %d%% of its highest hour, or has crested: the highest hour is at least %d h old and Cameron is at or below %d%% of it and falling. On the November 2026 Winslow flood the Gage Pool was still at its high after the window forecast from the crest closed, so for broad floods treat the window as early.",
    round(100 * sustained_pct), sustained_look_h, sustained_look_h,
    scales::comma(no_cross_cfs), round(100 * peaked_drop), crest_h, round(100 * crest_drop))),
  tags$p(
    "Peaks: peaks shown on this page are hourly means, as used by the model, so a ",
    "single bad 15-minute reading does not show up as a peak. 'Now' values are the ",
    "latest 15-minute reading."
  ),
  tags$p(
    "Reading it: 'Most likely' is the median predicted arrival; the likely window ",
    "holds the middle 80% of predicted times. Camp times are scaled from the Gage ",
    "Pool forecast by distance down the gorge, assuming steady travel speed. Times ",
    "are for the peak; crews will feel the rise earlier."
  ),
  tags$p(
    "Limits: USGS data arrive 30 to 60 min late, so the forecast trails Cameron ",
    "by about an hour. While Cameron is still rising the forecast is provisional ",
    "and true arrival is likely earlier. Not tested outside April to October. ",
    "This page keeps no record of past forecasts."
  ),
  tags$p(
    tags$strong("Disclaimer: "),
    "Experimental and not validated. For information purposes only. ",
    "Based on provisional USGS data subject to revision. Questions: Bill Pine."
  ),
  tags$p(class = "about-version", "Model version: Cameron to Gage Pool, November 2026.")
)

# ---- UI -------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .provisional { background:#F0E442; color:#000; padding:8px 12px;
                   margin-bottom:12px; font-weight:bold; }
    .usgs-error  { color:#D55E00; }
    .headline    { border:2px solid #000; padding:12px 16px; margin-bottom:10px; }
    .headline-main { font-size:1.6em; font-weight:bold; }
    .headline-level { font-size:1.15em; font-weight:bold; margin-top:4px; }
    .headline-sub  { color:#444; margin-top:4px; }
    .meaning     { font-size:1.15em; line-height:1.5; margin-bottom:16px; }
    .forecast    { border:2px dashed #0072B2; padding:10px 16px; margin-bottom:16px; }
    .experimental { font-weight:bold; margin-bottom:8px; }
    .about-forecast { border:1px solid #999; padding:6px 12px; margin:-8px 0 16px 0; }
    .about-forecast summary { cursor:pointer; font-weight:bold; }
    .about-version { color:#444; font-size:0.9em; }
    .crew-msg    { border:2px solid #0072B2; background:#F2F8FC; padding:8px 12px;
                   margin-bottom:12px; max-width:760px; }
    .crew-msg-title { font-weight:bold; }
    .crew-msg-text  { font-family:monospace; font-size:1.1em; margin:6px 0;
                      overflow-wrap:anywhere; }
    .crew-msg-sub   { color:#444; font-size:0.9em; }
    .crew-msg-sub .btn { margin-left:8px; }
  "))),
  titlePanel("Little Colorado River Discharge"),
  tags$div(class = "provisional",
           "Provisional USGS data. Forecast is experimental: for information purposes only."),
  uiOutput("crewMessage"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("status"),
      actionButton("refresh", "Refresh now"),
      tags$hr(),
      tags$p(tags$strong("Crew levels (Gage Pool): "),
             sprintf("caution when crossing above %s cfs; alert above %s cfs.",
                     scales::comma(no_cross_cfs), scales::comma(alert_cfs))),
      tags$p(tags$strong("Trend: "),
             sprintf("change over the last %d h (more than %d%% and %d cfs). 'Slowly' = change over %d h (more than %d%% and %d cfs).",
                     trend_hours, round(100 * trend_pct), trend_min_cfs,
                     trend_slow_h, round(100 * trend_slow_pct), trend_slow_min_cfs)),
      tags$p(tags$strong("Reporting: "),
             sprintf("Yes = reading within %d h; Delayed = older; No = no data.",
                     stale_hours)),
      tags$p(tags$strong("Forecast: "),
             "starts from a Cameron peak. Winslow and Moenkopi are both upstream of ",
             "Cameron; their rises count once they reach Cameron."),
      tags$p("Times are Arizona (MST)"),
      tags$p("All information provisional and experimental"),
      tags$hr(),
      tags$p("Built by ",
             tags$a(href = "https://www.smallwatersconsulting.com/",
                    target = "_blank", rel = "noopener",
                    "Small Waters Consulting"))
    ),
    mainPanel(
      uiOutput("gpHeadline"),
      uiOutput("meaning"),
      uiOutput("forecastPanel"),
      about_forecast,
      h3("Gages, upstream to downstream"),
      tags$p("Winslow (mainstem) and Moenkopi Wash (tributary) both join above Cameron."),
      tableOutput("gageTable"),
      h3(sprintf("Continuous discharge, last %d days", days_back)),
      plotlyOutput("hourlyDischargePlot"),
      h3(sprintf("Daily mean discharge, last %d days", days_back)),
      plotlyOutput("dischargePlot")
    )
  )
)

# ---- Server ---------------------------------------------------------------
server <- function(input, output, session) {

  bucket <- reactive({
    step <- refresh_minutes * 60
    now  <- as.numeric(Sys.time())
    invalidateLater(ceiling((step - now %% step) * 1000) + 1000)
    floor(now / step)
  })

  usgs <- reactive({
    end_time <- Sys.time()
    end_date <- as.Date(end_time, tz = tz_az)
    list(
      daily      = safe_daily(end_date - days_back, end_date),
      continuous = safe_continuous(end_date - fetch_days, end_time),
      pulled     = end_time
    )
  }) %>%
    bindCache(bucket(), input$refresh)

  daily_data <- reactive(usgs()$daily$result)
  continuous_all <- reactive(usgs()$continuous$result)
  continuous_data <- reactive({
    start <- as.POSIXct(paste(as.Date(usgs()$pulled, tz = tz_az) - days_back,
                              "00:00:00"), tz = tz_az)
    dplyr::filter(continuous_all(), dateTime >= start)
  })
  summ <- reactive(site_summary(continuous_data(), usgs()$pulled))
  upstream <- reactive(upstream_active(continuous_data(), summ()))

  fcst <- reactive(run_forecast(continuous_all(), usgs()$pulled)) %>%
    bindCache(bucket(), input$refresh)

  output$status <- renderUI({
    d <- usgs()
    errs <- list(Daily = d$daily$error, Continuous = d$continuous$error) %>%
      purrr::compact() %>%
      purrr::imap_chr(~ paste0(.y, " request failed: ", conditionMessage(.x)))
    tagList(
      tags$p(tags$strong("Last USGS pull: "),
             fmt_t(d$pulled), " MST"),
      tags$p(sprintf("Auto-refreshes every %d min.", refresh_minutes)),
      if (length(errs) > 0L) {
        tags$p(class = "usgs-error", tags$strong("WARNING: "),
               paste(errs, collapse = "; "))
      }
    )
  })

  # ---- Message for field crews ----
  output$crewMessage <- renderUI({
    msg <- crew_inreach(summ(), fcst(), upstream())
    tags$div(class = "crew-msg",
      tags$div(class = "crew-msg-title", "Message for field crews"),
      tags$div(class = "crew-msg-text", id = "crewMsgText", msg),
      tags$div(class = "crew-msg-sub",
        sprintf("%d of 160 characters. Built from provisional data and the experimental forecast; check before sending.",
                nchar(msg)),
        tags$button(type = "button", class = "btn btn-default btn-sm",
                    onclick = "navigator.clipboard.writeText(document.getElementById('crewMsgText').innerText)",
                    "Copy")))
  })

  # ---- Gage Pool headline ----
  output$gpHeadline <- renderUI({
    gp <- dplyr::filter(summ(), Site == "LCR Gage Pool")

    if (nrow(gp) == 0L) {
      return(tags$div(class = "headline",
        tags$div(class = "headline-main", "Gage Pool: NOT REPORTING"),
        tags$div(class = "headline-sub",
                 "No recent Gage Pool readings. Check upstream gages below.")))
    }

    trend_txt <- if (gp$trend == "-") "trend unknown" else tolower(gp$trend)
    level_txt <- dplyr::case_when(
      gp$now_cfs >= alert_cfs ~
        sprintf("ABOVE %s cfs CREW ALERT LEVEL. Caution when crossing.", scales::comma(alert_cfs)),
      gp$now_cfs >  no_cross_cfs ~
        sprintf("Above %s cfs: caution when crossing at camps.",
                scales::comma(no_cross_cfs)),
      TRUE ~
        sprintf("Below %s cfs.",
                scales::comma(no_cross_cfs))
    )
    delayed <- if (gp$hours_old > stale_hours) " READING IS DELAYED." else ""

    tags$div(class = "headline",
      tags$div(class = "headline-main",
               sprintf("Gage Pool: %s cfs, %s", fmt_cfs(gp$now_cfs), trend_txt)),
      tags$div(class = "headline-level", level_txt),
      tags$div(class = "headline-sub",
               sprintf("Reading at %s (%.1f h ago). Peak in last 24 h (hourly mean): %s cfs.%s",
                       fmt_t(gp$last_time), gp$hours_old,
                       fmt_cfs(gp$peak24_cfs), delayed)))
  })

  # ---- What this means ----
  output$meaning <- renderUI({
    s <- summ()
    quiet <- setdiff(gages$Site,
                     as.character(s$Site[s$hours_old <= stale_hours]))
    tags$div(class = "meaning",
      tags$strong("What this means: "),
      interpretation(s, fcst(), upstream()),
      if (length(quiet) > 0L) {
        tags$strong(paste0(" Not reporting or delayed: ",
                           paste(quiet, collapse = ", "),
                           ". A gage that is not reporting can hide a flood."))
      })
  })

  # ---- Experimental forecast ----
  output$forecastPanel <- renderUI({
    f <- fcst()

    body <- switch(
      f$status,
      no_data  = tags$p("No recent Cameron readings. No forecast."),
      no_flood = tags$p(sprintf("No Cameron flood in the last %d h. %s",
                                event_h, pending_sentence(f))),
      patchy   = tags$p(sprintf(
        "Cameron flood at %s, but too many missing Cameron readings to forecast.",
        fmt_t(f$pk$time))),
      # Window already closed: one line instead of full tables
      forecast = if (!is.na(f$s$arrive_late) && f$now > f$s$arrive_late) {
        tags$p(sprintf("Last forecast, from the Cameron %s of %s cfs at %s: Gage Pool peak expected %s. %s",
                       if (isTRUE(f$crested)) "crest" else "peak",
                       fmt_cfs(f$pk$flow), fmt_t(f$pk$time),
                       fmt_span(f$s$arrive_early, f$s$arrive_late), gp_since_text(f)))
      } else tagList(
        tags$p(tags$strong(sprintf("From the Cameron peak: %s cfs at %s (%.0f h ago).",
                                   fmt_cfs(f$pk$flow), fmt_t(f$pk$time), f$pk_age_h)),
               if (!f$peaked) " STILL RISING: provisional; arrival likely earlier than shown."
               else if (isTRUE(f$crested)) " CRESTED: broad peak, falling slowly; the Gage Pool may peak later than shown."),
        tableOutput("forecastTable"),
        tableOutput("campTable"),
        if (!is.na(f$extrap)) tags$p(tags$strong("Note: "), f$extrap),
        upstream_note(upstream()),
        if (f$out_of_season) {
          tags$p(tags$strong("Note: "), "outside the April to October fit season. Treat as a test.")
        }
      ),
      # Slow rise, no peak, Gage Pool already high: no peak window
      sustained = tagList(
        tags$p(tags$strong("SUSTAINED HIGH WATER: no peak forecast."),
               sprintf(" Cameron has risen slowly for %.0f h with no peak%s. A peak time would keep sliding later with every update, so none is shown. A peak forecast returns once Cameron peaks.",
                       f$cam_rising_h, sustained_source(upstream()))),
        tableOutput("sustainedTable"),
        upstream_note(upstream()),
        if (f$out_of_season) {
          tags$p(tags$strong("Note: "), "outside the April to October fit season. Treat as a test.")
        }
      )
    )

    tags$div(class = "forecast",
      tags$h3("Forecast (experimental): Cameron to Gage Pool"),
      body)
  })

  output$forecastTable <- renderTable({
    f <- fcst()
    req(f$status == "forecast")
    s <- f$s
    window_txt <- dplyr::case_when(
      is.na(s$arrive_late)    ~ "-",
      f$now > s$arrive_late   ~ "PASSED",
      f$now >= s$arrive_early ~ "OPEN NOW",
      TRUE                    ~ sprintf("Opens in %.0f h",
                                        as.numeric(difftime(s$arrive_early, f$now,
                                                            units = "hours")))
    )
    data.frame(
      Item = c("Chance it reaches the Gage Pool",
               "Gage Pool peak, most likely",
               "Gage Pool peak, likely window",
               "Window status",
               "What the Gage Pool did in the window"),
      Value = c(
        sprintf("%d%%", round(100 * s$p_arrive)),
        fmt_t(s$arrive_mid),
        sprintf("%s to %s", fmt_t(s$arrive_early), fmt_t(s$arrive_late)),
        window_txt,
        gp_window_text(f$gp_win, s, f$now)
      )
    )
  }, colnames = FALSE)

  output$sustainedTable <- renderTable({
    f <- fcst()
    req(f$status == "sustained")
    data.frame(
      Item = c("Cameron now (hourly mean)",
               sprintf("Cameron change, last %d h", sustained_look_h),
               "Gage Pool now",
               "Travel time, Cameron to Gage Pool",
               "Water at Cameron now reaches the Gage Pool",
               "What to expect"),
      Value = c(
        sprintf("%s cfs (%s)", fmt_cfs(f$cam_now), fmt_t(f$last_t)),
        sprintf("%s cfs", signed_cfs(f$cam_change)),
        sprintf("%s cfs, %s", fmt_cfs(f$gp_now), level_phrase(f$gp_now)),
        travel_txt(f$s),
        # From the newest Cameron hour (the peak hour can be older on a flat top)
        {
          at <- function(h) round_15(f$last_t + h * 3600)
          sprintf("around %s (likely %s to %s)",
                  fmt_t(at(f$s$lag_mid_h)), fmt_t(at(f$s$lag_early_h)),
                  fmt_t(at(f$s$lag_late_h)))
        },
        sprintf("Gage Pool likely stays above %s cfs (caution when crossing) while Cameron rises or holds",
                scales::comma(no_cross_cfs))
      )
    )
  }, colnames = FALSE)

  output$campTable <- renderTable({
    f <- fcst()
    req(f$status == "forecast")
    f$cf %>%
      dplyr::transmute(Camp                   = camp,
                       `Peak most likely`     = fmt_t(arrive_mid),
                       `Likely window`        = sprintf("%s to %s",
                                                        fmt_t(arrive_early),
                                                        fmt_t(arrive_late)))
  })

  # ---- Gage table ----
  output$gageTable <- renderTable({
    s <- summ()
    validate(need(nrow(s) > 0L,
                  "No USGS discharge data were returned. See status at left."))

    data.frame(Site = site_factor(gages$Site)) %>%
      left_join(s, by = "Site") %>%
      transmute(
        `Gage`                  = as.character(Site),
        `Now (cfs)`             = fmt_cfs(now_cfs),
        `Trend`                 = dplyr::coalesce(trend, "-"),
        `Peak, last 24 h (cfs, hourly mean)` = fmt_cfs(peak24_cfs),
        `Last reading`          = fmt_t(last_time),
        `Reporting`             = dplyr::case_when(
          is.na(hours_old)        ~ "No",
          hours_old > stale_hours ~ "Delayed",
          TRUE                    ~ "Yes")
      )
  }, align = "lrlrll")

  # ---- Plots ----
  output$hourlyDischargePlot <- renderPlotly({
    continuous <- continuous_data()
    validate(need(nrow(continuous) > 0L,
                  "No continuous discharge data available for this period."))

    p <- ggplot(continuous,
                aes(x = dateTime, y = Discharge,
                    color = Site, linetype = Site)) +
      crew_lines(min(continuous$dateTime)) +
      geom_line(linewidth = 1) +
      labs(x = "Date and time (Arizona MST)", y = "Discharge (cfs)") +
      scale_x_datetime(date_labels = "%m/%d %H:%M", timezone = tz_az) +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })

  output$dischargePlot <- renderPlotly({
    daily <- daily_data()
    validate(need(nrow(daily) > 0L,
                  "No daily mean discharge data available for this period."))

    p <- ggplot(daily,
                aes(x = Date, y = Discharge,
                    color = Site, linetype = Site, shape = Site)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(x = "Date", y = "Discharge (cfs)") +
      scale_x_date(date_breaks = "1 day", date_labels = "%m/%d") +
      scale_shape_manual(values = site_shapes, name = "Gage site") +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })
}

shinyApp(ui = ui, server = server)
