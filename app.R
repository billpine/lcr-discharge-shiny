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
#  1. Gauge Pool headline: flow now, trend, crew levels (600 / 3,000 cfs).
#  2. "What this means": plain-language paragraph built from the numbers.
#  3. EXPERIMENTAL forecast: Cameron peak -> Gauge Pool and camp arrival.
#     Same logic as 14_live_forecast.R; shown only when Cameron has a flood;
#     collapses to one line once the forecast window has passed.
#  4. "About this forecast": static, collapsed methods note and disclaimer.
#  5. Gauge table, upstream to downstream. Moenkopi Wash is a tributary that
#     joins the LCR below Cameron, so it sits between Cameron and the Gauge
#     Pool and its floods are NOT covered by the Cameron forecast.
#  6. Plots: continuous (with crew levels) and daily mean.
#
# 11/2/2026: a second Cameron pulse on the falling limb of an earlier flood
# (2,715 cfs at 00:00, down to 607 at 09:00, back to 2,532 by 13:00) was
# not detected. New floods are now measured from the low AFTER the biggest
# peak in the window, and the rise threshold scales with flow.

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
stale_hours     <- 3      # "Gauge reporting: Delayed" if last reading older
trend_hours     <- 3      # trend = now vs. this many hours ago
trend_pct       <- 0.10   # change must exceed 10% ...
trend_min_cfs   <- 20     # ... and 20 cfs to count as Rising/Falling
no_cross_cfs    <- 600    # crew level at Gauge Pool: no crossing at camps
alert_cfs       <- 3000   # crew level at Gauge Pool: alert

# Forecast settings, matching 00_setup.R and 14_live_forecast.R
cam_min_peak   <- 100     # peak_rules, Cameron
cam_min_rise   <- 50      # peak_rules, Cameron
cam_rel_rise   <- 0.10    # a rise must also be 10% of its starting low, so
                          # recession wiggles at high flow don't count
lookback_h     <- 48      # base flow search before the peak
event_h        <- 48      # current Cameron flood = highest hour in last 48 h
peaked_drop    <- 0.8     # peaked once flow < 80% of max ...
peaked_min_h   <- 2       # ... and the max is at least 2 h old
season_months  <- 4:10    # model fit on April-October floods

# Rise needed above a low of q cfs to count as a flood
rise_needed <- function(q) pmax(cam_min_rise, cam_rel_rise * q)

tz_az <- "US/Arizona"

m <- readRDS("model_cam_gp.rds")
ante_d <- if (is.null(m$ante_days)) 14 else m$ante_days

# ---- Gauges (flow order), colors, line types -------------------------------
# Winslow -> Cameron on the mainstem; Moenkopi Wash joins below Cameron.
gauges <- data.frame(
  monitoring_location_id = paste0(
    "USGS-", c("09400350", "09402000", "09401260", "09402300")
  ),
  Site = c("Winslow", "Cameron", "Moenkopi", "LCR Gauge Pool"),
  stringsAsFactors = FALSE
)

site_colors <- c("Winslow" = "#E69F00", "Moenkopi" = "#009E73",
                 "Cameron" = "#56B4E9", "LCR Gauge Pool" = "#000000")
site_lines  <- c("Winslow" = "dashed", "Moenkopi" = "longdash",
                 "Cameron" = "dotdash", "LCR Gauge Pool" = "solid")
site_shapes <- c("Winslow" = 16, "Moenkopi" = 15,
                 "Cameron" = 17, "LCR Gauge Pool" = 18)

lcr_scales <- list(
  scale_color_manual(values = site_colors, name = "Gauge site"),
  scale_linetype_manual(values = site_lines, name = "Gauge site"),
  theme_minimal(base_size = 13)
)

# Crew levels drawn on the continuous plot. Labels sit at the left edge
# (x_min) because ggplotly does not place text at x = -Inf.
crew_lines <- function(x_min) list(
  geom_hline(yintercept = c(no_cross_cfs, alert_cfs),
             linetype = "dotted", colour = "grey40"),
  annotate("text", x = x_min, y = c(no_cross_cfs, alert_cfs),
           label = c(sprintf("Gauge Pool %s: no crossing",
                             scales::comma(no_cross_cfs)),
                     sprintf("Gauge Pool %s: crew alert",
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

site_factor <- function(x) factor(x, levels = gauges$Site)
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
    monitoring_location_id = gauges$monitoring_location_id,
    parameter_code = "00060",
    statistic_id = "00003",
    time = c(as.character(start_date), as.character(end_date)),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_daily())

  x %>%
    dplyr::filter(.data$parameter_code == "00060",
                  .data$statistic_id == "00003") %>%
    dplyr::left_join(gauges, by = "monitoring_location_id") %>%
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
    monitoring_location_id = gauges$monitoring_location_id,
    parameter_code = "00060",
    time = c(start_time, end_time),
    skipGeometry = TRUE
  )
  if (nrow(x) == 0L) return(empty_continuous())

  x %>%
    dplyr::filter(.data$parameter_code == "00060") %>%
    dplyr::left_join(gauges, by = "monitoring_location_id") %>%
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

# ---- Per-gauge summary -----------------------------------------------------
trend_label <- function(now, before) {
  thresh <- pmax(trend_min_cfs, trend_pct * before)
  dplyr::case_when(
    is.na(now) | is.na(before) ~ "-",
    now - before > thresh      ~ "Rising",
    before - now > thresh      ~ "Falling",
    TRUE                       ~ "Steady"
  )
}

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
                      peak24_cfs = numeric(), hours_old = numeric(),
                      trend = character()))
  }
  continuous %>%
    group_by(Site) %>%
    summarise(
      last_time  = max(dateTime),
      now_cfs    = Discharge[which.max(dateTime)],
      before_cfs = value_hours_before(dateTime, Discharge, trend_hours),
      peak24_cfs = max(Discharge[as.numeric(dateTime) >
                                   as.numeric(max(dateTime)) - 24 * 3600]),
      .groups = "drop"
    ) %>%
    mutate(hours_old = as.numeric(difftime(pulled, last_time, units = "hours")),
           trend     = trend_label(now_cfs, before_cfs))
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
# 4. Otherwise the flood is pk_max, with base from the lookback before it.
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
  base <- if (new_flood) trough else cam %>%
    dplyr::filter(time <= pk$time, time > pk$time - lookback_h * 3600) %>%
    dplyr::filter(flow == min(flow)) %>%
    dplyr::slice_max(time, n = 1, with_ties = FALSE)

  # Low a future rise would be measured from: the post-peak trough when
  # there is one, else the plain recent low.
  post_low <- nrow(trough) == 1L
  low_ref  <- if (post_low) trough$flow else
    min(cam$flow[cam$time > last_t - lookback_h * 3600])

  list(pk = pk, base = base, low_ref = low_ref, post_low = post_low)
}

run_forecast <- function(continuous, now) {
  cam <- hourly(continuous, "Cameron")
  gp  <- hourly(continuous, "LCR Gauge Pool")
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

  out$peaked <- cam_now < peaked_drop * pk$flow &&
    as.numeric(difftime(last_t, pk$time, units = "hours")) >= peaked_min_h

  vol <- prior_volume_af(cam$time, cam$flow, base$time, days = ante_d)
  if (is.na(vol)) return(c(out, status = "patchy"))

  fc <- withr::with_seed(1, forecast_gp(m, pk$flow, pk$time, vol))
  s  <- summarise_forecast(fc)

  # What the Gauge Pool did inside the 80% forecast window. "Max since the
  # Cameron peak" is misleading when the Gauge Pool is still receding from an
  # earlier, larger flood, so look for a rise within the window instead.
  gp_win <- if (is.na(s$arrive_early)) gp[0, ] else {
    dplyr::filter(gp, time >= s$arrive_early, time <= s$arrive_late)
  }

  c(out, list(
    status        = "forecast",
    vol           = vol,
    s             = s,
    cf            = camp_forecast(m, fc, pk$time),
    gp_win        = gp_win,
    extrap        = extrapolation_note(m, pk$flow),
    out_of_season = !lubridate::month(pk$time) %in% season_months
  ))
}

# Did the Gauge Pool rise inside the forecast window? Uses the same
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
  if (nrow(w) == 0L)          return("No Gauge Pool readings in the window")

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
  low_txt <- if (isTRUE(f$low_is_post)) "its low since the last peak" else "its recent low"
  why <- if (f$trigger_cfs > cam_min_peak) {
    sprintf(" (%s cfs above %s of %s cfs)",
            fmt_cfs(f$trigger_cfs - f$low_recent), low_txt, fmt_cfs(f$low_recent))
  } else ""
  sprintf("A new forecast will be made once Cameron rises above %s cfs%s.",
          fmt_cfs(f$trigger_cfs), why)
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

level_phrase <- function(q) {
  dplyr::case_when(
    q >= alert_cfs   ~ sprintf("ABOVE the %s cfs crew alert level", scales::comma(alert_cfs)),
    q > no_cross_cfs ~ sprintf("above the %s cfs no-crossing level", scales::comma(no_cross_cfs)),
    TRUE             ~ sprintf("below the %s cfs no-crossing level", scales::comma(no_cross_cfs))
  )
}

# Plain-language "What this means" paragraph, rebuilt every refresh from the
# same numbers shown on the page. States facts only; no extra prediction.
interpretation <- function(summ, f) {
  pick <- function(site) dplyr::filter(summ, Site == site)
  gp  <- pick("LCR Gauge Pool")
  cam <- pick("Cameron")
  cam_rising <- nrow(cam) == 1L && cam$trend == "Rising"

  # 1. Gauge Pool now
  s1 <- if (nrow(gp) == 0L) {
    "The Gauge Pool is not reporting."
  } else {
    sprintf("The Gauge Pool is at %s cfs and %s, %s.",
            fmt_cfs(gp$now_cfs),
            if (gp$trend == "-") "its trend is unknown" else tolower(gp$trend),
            level_phrase(gp$now_cfs))
  }

  # 2. Upstream. Winslow water passes Cameron; Moenkopi water does not
  # (Moenkopi Wash joins below Cameron), so it is never in the forecast.
  win_up <- dplyr::filter(summ, Site == "Winslow",  trend == "Rising")
  mk_up  <- dplyr::filter(summ, Site == "Moenkopi", trend == "Rising")
  s2_main <- if (cam_rising) {
    sprintf("Cameron is rising (%s cfs); a forecast appears or updates as it peaks.",
            fmt_cfs(cam$now_cfs))
  } else if (nrow(win_up) == 1L) {
    sprintf("Winslow is rising (%s cfs). It has not reached Cameron yet, so there is no forecast for it.",
            fmt_cfs(win_up$now_cfs))
  } else if (nrow(mk_up) == 0L) {
    "Nothing is rising upstream."
  } else ""
  s2_mk <- if (nrow(mk_up) == 1L) {
    sprintf("Moenkopi is rising (%s cfs). Moenkopi Wash joins below Cameron, so this forecast does not cover it; watch the Gauge Pool directly.",
            fmt_cfs(mk_up$now_cfs))
  } else ""
  s2 <- trimws(paste(s2_main, s2_mk))

  # 3. Forecast
  s3 <- switch(
    f$status,
    no_data  = "Cameron is not reporting, so no forecast is possible.",
    no_flood = paste("There is no Cameron flood forecast yet.",
                     pending_sentence(f)),
    patchy   = "A recent Cameron flood could not be forecast because of missing readings.",
    forecast = {
      s  <- f$s
      cf <- sprintf("The Cameron flood of %s cfs (%s)", fmt_cfs(f$pk$flow), fmt_t(f$pk$time))
      if (is.na(s$arrive_late)) {
        sprintf("%s is unlikely to reach the Gauge Pool as a distinct peak (%d%% chance).",
                cf, round(100 * s$p_arrive))
      } else if (f$now > s$arrive_late) {
        res <- if (nrow(f$gp_win) == 0L) {
          "there were no Gauge Pool readings in its window"
        } else {
          g <- gp_window_check(f$gp_win)
          if (g$rose) sprintf("the Gauge Pool rose to %s cfs at %s",
                              fmt_cfs(g$top$flow), fmt_t(g$top$time))
          else "the Gauge Pool showed no distinct rise in its window"
        }
        sprintf("%s has passed: its forecast window closed %s, and %s. It clears from this page at %s; after that, %s",
                cf, fmt_t(s$arrive_late), res, fmt_t(f$clears_at),
                sub("^A new", "a new", trigger_sentence(f)))
      } else {
        when <- if (f$now >= s$arrive_early) "should be peaking at the Gauge Pool now" else
          sprintf("is forecast to peak at the Gauge Pool around %s", fmt_t(s$arrive_mid))
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
    "April to October floods at Cameron and the Gauge Pool over the USGS period ",
    "of record. It does not route water through the channel. It uses the size of ",
    sprintf("the Cameron peak and how wet the channel has been (Cameron volume over the prior %d days) ",
            ante_d),
    "to estimate the chance the peak reaches the Gauge Pool and when."
  ),
  tags$p(sprintf(
    "Trigger: Cameron must peak at %s cfs or more and rise above its low in the prior %d h by at least %s cfs or %d%% of that low, whichever is larger. A second pulse on the falling limb of an earlier flood counts as a new flood, measured from the low between them. Rises at Winslow count only once they reach Cameron.",
    cam_min_peak, lookback_h, cam_min_rise, round(100 * cam_rel_rise))),
  tags$p(
    tags$strong("Moenkopi is not covered: "),
    "Moenkopi Wash joins the river below Cameron, so its floods never pass the ",
    "Cameron gauge. A Moenkopi flood can reach the Gauge Pool with no forecast on ",
    "this page. When Moenkopi is rising, watch the Gauge Pool directly."
  ),
  tags$p(
    "Reading it: 'Most likely' is the median predicted arrival; the likely window ",
    "holds the middle 80% of predicted times. Camp times are scaled from the Gauge ",
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
    "Experimental and not validated. Not for crew safety or sampling decisions. ",
    "Based on provisional USGS data subject to revision. Questions: Bill Pine."
  ),
  tags$p(class = "about-version", "Model version: Cameron to Gauge Pool, November 2026.")
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
  "))),
  titlePanel("Little Colorado River Discharge"),
  tags$div(class = "provisional",
           "Provisional USGS data. Forecast is experimental: not for crew decisions."),
  sidebarLayout(
    sidebarPanel(
      uiOutput("status"),
      actionButton("refresh", "Refresh now"),
      tags$hr(),
      tags$p(tags$strong("Crew levels (Gauge Pool): "),
             sprintf("no crossing above %s cfs; alert above %s cfs.",
                     scales::comma(no_cross_cfs), scales::comma(alert_cfs))),
      tags$p(tags$strong("Trend: "),
             sprintf("change over the last %d h (more than %d%% and %d cfs).",
                     trend_hours, round(100 * trend_pct), trend_min_cfs)),
      tags$p(tags$strong("Reporting: "),
             sprintf("Yes = reading within %d h; Delayed = older; No = no data.",
                     stale_hours)),
      tags$p(tags$strong("Forecast: "),
             "starts from a Cameron peak. Winslow rises count once they reach Cameron. ",
             "Moenkopi joins below Cameron and is not forecast."),
      tags$p("Times are Arizona (MST).")
    ),
    mainPanel(
      uiOutput("gpHeadline"),
      uiOutput("meaning"),
      uiOutput("forecastPanel"),
      about_forecast,
      h3("Gauges, upstream to downstream"),
      tags$p("Moenkopi Wash is a tributary that joins below Cameron."),
      tableOutput("gaugeTable"),
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

  # ---- Gauge Pool headline ----
  output$gpHeadline <- renderUI({
    gp <- dplyr::filter(summ(), Site == "LCR Gauge Pool")

    if (nrow(gp) == 0L) {
      return(tags$div(class = "headline",
        tags$div(class = "headline-main", "Gauge Pool: NOT REPORTING"),
        tags$div(class = "headline-sub",
                 "No recent Gauge Pool readings. Check upstream gauges below.")))
    }

    trend_txt <- if (gp$trend == "-") "trend unknown" else tolower(gp$trend)
    level_txt <- dplyr::case_when(
      gp$now_cfs >= alert_cfs ~
        sprintf("ABOVE %s cfs CREW ALERT LEVEL.", scales::comma(alert_cfs)),
      gp$now_cfs >  no_cross_cfs ~
        sprintf("Above %s cfs: no crossing at camps.",
                scales::comma(no_cross_cfs)),
      TRUE ~
        sprintf("Below the %s cfs no-crossing level.",
                scales::comma(no_cross_cfs))
    )
    delayed <- if (gp$hours_old > stale_hours) " READING IS DELAYED." else ""

    tags$div(class = "headline",
      tags$div(class = "headline-main",
               sprintf("Gauge Pool: %s cfs, %s", fmt_cfs(gp$now_cfs), trend_txt)),
      tags$div(class = "headline-level", level_txt),
      tags$div(class = "headline-sub",
               sprintf("Reading at %s (%.1f h ago). Peak in last 24 h: %s cfs.%s",
                       fmt_t(gp$last_time), gp$hours_old,
                       fmt_cfs(gp$peak24_cfs), delayed)))
  })

  # ---- What this means ----
  output$meaning <- renderUI({
    s <- summ()
    quiet <- setdiff(gauges$Site,
                     as.character(s$Site[s$hours_old <= stale_hours]))
    tags$div(class = "meaning",
      tags$strong("What this means: "),
      interpretation(s, fcst()),
      if (length(quiet) > 0L) {
        tags$strong(paste0(" Not reporting or delayed: ",
                           paste(quiet, collapse = ", "),
                           ". A gauge that is not reporting can hide a flood."))
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
        tags$p(sprintf("Last forecast (Cameron peak %s cfs at %s): window closed %s. %s.",
                       fmt_cfs(f$pk$flow), fmt_t(f$pk$time), fmt_t(f$s$arrive_late),
                       gp_window_text(f$gp_win, f$s, f$now)))
      } else tagList(
        tags$p(tags$strong(sprintf("From the Cameron peak: %s cfs at %s (%.0f h ago).",
                                   fmt_cfs(f$pk$flow), fmt_t(f$pk$time), f$pk_age_h)),
               if (!f$peaked) " STILL RISING: provisional; arrival likely earlier than shown."),
        tableOutput("forecastTable"),
        tableOutput("campTable"),
        if (!is.na(f$extrap)) tags$p(tags$strong("Note: "), f$extrap),
        if (any(summ()$Site == "Moenkopi" & summ()$trend == "Rising")) {
          tags$p(tags$strong("Note: "),
                 "Moenkopi is also rising. A Gauge Pool rise in the window may be Moenkopi water, not the Cameron flood.")
        },
        if (f$out_of_season) {
          tags$p(tags$strong("Note: "), "outside the April to October fit season. Treat as a test.")
        }
      )
    )

    tags$div(class = "forecast",
      tags$h3("Forecast (experimental): Cameron to Gauge Pool"),
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
      Item = c("Chance it reaches the Gauge Pool",
               "Gauge Pool peak, most likely",
               "Gauge Pool peak, likely window",
               "Window status",
               "What the Gauge Pool did in the window"),
      Value = c(
        sprintf("%d%%", round(100 * s$p_arrive)),
        fmt_t(s$arrive_mid),
        sprintf("%s to %s", fmt_t(s$arrive_early), fmt_t(s$arrive_late)),
        window_txt,
        gp_window_text(f$gp_win, s, f$now)
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

  # ---- Gauge table ----
  output$gaugeTable <- renderTable({
    s <- summ()
    validate(need(nrow(s) > 0L,
                  "No USGS discharge data were returned. See status at left."))

    data.frame(Site = site_factor(gauges$Site)) %>%
      left_join(s, by = "Site") %>%
      transmute(
        `Gauge`                  = as.character(Site),
        `Now (cfs)`             = fmt_cfs(now_cfs),
        `Trend`                 = dplyr::coalesce(trend, "-"),
        `Peak, last 24 h (cfs)` = fmt_cfs(peak24_cfs),
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
      scale_shape_manual(values = site_shapes, name = "Gauge site") +
      lcr_scales
    ggplotly(p) %>% clean_legend()
  })
}

shinyApp(ui = ui, server = server)
