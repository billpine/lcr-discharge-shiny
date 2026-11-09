# =============================================================================
# app/R/forecast_helpers.R
# Plain-R forecast functions for the Shiny app (no brms/Stan needed).
# Shiny sources files in app/R/ automatically.
#
#   m <- readRDS("model_cam_gp.rds")
#   fc <- forecast_gp(m, cam_peak_cfs = 2100,
#                     cam_peak_time = as.POSIXct("2026-08-12 14:00", tz = "America/Phoenix"),
#                     vol_prior_af = 800)
#   summarise_forecast(fc)
#   camp_forecast(m, fc, cam_peak_time = as.POSIXct("2026-08-12 14:00", tz = "America/Phoenix"))
#   inreach_message(summarise_forecast(fc), cam_peak_cfs = 2100,
#                   cam_peak_time = as.POSIXct("2026-08-12 14:00", tz = "America/Phoenix"))
#
# IMPORTANT: this model predicts from the Cameron PEAK. While Cameron is still
# rising, the peak is not known; using the highest flow so far understates
# the peak and predicts arrival too LATE (bigger floods travel faster).
# Show forecasts as provisional until Cameron has clearly peaked.
# =============================================================================

# Prior volume (acre-ft) over the last `days` days from an hourly flow series
prior_volume_af <- function(time_local, flow, at_time, days = 14) {
  w <- !is.na(flow) & time_local > at_time - days * 86400 & time_local <= at_time
  if (sum(w) < 0.8 * days * 24) return(NA_real_)
  sum(flow[w]) * 3600 / 43560
}

# Posterior predictive draws for one Cameron event.
#   new_year = TRUE draws a fresh year effect (honest uncertainty for a new season)
forecast_gp <- function(m, cam_peak_cfs, cam_peak_time, vol_prior_af,
                        season = NULL, new_year = TRUE) {
  d  <- m$draws
  sc <- m$scaling
  n  <- nrow(d)
  zs <- function(x, v) (x - sc$mean[sc$var == v]) / sc$sd[sc$var == v]

  if (is.null(season)) {
    season <- if (as.integer(format(cam_peak_time, "%m")) <= 6) "spring" else "monsoon"
  }
  sp <- as.numeric(season == "spring")
  pz <- zs(log(cam_peak_cfs), "log_up_peak")
  dz <- zs(-log1p(vol_prior_af), "log_dry")    # higher = drier

  yr_mu <- if (new_year) rnorm(n, 0, d$sd_year_f__Intercept)    else 0
  yr_hu <- if (new_year) rnorm(n, 0, d$sd_year_f__hu_Intercept) else 0

  mu <- d$b_Intercept + d$b_peak_z * pz + d$b_dry_z * dz +
        d$b_seasonspring * sp + yr_mu
  hu <- plogis(d$b_hu_Intercept + d$b_hu_peak_z * pz + d$b_hu_dry_z * dz +
               d$b_hu_seasonspring * sp + yr_hu)

  arrive <- runif(n) > hu
  lag_h  <- ifelse(arrive, rlnorm(n, mu, d$sigma), NA_real_)

  data.frame(arrive = arrive,
             lag_h  = lag_h,
             arrival_time = cam_peak_time + lag_h * 3600)
}

# Round times to the nearest 15 min (forecasts are not minute-precise)
round_15 <- function(x) {
  as.POSIXct(round(as.numeric(x) / 900) * 900, origin = "1970-01-01",
             tz = attr(x, "tzone"))
}

# One-row summary for display
summarise_forecast <- function(fc, probs = c(0.1, 0.5, 0.9)) {
  pos <- fc$arrival_time[fc$arrive]
  tz  <- attr(fc$arrival_time, "tzone")
  q   <- if (length(pos) > 0) {
    as.POSIXct(quantile(as.numeric(pos), probs), origin = "1970-01-01", tz = tz)
  } else {
    as.POSIXct(rep(NA, length(probs)), tz = tz)
  }
  q <- round_15(q)
  data.frame(p_arrive     = mean(fc$arrive),
             arrive_early = q[1],
             arrive_mid   = q[2],
             arrive_late  = q[3],
             lag_early_h  = unname(quantile(fc$lag_h, probs[1], na.rm = TRUE)),
             lag_mid_h    = unname(quantile(fc$lag_h, probs[2], na.rm = TRUE)),
             lag_late_h   = unname(quantile(fc$lag_h, probs[3], na.rm = TRUE)),
             row.names    = NULL)
}

# P(Gage Pool peak has arrived) by each time in a grid (for a plot)
arrival_curve <- function(fc, from, to, by_h = 1) {
  t <- seq(from, to, by = by_h * 3600)
  data.frame(time = t,
             p = vapply(t, function(x) mean(fc$arrive & fc$arrival_time <= x), numeric(1)))
}

# inReach text, <= 160 characters. Times are local (Arizona).
inreach_message <- function(s, cam_peak_cfs, cam_peak_time, provisional = FALSE,
                            next_update = NULL, max_chars = 160) {
  hm  <- function(x) format(x, "%H:%M")
  day <- function(x) format(x, "%a")
  msg <- sprintf("LCR %s: Cameron %s cfs %s %s. %d%% chance reaches GP. Likely %s %s-%s, most likely %s.",
                 if (provisional) "PROVISIONAL" else "FCST",
                 format(round(cam_peak_cfs, -1), big.mark = ","),
                 if (provisional) "rising," else "peak",
                 hm(cam_peak_time),
                 round(100 * s$p_arrive),
                 day(s$arrive_early), hm(s$arrive_early), hm(s$arrive_late),
                 hm(s$arrive_mid))
  if (!is.null(next_update)) msg <- paste0(msg, " Next update ", hm(next_update), ".")
  if (nchar(msg) > max_chars) {
    warning("inReach message is ", nchar(msg), " characters; trimming to ", max_chars)
    msg <- substr(msg, 1, max_chars)
  }
  msg
}

# Flag forecasts outside the fitted range of Cameron peaks
extrapolation_note <- function(m, cam_peak_cfs) {
  r <- m$peak_range_cfs
  if (cam_peak_cfs > r[2]) {
    sprintf("Cameron peak above the largest fitted event (%s cfs); forecast is extrapolated.",
            format(round(r[2]), big.mark = ","))
  } else if (cam_peak_cfs < r[1]) {
    sprintf("Cameron peak below the smallest fitted event (%s cfs).",
            format(round(r[1]), big.mark = ","))
  } else {
    NA_character_
  }
}

# Arrival at each camp (and the Gage Pool), from Gage Pool forecast draws.
# All camps are upstream of the Gage Pool gage, so they see the flood first.
# Each draw's lag is scaled by the share of the Cameron-to-Gage-Pool distance
# travelled, assuming uniform wave speed; bigger, faster floods get
# proportionally shorter camp lags. p_arrive is the Gage Pool value for every
# camp (slightly conservative for upstream camps).
# Camps and rkm come from 00_setup.R via 06_export_for_app.R (m$camps).
camp_forecast <- function(m, fc, cam_peak_time) {
  cp   <- m$camps
  frac <- (m$cam_rkm - cp$rkm) / (m$cam_rkm - m$gp_rkm)
  rows <- Map(function(nm, rkm, f) {
    fc_c <- transform(fc, lag_h = lag_h * f,
                      arrival_time = cam_peak_time + lag_h * f * 3600)
    cbind(camp = nm, rkm = rkm, summarise_forecast(fc_c))
  }, cp$camp, cp$rkm, frac)
  out <- do.call(rbind, unname(rows))
  tibble::as_tibble(out[order(-out$rkm), ])   # upstream to downstream
}
