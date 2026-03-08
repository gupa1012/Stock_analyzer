# R/demo_data.R
# Generates realistic sample fundamental data for demonstration purposes.
# Used when RSelenium / live scraping is not available.

#' Generate a plausible time-series for a given metric.
#' @param years  Integer vector of fiscal years.
#' @param start  Starting value.
#' @param drift  Annual drift factor (e.g. 1.08 = ~8% CAGR).
#' @param noise  Relative noise level (0-1).
gen_series <- function(years, start, drift = 1.05, noise = 0.08) {
  n <- length(years)
  values <- numeric(n)
  v <- start
  for (i in seq_len(n)) {
    values[i] <- v * (1 + rnorm(1, mean = 0, sd = noise))
    v <- v * drift
  }
  data.frame(year = years, value = round(values, 4))
}

#' Return a named list of demo history data matching the structure
#' produced by scrape_gurufocus().
#' Ticker-specific seeds are used so the demo data is reproducible and
#' plausible for the chosen company.
generate_demo_data <- function(ticker) {
  ticker <- toupper(trimws(ticker))
  set.seed(sum(utf8ToInt(ticker)))   # deterministic per ticker

  years <- 2005:2024

  # Baseline parameters (vary lightly by ticker so each looks different)
  rev_base   <- runif(1, 50e9,  400e9)
  eps_base   <- runif(1, 1,     12)
  fcf_base   <- rev_base * runif(1, 0.05, 0.20)
  pe_base    <- runif(1, 12, 30)
  pb_base    <- runif(1, 2, 10)
  ps_base    <- runif(1, 1, 8)
  ev_base    <- runif(1, 8, 25)
  roe_base   <- runif(1, 15, 50)
  roa_base   <- runif(1, 5, 20)
  roic_base  <- runif(1, 10, 35)
  gm_base    <- runif(1, 35, 65)
  om_base    <- runif(1, 15, 35)
  nm_base    <- runif(1, 10, 25)
  de_base    <- runif(1, 0.3, 2.5)
  cr_base    <- runif(1, 1.0, 3.0)

  history <- list(
    revenue       = gen_series(years, rev_base,  drift = 1.08, noise = 0.06),
    eps_diluted   = gen_series(years, eps_base,  drift = 1.10, noise = 0.10),
    fcf           = gen_series(years, fcf_base,  drift = 1.09, noise = 0.12),
    pe            = gen_series(years, pe_base,   drift = 1.00, noise = 0.18),
    pb            = gen_series(years, pb_base,   drift = 1.00, noise = 0.15),
    ps            = gen_series(years, ps_base,   drift = 1.00, noise = 0.15),
    ev_ebitda     = gen_series(years, ev_base,   drift = 1.00, noise = 0.15),
    roe           = gen_series(years, roe_base,  drift = 1.01, noise = 0.10),
    roa           = gen_series(years, roa_base,  drift = 1.01, noise = 0.10),
    roic          = gen_series(years, roic_base, drift = 1.01, noise = 0.10),
    gross_margin  = gen_series(years, gm_base,   drift = 1.00, noise = 0.04),
    oper_margin   = gen_series(years, om_base,   drift = 1.00, noise = 0.07),
    net_margin    = gen_series(years, nm_base,   drift = 1.00, noise = 0.08),
    debt_equity   = gen_series(years, de_base,   drift = 1.00, noise = 0.15),
    current_ratio = gen_series(years, cr_base,   drift = 1.00, noise = 0.10)
  )

  # Ensure positivity for metrics that must be positive
  for (nm in c("revenue","eps_diluted","fcf","pe","pb","ps","ev_ebitda",
               "roe","roa","roic","gross_margin","oper_margin","net_margin",
               "current_ratio")) {
    if (!is.null(history[[nm]])) {
      history[[nm]]$value <- abs(history[[nm]]$value)
    }
  }

  # Summary metrics (fake but plausible)
  last_pe  <- tail(history$pe$value[!is.na(history$pe$value)], 1)
  last_rev <- tail(history$revenue$value[!is.na(history$revenue$value)], 1)
  last_eps <- tail(history$eps_diluted$value[!is.na(history$eps_diluted$value)], 1)
  mktcap   <- last_rev * runif(1, 3, 8)

  summary_metrics <- list(
    "Market Cap"     = format_large(mktcap),
    "Current Price"  = paste0("$", round(last_eps * last_pe, 2)),
    "PE Ratio"       = round(last_pe, 2),
    "PB Ratio"       = round(tail(history$pb$value, 1), 2),
    "PS Ratio"       = round(tail(history$ps$value, 1), 2),
    "EV/EBITDA"      = round(tail(history$ev_ebitda$value, 1), 2),
    "ROE (%)"        = round(tail(history$roe$value, 1), 2),
    "ROA (%)"        = round(tail(history$roa$value, 1), 2),
    "Net Margin (%)" = round(tail(history$net_margin$value, 1), 2),
    "Gross Margin (%)"= round(tail(history$gross_margin$value, 1), 2),
    "Debt/Equity"    = round(tail(history$debt_equity$value, 1), 2),
    "Current Ratio"  = round(tail(history$current_ratio$value, 1), 2),
    "Revenue (TTM)"  = format_large(last_rev),
    "EPS (Diluted)"  = round(last_eps, 2),
    "FCF"            = format_large(tail(history$fcf$value, 1))
  )

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    demo_mode = TRUE
  )
}
