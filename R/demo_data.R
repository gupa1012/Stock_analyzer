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

  years <- 2015:2024

  # Baseline parameters (vary lightly by ticker so each looks different)
  rev_base   <- runif(1, 50e9,  400e9)
  eps_base   <- runif(1, 1,     12)
  fcf_base   <- rev_base * runif(1, 0.05, 0.20)
  op_income_base <- rev_base * runif(1, 0.10, 0.30)
  net_income_base <- rev_base * runif(1, 0.08, 0.22)
  cash_base  <- rev_base * runif(1, 0.08, 0.25)
  debt_base  <- rev_base * runif(1, 0.05, 0.20)
  ocf_base   <- rev_base * runif(1, 0.10, 0.24)
  buyback_base <- rev_base * runif(1, 0.01, 0.05)
  pe_base    <- runif(1, 12, 30)
  fpe_base   <- pe_base * runif(1, 0.8, 1.0)
  peg_base   <- runif(1, 1.0, 2.8)
  pb_base    <- runif(1, 2, 10)
  ps_base    <- runif(1, 1, 8)
  ev_rev_base<- runif(1, 1, 8)
  ev_base    <- runif(1, 8, 25)
  divy_base  <- runif(1, 0.005, 0.04)
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
    operating_income = gen_series(years, op_income_base, drift = 1.09, noise = 0.09),
    net_income    = gen_series(years, net_income_base, drift = 1.09, noise = 0.10),
    eps_diluted   = gen_series(years, eps_base,  drift = 1.10, noise = 0.10),
    cash          = gen_series(years, cash_base, drift = 1.07, noise = 0.10),
    debt          = gen_series(years, debt_base, drift = 1.04, noise = 0.12),
    operating_cashflow = gen_series(years, ocf_base, drift = 1.09, noise = 0.10),
    free_cashflow = gen_series(years, fcf_base,  drift = 1.09, noise = 0.12),
    fcf           = gen_series(years, fcf_base,  drift = 1.09, noise = 0.12),
    buybacks      = gen_series(years, buyback_base, drift = 1.06, noise = 0.18),
    pe            = gen_series(years, pe_base,   drift = 1.00, noise = 0.18),
    forward_pe    = gen_series(years, fpe_base,  drift = 1.00, noise = 0.16),
    peg_ratio     = gen_series(years, peg_base,  drift = 1.00, noise = 0.12),
    pb            = gen_series(years, pb_base,   drift = 1.00, noise = 0.15),
    ps            = gen_series(years, ps_base,   drift = 1.00, noise = 0.15),
    ev_revenue    = gen_series(years, ev_rev_base, drift = 1.00, noise = 0.12),
    ev_ebitda     = gen_series(years, ev_base,   drift = 1.00, noise = 0.15),
    dividend_yield= gen_series(years, divy_base * 100, drift = 1.00, noise = 0.18),
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
  for (nm in c("revenue","operating_income","net_income","eps_diluted","cash",
               "debt","operating_cashflow","free_cashflow","fcf","buybacks",
               "pe","forward_pe","peg_ratio","pb","ps","ev_revenue","ev_ebitda",
               "dividend_yield",
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
  last_fcf <- tail(history$free_cashflow$value[!is.na(history$free_cashflow$value)], 1)
  last_cash <- tail(history$cash$value[!is.na(history$cash$value)], 1)
  last_debt <- tail(history$debt$value[!is.na(history$debt$value)], 1)
  last_ocf <- tail(history$operating_cashflow$value[!is.na(history$operating_cashflow$value)], 1)
  last_buybacks <- tail(history$buybacks$value[!is.na(history$buybacks$value)], 1)
  mktcap   <- last_rev * runif(1, 3, 8)
  cur_price <- last_eps * last_pe
  analyst_target <- cur_price * runif(1, 1.02, 1.18)
  target_upside <- (analyst_target / cur_price - 1) * 100
  low_52w <- cur_price * runif(1, 0.65, 0.85)
  high_52w <- cur_price * runif(1, 1.08, 1.28)
  company_name <- paste(ticker, "Holdings")

  summary_metrics <- list(
    "Company Name"     = company_name,
    "Sector"           = sample(c("Technology", "Healthcare", "Consumer", "Industrials"), 1),
    "Industry"         = sample(c("Software", "Semiconductors", "Medical Devices", "Consumer Brands"), 1),
    "Exchange"         = sample(c("NASDAQ", "NYSE"), 1),
    "Asset Type"       = "Common Stock",
    "Reported Currency"= "USD",
    "Description"      = paste(company_name, "is demo data generated for the redesigned Alpha Vantage dashboard."),
    "Official Site"    = "--",
    "Market Cap"       = format_large(mktcap),
    "Current Price"    = paste0("$", round(cur_price, 2)),
    "Analyst Target Price" = paste0("$", round(analyst_target, 2)),
    "Target Upside (%)" = paste0(round(target_upside, 2), "%"),
    "Trailing PE"      = round(last_pe, 2),
    "Forward PE"       = round(tail(history$forward_pe$value, 1), 2),
    "PEG Ratio"       = round(tail(history$peg_ratio$value, 1), 2),
    "Dividend Yield"  = paste0(round(tail(history$dividend_yield$value, 1), 2), "%"),
    "Dividend Per Share" = paste0("$", round(cur_price * tail(history$dividend_yield$value, 1) / 100, 2)),
    "PE Ratio"        = round(last_pe, 2),
    "PB Ratio"        = round(tail(history$pb$value, 1), 2),
    "PS Ratio"        = round(tail(history$ps$value, 1), 2),
    "Price to Book"   = round(tail(history$pb$value, 1), 2),
    "Price to Sales"  = round(tail(history$ps$value, 1), 2),
    "EV/Revenue"      = round(tail(history$ev_revenue$value, 1), 2),
    "EV/EBITDA"       = round(tail(history$ev_ebitda$value, 1), 2),
    "ROE (%)"         = round(tail(history$roe$value, 1), 2),
    "ROA (%)"         = round(tail(history$roa$value, 1), 2),
    "Profit Margin"   = paste0(round(tail(history$net_margin$value, 1), 2), "%"),
    "Operating Margin" = paste0(round(tail(history$oper_margin$value, 1), 2), "%"),
    "Net Margin (%)"  = paste0(round(tail(history$net_margin$value, 1), 2), "%"),
    "Gross Margin (%)" = paste0(round(tail(history$gross_margin$value, 1), 2), "%"),
    "Oper Margin (%)" = paste0(round(tail(history$oper_margin$value, 1), 2), "%"),
    "Debt/Equity"     = round(tail(history$debt_equity$value, 1), 2),
    "Current Ratio"   = round(tail(history$current_ratio$value, 1), 2),
    "52W Range"       = paste0("$", round(low_52w, 2), " - $", round(high_52w, 2)),
    "52W Low"         = paste0("$", round(low_52w, 2)),
    "52W High"        = paste0("$", round(high_52w, 2)),
    "Beta"            = round(runif(1, 0.7, 1.5), 2),
    "Quarterly Revenue Growth" = paste0(round(runif(1, 3, 18), 2), "%"),
    "Quarterly Earnings Growth" = paste0(round(runif(1, 2, 20), 2), "%"),
    "Revenue (TTM)"   = format_large(last_rev),
    "Revenue per Share" = round(last_rev / runif(1, 2e9, 8e9), 2),
    "EPS"             = round(last_eps, 2),
    "EPS (Diluted)"   = round(last_eps, 2),
    "Diluted EPS TTM" = round(last_eps, 2),
    "EBITDA (TTM)"    = format_large(last_rev * runif(1, 0.15, 0.32)),
    "Cash"            = format_large(last_cash),
    "Debt"            = format_large(last_debt),
    "Operating Cash Flow" = format_large(last_ocf),
    "Free Cash Flow"  = format_large(last_fcf),
    "Buybacks"        = format_large(last_buybacks),
    "FCF"             = format_large(last_fcf),
    "Source"          = "Demo Data"
  )

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    demo_mode = TRUE
  )
}
