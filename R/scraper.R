# R/scraper.R
# Fundamental data via Yahoo Finance JSON API â€“ no Selenium required.
# Endpoints used:
#   v8/finance/chart/{ticker}           â€“ current price & metadata
#   ws/fundamentals-timeseries/v1/â€¦     â€“ annual history (4 most recent years)

library(jsonlite)
library(httr)

YF_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"
)

# â”€â”€ HTTP helper â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

yf_get <- function(url) {
  tryCatch({
    resp <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = YF_UA,
        "Accept"     = "application/json, */*"
      ),
      httr::timeout(20)
    )
    if (httr::status_code(resp) != 200) return(NULL)
    jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )
  }, error = function(e) NULL)
}

# â”€â”€ Current price via v8 chart â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

fetch_current_price <- function(ticker) {
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    utils::URLencode(ticker),
    "?interval=1d&range=1d"
  )
  js <- yf_get(url)
  if (is.null(js)) return(NA_real_)
  tryCatch(
    as.numeric(js$chart$result[[1]]$meta$regularMarketPrice),
    error = function(e) NA_real_
  )
}

# â”€â”€ Annual time-series â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

TS_TYPES <- c(
  "annualTotalRevenue",
  "annualDilutedEPS",
  "annualFreeCashFlow",
  "annualNetIncome",
  "annualGrossProfit",
  "annualOperatingIncome",
  "annualPeRatio",
  "annualPriceToBook",
  "annualReturnOnEquity",
  "annualReturnOnAssets",
  "annualCurrentRatio",
  "annualDebtToEquity",
  "annualStockholdersEquity",
  "annualTotalAssets",
  "annualCurrentAssets",
  "annualCurrentLiabilities",
  "annualLongTermDebt"
)

fetch_timeseries <- function(ticker) {
  url <- paste0(
    "https://query1.finance.yahoo.com/ws/fundamentals-timeseries/v1/finance/timeseries/",
    utils::URLencode(ticker),
    "?type=", paste(TS_TYPES, collapse = ","),
    "&period1=946684800",
    "&period2=", as.integer(Sys.time()) + 86400
  )
  js <- yf_get(url)
  if (is.null(js)) return(list())

  results <- list()
  for (item in js$timeseries$result) {
    nms <- names(item)
    sn  <- nms[length(nms)]
    rows <- item[[sn]]
    if (!is.list(rows) || length(rows) == 0) next
    years <- vapply(rows, function(x)
      as.integer(format(as.Date(x$asOfDate), "%Y")), integer(1))
    values <- vapply(rows, function(x) {
      rv <- x$reportedValue
      if (is.list(rv)) as.numeric(rv$raw) else as.numeric(rv)
    }, numeric(1))
    df <- data.frame(year = years, value = values, stringsAsFactors = FALSE)
    df <- df[!is.na(df$value), ]
    if (nrow(df) > 0) results[[sn]] <- df[order(df$year), ]
  }
  results
}

# â”€â”€ Derived series helpers â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

pct_series <- function(numer_df, denom_df) {
  if (is.null(numer_df) || is.null(denom_df)) return(NULL)
  m <- merge(numer_df, denom_df, by = "year", suffixes = c("_n", "_d"))
  if (nrow(m) == 0) return(NULL)
  data.frame(year = m$year,
             value = round(m$value_n / m$value_d * 100, 4),
             stringsAsFactors = FALSE)
}

ratio_series <- function(numer_df, denom_df) {
  if (is.null(numer_df) || is.null(denom_df)) return(NULL)
  m <- merge(numer_df, denom_df, by = "year", suffixes = c("_n", "_d"))
  if (nrow(m) == 0) return(NULL)
  data.frame(year = m$year,
             value = round(m$value_n / m$value_d, 4),
             stringsAsFactors = FALSE)
}

scale_pct <- function(df) {
  if (is.null(df)) return(NULL)
  df$value <- df$value * 100
  df
}

# â”€â”€ Main fetch entry point â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

#' Fetch fundamental data for a ticker from Yahoo Finance (no Selenium needed).
#' Returns the same structure expected by the analysis module:
#'   $ticker, $summary (named list), $history (named list of data.frames),
#'   $timestamp, $demo_mode
fetch_yahoo_fundamentals <- function(ticker) {
  ticker <- toupper(trimws(ticker))
  message("Fetching Yahoo Finance data for: ", ticker)

  ts    <- fetch_timeseries(ticker)
  price <- fetch_current_price(ticker)

  # Build named history list
  history <- list()
  history$revenue      <- ts[["annualTotalRevenue"]]
  history$eps_diluted  <- ts[["annualDilutedEPS"]]
  history$fcf          <- ts[["annualFreeCashFlow"]]
  history$pe           <- ts[["annualPeRatio"]]
  history$pb           <- ts[["annualPriceToBook"]]

  # Margins â€“ computed from IS items
  history$gross_margin <- pct_series(ts[["annualGrossProfit"]],
                                     ts[["annualTotalRevenue"]])
  history$oper_margin  <- pct_series(ts[["annualOperatingIncome"]],
                                     ts[["annualTotalRevenue"]])
  history$net_margin   <- pct_series(ts[["annualNetIncome"]],
                                     ts[["annualTotalRevenue"]])

  # ROE / ROA â€“ timeseries values are 0â€“1 fractions; scale to %
  history$roe   <- if (!is.null(ts[["annualReturnOnEquity"]]))
    scale_pct(ts[["annualReturnOnEquity"]])
  else
    pct_series(ts[["annualNetIncome"]], ts[["annualStockholdersEquity"]])

  history$roa   <- if (!is.null(ts[["annualReturnOnAssets"]]))
    scale_pct(ts[["annualReturnOnAssets"]])
  else
    pct_series(ts[["annualNetIncome"]], ts[["annualTotalAssets"]])

  history$debt_equity <- if (!is.null(ts[["annualDebtToEquity"]]))
    ts[["annualDebtToEquity"]]
  else
    ratio_series(ts[["annualLongTermDebt"]], ts[["annualStockholdersEquity"]])

  history$current_ratio <- if (!is.null(ts[["annualCurrentRatio"]]))
    ts[["annualCurrentRatio"]]
  else
    ratio_series(ts[["annualCurrentAssets"]], ts[["annualCurrentLiabilities"]])

  # Not available in free Yahoo timeseries
  history$roic     <- NULL
  history$ev_ebitda <- NULL
  history$ps        <- NULL

  history <- Filter(Negate(is.null), history)

  # Summary â€“ most-recent value from each series + current price
  last_val <- function(series) {
    if (is.null(series) || nrow(series) == 0) return(NA_real_)
    series$value[nrow(series)]
  }

  last_eps <- last_val(history$eps_diluted)
  last_pe  <- last_val(history$pe)
  cur_price <- if (!is.na(price)) price else
    if (!is.na(last_eps) && !is.na(last_pe)) round(last_eps * last_pe, 2) else NA_real_

  fmt_num <- function(x, digits = 2) if (!is.na(x)) round(x, digits) else "--"
  fmt_pct <- function(x) if (!is.na(x)) paste0(round(x, 2), "%") else "--"

  summary_metrics <- list(
    "Current Price"    = if (!is.na(cur_price)) paste0("$", round(cur_price, 2)) else "--",
    "PE Ratio"         = fmt_num(last_pe),
    "PB Ratio"         = fmt_num(last_val(history$pb)),
    "PS Ratio"         = "--",
    "EV/EBITDA"        = "--",
    "ROE (%)"          = fmt_num(last_val(history$roe)),
    "ROA (%)"          = fmt_num(last_val(history$roa)),
    "Net Margin (%)"   = fmt_num(last_val(history$net_margin)),
    "Gross Margin (%)" = fmt_num(last_val(history$gross_margin)),
    "Oper Margin (%)"  = fmt_num(last_val(history$oper_margin)),
    "Debt/Equity"      = fmt_num(last_val(history$debt_equity)),
    "Current Ratio"    = fmt_num(last_val(history$current_ratio)),
    "Revenue (TTM)"    = if (!is.na(last_val(history$revenue)))
      format_large(last_val(history$revenue)) else "--",
    "EPS (Diluted)"    = fmt_num(last_eps),
    "FCF"              = if (!is.na(last_val(history$fcf)))
      format_large(last_val(history$fcf)) else "--",
    "Source"           = "Yahoo Finance"
  )

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    demo_mode = FALSE
  )
}

# ── Alpha Vantage (via alphavantager package) ─────────────────────────────────
#  Uses the alphavantager R package instead of raw HTTP requests.
#  4 API calls per analysis: OVERVIEW + INCOME_STATEMENT + BALANCE_SHEET + CASH_FLOW
#  Free tier: 25 req/day → ~6 full analyses/day.
#  Key is read from env var alpha_vantage_api_key (set via .Renviron or system).

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Safe wrapper: call alphavantager::av_get, return NULL on failure
safe_av <- function(symbol, av_fun) {
  tryCatch({
    alphavantager::av_get(symbol = symbol, av_fun = av_fun)
  }, error = function(e) {
    message("Alpha Vantage (", av_fun, ") error: ", e$message)
    NULL
  })
}

# Parse a single field from the OVERVIEW tibble (1-row, wide format)
av_ov_num <- function(ov, field) {
  if (is.null(ov) || !field %in% names(ov)) return(NA_real_)
  v <- ov[[field]][1]
  if (is.null(v) || is.na(v) || v %in% c("None", "N/A", "-", "")) return(NA_real_)
  suppressWarnings(as.numeric(v))
}

# Convert an annual-reports tibble (INCOME_STATEMENT etc.) column → data.frame(year, value)
av_col_to_df <- function(tbl, col) {
  if (is.null(tbl) || !col %in% names(tbl) || !"fiscalDateEnding" %in% names(tbl))
    return(NULL)
  df <- data.frame(
    year  = as.integer(substr(as.character(tbl$fiscalDateEnding), 1, 4)),
    value = suppressWarnings(as.numeric(tbl[[col]])),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$year) & !is.na(df$value), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df[order(df$year), ]
}

#' Fetch fundamentals via Alpha Vantage (alphavantager package).
#' Returns the same structure as fetch_yahoo_fundamentals().
fetch_av_fundamentals <- function(ticker, api_key) {
  ticker <- toupper(trimws(ticker))
  if (nchar(trimws(api_key)) == 0) return(NULL)

  # Set the API key for the alphavantager session
  Sys.setenv(AV_API_KEY = api_key)
  message("Fetching Alpha Vantage fundamentals for: ", ticker)

  ov  <- safe_av(ticker, "OVERVIEW")
  is_ <- safe_av(ticker, "INCOME_STATEMENT")
  bs_ <- safe_av(ticker, "BALANCE_SHEET")
  cf_ <- safe_av(ticker, "CASH_FLOW")

  if (is.null(ov) || ncol(ov) < 3) {
    message("Alpha Vantage OVERVIEW empty for ", ticker)
    return(NULL)
  }

  # alphavantager returns annual + quarterly in one list; filter annuals
  ar_is <- if (is.data.frame(is_)) is_ else NULL
  ar_bs <- if (is.data.frame(bs_)) bs_ else NULL
  ar_cf <- if (is.data.frame(cf_)) cf_ else NULL

  rev_df   <- av_col_to_df(ar_is, "totalRevenue")
  gp_df    <- av_col_to_df(ar_is, "grossProfit")
  oi_df    <- av_col_to_df(ar_is, "operatingIncome")
  ni_df    <- av_col_to_df(ar_is, "netIncome")
  eps_df   <- av_col_to_df(ar_is, "dilutedEPS")

  ta_df    <- av_col_to_df(ar_bs, "totalAssets")
  tca_df   <- av_col_to_df(ar_bs, "totalCurrentAssets")
  tcl_df   <- av_col_to_df(ar_bs, "totalCurrentLiabilities")
  eq_df    <- av_col_to_df(ar_bs, "totalShareholderEquity")
  ltd_df   <- av_col_to_df(ar_bs, "longTermDebt")

  ocf_df   <- av_col_to_df(ar_cf, "operatingCashflow")
  capex_df <- av_col_to_df(ar_cf, "capitalExpenditures")

  fcf_df <- if (!is.null(ocf_df) && !is.null(capex_df)) {
    m <- merge(ocf_df, capex_df, by = "year", suffixes = c("_o", "_c"))
    if (nrow(m) > 0)
      data.frame(year = m$year,
                 value = m$value_o - abs(m$value_c),
                 stringsAsFactors = FALSE)
    else ocf_df
  } else ocf_df

  history <- list(
    revenue       = rev_df,
    eps_diluted   = eps_df,
    fcf           = fcf_df,
    gross_margin  = pct_series(gp_df, rev_df),
    oper_margin   = pct_series(oi_df, rev_df),
    net_margin    = pct_series(ni_df, rev_df),
    roe           = pct_series(ni_df, eq_df),
    roa           = pct_series(ni_df, ta_df),
    debt_equity   = ratio_series(ltd_df, eq_df),
    current_ratio = ratio_series(tca_df, tcl_df)
  )

  # Valuation ratios — current snapshot from OVERVIEW
  pe_cur <- av_ov_num(ov, "PERatio")
  pb_cur <- av_ov_num(ov, "PriceToBookRatio")
  ps_cur <- av_ov_num(ov, "PriceToSalesRatioTTM")
  ev_cur <- av_ov_num(ov, "EVToEBITDA")

  make1 <- function(v) {
    if (is.na(v)) return(NULL)
    data.frame(year = as.integer(format(Sys.Date(), "%Y")),
               value = v, stringsAsFactors = FALSE)
  }
  history$pe        <- make1(pe_cur)
  history$pb        <- make1(pb_cur)
  history$ps        <- make1(ps_cur)
  history$ev_ebitda <- make1(ev_cur)
  history <- Filter(Negate(is.null), history)

  # Current price via Yahoo (free, no key needed)
  cur_price <- fetch_current_price(ticker)
  if (is.na(cur_price)) {
    leps <- if (!is.null(eps_df) && nrow(eps_df) > 0) tail(eps_df$value, 1) else NA_real_
    cur_price <- if (!is.na(leps) && !is.na(pe_cur)) round(leps * pe_cur, 2) else NA_real_
  }

  last_val     <- function(s) if (!is.null(s) && nrow(s) > 0) tail(s$value, 1) else NA_real_
  fmt_num      <- function(x, d = 2) if (!is.na(x)) round(x, d) else "--"
  fmt_pct_dec  <- function(x) if (!is.na(x)) paste0(round(x * 100, 2), "%") else "--"
  fmt_pct_frac <- function(x) if (!is.na(x)) paste0(round(x,       2), "%") else "--"

  roe_ttm <- av_ov_num(ov, "ReturnOnEquityTTM")
  roa_ttm <- av_ov_num(ov, "ReturnOnAssetsTTM")
  pm_ttm  <- av_ov_num(ov, "ProfitMargin")
  om_ttm  <- av_ov_num(ov, "OperatingMarginTTM")
  rev_ttm <- av_ov_num(ov, "RevenueTTM")
  gp_ttm  <- av_ov_num(ov, "GrossProfitTTM")
  gm_ttm  <- if (!is.na(rev_ttm) && rev_ttm > 0 && !is.na(gp_ttm))
               gp_ttm / rev_ttm else NA_real_

  summary_metrics <- list(
    "Current Price"    = if (!is.na(cur_price)) paste0("$", round(cur_price, 2)) else "--",
    "PE Ratio"         = fmt_num(pe_cur),
    "PB Ratio"         = fmt_num(pb_cur),
    "PS Ratio"         = fmt_num(ps_cur),
    "EV/EBITDA"        = fmt_num(ev_cur),
    "ROE (%)"          = fmt_pct_dec(roe_ttm),
    "ROA (%)"          = fmt_pct_dec(roa_ttm),
    "Net Margin (%)"   = fmt_pct_dec(pm_ttm),
    "Gross Margin (%)" = fmt_pct_frac(gm_ttm * 100),
    "Oper Margin (%)"  = fmt_pct_dec(om_ttm),
    "Debt/Equity"      = fmt_num(last_val(history$debt_equity)),
    "Current Ratio"    = fmt_num(last_val(history$current_ratio)),
    "Revenue (TTM)"    = if (!is.na(rev_ttm)) format_large(rev_ttm) else "--",
    "EPS (Diluted)"    = fmt_num(av_ov_num(ov, "EPS")),
    "FCF"              = if (!is.na(last_val(history$fcf)))
                           format_large(last_val(history$fcf)) else "--",
    "Source"           = "Alpha Vantage"
  )

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    demo_mode = FALSE
  )
}

#' Search the summary list by partial label match (case-insensitive).
get_metric <- function(summary_list, pattern, default = NA_character_) {
  if (length(summary_list) == 0) return(default)
  idx <- grep(pattern, names(summary_list), ignore.case = TRUE, value = TRUE)
  if (length(idx) == 0) return(default)
  val <- as.character(summary_list[[idx[[1]]]])
  if (is.na(val) || val == "--") return(default)
  val
}

#' As above but coerce to numeric.
get_metric_num <- function(summary_list, pattern, default = NA_real_) {
  val <- get_metric(summary_list, pattern, default = NA_character_)
  if (is.null(val) || is.na(val)) return(default)
  parse_numeric_str(val)
}
