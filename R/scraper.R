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

# â”€â”€ Convenience: extract a labelled metric from the summary list â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

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
