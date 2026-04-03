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
  history$operating_income <- ts[["annualOperatingIncome"]]
  history$net_income   <- ts[["annualNetIncome"]]
  history$eps_diluted  <- ts[["annualDilutedEPS"]]
  history$fcf          <- ts[["annualFreeCashFlow"]]
  history$free_cashflow <- history$fcf
  history$debt         <- ts[["annualLongTermDebt"]]
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
    "Company Name"     = ticker,
    "Sector"           = "--",
    "Industry"         = "--",
    "Description"      = "Reduced Yahoo Finance fallback dataset.",
    "Reported Currency"= "USD",
    "Market Cap"       = "--",
    "Current Price"    = if (!is.na(cur_price)) paste0("$", round(cur_price, 2)) else "--",
    "Trailing PE"      = fmt_num(last_pe),
    "Forward PE"       = "--",
    "PEG Ratio"        = "--",
    "Dividend Yield"   = "--",
    "PE Ratio"         = fmt_num(last_pe),
    "PB Ratio"         = fmt_num(last_val(history$pb)),
    "PS Ratio"         = "--",
    "EV/Revenue"       = "--",
    "EV/EBITDA"        = "--",
    "ROE (%)"          = fmt_num(last_val(history$roe)),
    "ROA (%)"          = fmt_num(last_val(history$roa)),
    "Profit Margin"    = fmt_num(last_val(history$net_margin)),
    "Operating Margin" = fmt_num(last_val(history$oper_margin)),
    "Net Margin (%)"   = fmt_num(last_val(history$net_margin)),
    "Gross Margin (%)" = fmt_num(last_val(history$gross_margin)),
    "Oper Margin (%)"  = fmt_num(last_val(history$oper_margin)),
    "Debt/Equity"      = fmt_num(last_val(history$debt_equity)),
    "Current Ratio"    = fmt_num(last_val(history$current_ratio)),
    "Cash"             = "--",
    "Debt"             = if (!is.na(last_val(history$debt)))
      format_large(last_val(history$debt)) else "--",
    "Operating Cash Flow" = "--",
    "Revenue (TTM)"    = if (!is.na(last_val(history$revenue)))
      format_large(last_val(history$revenue)) else "--",
    "EPS (Diluted)"    = fmt_num(last_eps),
    "Free Cash Flow"   = if (!is.na(last_val(history$free_cashflow)))
      format_large(last_val(history$free_cashflow)) else "--",
    "FCF"              = if (!is.na(last_val(history$fcf)))
      format_large(last_val(history$fcf)) else "--",
    "Source"           = "Yahoo Finance"
  )

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    metadata  = list(
      source_breakdown = c(
        "Fundamentals" = "Yahoo Finance JSON fallback"
      )
    ),
    demo_mode = FALSE
  )
}

# ── Alpha Vantage (via alphavantager package) ─────────────────────────────────
#  Uses the alphavantager R package instead of raw HTTP requests.
#  4 API calls per analysis: OVERVIEW + INCOME_STATEMENT + BALANCE_SHEET + CASH_FLOW
#  Free tier: 25 req/day → ~6 full analyses/day.
#  Key is read from env var alpha_vantage_api_key (set via .Renviron or system).

.av_cache <- new.env(parent = emptyenv())
.av_cache_ttl <- 43200L  # 12 hours
.av_min_interval <- 1.2
.av_last_request_at <- as.POSIXct(NA)
.av_daily_limit <- 25L

.fundamentals_cache_dir <- file.path("user_data", "cache", "fundamentals")
.fundamentals_cache_ttl <- 30L * 24L * 60L * 60L  # 30 days

.sec_ticker_cache <- new.env(parent = emptyenv())
.sec_companyfacts_cache <- new.env(parent = emptyenv())
.sec_companyfacts_ttl <- 43200L  # 12 hours
SEC_UA <- "StockAnalyzer/1.0 (local Shiny app; contact unavailable)"

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

av_ov_text <- function(ov, field, default = NA_character_) {
  if (is.null(ov) || !field %in% names(ov)) return(default)
  v <- as.character(ov[[field]][1])
  if (is.na(v) || trimws(v) %in% c("", "None", "N/A", "-")) return(default)
  trimws(v)
}

av_ov_num_first <- function(ov, fields) {
  for (field in fields) {
    value <- av_ov_num(ov, field)
    if (!is.na(value)) return(value)
  }
  NA_real_
}

av_cache_key <- function(symbol, av_fun) {
  make.names(paste(toupper(symbol), av_fun, sep = "__"))
}

ensure_fundamentals_cache_dir <- function() {
  if (!dir.exists(.fundamentals_cache_dir)) {
    dir.create(.fundamentals_cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

fundamentals_cache_file <- function(namespace, key) {
  ensure_fundamentals_cache_dir()
  file.path(
    .fundamentals_cache_dir,
    paste0(make.names(namespace), "__", make.names(key), ".rds")
  )
}

fundamentals_cache_get <- function(namespace, key, ttl = .fundamentals_cache_ttl) {
  cache_file <- fundamentals_cache_file(namespace, key)
  if (!file.exists(cache_file)) return(NULL)

  entry <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (is.null(entry)) return(NULL)

  fetched_at <- entry$fetched_at %||% file.info(cache_file)$mtime
  fetched_at <- tryCatch(as.POSIXct(fetched_at, tz = "UTC"), error = function(e) NA)
  if (is.na(fetched_at)) return(NULL)

  age <- as.numeric(difftime(Sys.time(), fetched_at, units = "secs"))
  if (is.na(age) || age >= ttl) return(NULL)

  entry$data %||% NULL
}

fundamentals_cache_put <- function(namespace, key, data) {
  cache_file <- fundamentals_cache_file(namespace, key)
  tryCatch({
    saveRDS(list(
      fetched_at = Sys.time(),
      namespace = namespace,
      key = key,
      data = data
    ), cache_file)
  }, error = function(e) invisible(NULL))
  invisible(data)
}

av_usage_log_file <- function() {
  ensure_fundamentals_cache_dir()
  file.path(.fundamentals_cache_dir, "av_usage__live_requests.rds")
}

read_av_usage_log <- function() {
  log_file <- av_usage_log_file()
  empty_log <- data.frame(
    timestamp = as.POSIXct(character()),
    symbol = character(),
    endpoint = character(),
    stringsAsFactors = FALSE
  )

  if (!file.exists(log_file)) return(empty_log)

  log_df <- tryCatch(readRDS(log_file), error = function(e) NULL)
  if (is.null(log_df) || !is.data.frame(log_df) || nrow(log_df) == 0) return(empty_log)

  if (!"timestamp" %in% names(log_df)) log_df$timestamp <- as.POSIXct(character())
  if (!"symbol" %in% names(log_df)) log_df$symbol <- character(nrow(log_df))
  if (!"endpoint" %in% names(log_df)) log_df$endpoint <- character(nrow(log_df))

  log_df$timestamp <- suppressWarnings(as.POSIXct(log_df$timestamp, origin = "1970-01-01", tz = "UTC"))
  log_df <- log_df[!is.na(log_df$timestamp), c("timestamp", "symbol", "endpoint"), drop = FALSE]
  log_df[order(log_df$timestamp), , drop = FALSE]
}

write_av_usage_log <- function(log_df) {
  log_file <- av_usage_log_file()
  tryCatch(saveRDS(log_df, log_file), error = function(e) invisible(NULL))
  invisible(log_df)
}

append_av_usage_log <- function(symbol, av_fun) {
  log_df <- read_av_usage_log()
  log_df <- rbind(
    log_df,
    data.frame(
      timestamp = Sys.time(),
      symbol = toupper(trimws(symbol)),
      endpoint = av_fun,
      stringsAsFactors = FALSE
    )
  )

  cutoff <- Sys.time() - (14L * 24L * 60L * 60L)
  log_df <- log_df[log_df$timestamp >= cutoff, , drop = FALSE]
  write_av_usage_log(log_df)
}

get_av_usage_status <- function(limit = .av_daily_limit) {
  log_df <- read_av_usage_log()
  today <- as.Date(Sys.time())
  today_count <- if (nrow(log_df) == 0) {
    0L
  } else {
    sum(as.Date(log_df$timestamp) == today, na.rm = TRUE)
  }

  list(
    date = today,
    live_requests_today = as.integer(today_count),
    estimated_remaining = as.integer(max(limit - today_count, 0L)),
    limit = as.integer(limit),
    official_endpoint = FALSE
  )
}

av_cache_get <- function(symbol, av_fun) {
  key <- av_cache_key(symbol, av_fun)
  entry <- .av_cache[[key]]
  if (!is.null(entry)) {
    age <- as.numeric(difftime(Sys.time(), entry$time, units = "secs"))
    if (!is.na(age) && age < .av_cache_ttl) {
      cached <- entry$data
      attr(cached, "cache_origin") <- "memory"
      return(cached)
    }
  }

  disk_cached <- fundamentals_cache_get("av_endpoint", key)
  if (!is.null(disk_cached)) {
    .av_cache[[key]] <- list(time = Sys.time(), data = disk_cached)
    attr(disk_cached, "cache_origin") <- "disk"
    return(disk_cached)
  }

  NULL
}

av_cache_put <- function(symbol, av_fun, data) {
  key <- av_cache_key(symbol, av_fun)
  .av_cache[[key]] <- list(time = Sys.time(), data = data)
  fundamentals_cache_put("av_endpoint", key, data)
}

av_rate_limit_wait <- function() {
  if (is.na(.av_last_request_at[1])) return(invisible(NULL))
  elapsed <- as.numeric(difftime(Sys.time(), .av_last_request_at, units = "secs"))
  remaining <- .av_min_interval - elapsed
  if (is.finite(remaining) && remaining > 0) Sys.sleep(remaining)
  invisible(NULL)
}

is_missing_summary_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(TRUE)
  if (all(is.na(value))) return(TRUE)
  txt <- trimws(as.character(value[[1]]))
  identical(txt, "") || identical(txt, "--") || identical(txt, "N/A")
}

has_series_data <- function(df) {
  !is.null(df) && is.data.frame(df) && nrow(df) > 0
}

history_coverage_count <- function(history, names) {
  sum(vapply(names, function(name) has_series_data(history[[name]]), logical(1)))
}

has_strong_statement_coverage <- function(history) {
  core_fields <- c(
    "revenue", "operating_income", "net_income",
    "cash", "debt", "operating_cashflow", "free_cashflow"
  )

  history_coverage_count(history, core_fields) >= 5 &&
    has_series_data(history[["revenue"]]) &&
    has_series_data(history[["net_income"]]) &&
    has_series_data(history[["operating_cashflow"]])
}

merge_summary_lists <- function(primary, fallback) {
  if (is.null(primary) || length(primary) == 0) return(fallback)
  if (is.null(fallback) || length(fallback) == 0) return(primary)

  merged <- primary
  for (name in names(fallback)) {
    if (!name %in% names(merged) || is_missing_summary_value(merged[[name]])) {
      merged[[name]] <- fallback[[name]]
    }
  }
  merged
}

merge_history_lists <- function(primary, fallback) {
  if (is.null(primary) || length(primary) == 0) return(fallback)
  if (is.null(fallback) || length(fallback) == 0) return(primary)

  merged <- primary
  for (name in names(fallback)) {
    if (!name %in% names(merged) || !has_series_data(merged[[name]])) {
      merged[[name]] <- fallback[[name]]
    }
  }
  Filter(Negate(is.null), merged)
}

is_probably_us_ticker <- function(ticker) {
  ticker <- toupper(trimws(as.character(ticker)))
  nzchar(ticker) && !grepl("[.=]", ticker)
}

sec_get_json <- function(url) {
  tryCatch({
    resp <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = SEC_UA,
        "Accept" = "application/json"
      ),
      httr::timeout(20)
    )
    if (httr::status_code(resp) != 200) return(NULL)
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  }, error = function(e) NULL)
}

sec_get_ticker_map <- function() {
  cached <- .sec_ticker_cache[["ticker_map"]]
  if (!is.null(cached)) {
    attr(cached, "cache_origin") <- "memory"
    return(cached)
  }

  disk_cached <- fundamentals_cache_get("sec", "ticker_map")
  if (!is.null(disk_cached)) {
    .sec_ticker_cache[["ticker_map"]] <- disk_cached
    attr(disk_cached, "cache_origin") <- "disk"
    return(disk_cached)
  }

  payload <- sec_get_json("https://www.sec.gov/files/company_tickers.json")
  if (is.null(payload)) return(NULL)

  df <- if (is.data.frame(payload)) {
    payload
  } else if (is.list(payload)) {
    items <- Filter(is.list, unname(payload))
    if (length(items) == 0) return(NULL)
    do.call(rbind, lapply(items, function(item) {
      data.frame(
        cik_str = suppressWarnings(as.integer(item$cik_str %||% item$cik)),
        ticker = toupper(trimws(as.character(item$ticker %||% ""))),
        title = as.character(item$title %||% ""),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    NULL
  }

  if (is.null(df) || nrow(df) == 0) return(NULL)
  .sec_ticker_cache[["ticker_map"]] <- df
  fundamentals_cache_put("sec", "ticker_map", df)
  attr(df, "cache_origin") <- "live"
  df
}

sec_lookup_cik <- function(ticker) {
  ticker_map <- sec_get_ticker_map()
  if (is.null(ticker_map) || nrow(ticker_map) == 0) return(NA_character_)

  row <- ticker_map[ticker_map$ticker == toupper(trimws(ticker)), , drop = FALSE]
  if (nrow(row) == 0 || is.na(row$cik_str[1])) return(NA_character_)
  sprintf("%010d", as.integer(row$cik_str[1]))
}

sec_companyfacts_get <- function(cik) {
  key <- make.names(cik)
  cached <- .sec_companyfacts_cache[[key]]
  if (!is.null(cached)) {
    age <- as.numeric(difftime(Sys.time(), cached$time, units = "secs"))
    if (!is.na(age) && age < .sec_companyfacts_ttl) {
      cached_data <- cached$data
      attr(cached_data, "cache_origin") <- "memory"
      return(cached_data)
    }
  }

  disk_cached <- fundamentals_cache_get("sec_companyfacts", cik)
  if (!is.null(disk_cached)) {
    .sec_companyfacts_cache[[key]] <- list(time = Sys.time(), data = disk_cached)
    attr(disk_cached, "cache_origin") <- "disk"
    return(disk_cached)
  }

  payload <- sec_get_json(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json"))
  if (is.null(payload)) return(NULL)

  .sec_companyfacts_cache[[key]] <- list(time = Sys.time(), data = payload)
  fundamentals_cache_put("sec_companyfacts", cik, payload)
  attr(payload, "cache_origin") <- "live"
  payload
}

sec_normalize_fact_df <- function(entries, instant = FALSE) {
  if (is.null(entries)) return(NULL)
  df <- as.data.frame(entries, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(NULL)

  annual_forms <- c("10-K", "10-K/A", "20-F", "20-F/A", "40-F", "40-F/A")
  if (!"form" %in% names(df)) df$form <- NA_character_
  if (!"fy" %in% names(df)) df$fy <- NA_integer_
  if (!"fp" %in% names(df)) df$fp <- NA_character_
  if (!"filed" %in% names(df)) df$filed <- NA_character_
  if (!"val" %in% names(df)) return(NULL)

  df <- df[df$form %in% annual_forms, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  df$fy <- suppressWarnings(as.integer(df$fy))
  df$val <- suppressWarnings(as.numeric(df$val))
  df$filed_date <- suppressWarnings(as.Date(df$filed))
  df <- df[!is.na(df$fy) & !is.na(df$val), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  if (!instant && all(c("start", "end") %in% names(df))) {
    start_date <- suppressWarnings(as.Date(df$start))
    end_date <- suppressWarnings(as.Date(df$end))
    duration_days <- as.numeric(end_date - start_date)
    keep <- is.na(duration_days) | (duration_days >= 300 & duration_days <= 390) | df$fp %in% c("FY", "CY")
    df <- df[keep, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
  }

  df <- df[order(df$fy, df$filed_date, na.last = TRUE, decreasing = TRUE), , drop = FALSE]
  df <- df[!duplicated(df$fy), c("fy", "val"), drop = FALSE]
  names(df) <- c("year", "value")
  df <- df[order(df$year), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df
}

sec_tag_to_df <- function(companyfacts, tags, instant = FALSE) {
  if (is.null(companyfacts) || !"facts" %in% names(companyfacts)) return(NULL)
  gaap <- companyfacts$facts[["us-gaap"]]
  if (is.null(gaap)) return(NULL)

  for (tag in tags) {
    fact <- gaap[[tag]]
    if (is.null(fact) || is.null(fact$units)) next

    units_list <- fact$units
    preferred_units <- c("USD", "pure", "shares")
    unit_names <- names(units_list)
    ordered_units <- c(intersect(preferred_units, unit_names), setdiff(unit_names, preferred_units))

    for (unit_name in ordered_units) {
      df <- sec_normalize_fact_df(units_list[[unit_name]], instant = instant)
      if (!is.null(df) && nrow(df) > 0) return(trim_history_years(df))
    }
  }

  NULL
}

fetch_sec_fundamentals <- function(ticker) {
  ticker <- toupper(trimws(ticker))
  if (!is_probably_us_ticker(ticker)) return(NULL)

  cik <- sec_lookup_cik(ticker)
  if (is.na(cik) || !nzchar(cik)) return(NULL)

  facts <- sec_companyfacts_get(cik)
  if (is.null(facts)) return(NULL)

  rev_df <- sec_tag_to_df(facts, c(
    "RevenueFromContractWithCustomerExcludingAssessedTax",
    "RevenueFromContractWithCustomerIncludingAssessedTax",
    "SalesRevenueNet"
  ))
  oi_df <- sec_tag_to_df(facts, c("OperatingIncomeLoss"))
  ni_df <- sec_tag_to_df(facts, c("NetIncomeLoss"))
  cash_df <- sec_tag_to_df(facts, c(
    "CashAndCashEquivalentsAtCarryingValue",
    "CashCashEquivalentsRestrictedCashAndRestrictedCashEquivalents"
  ), instant = TRUE)
  debt_current_df <- sec_tag_to_df(facts, c("LongTermDebtCurrent", "LongTermDebtAndCapitalLeaseObligationsCurrent"), instant = TRUE)
  debt_long_df <- sec_tag_to_df(facts, c(
    "LongTermDebtAndCapitalLeaseObligations",
    "LongTermDebtNoncurrent",
    "LongTermDebt"
  ), instant = TRUE)
  debt_df <- trim_history_years(sum_series(debt_current_df, debt_long_df))
  ocf_df <- sec_tag_to_df(facts, c(
    "NetCashProvidedByUsedInOperatingActivities",
    "NetCashProvidedByUsedInOperatingActivitiesContinuingOperations"
  ))
  capex_df <- sec_tag_to_df(facts, c("PaymentsToAcquirePropertyPlantAndEquipment"))
  fcf_df <- trim_history_years(difference_series(ocf_df, capex_df, absolute_subtrahend = TRUE))
  buyback_df <- trim_history_years(sec_tag_to_df(facts, c(
    "PaymentsForRepurchaseOfCommonStock",
    "PaymentsForRepurchaseOfEquity"
  )))
  if (!is.null(buyback_df)) buyback_df$value <- abs(buyback_df$value)

  equity_df <- sec_tag_to_df(facts, c(
    "StockholdersEquity",
    "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest"
  ), instant = TRUE)
  assets_df <- sec_tag_to_df(facts, c("Assets"), instant = TRUE)
  current_assets_df <- sec_tag_to_df(facts, c("AssetsCurrent"), instant = TRUE)
  current_liabilities_df <- sec_tag_to_df(facts, c("LiabilitiesCurrent"), instant = TRUE)

  history <- Filter(Negate(is.null), list(
    revenue = rev_df,
    operating_income = oi_df,
    net_income = ni_df,
    cash = cash_df,
    debt = debt_df,
    operating_cashflow = ocf_df,
    free_cashflow = fcf_df,
    fcf = fcf_df,
    buybacks = buyback_df,
    gross_margin = NULL,
    oper_margin = pct_series(oi_df, rev_df),
    net_margin = pct_series(ni_df, rev_df),
    roe = pct_series(ni_df, equity_df),
    roa = pct_series(ni_df, assets_df),
    debt_equity = ratio_series(debt_df, equity_df),
    current_ratio = ratio_series(current_assets_df, current_liabilities_df)
  ))

  last_val <- function(s) if (!is.null(s) && nrow(s) > 0) tail(s$value, 1) else NA_real_
  summary <- list(
    "Company Name" = facts$entityName %||% ticker,
    "Cash" = if (!is.na(last_val(history$cash))) format_large(last_val(history$cash)) else "--",
    "Debt" = if (!is.na(last_val(history$debt))) format_large(last_val(history$debt)) else "--",
    "Operating Cash Flow" = if (!is.na(last_val(history$operating_cashflow))) format_large(last_val(history$operating_cashflow)) else "--",
    "Free Cash Flow" = if (!is.na(last_val(history$free_cashflow))) format_large(last_val(history$free_cashflow)) else "--",
    "Buybacks" = if (!is.na(last_val(history$buybacks))) format_large(last_val(history$buybacks)) else "--",
    "Revenue (TTM)" = if (!is.na(last_val(history$revenue))) format_large(last_val(history$revenue)) else "--",
    "Debt/Equity" = if (!is.na(last_val(history$debt_equity))) round(last_val(history$debt_equity), 2) else "--",
    "Current Ratio" = if (!is.na(last_val(history$current_ratio))) round(last_val(history$current_ratio), 2) else "--",
    "Source" = "SEC EDGAR"
  )

  list(
    ticker = ticker,
    summary = summary,
    history = history,
    timestamp = Sys.time(),
    metadata = list(
      source_breakdown = c(
        "Statements" = paste0(
          "SEC EDGAR companyfacts (",
          format_cache_origin_label(attr(facts, "cache_origin")),
          ")"
        )
      )
    ),
    demo_mode = FALSE
  )
}

safe_av_json <- function(symbol, av_fun, api_key) {
  tryCatch({
    cached <- av_cache_get(symbol, av_fun)
    if (!is.null(cached)) return(cached)

    av_rate_limit_wait()
    resp <- httr::GET(
      "https://www.alphavantage.co/query",
      query = list(`function` = av_fun, symbol = symbol, apikey = api_key),
      httr::user_agent(YF_UA),
      httr::timeout(20)
    )
    .av_last_request_at <<- Sys.time()
    append_av_usage_log(symbol, av_fun)
    if (httr::status_code(resp) != 200) return(NULL)

    payload <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyDataFrame = TRUE
    )

    if (is.null(payload)) return(NULL)

    for (field in c("Note", "Information", "Error Message")) {
      if (field %in% names(payload)) {
        message("Alpha Vantage (", av_fun, ") message: ", payload[[field]][1])
        return(NULL)
      }
    }

    attr(payload, "cache_origin") <- "live"
    av_cache_put(symbol, av_fun, payload)
    payload
  }, error = function(e) {
    message("Alpha Vantage (", av_fun, ") error: ", e$message)
    NULL
  })
}

av_annual_reports <- function(payload) {
  if (is.null(payload) || !"annualReports" %in% names(payload)) return(NULL)
  reports <- payload$annualReports
  if (!is.data.frame(reports) || nrow(reports) == 0) return(NULL)
  reports
}

trim_history_years <- function(df, years = 10) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df[order(df$year), , drop = FALSE]
  tail(df, years)
}

sum_series <- function(...) {
  series_list <- Filter(Negate(is.null), list(...))
  if (length(series_list) == 0) return(NULL)

  renamed <- lapply(seq_along(series_list), function(idx) {
    df <- series_list[[idx]]
    names(df)[names(df) == "value"] <- paste0("value_", idx)
    df
  })

  merged <- Reduce(function(lhs, rhs) merge(lhs, rhs, by = "year", all = TRUE), renamed)
  value_cols <- grep("^value_", names(merged), value = TRUE)
  for (col in value_cols) {
    merged[[col]][is.na(merged[[col]])] <- 0
  }

  data.frame(
    year = merged$year,
    value = rowSums(merged[value_cols]),
    stringsAsFactors = FALSE
  )[order(merged$year), , drop = FALSE]
}

difference_series <- function(minuend_df, subtrahend_df, absolute_subtrahend = FALSE) {
  if (is.null(minuend_df)) return(NULL)
  if (is.null(subtrahend_df)) return(minuend_df[order(minuend_df$year), , drop = FALSE])

  merged <- merge(minuend_df, subtrahend_df, by = "year", all = FALSE,
                  suffixes = c("_m", "_s"))
  if (nrow(merged) == 0) return(minuend_df[order(minuend_df$year), , drop = FALSE])

  subtrahend_vals <- if (absolute_subtrahend) abs(merged$value_s) else merged$value_s
  data.frame(
    year = merged$year,
    value = merged$value_m - subtrahend_vals,
    stringsAsFactors = FALSE
  )[order(merged$year), , drop = FALSE]
}

format_price_value <- function(x, currency = "USD") {
  if (is.na(x)) return("--")

  prefix <- switch(toupper(currency),
    "USD" = "$",
    "EUR" = "\u20ac",
    paste0(currency, " ")
  )

  paste0(prefix, round(x, 2))
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

build_av_summary_metrics <- function(ticker, ov, history = list(), source_label = "Alpha Vantage") {
  last_val <- function(s) if (!is.null(s) && nrow(s) > 0) tail(s$value, 1) else NA_real_
  fmt_num <- function(x, d = 2) if (!is.na(x)) round(x, d) else "--"
  fmt_pct_dec <- function(x) if (!is.na(x)) paste0(round(x * 100, 2), "%") else "--"

  company_name  <- av_ov_text(ov, "Name", ticker)
  sector        <- av_ov_text(ov, "Sector", "--")
  industry      <- av_ov_text(ov, "Industry", "--")
  description   <- av_ov_text(ov, "Description", "")
  exchange      <- av_ov_text(ov, "Exchange", "--")
  asset_type    <- av_ov_text(ov, "AssetType", "--")
  currency      <- av_ov_text(ov, "Currency", "USD")
  website       <- av_ov_text(ov, "OfficialSite", "--")

  market_cap    <- av_ov_num_first(ov, c("MarketCapitalization"))
  pe_cur        <- av_ov_num_first(ov, c("PERatio", "TrailingPE"))
  forward_pe    <- av_ov_num_first(ov, c("ForwardPE"))
  peg_ratio     <- av_ov_num_first(ov, c("PEGRatio"))
  pb_cur        <- av_ov_num_first(ov, c("PriceToBookRatio"))
  ps_cur        <- av_ov_num_first(ov, c("PriceToSalesRatioTTM"))
  ev_rev_cur    <- av_ov_num_first(ov, c("EVToRevenue"))
  ev_cur        <- av_ov_num_first(ov, c("EVToEBITDA"))
  dividend_yield <- av_ov_num_first(ov, c("DividendYield"))
  dividend_per_share <- av_ov_num_first(ov, c("DividendPerShare"))
  roe_ttm       <- av_ov_num_first(ov, c("ReturnOnEquityTTM"))
  roa_ttm       <- av_ov_num_first(ov, c("ReturnOnAssetsTTM"))
  pm_ttm        <- av_ov_num_first(ov, c("ProfitMargin"))
  om_ttm        <- av_ov_num_first(ov, c("OperatingMarginTTM"))
  rev_ttm       <- av_ov_num_first(ov, c("RevenueTTM"))
  gp_ttm        <- av_ov_num_first(ov, c("GrossProfitTTM"))
  eps_now       <- av_ov_num_first(ov, c("EPS", "DilutedEPSTTM"))
  diluted_eps_ttm <- av_ov_num_first(ov, c("DilutedEPSTTM", "EPS"))
  revenue_per_share <- av_ov_num_first(ov, c("RevenuePerShareTTM"))
  analyst_target <- av_ov_num_first(ov, c("AnalystTargetPrice"))
  beta_val      <- av_ov_num_first(ov, c("Beta"))
  high_52w      <- av_ov_num_first(ov, c("52WeekHigh"))
  low_52w       <- av_ov_num_first(ov, c("52WeekLow"))
  rev_growth_yoy <- av_ov_num_first(ov, c("QuarterlyRevenueGrowthYOY"))
  eps_growth_yoy <- av_ov_num_first(ov, c("QuarterlyEarningsGrowthYOY"))
  ebitda_ttm    <- av_ov_num_first(ov, c("EBITDA"))

  gm_ttm <- if (!is.na(rev_ttm) && rev_ttm > 0 && !is.na(gp_ttm)) gp_ttm / rev_ttm else NA_real_

  cur_price <- fetch_current_price(ticker)
  if (is.na(cur_price)) {
    cur_price <- if (!is.na(diluted_eps_ttm) && !is.na(pe_cur)) {
      round(diluted_eps_ttm * pe_cur, 2)
    } else {
      analyst_target
    }
  }

  target_upside <- if (!is.na(cur_price) && !is.na(analyst_target) && cur_price != 0) {
    (analyst_target / cur_price - 1) * 100
  } else {
    NA_real_
  }

  range_52w <- if (!is.na(low_52w) && !is.na(high_52w)) {
    paste0(format_price_value(low_52w, currency), " - ", format_price_value(high_52w, currency))
  } else {
    "--"
  }

  list(
    "Company Name"     = company_name,
    "Sector"           = sector,
    "Industry"         = industry,
    "Exchange"         = exchange,
    "Asset Type"       = asset_type,
    "Reported Currency"= currency,
    "Description"      = description,
    "Official Site"    = website,
    "Market Cap"       = if (!is.na(market_cap)) format_large(market_cap) else "--",
    "Current Price"    = format_price_value(cur_price, currency),
    "Analyst Target Price" = format_price_value(analyst_target, currency),
    "Target Upside (%)"= if (!is.na(target_upside)) paste0(round(target_upside, 2), "%") else "--",
    "Trailing PE"      = fmt_num(pe_cur),
    "Forward PE"       = fmt_num(forward_pe),
    "PEG Ratio"        = fmt_num(peg_ratio),
    "Dividend Yield"   = fmt_pct_dec(dividend_yield),
    "Dividend Per Share" = format_price_value(dividend_per_share, currency),
    "PE Ratio"         = fmt_num(pe_cur),
    "PB Ratio"         = fmt_num(pb_cur),
    "PS Ratio"         = fmt_num(ps_cur),
    "Price to Book"    = fmt_num(pb_cur),
    "Price to Sales"   = fmt_num(ps_cur),
    "EV/Revenue"       = fmt_num(ev_rev_cur),
    "EV/EBITDA"        = fmt_num(ev_cur),
    "ROE (%)"          = fmt_pct_dec(roe_ttm),
    "ROA (%)"          = fmt_pct_dec(roa_ttm),
    "Profit Margin"    = fmt_pct_dec(pm_ttm),
    "Operating Margin" = fmt_pct_dec(om_ttm),
    "Net Margin (%)"   = fmt_pct_dec(pm_ttm),
    "Gross Margin (%)" = fmt_pct_dec(gm_ttm),
    "Oper Margin (%)"  = fmt_pct_dec(om_ttm),
    "Debt/Equity"      = fmt_num(last_val(history$debt_equity)),
    "Current Ratio"    = fmt_num(last_val(history$current_ratio)),
    "52W Range"        = range_52w,
    "52W Low"          = format_price_value(low_52w, currency),
    "52W High"         = format_price_value(high_52w, currency),
    "Beta"             = fmt_num(beta_val),
    "Quarterly Revenue Growth" = fmt_pct_dec(rev_growth_yoy),
    "Quarterly Earnings Growth" = fmt_pct_dec(eps_growth_yoy),
    "Revenue (TTM)"    = if (!is.na(rev_ttm)) format_large(rev_ttm) else "--",
    "Revenue per Share"= if (!is.na(revenue_per_share)) round(revenue_per_share, 2) else "--",
    "EPS"              = fmt_num(eps_now),
    "EPS (Diluted)"    = fmt_num(diluted_eps_ttm),
    "Diluted EPS TTM"  = fmt_num(diluted_eps_ttm),
    "EBITDA (TTM)"     = if (!is.na(ebitda_ttm)) format_large(ebitda_ttm) else "--",
    "Cash"             = if (!is.na(last_val(history$cash))) format_large(last_val(history$cash)) else "--",
    "Debt"             = if (!is.na(last_val(history$debt))) format_large(last_val(history$debt)) else "--",
    "Operating Cash Flow" = if (!is.na(last_val(history$operating_cashflow))) {
      format_large(last_val(history$operating_cashflow))
    } else "--",
    "Free Cash Flow"   = if (!is.na(last_val(history$free_cashflow))) {
      format_large(last_val(history$free_cashflow))
    } else "--",
    "Buybacks"         = if (!is.na(last_val(history$buybacks))) format_large(last_val(history$buybacks)) else "--",
    "FCF"              = if (!is.na(last_val(history$fcf))) format_large(last_val(history$fcf)) else "--",
    "Source"           = source_label
  )
}

fetch_av_overview_data <- function(ticker, api_key) {
  ticker <- toupper(trimws(ticker))
  if (nchar(trimws(api_key)) == 0) return(NULL)

  ov <- safe_av_json(ticker, "OVERVIEW", api_key)
  if (is.null(ov) || length(ov) == 0) return(NULL)

  list(
    ticker = ticker,
    summary = build_av_summary_metrics(ticker, ov, history = list(), source_label = "Alpha Vantage (Overview)"),
    history = list(),
    timestamp = Sys.time(),
    metadata = list(
      source_breakdown = c(
        "Overview" = paste0(
          "Alpha Vantage OVERVIEW (",
          format_cache_origin_label(attr(ov, "cache_origin")),
          ")"
        ),
        "Statements" = "Loaded on demand after opening a detail tab"
      )
    ),
    demo_mode = FALSE
  )
}

#' Fetch fundamentals via Alpha Vantage (alphavantager package).
#' Returns the same structure as fetch_yahoo_fundamentals().
fetch_av_fundamentals <- function(ticker, api_key) {
  ticker <- toupper(trimws(ticker))
  if (nchar(trimws(api_key)) == 0) return(NULL)
  message("Fetching Alpha Vantage fundamentals for: ", ticker)

  yahoo_fallback <- NULL
  sec_fallback <- NULL
  is_us_ticker <- is_probably_us_ticker(ticker)
  get_yahoo_fallback <- function() {
    if (is.null(yahoo_fallback)) {
      yahoo_fallback <<- tryCatch(fetch_yahoo_fundamentals(ticker), error = function(e) NULL)
    }
    yahoo_fallback
  }
  get_sec_fallback <- function() {
    if (is.null(sec_fallback)) {
      sec_fallback <<- tryCatch(fetch_sec_fundamentals(ticker), error = function(e) NULL)
    }
    sec_fallback
  }

  ov  <- safe_av_json(ticker, "OVERVIEW", api_key)

  if (is.null(ov) || length(ov) == 0) {
    message("Alpha Vantage OVERVIEW empty for ", ticker)
    return(NULL)
  }

  sec_data <- if (is_us_ticker) get_sec_fallback() else NULL
  use_sec_first <- !is.null(sec_data) && has_strong_statement_coverage(sec_data$history)

  is_ <- NULL
  bs_ <- NULL
  cf_ <- NULL

  if (!use_sec_first) {
    is_ <- safe_av_json(ticker, "INCOME_STATEMENT", api_key)
    bs_ <- safe_av_json(ticker, "BALANCE_SHEET", api_key)
    cf_ <- safe_av_json(ticker, "CASH_FLOW", api_key)
  }

  ar_is <- av_annual_reports(is_)
  ar_bs <- av_annual_reports(bs_)
  ar_cf <- av_annual_reports(cf_)

  rev_df   <- trim_history_years(av_col_to_df(ar_is, "totalRevenue"))
  gp_df    <- trim_history_years(av_col_to_df(ar_is, "grossProfit"))
  oi_df    <- trim_history_years(av_col_to_df(ar_is, "operatingIncome"))
  ni_df    <- trim_history_years(av_col_to_df(ar_is, "netIncome"))
  eps_df   <- trim_history_years(av_col_to_df(ar_is, "dilutedEPS"))

  ta_df    <- trim_history_years(av_col_to_df(ar_bs, "totalAssets"))
  tca_df   <- trim_history_years(av_col_to_df(ar_bs, "totalCurrentAssets"))
  tcl_df   <- trim_history_years(av_col_to_df(ar_bs, "totalCurrentLiabilities"))
  eq_df    <- trim_history_years(av_col_to_df(ar_bs, "totalShareholderEquity"))
  cash_df  <- trim_history_years(
    av_col_to_df(ar_bs, "cashAndShortTermInvestments") %||%
      av_col_to_df(ar_bs, "cashAndCashEquivalentsAtCarryingValue")
  )

  debt_current_df <- av_col_to_df(ar_bs, "shortLongTermDebtTotal")
  if (is.null(debt_current_df)) {
    debt_current_df <- sum_series(
      av_col_to_df(ar_bs, "shortTermDebt"),
      av_col_to_df(ar_bs, "currentDebt"),
      av_col_to_df(ar_bs, "currentLongTermDebt")
    )
  }
  debt_long_df <- av_col_to_df(ar_bs, "longTermDebt")
  if (is.null(debt_long_df)) {
    debt_long_df <- av_col_to_df(ar_bs, "longTermDebtNoncurrent")
  }
  debt_df <- trim_history_years(sum_series(debt_current_df, debt_long_df))

  ocf_df   <- trim_history_years(av_col_to_df(ar_cf, "operatingCashflow"))
  capex_df <- trim_history_years(av_col_to_df(ar_cf, "capitalExpenditures"))
  fcf_df   <- trim_history_years(
    difference_series(ocf_df, capex_df, absolute_subtrahend = TRUE)
  )
  buyback_df <- trim_history_years(sum_series(
    av_col_to_df(ar_cf, "paymentsForRepurchaseOfCommonStock"),
    av_col_to_df(ar_cf, "paymentsForRepurchaseOfEquity"),
    av_col_to_df(ar_cf, "paymentsForRepurchaseOfPreferredStock")
  ))
  if (!is.null(buyback_df)) {
    buyback_df$value <- abs(buyback_df$value)
  }

  history <- if (use_sec_first && !is.null(sec_data)) {
    sec_data$history
  } else {
    list(
      revenue       = rev_df,
      operating_income = oi_df,
      net_income    = ni_df,
      eps_diluted   = eps_df,
      cash          = cash_df,
      debt          = debt_df,
      operating_cashflow = ocf_df,
      free_cashflow = fcf_df,
      fcf           = fcf_df,
      buybacks      = buyback_df,
      gross_margin  = pct_series(gp_df, rev_df),
      oper_margin   = pct_series(oi_df, rev_df),
      net_margin    = pct_series(ni_df, rev_df),
      roe           = pct_series(ni_df, eq_df),
      roa           = pct_series(ni_df, ta_df),
      debt_equity   = ratio_series(debt_df, eq_df),
      current_ratio = ratio_series(tca_df, tcl_df)
    )
  }
  history <- Filter(Negate(is.null), history)

  if (use_sec_first) {
    yahoo_data <- get_yahoo_fallback()
    if (!is.null(yahoo_data)) {
      history <- merge_history_lists(history, yahoo_data$history)
    }
  } else if (is.null(is_) || is.null(bs_) || is.null(cf_)) {
    sec_data <- if (is_us_ticker) get_sec_fallback() else NULL
    if (!is.null(sec_data)) {
      history <- merge_history_lists(history, sec_data$history)
    }
    yahoo_data <- get_yahoo_fallback()
    if (!is.null(yahoo_data)) {
      history <- merge_history_lists(history, yahoo_data$history)
    }
  }

  has_missing_statements <- use_sec_first || is.null(is_) || is.null(bs_) || is.null(cf_)
  sec_data <- if (use_sec_first || (has_missing_statements && is_us_ticker)) get_sec_fallback() else NULL
  yahoo_data <- if (use_sec_first || has_missing_statements) get_yahoo_fallback() else NULL

  source_label <- if (use_sec_first && !is.null(yahoo_data)) {
    "Alpha Vantage Overview + SEC EDGAR + Yahoo Finance"
  } else if (use_sec_first) {
    "Alpha Vantage Overview + SEC EDGAR"
  } else if (!is.null(sec_data) && !is.null(yahoo_data)) {
    "Alpha Vantage + SEC EDGAR + Yahoo Finance"
  } else if (!is.null(sec_data)) {
    "Alpha Vantage + SEC EDGAR"
  } else if (!is.null(yahoo_data)) {
    "Alpha Vantage + Yahoo Finance"
  } else {
    "Alpha Vantage"
  }

  summary_metrics <- build_av_summary_metrics(ticker, ov, history, source_label = source_label)

  if (!is.null(sec_data)) {
    summary_metrics <- merge_summary_lists(summary_metrics, sec_data$summary)
  }
  if (!is.null(yahoo_data)) {
    summary_metrics <- merge_summary_lists(summary_metrics, yahoo_data$summary)
  }

  source_breakdown <- c(
    "Overview" = paste0(
      "Alpha Vantage OVERVIEW (",
      format_cache_origin_label(attr(ov, "cache_origin")),
      ")"
    )
  )

  if (use_sec_first && !is.null(sec_data)) {
    source_breakdown <- c(
      source_breakdown,
      "Statements" = sec_data$metadata$source_breakdown[[1]] %||% "SEC EDGAR companyfacts"
    )
  } else {
    av_statement_bits <- c()

    for (endpoint_name in c("INCOME_STATEMENT", "BALANCE_SHEET", "CASH_FLOW")) {
      payload <- switch(endpoint_name,
        INCOME_STATEMENT = is_,
        BALANCE_SHEET = bs_,
        CASH_FLOW = cf_
      )

      if (!is.null(payload)) {
        av_statement_bits <- c(
          av_statement_bits,
          paste0(endpoint_name, " (", format_cache_origin_label(attr(payload, "cache_origin")), ")")
        )
      }
    }

    if (length(av_statement_bits) > 0) {
      source_breakdown <- c(
        source_breakdown,
        "Statements" = paste(av_statement_bits, collapse = " · ")
      )
    }

    if (!is.null(sec_data)) {
      source_breakdown <- c(
        source_breakdown,
        "SEC Backfill" = sec_data$metadata$source_breakdown[[1]] %||% "SEC EDGAR companyfacts"
      )
    }
  }

  if (!is.null(yahoo_data)) {
    yahoo_line <- yahoo_data$metadata$source_breakdown[[1]] %||% "Yahoo Finance JSON fallback"
    source_breakdown <- c(
      source_breakdown,
      if (use_sec_first) c("Gap Fill" = yahoo_line) else c("Fallback" = yahoo_line)
    )
  }

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time(),
    metadata  = list(
      source_breakdown = source_breakdown
    ),
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
