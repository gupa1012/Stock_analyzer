# ──────────────────────────────────────────────────────────────
#  market_data.R – Fetch live market data via quantmod / Yahoo
# ──────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(jsonlite)
})

# ── Single quote ─────────────────────────────────────────────

fetch_quote <- function(ticker) {
  tryCatch({
    if (requireNamespace("quantmod", quietly = TRUE)) {
      q <- quantmod::getQuote(ticker, src = "yahoo")
      if (nrow(q) == 0) return(NULL)
      list(
        ticker  = ticker,
        price   = as.numeric(q[["Last"]]),
        change  = as.numeric(q[["Change"]]),
        pct     = as.numeric(q[["% Change"]]),
        volume  = as.numeric(q[["Volume"]]),
        time    = Sys.time()
      )
    } else {
      fetch_quote_fallback(ticker)
    }
  }, error = function(e) {
    fetch_quote_fallback(ticker)
  })
}

fetch_quote_fallback <- function(ticker) {
  tryCatch({
    url <- paste0(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      utils::URLencode(ticker),
      "?range=2d&interval=1d"
    )
    resp <- readLines(url, warn = FALSE)
    js   <- jsonlite::fromJSON(paste(resp, collapse = ""))
    meta <- js$chart$result[[1]]$meta
    close_prices <- js$chart$result[[1]]$indicators$quote[[1]]$close[[1]]

    price <- meta$regularMarketPrice
    prev  <- meta$chartPreviousClose

    list(
      ticker = ticker,
      price  = round(price, 2),
      change = round(price - prev, 2),
      pct    = round((price - prev) / prev * 100, 2),
      volume = meta$regularMarketVolume,
      time   = Sys.time()
    )
  }, error = function(e) NULL)
}

# ── Batch quotes ─────────────────────────────────────────────

fetch_quotes <- function(tickers) {
  results <- lapply(tickers, function(tkr) {
    Sys.sleep(0.3)
    q <- fetch_quote(tkr)
    if (is.null(q)) {
      list(ticker = tkr, price = NA, change = NA,
           pct = NA, volume = NA, time = Sys.time())
    } else {
      q
    }
  })
  do.call(rbind, lapply(results, as.data.frame,
                         stringsAsFactors = FALSE))
}

# ── Historical price series (for sparklines / mini charts) ───

fetch_history <- function(ticker, period = "6mo") {
  tryCatch({
    if (requireNamespace("quantmod", quietly = TRUE)) {
      env <- new.env()
      quantmod::getSymbols(ticker, src = "yahoo", from = Sys.Date() - 180,
                           env = env, auto.assign = TRUE)
      xts_obj <- env[[ticker]]
      df <- data.frame(
        date  = zoo::index(xts_obj),
        close = as.numeric(quantmod::Cl(xts_obj))
      )
      return(df)
    }
    fetch_history_fallback(ticker, period)
  }, error = function(e) {
    fetch_history_fallback(ticker, period)
  })
}

fetch_history_fallback <- function(ticker, period = "6mo") {
  tryCatch({
    url <- paste0(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      utils::URLencode(ticker),
      "?range=", period, "&interval=1d"
    )
    resp <- readLines(url, warn = FALSE)
    js   <- jsonlite::fromJSON(paste(resp, collapse = ""))
    ts   <- js$chart$result[[1]]$timestamp
    cl   <- js$chart$result[[1]]$indicators$quote[[1]]$close[[1]]
    data.frame(
      date  = as.Date(as.POSIXct(ts, origin = "1970-01-01")),
      close = round(as.numeric(cl), 2)
    )
  }, error = function(e) {
    data.frame(date = as.Date(character()), close = numeric())
  })
}
