# ──────────────────────────────────────────────────────────────
#  market_data.R – Fetch live market data via quantmod / Yahoo
# ──────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(jsonlite)
})

# ── Per-ticker quote cache (shared across all Shiny modules) ──
# Prevents dashboard + portfolio + watchlist from making the same
# HTTP requests independently within the same session.
.quote_cache     <- new.env(parent = emptyenv())
.quote_cache_ttl <- 30L  # seconds before a cached quote is considered stale
.history_cache     <- new.env(parent = emptyenv())
.history_cache_ttl <- 300L  # seconds before a cached history series is considered stale

.cache_get <- function(ticker) {
  key <- make.names(ticker)
  entry <- .quote_cache[[key]]
  if (is.null(entry)) return(NULL)
  if (as.numeric(difftime(Sys.time(), entry$time, units = "secs")) >= .quote_cache_ttl)
    return(NULL)
  entry
}

.cache_put <- function(q) {
  if (!is.null(q)) .quote_cache[[make.names(q$ticker)]] <- q
}

.history_cache_key <- function(ticker, period, field = "close") {
  make.names(paste(ticker, period, field, sep = "__"))
}

.history_cache_get <- function(ticker, period, field = "close") {
  key <- .history_cache_key(ticker, period, field)
  entry <- .history_cache[[key]]
  if (is.null(entry)) return(NULL)
  if (as.numeric(difftime(Sys.time(), entry$time, units = "secs")) >= .history_cache_ttl)
    return(NULL)
  if (is.null(entry$data)) return(NULL)
  if (is.data.frame(entry$data)) {
    if (nrow(entry$data) == 0) return(NULL)
    if (!"close" %in% names(entry$data)) return(NULL)
    if (!any(is.finite(entry$data$close), na.rm = TRUE)) return(NULL)
  }
  entry$data
}

.history_cache_put <- function(ticker, period, data, field = "close") {
  key <- .history_cache_key(ticker, period, field)
  .history_cache[[key]] <- list(time = Sys.time(), data = data)
}

json_numeric_series <- function(x) {
  if (is.null(x)) return(numeric())

  if (!is.list(x)) {
    return(as.numeric(x))
  }

  vapply(x, function(item) {
    if (is.null(item) || length(item) == 0) return(NA_real_)
    suppressWarnings(as.numeric(item[[1]]))
  }, numeric(1), USE.NAMES = FALSE)
}


# ── Single quote ─────────────────────────────────────────────
# Uses the Yahoo Finance v8 chart endpoint (readLines, no auth needed).
# quantmod::getQuote is skipped — it requires a GDPR crumb that cannot
# be obtained non-interactively in Germany.
fetch_quote <- function(ticker) {
  tryCatch({
    url <- paste0(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      utils::URLencode(ticker),
      "?range=5d&interval=1d"
    )
    resp <- readLines(url, warn = FALSE)
    js   <- jsonlite::fromJSON(paste(resp, collapse = ""),
                               simplifyVector = FALSE)
    meta  <- js$chart$result[[1]]$meta

    price <- as.numeric(meta$regularMarketPrice)
    prev  <- as.numeric(meta$chartPreviousClose)

    if (is.null(price) || length(price) == 0 || is.na(price)) return(NULL)

    list(
      ticker = ticker,
      price  = round(price, 2),
      change = round(price - prev, 2),
      pct    = round((price - prev) / prev * 100, 2),
      low_52w = as.numeric(meta$fiftyTwoWeekLow),
      high_52w = as.numeric(meta$fiftyTwoWeekHigh),
      currency = as.character(meta$currency),
      volume = as.numeric(meta$regularMarketVolume),
      time   = Sys.time()
    )
  }, error = function(e) NULL)
}


# ── Batch quotes ─────────────────────────────────────────────
# Fetches uncached tickers in parallel (PSOCK cluster, Windows-safe).
# All results are stored in the per-ticker cache so subsequent calls
# from other modules (dashboard/portfolio/watchlist) return instantly.
#
# workers: max parallel R processes. Keep ≤ 6 to avoid overwhelming
#          Yahoo's rate limits while still being fast.
fetch_quotes <- function(tickers, workers = 5L) {
  tickers <- unique(tickers)

  # Split into cached / uncached
  uncached <- Filter(function(tkr) is.null(.cache_get(tkr)), tickers)

  if (length(uncached) > 0) {
    n_workers <- min(workers, length(uncached))

    if (n_workers > 1L) {
      # ── Parallel fetch ────────────────────────────────────
      cl <- parallel::makeCluster(n_workers, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterExport(
        cl,
        varlist = c("fetch_quote"),
        envir   = .GlobalEnv
      )
      parallel::clusterEvalQ(cl, {
        suppressPackageStartupMessages(library(jsonlite))
      })

      new_results <- parallel::parLapply(cl, uncached, function(tkr) {
        tryCatch(fetch_quote(tkr), error = function(e) NULL)
      })
    } else {
      # Single ticker — no cluster overhead
      new_results <- lapply(uncached, function(tkr) {
        tryCatch(fetch_quote(tkr), error = function(e) NULL)
      })
    }

    # Store results in cache (NULL = failed fetch → also cache as NA row)
    for (i in seq_along(uncached)) {
      tkr <- uncached[[i]]
      q   <- new_results[[i]]
      if (is.null(q)) {
        .cache_put(list(ticker = tkr, price = NA_real_, change = NA_real_,
                        pct = NA_real_, low_52w = NA_real_,
                        high_52w = NA_real_, currency = NA_character_,
                        volume = NA_real_, time = Sys.time()))
      } else {
        .cache_put(q)
      }
    }
  }

  # Assemble return data.frame from cache
  results <- lapply(tickers, function(tkr) {
    q <- .cache_get(tkr)
    if (is.null(q))
      list(ticker = tkr, price = NA_real_, change = NA_real_,
           pct = NA_real_, low_52w = NA_real_, high_52w = NA_real_,
           currency = NA_character_,
           volume = NA_real_, time = Sys.time())
    else
      q
  })
  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}

# ── Historical price series (for sparklines / analytics) ─────

download_history_series <- function(ticker, period = "6mo", field = c("close", "adjusted")) {
  field <- match.arg(field)

  tryCatch({
    url <- paste0(
      "https://query1.finance.yahoo.com/v8/finance/chart/",
      utils::URLencode(ticker),
      "?range=", period, "&interval=1d&includeAdjustedClose=true"
    )
    resp <- readLines(url, warn = FALSE)
    js <- jsonlite::fromJSON(
      paste(resp, collapse = ""),
      simplifyVector = FALSE
    )

    result <- js$chart$result[[1]]
    if (is.null(result)) {
      return(data.frame(date = as.Date(character()), close = numeric()))
    }

    timestamps <- json_numeric_series(result$timestamp)
    close_vals <- json_numeric_series(result$indicators$quote[[1]]$close)
    adj_vals <- NULL

    if (!is.null(result$indicators$adjclose) && length(result$indicators$adjclose) > 0) {
      adj_vals <- json_numeric_series(result$indicators$adjclose[[1]]$adjclose)
    }

    series_vals <- if (identical(field, "adjusted") && !is.null(adj_vals)) {
      adj_vals
    } else {
      close_vals
    }

    df <- data.frame(
      date = as.Date(as.POSIXct(timestamps, origin = "1970-01-01", tz = "UTC")),
      close = as.numeric(series_vals),
      stringsAsFactors = FALSE
    )

    df <- df[!is.na(df$date) & is.finite(df$close) & df$close > 0, , drop = FALSE]
    df <- df[!duplicated(df$date), , drop = FALSE]
    df[order(df$date), , drop = FALSE]
  }, error = function(e) {
    data.frame(date = as.Date(character()), close = numeric())
  })
}

fetch_history <- function(ticker, period = "6mo", field = c("close", "adjusted")) {
  field <- match.arg(field)
  cached <- .history_cache_get(ticker, period, field)
  if (!is.null(cached)) return(cached)

  df <- download_history_series(ticker, period = period, field = field)
  .history_cache_put(ticker, period, df, field = field)
  df
}

fetch_history_batch <- function(tickers, period = "6mo", field = c("close", "adjusted"), workers = 4L) {
  field <- match.arg(field)
  tickers <- unique(as.character(tickers))
  tickers <- tickers[!is.na(tickers) & nzchar(tickers)]
  if (length(tickers) == 0) return(setNames(list(), character()))

  uncached <- Filter(function(tkr) is.null(.history_cache_get(tkr, period, field)), tickers)

  if (length(uncached) > 0) {
    n_workers <- min(as.integer(workers), length(uncached))
    if (is.na(n_workers) || n_workers < 1L) n_workers <- 1L

    if (n_workers > 1L) {
      cl <- parallel::makeCluster(n_workers, type = "PSOCK")
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterExport(
        cl,
        varlist = c("download_history_series", "json_numeric_series"),
        envir = .GlobalEnv
      )
      parallel::clusterEvalQ(cl, {
        suppressPackageStartupMessages(library(jsonlite))
        NULL
      })

      new_results <- parallel::parLapply(cl, uncached, function(tkr) {
        download_history_series(tkr, period = period, field = field)
      })
    } else {
      new_results <- lapply(uncached, function(tkr) {
        download_history_series(tkr, period = period, field = field)
      })
    }

    for (i in seq_along(uncached)) {
      .history_cache_put(uncached[[i]], period, new_results[[i]], field = field)
    }
  }

  out <- lapply(tickers, function(tkr) {
    cached <- .history_cache_get(tkr, period, field)
    if (is.null(cached)) {
      data.frame(date = as.Date(character()), close = numeric())
    } else {
      cached
    }
  })

  names(out) <- tickers
  out
}

fetch_history_fallback <- function(ticker, period = "6mo", field = c("close", "adjusted")) {
  fetch_history(ticker, period = period, field = field)
}
