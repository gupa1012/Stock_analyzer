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
                        pct = NA_real_, currency = NA_character_,
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
           pct = NA_real_, currency = NA_character_,
           volume = NA_real_, time = Sys.time())
    else
      q
  })
  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
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
