# ──────────────────────────────────────────────────────────────
#  portfolio_analytics.R – Correlation / factor / diversification
# ──────────────────────────────────────────────────────────────

PORTFOLIO_ANALYTICS_BENCHMARKS <- data.frame(
  ticker = c("GLD", "SPY", "QQQ", "IWM", "EFA", "EEM", "TLT", "VNQ", "XLU", "XLP", "XLV", "USMV"),
  label = c("Gold", "S&P 500", "Nasdaq 100", "Russell 2000", "Developed ex US", "Emerging Markets",
            "Long Treasuries", "US REITs", "Utilities", "Consumer Staples", "Health Care", "Low Volatility"),
  category = c("Commodity", "Equity", "Equity", "Equity", "Equity", "Equity",
               "Rates", "Real Estate", "Defensive Equity", "Defensive Equity", "Defensive Equity", "Factor ETF"),
  currency = rep("USD", 12),
  stringsAsFactors = FALSE
)

PORTFOLIO_ANALYTICS_FACTOR_PROXIES <- data.frame(
  factor = c("CAPM Market", "Gold Hedge", "Size Proxy", "Value Proxy", "Momentum Proxy", "Quality Proxy", "Low-Vol Proxy"),
  proxy = c("SPY", "GLD", "IWM - SPY", "VTV - VUG", "MTUM - SPY", "QUAL - SPY", "USMV - SPY"),
  long_ticker = c("SPY", "GLD", "IWM", "VTV", "MTUM", "QUAL", "USMV"),
  short_ticker = c(NA_character_, NA_character_, "SPY", "VUG", "SPY", "SPY", "SPY"),
  currency = rep("USD", 7),
  stringsAsFactors = FALSE
)

PORTFOLIO_PERFORMANCE_BENCHMARKS <- data.frame(
  key = c("none", "msci_world", "sp500"),
  ticker = c(NA_character_, "URTH", "SPY"),
  label = c("No Benchmark", "MSCI World", "S&P 500"),
  currency = c(NA_character_, "USD", "USD"),
  stringsAsFactors = FALSE
)

empty_analytics_table <- function(columns) {
  out <- as.data.frame(setNames(vector("list", length(columns)), columns), stringsAsFactors = FALSE)
  out[0, , drop = FALSE]
}

format_pct_signed <- function(x, digits = 2, na_value = "--") {
  ifelse(
    is.na(x),
    na_value,
    sprintf(paste0("%+.", digits, "f%%"), x)
  )
}

format_pct_plain <- function(x, digits = 2, na_value = "--") {
  ifelse(
    is.na(x),
    na_value,
    paste0(formatC(x, digits = digits, format = "f"), "%")
  )
}

format_num <- function(x, digits = 2, na_value = "--") {
  ifelse(
    is.na(x),
    na_value,
    formatC(x, digits = digits, format = "f")
  )
}

safe_date_merge <- function(left, right, by = "date", all = FALSE) {
  if (is.null(left) || nrow(left) == 0) return(right)
  if (is.null(right) || nrow(right) == 0) return(left)
  merge(left, right, by = by, all = all)
}

annualize_volatility <- function(returns) {
  valid <- returns[is.finite(returns)]
  if (length(valid) < 20) return(NA_real_)
  stats::sd(valid) * sqrt(252) * 100
}

annualize_compound_return <- function(returns) {
  valid <- returns[is.finite(returns)]
  if (length(valid) < 20) return(NA_real_)
  (prod(1 + valid)^(252 / length(valid)) - 1) * 100
}

safe_correlation <- function(x, y, min_obs = 40L) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < min_obs) return(NA_real_)
  stats::cor(x[ok], y[ok])
}

safe_regression_metrics <- function(asset_returns, factor_returns, min_obs = 40L) {
  ok <- is.finite(asset_returns) & is.finite(factor_returns)
  obs <- sum(ok)
  if (obs < min_obs || stats::var(factor_returns[ok]) <= 0) {
    return(list(beta = NA_real_, alpha_daily = NA_real_, r2 = NA_real_, corr = NA_real_, obs = obs))
  }

  fit <- stats::lm(asset_returns[ok] ~ factor_returns[ok])
  fit_sum <- summary(fit)

  list(
    beta = unname(stats::coef(fit)[2]),
    alpha_daily = unname(stats::coef(fit)[1]),
    r2 = unname(fit_sum$adj.r.squared),
    corr = safe_correlation(asset_returns, factor_returns, min_obs = min_obs),
    obs = obs
  )
}

normalize_history_frame <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(date = as.Date(character()), close = numeric(), stringsAsFactors = FALSE))
  }

  df <- df[, c("date", "close"), drop = FALSE]
  df$date <- as.Date(df$date)
  df$close <- as.numeric(df$close)
  df <- df[!is.na(df$date) & is.finite(df$close) & df$close > 0, , drop = FALSE]
  df <- df[!duplicated(df$date), , drop = FALSE]
  df[order(df$date), , drop = FALSE]
}

build_return_series_from_histories <- function(price_history, fx_history = NULL) {
  price_history <- normalize_history_frame(price_history)
  if (nrow(price_history) < 3) {
    return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
  }

  if (!is.null(fx_history)) {
    fx_history <- normalize_history_frame(fx_history)
    if (nrow(fx_history) < 3) {
      return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
    }
    merged <- merge(price_history, fx_history, by = "date", suffixes = c("_asset", "_fx"))
    merged$price_base <- merged$close_asset * merged$close_fx
  } else {
    merged <- price_history
    merged$price_base <- merged$close
  }

  merged <- merged[is.finite(merged$price_base) & merged$price_base > 0, c("date", "price_base"), drop = FALSE]
  if (nrow(merged) < 3) {
    return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
  }

  merged$ret <- c(NA_real_, diff(merged$price_base) / head(merged$price_base, -1))
  merged <- merged[is.finite(merged$ret), c("date", "ret"), drop = FALSE]
  rownames(merged) <- NULL
  merged
}

merge_return_map <- function(series_map) {
  valid_names <- names(series_map)[vapply(series_map, function(df) is.data.frame(df) && nrow(df) > 0, logical(1))]
  if (length(valid_names) == 0) {
    return(data.frame(date = as.Date(character()), stringsAsFactors = FALSE))
  }

  wide <- NULL
  for (nm in valid_names) {
    df <- series_map[[nm]][, c("date", "ret"), drop = FALSE]
    names(df)[2] <- nm
    wide <- safe_date_merge(wide, df, by = "date", all = TRUE)
  }

  wide <- wide[order(wide$date), , drop = FALSE]
  rownames(wide) <- NULL
  wide
}

compute_weighted_portfolio_returns <- function(return_wide, weights) {
  cols <- intersect(names(weights), setdiff(names(return_wide), "date"))
  if (length(cols) == 0 || nrow(return_wide) == 0) {
    return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
  }

  weight_vec <- as.numeric(weights[cols])
  mat <- as.matrix(return_wide[, cols, drop = FALSE])

  port_ret <- apply(mat, 1, function(row_vals) {
    ok <- is.finite(row_vals) & is.finite(weight_vec)
    if (!any(ok)) return(NA_real_)
    sum(row_vals[ok] * weight_vec[ok]) / sum(weight_vec[ok])
  })

  data.frame(date = return_wide$date, ret = as.numeric(port_ret), stringsAsFactors = FALSE)
}

returns_to_index_series <- function(return_series, base_value = 100) {
  if (is.null(return_series) || nrow(return_series) == 0) {
    return(data.frame(date = as.Date(character()), indexed = numeric(), stringsAsFactors = FALSE))
  }

  valid <- return_series[is.finite(return_series$ret), c("date", "ret"), drop = FALSE]
  if (nrow(valid) == 0) {
    return(data.frame(date = as.Date(character()), indexed = numeric(), stringsAsFactors = FALSE))
  }

  valid$indexed <- base_value * cumprod(1 + valid$ret)
  valid[, c("date", "indexed"), drop = FALSE]
}

build_position_snapshot <- function(portfolio_df, quotes_df) {
  if (is.null(portfolio_df) || nrow(portfolio_df) == 0) {
    return(data.frame(
      ticker = character(), label = character(), currency = character(),
      value_eur = numeric(), stringsAsFactors = FALSE
    ))
  }

  tickers <- unique(portfolio_df$ticker)
  rows <- lapply(tickers, function(tkr) {
    part <- portfolio_df[portfolio_df$ticker == tkr, , drop = FALSE]
    quote_row <- if (!is.null(quotes_df) && nrow(quotes_df) > 0) {
      quotes_df[quotes_df$ticker == tkr, , drop = FALSE]
    } else {
      NULL
    }

    market_values <- mapply(
      function(shares, avg_cost, currency) {
        if (!is.null(quote_row) && nrow(quote_row) > 0 && !is.na(quote_row$price[1])) {
          return(convert_amount_to_base(quote_row$price[1] * shares, currency, quotes_df))
        }
        convert_amount_to_base(avg_cost * shares, currency, quotes_df)
      },
      shares = part$shares,
      avg_cost = part$avg_cost,
      currency = part$currency,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )

    note_labels <- extract_company_name_from_notes(part$notes)
    note_labels <- note_labels[!is.na(note_labels) & nzchar(note_labels)]
    label <- if (length(note_labels) > 0) note_labels[1] else tkr

    data.frame(
      ticker = tkr,
      label = label,
      currency = toupper(trimws(as.character(part$currency[1]))),
      value_eur = sum(market_values, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  if (sum(out$value_eur, na.rm = TRUE) <= 0) {
    out$value_eur <- rep(1 / max(nrow(out), 1), nrow(out))
  }
  out[order(-out$value_eur, out$ticker), , drop = FALSE]
}

read_similarity_universe <- function() {
  path <- file.path("data", "sp500_tickers.csv")
  if (!file.exists(path)) {
    return(data.frame(ticker = character(), company = character(), sector = character(), currency = character(), stringsAsFactors = FALSE))
  }

  df <- read.csv(path, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  required <- c("ticker", "company", "sector")
  if (!all(required %in% names(df))) {
    return(data.frame(ticker = character(), company = character(), sector = character(), currency = character(), stringsAsFactors = FALSE))
  }

  df <- df[, required, drop = FALSE]
  df$ticker <- toupper(trimws(as.character(df$ticker)))
  df$company <- as.character(df$company)
  df$sector <- as.character(df$sector)
  df$currency <- "USD"
  df <- df[!is.na(df$ticker) & nzchar(df$ticker), , drop = FALSE]
  df[!duplicated(df$ticker), , drop = FALSE]
}

build_return_map <- function(meta_df, price_map, fx_map) {
  if (is.null(meta_df) || nrow(meta_df) == 0) return(setNames(list(), character()))

  rows <- lapply(seq_len(nrow(meta_df)), function(i) {
    currency <- toupper(trimws(as.character(meta_df$currency[i])))
    fx_hist <- NULL
    if (!is.na(currency) && nzchar(currency) && currency != BASE_CURRENCY) {
      fx_ticker <- fx_pair_ticker(currency)
      fx_hist <- fx_map[[fx_ticker]]
    }
    build_return_series_from_histories(price_map[[meta_df$ticker[i]]], fx_hist)
  })

  names(rows) <- meta_df$ticker
  rows
}

build_factor_proxy_series <- function(factor_def, core_returns_map) {
  long_df <- core_returns_map[[factor_def$long_ticker]]
  if (is.null(long_df) || nrow(long_df) == 0) {
    return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
  }

  if (is.na(factor_def$short_ticker) || !nzchar(factor_def$short_ticker)) {
    return(long_df)
  }

  short_df <- core_returns_map[[factor_def$short_ticker]]
  if (is.null(short_df) || nrow(short_df) == 0) {
    return(data.frame(date = as.Date(character()), ret = numeric(), stringsAsFactors = FALSE))
  }

  merged <- merge(long_df, short_df, by = "date", suffixes = c("_long", "_short"))
  merged$ret <- merged$ret_long - merged$ret_short
  merged[, c("date", "ret"), drop = FALSE]
}

simulate_portfolio_volatility <- function(portfolio_series, candidate_series, add_weight = 0.10, min_obs = 40L) {
  merged <- merge(portfolio_series, candidate_series, by = "date", suffixes = c("_portfolio", "_candidate"))
  merged <- merged[is.finite(merged$ret_portfolio) & is.finite(merged$ret_candidate), , drop = FALSE]
  if (nrow(merged) < min_obs) {
    return(list(current_vol = NA_real_, new_vol = NA_real_, delta_vol = NA_real_, obs = nrow(merged)))
  }

  current_vol <- annualize_volatility(merged$ret_portfolio)
  new_returns <- (1 - add_weight) * merged$ret_portfolio + add_weight * merged$ret_candidate
  new_vol <- annualize_volatility(new_returns)

  list(
    current_vol = current_vol,
    new_vol = new_vol,
    delta_vol = new_vol - current_vol,
    obs = nrow(merged)
  )
}

build_benchmark_comparison_table <- function(portfolio_series, benchmark_meta, benchmark_returns_map) {
  rows <- lapply(seq_len(nrow(benchmark_meta)), function(i) {
    ticker <- benchmark_meta$ticker[i]
    bench_df <- benchmark_returns_map[[ticker]]
    merged <- merge(portfolio_series, bench_df, by = "date", suffixes = c("_portfolio", "_benchmark"))
    merged <- merged[is.finite(merged$ret_portfolio) & is.finite(merged$ret_benchmark), , drop = FALSE]
    metrics <- safe_regression_metrics(merged$ret_portfolio, merged$ret_benchmark)

    data.frame(
      Instrument = benchmark_meta$label[i],
      Ticker = ticker,
      Category = benchmark_meta$category[i],
      Corr = format_pct_signed(metrics$corr * 100, digits = 1),
      Beta = format_num(metrics$beta, digits = 2),
      `Ann.Return` = format_pct_signed(annualize_compound_return(merged$ret_benchmark), digits = 1),
      `Ann.Vol` = format_pct_plain(annualize_volatility(merged$ret_benchmark), digits = 1),
      Obs = metrics$obs,
      corr_num = metrics$corr,
      beta_num = metrics$beta,
      ann_return_num = annualize_compound_return(merged$ret_benchmark),
      ann_vol_num = annualize_volatility(merged$ret_benchmark),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[order(match(out$Ticker, benchmark_meta$ticker)), , drop = FALSE]
}

build_diversifier_table <- function(portfolio_series, candidate_meta, candidate_returns_map,
                                    held_tickers, add_weight = 0.10) {
  port_vol_col <- paste0("Port.Vol @ +", round(add_weight * 100), "%")
  keep <- !candidate_meta$ticker %in% held_tickers
  candidate_meta <- candidate_meta[keep, , drop = FALSE]
  if (nrow(candidate_meta) == 0) {
    return(empty_analytics_table(c(
      "Candidate", "Ticker", "Bucket", "Corr", "Ann.Vol",
      port_vol_col, "Delta Vol", "Obs",
      "projected_vol_num", "delta_vol_num", "corr_num"
    )))
  }

  rows <- lapply(seq_len(nrow(candidate_meta)), function(i) {
    ticker <- candidate_meta$ticker[i]
    cand_df <- candidate_returns_map[[ticker]]
    if (is.null(cand_df) || nrow(cand_df) == 0) return(NULL)

    corr_merged <- merge(portfolio_series, cand_df, by = "date", suffixes = c("_portfolio", "_candidate"))
    corr_val <- safe_correlation(corr_merged$ret_portfolio, corr_merged$ret_candidate, min_obs = 60L)
    sim <- simulate_portfolio_volatility(portfolio_series, cand_df, add_weight = add_weight, min_obs = 60L)
    cand_vol <- annualize_volatility(corr_merged$ret_candidate)

    row <- data.frame(
      Candidate = candidate_meta$label[i],
      Ticker = ticker,
      Bucket = candidate_meta$bucket[i],
      Corr = format_pct_signed(corr_val * 100, digits = 1),
      `Ann.Vol` = format_pct_plain(cand_vol, digits = 1),
      `Delta Vol` = format_pct_signed(sim$delta_vol, digits = 1),
      Obs = sim$obs,
      projected_vol_num = sim$new_vol,
      delta_vol_num = sim$delta_vol,
      corr_num = corr_val,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    row[[port_vol_col]] <- format_pct_plain(sim$new_vol, digits = 1)
    row[, c("Candidate", "Ticker", "Bucket", "Corr", "Ann.Vol", port_vol_col,
            "Delta Vol", "Obs", "projected_vol_num", "delta_vol_num", "corr_num"), drop = FALSE]
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(empty_analytics_table(c(
      "Candidate", "Ticker", "Bucket", "Corr", "Ann.Vol",
      port_vol_col, "Delta Vol", "Obs",
      "projected_vol_num", "delta_vol_num", "corr_num"
    )))
  }

  out <- do.call(rbind, rows)
  improving <- out[is.finite(out$delta_vol_num) & out$delta_vol_num < 0, , drop = FALSE]
  if (nrow(improving) == 0) improving <- out

  improving <- improving[order(improving$projected_vol_num, improving$corr_num, improving$Obs), , drop = FALSE]
  head(improving, 12)
}

build_similar_names_table <- function(position_snapshot, held_returns_map, stock_universe,
                                      stock_returns_map, top_n = 2L) {
  direct_positions <- position_snapshot[!position_snapshot$ticker %in% names(ETF_ISINS), , drop = FALSE]
  if (nrow(direct_positions) == 0 || nrow(stock_universe) == 0) {
    return(empty_analytics_table(c(
      "Held", "Candidate", "Ticker", "Sector", "Corr", "Obs", "corr_num"
    )))
  }

  held_tickers <- direct_positions$ticker
  candidate_meta <- stock_universe[!stock_universe$ticker %in% held_tickers, , drop = FALSE]

  rows <- list()
  for (i in seq_len(nrow(direct_positions))) {
    held_ticker <- direct_positions$ticker[i]
    held_label <- direct_positions$label[i]
    held_df <- held_returns_map[[held_ticker]]
    if (is.null(held_df) || nrow(held_df) == 0) next

    corrs <- lapply(seq_len(nrow(candidate_meta)), function(j) {
      cand_df <- stock_returns_map[[candidate_meta$ticker[j]]]
      if (is.null(cand_df) || nrow(cand_df) == 0) return(NULL)

      merged <- merge(held_df, cand_df, by = "date", suffixes = c("_held", "_candidate"))
      corr_val <- safe_correlation(merged$ret_held, merged$ret_candidate, min_obs = 60L)
      if (is.na(corr_val)) return(NULL)

      data.frame(
        Held = held_label,
        Candidate = candidate_meta$company[j],
        Ticker = candidate_meta$ticker[j],
        Sector = candidate_meta$sector[j],
        Corr = format_pct_signed(corr_val * 100, digits = 1),
        Obs = nrow(merged),
        corr_num = corr_val,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })

    corrs <- Filter(Negate(is.null), corrs)
    if (length(corrs) == 0) next

    corrs_df <- do.call(rbind, corrs)
    corrs_df <- corrs_df[order(-corrs_df$corr_num, -corrs_df$Obs), , drop = FALSE]
    rows[[length(rows) + 1]] <- head(corrs_df, top_n)
  }

  if (length(rows) == 0) {
    return(empty_analytics_table(c(
      "Held", "Candidate", "Ticker", "Sector", "Corr", "Obs", "corr_num"
    )))
  }

  do.call(rbind, rows)
}

build_factor_table <- function(portfolio_series, factor_defs, core_returns_map) {
  rows <- lapply(seq_len(nrow(factor_defs)), function(i) {
    factor_df <- build_factor_proxy_series(factor_defs[i, , drop = FALSE], core_returns_map)
    merged <- merge(portfolio_series, factor_df, by = "date", suffixes = c("_portfolio", "_factor"))
    merged <- merged[is.finite(merged$ret_portfolio) & is.finite(merged$ret_factor), , drop = FALSE]
    metrics <- safe_regression_metrics(merged$ret_portfolio, merged$ret_factor, min_obs = 60L)

    data.frame(
      Factor = factor_defs$factor[i],
      Proxy = factor_defs$proxy[i],
      Beta = format_num(metrics$beta, digits = 2),
      Corr = format_pct_signed(metrics$corr * 100, digits = 1),
      `Adj.R2` = format_pct_plain(metrics$r2 * 100, digits = 1),
      `Alpha p.a.` = format_pct_signed(metrics$alpha_daily * 252 * 100, digits = 1),
      Obs = metrics$obs,
      beta_num = metrics$beta,
      corr_num = metrics$corr,
      r2_num = metrics$r2,
      alpha_num = metrics$alpha_daily * 252 * 100,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, rows)
}

build_correlation_heatmap <- function(position_snapshot, held_returns_map, portfolio_series,
                                      benchmark_returns_map, max_positions = 12L) {
  top_positions <- head(position_snapshot[order(-position_snapshot$value_eur), , drop = FALSE], max_positions)
  series_map <- list(PORT = portfolio_series)

  for (i in seq_len(nrow(top_positions))) {
    series_map[[top_positions$ticker[i]]] <- held_returns_map[[top_positions$ticker[i]]]
  }

  for (ticker in c("GLD", "SPY", "QQQ", "TLT")) {
    if (!is.null(benchmark_returns_map[[ticker]]) && nrow(benchmark_returns_map[[ticker]]) > 0) {
      series_map[[ticker]] <- benchmark_returns_map[[ticker]]
    }
  }

  wide <- merge_return_map(series_map)
  if (nrow(wide) == 0 || ncol(wide) < 3) {
    return(list(matrix = matrix(NA_real_, nrow = 0, ncol = 0), labels = character()))
  }

  corr_matrix <- stats::cor(wide[, setdiff(names(wide), "date"), drop = FALSE], use = "pairwise.complete.obs")
  corr_matrix[is.na(corr_matrix)] <- 0

  list(matrix = corr_matrix, labels = colnames(corr_matrix))
}

build_portfolio_performance_data <- function(portfolio_df, quotes_df, benchmark_key = "none",
                                             period = "5y", workers = 4L) {
  snapshot <- build_position_snapshot(portfolio_df, quotes_df)
  if (nrow(snapshot) == 0) {
    return(list(
      series = data.frame(date = as.Date(character()), series = character(), indexed = numeric(), stringsAsFactors = FALSE),
      benchmark_label = "No Benchmark",
      benchmark_ticker = NA_character_,
      observations = 0
    ))
  }

  total_value <- sum(snapshot$value_eur, na.rm = TRUE)
  weights <- snapshot$value_eur
  names(weights) <- snapshot$ticker
  if (!is.finite(total_value) || total_value <= 0) {
    weights[] <- 1 / length(weights)
  } else {
    weights <- weights / total_value
  }

  benchmark_row <- PORTFOLIO_PERFORMANCE_BENCHMARKS[
    PORTFOLIO_PERFORMANCE_BENCHMARKS$key == benchmark_key,
    ,
    drop = FALSE
  ]
  if (nrow(benchmark_row) == 0) {
    benchmark_row <- PORTFOLIO_PERFORMANCE_BENCHMARKS[PORTFOLIO_PERFORMANCE_BENCHMARKS$key == "none", , drop = FALSE]
  }

  history_tickers <- snapshot$ticker
  if (!is.na(benchmark_row$ticker[1]) && nzchar(benchmark_row$ticker[1])) {
    history_tickers <- unique(c(history_tickers, benchmark_row$ticker[1]))
  }

  history_map <- fetch_history_batch(history_tickers, period = period, field = "adjusted", workers = workers)

  fx_currencies <- snapshot$currency
  if (!is.na(benchmark_row$currency[1]) && nzchar(benchmark_row$currency[1])) {
    fx_currencies <- unique(c(fx_currencies, benchmark_row$currency[1]))
  }
  fx_map <- fetch_history_batch(required_fx_tickers(fx_currencies), period = period, field = "adjusted", workers = min(workers, 3L))

  held_returns_map <- build_return_map(snapshot[, c("ticker", "currency"), drop = FALSE], history_map, fx_map)
  portfolio_wide <- merge_return_map(held_returns_map)
  portfolio_returns <- compute_weighted_portfolio_returns(portfolio_wide, weights)
  portfolio_returns <- portfolio_returns[is.finite(portfolio_returns$ret), , drop = FALSE]
  portfolio_index <- returns_to_index_series(portfolio_returns)
  portfolio_index$series <- "Portfolio"

  output_frames <- list(portfolio_index[, c("date", "series", "indexed"), drop = FALSE])

  if (!is.na(benchmark_row$ticker[1]) && nzchar(benchmark_row$ticker[1])) {
    benchmark_meta <- data.frame(
      ticker = benchmark_row$ticker[1],
      currency = benchmark_row$currency[1],
      stringsAsFactors = FALSE
    )
    benchmark_returns_map <- build_return_map(benchmark_meta, history_map, fx_map)
    benchmark_returns <- benchmark_returns_map[[benchmark_row$ticker[1]]]
    benchmark_returns <- benchmark_returns[is.finite(benchmark_returns$ret), , drop = FALSE]
    benchmark_index <- returns_to_index_series(benchmark_returns)
    if (nrow(benchmark_index) > 0) {
      benchmark_index$series <- benchmark_row$label[1]
      output_frames[[length(output_frames) + 1]] <- benchmark_index[, c("date", "series", "indexed"), drop = FALSE]
    }
  }

  out <- do.call(rbind, output_frames)
  out <- out[order(out$date, out$series), , drop = FALSE]
  rownames(out) <- NULL

  list(
    series = out,
    benchmark_label = benchmark_row$label[1],
    benchmark_ticker = benchmark_row$ticker[1],
    observations = length(unique(out$date[out$series == "Portfolio"]))
  )
}

build_portfolio_analytics <- function(portfolio_df, quotes_df, period = "1y",
                                      add_weight = 0.10, workers = 4L,
                                      progress = NULL) {
  advance <- function(value = NULL, detail = NULL) {
    if (is.function(progress)) progress(value, detail)
  }

  snapshot <- build_position_snapshot(portfolio_df, quotes_df)
  if (nrow(snapshot) == 0) {
    return(NULL)
  }

  total_value <- sum(snapshot$value_eur, na.rm = TRUE)
  weights <- snapshot$value_eur
  names(weights) <- snapshot$ticker
  if (!is.finite(total_value) || total_value <= 0) {
    weights[] <- 1 / length(weights)
  } else {
    weights <- weights / total_value
  }

  factor_tickers <- unique(c(PORTFOLIO_ANALYTICS_FACTOR_PROXIES$long_ticker, PORTFOLIO_ANALYTICS_FACTOR_PROXIES$short_ticker))
  factor_tickers <- factor_tickers[!is.na(factor_tickers) & nzchar(factor_tickers)]
  similarity_universe <- read_similarity_universe()

  advance(0.10, "Loading portfolio, benchmark, and factor histories from Yahoo")
  core_meta <- unique(rbind(
    data.frame(ticker = snapshot$ticker, label = snapshot$label, category = "Held", currency = snapshot$currency, stringsAsFactors = FALSE),
    PORTFOLIO_ANALYTICS_BENCHMARKS[, c("ticker", "label", "category", "currency")],
    data.frame(ticker = factor_tickers, label = factor_tickers, category = "Factor", currency = "USD", stringsAsFactors = FALSE)
  ))

  core_price_map <- fetch_history_batch(core_meta$ticker, period = period, field = "adjusted", workers = workers)

  advance(0.35, "Loading FX histories for EUR-based comparison")
  fx_tickers <- required_fx_tickers(unique(c(core_meta$currency, similarity_universe$currency)))
  fx_map <- fetch_history_batch(fx_tickers, period = period, field = "adjusted", workers = min(workers, 3L))

  core_returns_map <- build_return_map(core_meta[, c("ticker", "currency"), drop = FALSE], core_price_map, fx_map)
  held_returns_map <- core_returns_map[snapshot$ticker]
  benchmark_returns_map <- core_returns_map[PORTFOLIO_ANALYTICS_BENCHMARKS$ticker]

  held_wide <- merge_return_map(held_returns_map)
  portfolio_series <- compute_weighted_portfolio_returns(held_wide, weights)
  portfolio_series <- portfolio_series[is.finite(portfolio_series$ret), , drop = FALSE]

  advance(0.60, "Scanning S&P 500 candidates for similarity and diversification")
  stock_price_map <- fetch_history_batch(similarity_universe$ticker, period = period, field = "adjusted", workers = workers)
  stock_returns_map <- build_return_map(similarity_universe[, c("ticker", "currency"), drop = FALSE], stock_price_map, fx_map)

  candidate_meta <- unique(rbind(
    data.frame(
      ticker = similarity_universe$ticker,
      label = similarity_universe$company,
      bucket = similarity_universe$sector,
      stringsAsFactors = FALSE
    ),
    data.frame(
      ticker = PORTFOLIO_ANALYTICS_BENCHMARKS$ticker,
      label = PORTFOLIO_ANALYTICS_BENCHMARKS$label,
      bucket = PORTFOLIO_ANALYTICS_BENCHMARKS$category,
      stringsAsFactors = FALSE
    )
  ))

  candidate_returns_map <- c(stock_returns_map, benchmark_returns_map)

  advance(0.82, "Computing benchmark links, factor tilts, and candidate ranks")
  benchmark_table <- build_benchmark_comparison_table(portfolio_series, PORTFOLIO_ANALYTICS_BENCHMARKS, benchmark_returns_map)
  factor_table <- build_factor_table(portfolio_series, PORTFOLIO_ANALYTICS_FACTOR_PROXIES, core_returns_map)
  diversifier_table <- build_diversifier_table(
    portfolio_series = portfolio_series,
    candidate_meta = candidate_meta,
    candidate_returns_map = candidate_returns_map,
    held_tickers = snapshot$ticker,
    add_weight = add_weight
  )
  similar_table <- build_similar_names_table(snapshot, held_returns_map, similarity_universe, stock_returns_map, top_n = 2L)
  heatmap <- build_correlation_heatmap(snapshot, held_returns_map, portfolio_series, benchmark_returns_map)

  current_vol <- annualize_volatility(portfolio_series$ret)
  capm_row <- factor_table[factor_table$Factor == "CAPM Market", , drop = FALSE]
  gold_row <- benchmark_table[benchmark_table$Ticker == "GLD", , drop = FALSE]
  best_div <- if (nrow(diversifier_table) > 0) diversifier_table$Ticker[1] else "--"

  advance(1, "Done")
  list(
    generated_at = Sys.time(),
    period = period,
    add_weight = add_weight,
    universe_size = nrow(similarity_universe),
    observations = sum(is.finite(portfolio_series$ret)),
    portfolio_vol = current_vol,
    capm_beta = if (nrow(capm_row) > 0) capm_row$beta_num[1] else NA_real_,
    gold_corr = if (nrow(gold_row) > 0) gold_row$corr_num[1] else NA_real_,
    best_diversifier = best_div,
    benchmark_table = benchmark_table,
    factor_table = factor_table,
    diversifier_table = diversifier_table,
    similar_table = similar_table,
    heatmap = heatmap
  )
}
