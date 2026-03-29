# ──────────────────────────────────────────────────────────────
#  etf_xray.R – ETF look-through (X-Ray) helpers
#
#  Data sources (priority order per ETF):
#   1. Local XLSX file in user_data/  (full holdings, manually refreshed)
#      – Matched via glob pattern in LOCAL_XLSX_PATTERNS; newest file wins.
#   2. iShares CSV download from ishares.com (full holdings, live)
#      – Used for iShares ETFs with an entry in ISHARES_CSV_URLS.
#   3. justETF.com SSR HTML (top-10 only, live)
#      – Fallback for any ETF with an ISIN in ETF_ISINS.
#
#  To add a new ETF: add to ETF_ISINS (required), and optionally
#  LOCAL_XLSX_PATTERNS and/or ISHARES_CSV_URLS.
# ──────────────────────────────────────────────────────────────

# Glob patterns to find local XLSX holdings files in user_data/
# The newest matching file is used (handles dated filenames like "... 29.3.2026.xlsx").
# Drop a fresh Vanguard export into user_data/ whenever you want to refresh.
LOCAL_XLSX_PATTERNS <- c(
  "VGWD.DE" = "vanguard"   # case-insensitive match against filenames in user_data/
)

# Tickers in the portfolio that are ETFs, mapped to their ISIN
ETF_ISINS <- c(
  "VGWD.DE" = "IE00B8GKDB10",  # Vanguard FTSE AW High Div Yield UCITS ETF
  "XSMI.DE" = "LU0274221281",  # Xtrackers MSCI Switzerland UCITS ETF
  "SXR8.DE" = "IE00B5BMR087",  # iShares Core S&P 500 UCITS ETF (Acc)
  "IS3Q.DE" = "IE00BP3QZ601",  # iShares Edge MSCI World Quality Factor UCITS ETF (Acc)
  "QDVW.DE" = "IE00BYYHSQ67"   # iShares MSCI World Quality Dividend ESG UCITS ETF (Dist)
)

# Direct CSV download URLs for iShares ETFs (from ishares.com DE)
ISHARES_CSV_URLS <- c(
  "SXR8.DE" = "https://www.ishares.com/de/privatanleger/de/produkte/253743/ishares-sp-500-b-ucits-etf-acc-fund/1478358465952.ajax?fileType=csv&fileName=SXR8_holdings&dataType=fund",
  "IS3Q.DE" = "https://www.ishares.com/de/privatanleger/de/produkte/270054/ishares-msci-world-quality-factor-ucits-etf/1478358465952.ajax?fileType=csv&fileName=IS3Q_holdings&dataType=fund",
  "QDVW.DE" = "https://www.ishares.com/de/privatanleger/de/produkte/288147/fund/1478358465952.ajax?fileType=csv&fileName=QDVW_holdings&dataType=fund"
)

# ── Fetch holdings for an ETF (dispatcher) ───────────────────
# Priority: local XLSX > iShares CSV > justETF HTML.
# Returns data.frame with columns: name, weight_pct
fetch_etf_holdings <- function(ticker, isin = ETF_ISINS[[ticker]]) {
  # 1. Local XLSX (full holdings, manually refreshed)
  if (ticker %in% names(LOCAL_XLSX_PATTERNS)) {
    result <- fetch_local_xlsx_holdings(LOCAL_XLSX_PATTERNS[[ticker]])
    if (!is.null(result) && nrow(result) > 0) return(result)
  }
  # 2. iShares CSV (full holdings, live)
  if (ticker %in% names(ISHARES_CSV_URLS)) {
    result <- fetch_ishares_holdings(ISHARES_CSV_URLS[[ticker]])
    if (!is.null(result) && nrow(result) > 0) return(result)
  }
  # 3. Fallback: justETF HTML (top 10)
  fetch_justetf_holdings(isin)
}

# ── Local XLSX reader ─────────────────────────────────────────
# Finds the newest XLSX in user_data/ whose filename (case-insensitive)
# contains `pattern`. Expects the Vanguard export format:
#   - 5 header rows to skip
#   - Columns: Ticker | Wertpapiere | % der Assets | Sektor | Region | ...
# Returns all holdings by default as data.frame(name, weight_pct).
fetch_local_xlsx_holdings <- function(pattern, top_n = NULL) {
  files <- list.files("user_data", pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)
  files <- files[grepl("\\.xlsx$", files, ignore.case = TRUE)]
  if (length(files) == 0) return(NULL)

  # Pick the most recently modified file
  info <- file.info(files)
  f    <- rownames(info)[which.max(info$mtime)]

  df <- tryCatch(
    readxl::read_excel(f, sheet = 1, skip = 5),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  name_col   <- "Wertpapiere"
  weight_col <- "% der Assets"
  if (!all(c(name_col, weight_col) %in% names(df))) return(NULL)

  # Parse "1,7313 %" → 1.7313
  parse_pct <- function(x) {
    x <- as.character(x)
    x <- gsub("\u00A0", "", x, useBytes = TRUE)
    x <- gsub("[[:space:]]", "", x)
    x <- sub("%$", "", x)
    as.numeric(gsub(",", ".", gsub("\\.", "", x)))
  }

  df$weight_pct <- suppressWarnings(parse_pct(df[[weight_col]]))
  df <- df[!is.na(df$weight_pct) & nchar(trimws(df[[name_col]])) > 0, ]
  df <- df[order(-df$weight_pct), ]
  if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) {
    df <- utils::head(df, top_n)
  }

  data.frame(
    name       = trimws(df[[name_col]]),
    weight_pct = df$weight_pct,
    stringsAsFactors = FALSE
  )
}

# ── iShares CSV parser ───────────────────────────────────────
# Downloads the CSV from ishares.com, parses German number format,
# returns top-N holdings by weight (default 10).
fetch_ishares_holdings <- function(csv_url, top_n = 10) {
  r <- tryCatch(
    httr::GET(
      csv_url,
      httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/121"),
      httr::timeout(25)
    ),
    error = function(e) NULL
  )
  if (is.null(r) || httr::status_code(r) != 200) return(NULL)

  txt <- rawToChar(httr::content(r, "raw"))
  lines <- strsplit(txt, "\n")[[1]]

  # Find the header row containing "Emittententicker"
  hdr_idx <- grep("Emittententicker", lines)
  if (length(hdr_idx) == 0) return(NULL)

  # Read CSV from header onward – German format: comma = decimal, semicolons irrelevant
  # The file uses commas as field separators but values are quoted
  data_lines <- lines[(hdr_idx[1]):length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  df <- tryCatch(
    read.csv(textConnection(paste(data_lines, collapse = "\n")),
             stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Columns: "Name", "Gewichtung (%)"
  name_col   <- "Name"
  weight_col <- "Gewichtung (%)"
  if (!all(c(name_col, weight_col) %in% names(df))) return(NULL)

  # Filter to Aktien rows only (skip cash, futures etc.)
  if ("Anlageklasse" %in% names(df)) {
    df <- df[df$Anlageklasse == "Aktien", ]
  }

  # Parse German decimals: "7,49" -> 7.49
  parse_de_num <- function(x) as.numeric(gsub(",", ".", gsub("\\.", "", x)))

  df$weight_pct <- parse_de_num(df[[weight_col]])
  df <- df[!is.na(df$weight_pct), ]
  df <- df[order(-df$weight_pct), ]
  df <- utils::head(df, top_n)

  data.frame(
    name       = trimws(df[[name_col]]),
    weight_pct = df$weight_pct,
    stringsAsFactors = FALSE
  )
}

# ── Fetch top-10 holdings via justETF.com SSR HTML ───────────
# Returns data.frame with columns: name, weight_pct
fetch_justetf_holdings <- function(isin) {
  url <- paste0("https://www.justetf.com/de/etf-profile.html?isin=", isin)
  r <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/121",
        "Accept-Language" = "de-DE,de;q=0.9"
      ),
      httr::timeout(20)
    ),
    error = function(e) NULL
  )

  if (is.null(r) || httr::status_code(r) != 200) return(NULL)

  txt <- rawToChar(httr::content(r, "raw"))

  # Pattern matches each top-holdings row in justETF SSR HTML
  row_patt <- paste0(
    'etf-holdings_top-holdings_row">[^<]*<td>[^<]*',
    '<a[^>]+title="([^"]+)"[^>]*>[^<]*<span>([^<]+)</span>',
    '.*?<span[^>]*data-testid="tl_etf-holdings_top-holdings_value_percentage"[^>]*>',
    '([0-9,\\.]+)%</span>'
  )

  ms  <- gregexpr(row_patt, txt, perl = TRUE)
  rows <- regmatches(txt, ms)[[1]]
  if (length(rows) == 0) return(NULL)

  parsed <- lapply(rows, function(row) {
    m <- regmatches(row, regexec(row_patt, row, perl = TRUE))[[1]]
    data.frame(
      name       = html_decode(trimws(m[3])),
      weight_pct = as.numeric(gsub(",", ".", m[4])),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, parsed)
}

# ── Simple HTML entity decoder ────────────────────────────────
html_decode <- function(x) {
  x <- gsub("&amp;",  "&",  x, fixed = TRUE)
  x <- gsub("&lt;",   "<",  x, fixed = TRUE)
  x <- gsub("&gt;",   ">",  x, fixed = TRUE)
  x <- gsub("&quot;", '"',  x, fixed = TRUE)
  x <- gsub("&#39;",  "'",  x, fixed = TRUE)
  x
}

# ── Build the X-Ray look-through table ───────────────────────
# portfolio_df : the portfolio data.frame (read_portfolio() output)
# quotes_df    : result of fetch_quotes() – has ticker, price columns
# holdings_cache : named list (ticker -> data.frame from fetch_etf_holdings)
#
# Returns a data.frame ready for DT display, or NULL if nothing to expand.
build_xray_table <- function(portfolio_df, quotes_df, holdings_cache) {
  if (is.null(portfolio_df) || nrow(portfolio_df) == 0) return(NULL)

  etf_rows <- portfolio_df[portfolio_df$ticker %in% names(ETF_ISINS), ]
  if (nrow(etf_rows) == 0) return(NULL)

  # Total portfolio market value (best effort)
  total_mv <- 0
  if (!is.null(quotes_df) && nrow(quotes_df) > 0) {
    for (i in seq_len(nrow(portfolio_df))) {
      pq <- quotes_df[quotes_df$ticker == portfolio_df$ticker[i], ]
      if (nrow(pq) > 0 && !is.na(pq$price[1])) {
        mv_native <- pq$price[1] * portfolio_df$shares[i]
        mv_base <- convert_amount_to_base(
          mv_native,
          portfolio_df$currency[i],
          quotes_df
        )
        if (!is.na(mv_base)) total_mv <- total_mv + mv_base
      }
    }
  }

  # Total market value of the ETF sleeve only.
  total_etf_mv <- 0
  if (!is.null(quotes_df) && nrow(quotes_df) > 0) {
    for (i in seq_len(nrow(etf_rows))) {
      pq <- quotes_df[quotes_df$ticker == etf_rows$ticker[i], ]
      if (nrow(pq) > 0 && !is.na(pq$price[1])) {
        etf_mv <- convert_amount_to_base(
          pq$price[1] * etf_rows$shares[i],
          etf_rows$currency[i],
          quotes_df
        )
        if (!is.na(etf_mv)) total_etf_mv <- total_etf_mv + etf_mv
      }
    }
  }

  out_rows <- list()

  for (i in seq_len(nrow(etf_rows))) {
    tkr   <- etf_rows$ticker[i]
    shr   <- etf_rows$shares[i]
    ccy   <- etf_rows$currency[i]
    hld   <- holdings_cache[[tkr]]
    if (is.null(hld) || nrow(hld) == 0) next

    # Market value of this ETF position
    etf_mv <- NA_real_
    if (!is.null(quotes_df) && nrow(quotes_df) > 0) {
      pq <- quotes_df[quotes_df$ticker == tkr, ]
      if (nrow(pq) > 0 && !is.na(pq$price[1])) {
        etf_mv <- convert_amount_to_base(pq$price[1] * shr, ccy, quotes_df)
      }
    }

    for (j in seq_len(nrow(hld))) {
      eff_mv <- if (!is.na(etf_mv)) etf_mv * hld$weight_pct[j] / 100 else NA
      etf_sleeve_wt <- if (total_etf_mv > 0 && !is.na(eff_mv)) eff_mv / total_etf_mv * 100 else NA
      total_portfolio_wt <- if (total_mv > 0 && !is.na(eff_mv)) eff_mv / total_mv * 100 else NA

      out_rows[[length(out_rows) + 1]] <- data.frame(
        `ETF`            = tkr,
        `Unternehmen`    = hld$name[j],
        `Gewicht im ETF` = paste0(formatC(hld$weight_pct[j], digits = 2, format = "f"), " %"),
        `Eff. Wert`      = if (is.na(eff_mv)) "--"
                           else format_base_currency(eff_mv, digits = 0),
        `Portfolioanteil`= if (is.na(etf_sleeve_wt)) "--"
                           else paste0(formatC(etf_sleeve_wt, digits = 2, format = "f"), " %"),
        eff_value_num    = if (is.na(eff_mv)) NA_real_ else eff_mv,
        portfolio_pct_num= if (is.na(etf_sleeve_wt)) NA_real_ else etf_sleeve_wt,
        total_portfolio_pct_num = if (is.na(total_portfolio_wt)) NA_real_ else total_portfolio_wt,
        check.names      = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out_rows) == 0) return(NULL)

  out <- do.call(rbind, out_rows)
  grouped_rows <- split(out, out$Unternehmen)

  out <- do.call(rbind, lapply(grouped_rows, function(part) {
    part <- part[order(-part$eff_value_num, -part$portfolio_pct_num), , drop = FALSE]

    total_eff_value <- if (all(is.na(part$eff_value_num))) {
      NA_real_
    } else {
      sum(part$eff_value_num, na.rm = TRUE)
    }

    total_portfolio_pct <- if (all(is.na(part$portfolio_pct_num))) {
      NA_real_
    } else {
      sum(part$portfolio_pct_num, na.rm = TRUE)
    }

    total_overall_pct <- if (all(is.na(part$total_portfolio_pct_num))) {
      NA_real_
    } else {
      sum(part$total_portfolio_pct_num, na.rm = TRUE)
    }

    data.frame(
      `ETF`             = paste(unique(part$ETF), collapse = " + "),
      `Unternehmen`     = part$Unternehmen[1],
      `Gewicht im ETF`  = paste(
        paste0(part$ETF, " ", part$`Gewicht im ETF`),
        collapse = " + "
      ),
      `Eff. Wert`       = if (is.na(total_eff_value)) "--"
                          else format_base_currency(total_eff_value, digits = 0),
      `Portfolioanteil` = if (is.na(total_portfolio_pct)) "--"
                          else paste0(formatC(total_portfolio_pct, digits = 2, format = "f"), " %"),
      eff_value_num     = total_eff_value,
      portfolio_pct_num = total_portfolio_pct,
      total_portfolio_pct_num = total_overall_pct,
      check.names       = FALSE,
      stringsAsFactors  = FALSE
    )
  }))

  out[order(-out$portfolio_pct_num, -out$eff_value_num), , drop = FALSE]
}
