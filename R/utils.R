# R/utils.R
# Utility / formatting helpers for the Stock Analyzer app

# ── Number formatting ──────────────────────────────────────────────────────────

#' Format a large number with B/M/K suffix
format_large <- function(x) {
  if (is.na(x) || !is.numeric(x)) return("N/A")
  if (abs(x) >= 1e12) return(paste0(round(x / 1e12, 2), "T"))
  if (abs(x) >= 1e9)  return(paste0(round(x / 1e9,  2), "B"))
  if (abs(x) >= 1e6)  return(paste0(round(x / 1e6,  2), "M"))
  if (abs(x) >= 1e3)  return(paste0(round(x / 1e3,  2), "K"))
  return(as.character(round(x, 2)))
}

#' Format a ratio to 2 decimal places
format_ratio <- function(x) {
  if (is.na(x) || !is.numeric(x)) return("N/A")
  paste0(round(x, 2), "x")
}

#' Format a percentage
format_pct <- function(x) {
  if (is.na(x) || !is.numeric(x)) return("N/A")
  paste0(round(x, 2), "%")
}

BASE_CURRENCY <- "EUR"

fx_pair_ticker <- function(from_currency, to_currency = BASE_CURRENCY) {
  from_currency <- toupper(trimws(as.character(from_currency)))
  to_currency   <- toupper(trimws(as.character(to_currency)))
  if (is.na(from_currency) || from_currency == "" || from_currency == to_currency)
    return(NA_character_)
  paste0(from_currency, to_currency, "=X")
}

required_fx_tickers <- function(currencies, to_currency = BASE_CURRENCY) {
  tickers <- unique(vapply(currencies, fx_pair_ticker, character(1),
                           to_currency = to_currency))
  tickers[!is.na(tickers) & nchar(tickers) > 0]
}

convert_amount_to_base <- function(amount, from_currency, quotes_df,
                                   to_currency = BASE_CURRENCY) {
  if (is.na(amount)) return(NA_real_)

  from_currency <- toupper(trimws(as.character(from_currency)))
  to_currency   <- toupper(trimws(as.character(to_currency)))

  if (is.na(from_currency) || from_currency == "" || from_currency == to_currency)
    return(amount)

  fx_ticker <- fx_pair_ticker(from_currency, to_currency)
  if (is.na(fx_ticker) || is.null(quotes_df) || nrow(quotes_df) == 0) return(NA_real_)

  row_q <- quotes_df[quotes_df$ticker == fx_ticker, ]
  if (nrow(row_q) == 0 || is.na(row_q$price[1])) return(NA_real_)

  amount * row_q$price[1]
}

format_base_currency <- function(x, digits = 0, na_value = "--",
                                 currency_symbol = "EUR") {
  if (!is.numeric(x)) {
    return(rep(na_value, length(x)))
  }

  symbol <- switch(toupper(currency_symbol),
    "EUR" = "\u20ac",
    "USD" = "$",
    currency_symbol
  )

  out <- rep(na_value, length(x))
  ok <- !is.na(x)
  out[ok] <- paste0(
    symbol,
    formatC(x[ok], format = "f", digits = digits, big.mark = ",")
  )
  out
}

format_signed_base_currency <- function(x, digits = 0, na_value = "--",
                                        currency_symbol = "EUR") {
  if (!is.numeric(x)) {
    return(rep(na_value, length(x)))
  }

  out <- rep(na_value, length(x))
  ok <- !is.na(x)
  sign <- ifelse(x[ok] >= 0, "+", "-")
  out[ok] <- paste0(
    sign,
    format_base_currency(abs(x[ok]), digits = digits,
                         currency_symbol = currency_symbol)
  )
  out
}

# ── Colour helpers ─────────────────────────────────────────────────────────────

#' Return green / red depending on the sign of a value
sign_colour <- function(x) {
  if (is.na(x) || !is.numeric(x)) return("#888888")
  if (x >= 0) "#27ae60" else "#e74c3c"
}

# ── KPI card HTML builder ──────────────────────────────────────────────────────

#' Create a coloured KPI value box as HTML
kpi_card <- function(title, value, subtitle = NULL, colour = "#2980b9") {
  sub_html <- if (!is.null(subtitle)) {
    paste0('<div class="kpi-sub">', subtitle, '</div>')
  } else ""

  htmltools::HTML(
    paste0(
      '<div class="kpi-card" style="border-top:4px solid ', colour, ';">',
      '  <div class="kpi-title">', title, '</div>',
      '  <div class="kpi-value">', value, '</div>',
      sub_html,
      '</div>'
    )
  )
}

# ── Data parsing helpers ───────────────────────────────────────────────────────

#' Parse a numeric string that may contain commas, %, B, M, T, K
parse_numeric_str <- function(s) {
  if (is.null(s) || is.na(s) || s == "" || s == "N/A" || s == "-") return(NA_real_)
  s <- trimws(s)
  multiplier <- 1
  if (grepl("T$", s)) { multiplier <- 1e12; s <- sub("T$", "", s) }
  else if (grepl("B$", s)) { multiplier <- 1e9;  s <- sub("B$", "", s) }
  else if (grepl("M$", s)) { multiplier <- 1e6;  s <- sub("M$", "", s) }
  else if (grepl("K$", s)) { multiplier <- 1e3;  s <- sub("K$", "", s) }
  # remove % and commas
  s <- gsub("[%,]", "", s)
  val <- suppressWarnings(as.numeric(s))
  if (is.na(val)) return(NA_real_)
  val * multiplier
}

#' Safe numeric coercion of a vector of strings
safe_numeric <- function(x) {
  vapply(x, parse_numeric_str, numeric(1), USE.NAMES = FALSE)
}

# ── Bloomberg-style UI helpers ───────────────────────────────────────────────

#' Bloomberg-style KPI card
bb_kpi <- function(title, value, subtitle = NULL, colour = "#f5a623") {
  htmltools::div(class = "bb-kpi",
    htmltools::div(class = "bb-kpi-title", title),
    htmltools::div(class = "bb-kpi-value",
                  style = paste0("color:", colour, ";"), value),
    if (!is.null(subtitle))
      htmltools::div(class = "bb-kpi-subtitle", subtitle)
  )
}

#' Empty plotly chart placeholder
bb_empty_chart <- function(msg = "No data") {
  plotly::plot_ly() |>
    plotly::layout(
      title = list(text = msg,
                   font = list(color = "#6c757d", size = 14)),
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# ── Colour palette ─────────────────────────────────────────────────────────────
APP_COLOURS <- list(
  primary   = "#2980b9",
  success   = "#27ae60",
  warning   = "#f39c12",
  danger    = "#e74c3c",
  info      = "#8e44ad",
  secondary = "#95a5a6",
  bg        = "#1a1a2e",
  card      = "#16213e",
  text      = "#ecf0f1"
)
