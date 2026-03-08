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
