# ──────────────────────────────────────────────────────────────
#  data_manager.R – CSV-based persistence for portfolio & watchlist
# ──────────────────────────────────────────────────────────────

DATA_DIR <- file.path("user_data")

ensure_data_dir <- function() {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
}

# ── Portfolio ────────────────────────────────────────────────

PORTFOLIO_FILE <- file.path(DATA_DIR, "portfolio.csv")

PORTFOLIO_COLS <- c("id", "ticker", "shares", "avg_cost",
                    "currency", "date_added", "notes")

read_portfolio <- function() {
  ensure_data_dir()
  if (!file.exists(PORTFOLIO_FILE)) {
    return(data.frame(
      id         = character(),
      ticker     = character(),
      shares     = numeric(),
      avg_cost   = numeric(),
      currency   = character(),
      date_added = character(),
      notes      = character(),
      stringsAsFactors = FALSE
    ))
  }
  df <- read.csv(PORTFOLIO_FILE, stringsAsFactors = FALSE)
  # ensure all expected columns exist

  for (col in PORTFOLIO_COLS) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df
}

write_portfolio <- function(df) {
  ensure_data_dir()
  write.csv(df, PORTFOLIO_FILE, row.names = FALSE)
}

add_position <- function(ticker, shares, avg_cost,
                         currency = "USD", notes = "") {
  df <- read_portfolio()
  new_row <- data.frame(
    id         = paste0("pos_", format(Sys.time(), "%Y%m%d%H%M%S"),
                        "_", sample(1000:9999, 1)),
    ticker     = toupper(trimws(ticker)),
    shares     = as.numeric(shares),
    avg_cost   = as.numeric(avg_cost),
    currency   = currency,
    date_added = as.character(Sys.Date()),
    notes      = notes,
    stringsAsFactors = FALSE
  )
  df <- rbind(df, new_row)
  write_portfolio(df)
  df
}

update_position <- function(id, shares = NULL, avg_cost = NULL,
                            notes = NULL) {
  df <- read_portfolio()
  idx <- which(df$id == id)
  if (length(idx) == 0) return(df)
  if (!is.null(shares))   df$shares[idx]   <- as.numeric(shares)
  if (!is.null(avg_cost)) df$avg_cost[idx] <- as.numeric(avg_cost)
  if (!is.null(notes))    df$notes[idx]    <- notes
  write_portfolio(df)
  df
}

remove_position <- function(id) {
  df <- read_portfolio()
  df <- df[df$id != id, , drop = FALSE]
  write_portfolio(df)
  df
}

# ── Watchlist ────────────────────────────────────────────────

WATCHLIST_FILE <- file.path(DATA_DIR, "watchlist.csv")

WATCHLIST_COLS <- c("ticker", "date_added", "target_price", "notes")

read_watchlist <- function() {
  ensure_data_dir()
  if (!file.exists(WATCHLIST_FILE)) {
    return(data.frame(
      ticker       = character(),
      date_added   = character(),
      target_price = numeric(),
      notes        = character(),
      stringsAsFactors = FALSE
    ))
  }
  df <- read.csv(WATCHLIST_FILE, stringsAsFactors = FALSE)
  for (col in WATCHLIST_COLS) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df
}

write_watchlist <- function(df) {
  ensure_data_dir()
  write.csv(df, WATCHLIST_FILE, row.names = FALSE)
}

add_to_watchlist <- function(ticker, target_price = NA, notes = "") {
  df <- read_watchlist()
  tkr <- toupper(trimws(ticker))
  if (tkr %in% df$ticker) return(df)
  new_row <- data.frame(
    ticker       = tkr,
    date_added   = as.character(Sys.Date()),
    target_price = as.numeric(target_price),
    notes        = notes,
    stringsAsFactors = FALSE
  )
  df <- rbind(df, new_row)
  write_watchlist(df)
  df
}

update_watchlist_item <- function(ticker, target_price = NULL,
                                  notes = NULL) {
  df <- read_watchlist()
  idx <- which(df$ticker == toupper(trimws(ticker)))
  if (length(idx) == 0) return(df)
  if (!is.null(target_price)) df$target_price[idx] <- as.numeric(target_price)
  if (!is.null(notes))        df$notes[idx]        <- notes
  write_watchlist(df)
  df
}

remove_from_watchlist <- function(ticker) {
  df <- read_watchlist()
  df <- df[df$ticker != toupper(trimws(ticker)), , drop = FALSE]
  write_watchlist(df)
  df
}
