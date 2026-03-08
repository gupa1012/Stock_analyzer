# R/scraper.R
# GuruFocus scraping functions using RSelenium + rvest

library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)

RSELENIUM_AVAILABLE <- requireNamespace("RSelenium", quietly = TRUE)
if (RSELENIUM_AVAILABLE) library(RSelenium)

# ── Selenium session management ────────────────────────────────────────────────

#' Start a headless Chrome Selenium driver.
#' Returns a list with $driver (remoteDriver) and $server (wdman server or NULL).
#' Raises an error with a helpful message if RSelenium is not installed.
start_selenium_driver <- function(port = 4567L) {
  if (!RSELENIUM_AVAILABLE) {
    stop(
      "The 'RSelenium' package is not installed. ",
      "Please run: install.packages('RSelenium') ",
      "and ensure Google Chrome + chromedriver are installed on this machine."
    )
  }
  options <- list(
    chromeOptions = list(
      args = c(
        "--headless",
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--disable-gpu",
        "--window-size=1920,1080",
        "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36"
      )
    )
  )

  tryCatch({
    rD <- rsDriver(
      browser     = "chrome",
      port        = port,
      verbose     = FALSE,
      extraCapabilities = options,
      chromever   = NULL   # use system chromedriver
    )
    list(driver = rD[["client"]], server = rD[["server"]])
  }, error = function(e) {
    # Fallback: connect to an already-running Selenium / chromedriver
    message("rsDriver failed (", conditionMessage(e), "); trying direct connection...")
    remDr <- tryCatch(
      remoteDriver(
        remoteServerAddr = "localhost",
        port             = 9515L,
        browserName      = "chrome"
      ),
      error = function(e2) stop("Cannot start or connect to a Chrome WebDriver: ", conditionMessage(e2))
    )
    remDr$open(silent = TRUE)
    list(driver = remDr, server = NULL)
  })
}

#' Stop and clean up a Selenium session returned by start_selenium_driver().
stop_selenium_driver <- function(sel) {
  tryCatch({
    if (!is.null(sel$driver))  sel$driver$close()
    if (!is.null(sel$server))  sel$server$stop()
  }, error = function(e) NULL)
}

# ── GuruFocus helpers ──────────────────────────────────────────────────────────

GURUFOCUS_BASE <- "https://www.gurufocus.com"

#' Navigate to a URL and wait for an element (by CSS selector) to appear.
gf_navigate <- function(driver, url, wait_selector = NULL, timeout = 15) {
  driver$navigate(url)
  Sys.sleep(2)

  if (!is.null(wait_selector)) {
    start <- proc.time()["elapsed"]
    repeat {
      els <- tryCatch(
        driver$findElements(using = "css selector", value = wait_selector),
        error = function(e) list()
      )
      if (length(els) > 0) break
      if ((proc.time()["elapsed"] - start) > timeout) break
      Sys.sleep(1)
    }
  }
  Sys.sleep(1)
}

#' Get the rendered page source as an rvest html_document.
get_page <- function(driver) {
  src <- driver$getPageSource()[[1]]
  read_html(src)
}

# ── Parse a GuruFocus "term" history table ─────────────────────────────────────

#' Scrape historical annual data from a GuruFocus term page.
#' Returns a data.frame with columns: year (numeric), value (numeric).
scrape_term_history <- function(driver, ticker, term) {
  url <- paste0(GURUFOCUS_BASE, "/term/", term, "/", ticker)
  gf_navigate(driver, url, wait_selector = ".t-body, table", timeout = 12)
  page <- get_page(driver)

  df <- tryCatch({
    # GuruFocus term pages render data in a Vue/Element-UI table.
    # Try extracting the JSON embedded in a <script> tag first.
    scripts <- html_nodes(page, "script") %>% html_text()
    json_hit <- NA

    for (sc in scripts) {
      if (grepl('"annualValue"', sc) || grepl('"annual"', sc)) {
        # locate the JSON blob
        m <- regmatches(sc, regexpr('\\{[^{}]*"annual[^}]*\\}', sc, perl = TRUE))
        if (length(m) > 0) {
          json_hit <- tryCatch(fromJSON(m[1]), error = function(e) NA)
          break
        }
      }
    }

    # Fallback: parse the visible data table
    tables <- html_nodes(page, "table")
    if (length(tables) == 0) return(NULL)

    best <- NULL
    for (tbl in tables) {
      rows <- html_nodes(tbl, "tr")
      if (length(rows) < 3) next
      cells <- html_nodes(rows[[2]], "td")
      if (length(cells) < 2) next
      # Check first column looks like a year
      first <- trimws(html_text(cells[[1]]))
      if (grepl("^(19|20)\\d{2}", first)) {
        best <- tbl
        break
      }
    }
    if (is.null(best)) return(NULL)

    rows   <- html_nodes(best, "tr")
    parsed <- lapply(rows, function(r) {
      cells <- html_nodes(r, "td")
      if (length(cells) < 2) return(NULL)
      yr  <- trimws(html_text(cells[[1]]))
      val <- trimws(html_text(cells[[2]]))
      if (!grepl("^(19|20)\\d{2}", yr)) return(NULL)
      data.frame(year = as.integer(substr(yr, 1, 4)),
                 value = parse_numeric_str(val),
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), parsed))
  }, error = function(e) NULL)

  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df[order(df$year), ]
  df <- df[!is.na(df$value), ]
  df
}

# ── Parse the GuruFocus summary page ──────────────────────────────────────────

#' Extract the key-statistics grid from the GuruFocus summary page.
#' Returns a named list of current values.
scrape_summary_metrics <- function(driver, ticker) {
  url <- paste0(GURUFOCUS_BASE, "/term/summary/", ticker)
  gf_navigate(driver, url, wait_selector = ".t-body, .summary-table, table", timeout = 15)
  page <- get_page(driver)

  result <- list()

  # Strategy 1 – look for element-ui table rows (".el-table__row td .cell")
  rows <- html_nodes(page, ".el-table__row")
  if (length(rows) > 0) {
    for (r in rows) {
      cells <- html_nodes(r, ".cell")
      if (length(cells) >= 2) {
        label <- trimws(html_text(cells[[1]]))
        value <- trimws(html_text(cells[[2]]))
        if (nchar(label) > 0 && nchar(value) > 0)
          result[[label]] <- value
      }
    }
  }

  # Strategy 2 – plain <table> rows
  if (length(result) == 0) {
    for (tbl in html_nodes(page, "table")) {
      for (r in html_nodes(tbl, "tr")) {
        cells <- html_nodes(r, "td")
        if (length(cells) >= 2) {
          label <- trimws(html_text(cells[[1]]))
          value <- trimws(html_text(cells[[2]]))
          if (nchar(label) > 0 && nchar(value) > 0)
            result[[label]] <- value
        }
      }
    }
  }

  # Strategy 3 – look for <div class="t-body"> or similar
  if (length(result) == 0) {
    rows <- html_nodes(page, ".t-body .el-col, .t-body div")
    for (i in seq_along(rows)) {
      txt <- trimws(html_text(rows[[i]]))
      if (nchar(txt) > 0 && i < length(rows)) {
        next_txt <- trimws(html_text(rows[[i + 1]]))
        if (nchar(next_txt) > 0)
          result[[txt]] <- next_txt
      }
    }
  }

  result
}

# ── Main scraping entry point ──────────────────────────────────────────────────

#' Fetch all data for a given ticker from GuruFocus.
#' Returns a named list with:
#'   $summary   – named list of current metric strings
#'   $history   – named list of data.frames (one per metric)
#'   $ticker    – the ticker
#'   $timestamp – POSIXct when data was fetched
scrape_gurufocus <- function(driver, ticker) {
  ticker <- toupper(trimws(ticker))
  message("Scraping GuruFocus for: ", ticker)

  # --- Summary metrics ---
  summary_metrics <- tryCatch(
    scrape_summary_metrics(driver, ticker),
    error = function(e) {
      message("Summary scrape error: ", e$message)
      list()
    }
  )

  # --- Historical term data ---
  terms <- c(
    pe            = "pe",
    pb            = "pb",
    ps            = "ps",
    ev_ebitda     = "ev2ebitda",
    revenue       = "revenue",
    eps_diluted   = "eps_diluted",
    fcf           = "fcf",
    roe           = "roe",
    roa           = "roa",
    roic          = "roic",
    gross_margin  = "grossprofitmargin",
    oper_margin   = "operatingmargin",
    net_margin    = "netmargin",
    debt_equity   = "deb2equity",
    current_ratio = "current_ratio"
  )

  history <- list()
  for (nm in names(terms)) {
    Sys.sleep(0.8)   # polite delay
    df <- tryCatch(
      scrape_term_history(driver, ticker, terms[[nm]]),
      error = function(e) { message(nm, " history error: ", e$message); NULL }
    )
    if (!is.null(df) && nrow(df) > 0) history[[nm]] <- df
  }

  list(
    ticker    = ticker,
    summary   = summary_metrics,
    history   = history,
    timestamp = Sys.time()
  )
}

# ── Convenience: extract a labelled metric from the summary list ───────────────

#' Search the summary list by partial label match (case-insensitive).
get_metric <- function(summary_list, pattern, default = NA_character_) {
  if (length(summary_list) == 0) return(default)
  idx <- grep(pattern, names(summary_list), ignore.case = TRUE, value = TRUE)
  if (length(idx) == 0) return(default)
  summary_list[[idx[[1]]]]
}

#' As above but parse to numeric.
get_metric_num <- function(summary_list, pattern, default = NA_real_) {
  val <- get_metric(summary_list, pattern, default = NA_character_)
  if (is.na(val)) return(default)
  parse_numeric_str(val)
}
