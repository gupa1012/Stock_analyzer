# ──────────────────────────────────────────────────────────────
#  sentiment.R – Financial news sentiment analysis engine
#
#  Fetches news from multiple RSS sources (Yahoo Finance,
#  Google News, Handelsblatt, Financial Times) and scores
#  sentiment using a Loughran-McDonald–inspired financial
#  lexicon.  No external API keys required.
# ──────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(xml2)
  library(httr)
})

# ── Loughran-McDonald–inspired financial sentiment lexicon ───
# Curated word lists for financial text analysis.

POSITIVE_WORDS <- tolower(c(
 "achieve", "achievement", "advance", "advancing", "advantage",
 "attain", "attract", "attractive", "benefit", "beneficial",
 "best", "better", "boost", "breakthrough", "bull", "bullish",
 "climb", "confident", "creative", "deliver", "dividend",
 "earn", "earnings", "efficient", "enable", "enhance",
 "exceed", "excellent", "expand", "expansion", "favorable",
 "gain", "gains", "good", "great", "grew", "grow", "growing",
 "growth", "high", "higher", "highest", "improve", "improved",
 "improvement", "increase", "increased", "increasing",
 "innovative", "lead", "leader", "leadership", "momentum",
 "opportunity", "optimal", "optimism", "optimistic", "outpace",
 "outperform", "outperformance", "overcome", "positive",
 "premium", "profit", "profitable", "profitability", "progress",
 "prosper", "rally", "rallied", "record", "recovery", "rebound",
 "resilient", "resolve", "revenue", "reward", "rise", "rising",
 "robust", "solid", "stable", "stability", "steady",
 "strength", "strengthen", "strong", "succeed", "success",
 "successful", "superior", "surge", "surged", "surpass",
 "transform", "turnaround", "upgrade", "upgraded", "upside",
 "upturn", "value", "win", "winner",
 # German positive words (Handelsblatt)
 "anstieg", "aufwaerts", "boom", "chance", "chancen",
 "durchbruch", "erfolgreich", "erholung", "gewinn", "gewinne",
 "hoch", "kursgewinn", "ueberraschend", "positiv", "rekord",
 "stark", "staerke", "steigerung", "uebertroffen", "wachstum",
 "zuwachs", "zuversicht"
))

NEGATIVE_WORDS <- tolower(c(
 "abandon", "adverse", "against", "bankrupt", "bankruptcy",
 "bear", "bearish", "blame", "catastrophe", "caution",
 "challenge", "close", "closure", "collapse", "concern",
 "concerns", "contraction", "costly", "crash", "crisis",
 "critical", "cut", "cuts", "danger", "debt", "decline",
 "declined", "declining", "decrease", "decreasing", "default",
 "deficit", "delay", "deteriorate", "difficult", "difficulty",
 "disappointing", "disappointment", "disruption", "doubt",
 "downturn", "downgrade", "downgraded", "downside", "drag",
 "drop", "dropped", "dropping", "fail", "failed", "failure",
 "fall", "fallen", "falling", "fear", "fraud", "headwind",
 "impair", "impairment", "inflation", "lawsuit", "layoff",
 "layoffs", "liability", "liquidate", "litigation", "lose",
 "loss", "losses", "low", "lower", "lowest", "miss", "missed",
 "negative", "penalty", "plummet", "plunge", "poor",
 "pressure", "problem", "problems", "recession", "restructure",
 "risk", "risky", "sanction", "sanctions", "sell", "selloff",
 "shortage", "shrink", "slump", "slowdown", "stagnation",
 "struggle", "sued", "suspend", "tariff", "tariffs",
 "terminate", "threat", "trouble", "turmoil", "uncertain",
 "uncertainty", "underperform", "unfavorable", "volatile",
 "volatility", "vulnerability", "war", "warn", "warning",
 "weak", "weaker", "weakness", "worsen", "worst", "writedown",
 # German negative words (Handelsblatt)
 "absturz", "angst", "einbruch", "krise", "kurzarbeit",
 "negativ", "pleite", "rezession", "risiko", "rueckgang",
 "schwach", "schwaeche", "sorge", "sorgen", "verlust",
 "verluste", "warnung", "zoelle"
))


# ── RSS feed sources ─────────────────────────────────────────

build_feed_urls <- function(ticker = NULL) {
  feeds <- list()

  if (!is.null(ticker) && nchar(ticker) > 0) {
    # Yahoo Finance ticker-specific RSS (via Google News)
    feeds$yahoo_ticker <- paste0(
      "https://news.google.com/rss/search?q=",
      utils::URLencode(paste(ticker, "stock")),
      "&hl=en&gl=US&ceid=US:en"
    )
    # German-language search for Handelsblatt coverage
    feeds$de_ticker <- paste0(
      "https://news.google.com/rss/search?q=",
      utils::URLencode(paste(ticker, "Aktie")),
      "&hl=de&gl=DE&ceid=DE:de"
    )
  }

  # Financial Times – markets RSS
  feeds$ft_markets <- "https://www.ft.com/markets?format=rss"
  feeds$ft_companies <- "https://www.ft.com/companies?format=rss"

  # Handelsblatt RSS feeds
  feeds$hb_finanzen <- "https://www.handelsblatt.com/contentexport/feed/finanzen"
  feeds$hb_boerse   <- "https://www.handelsblatt.com/contentexport/feed/finanzen/boerse"

  # Reuters business
  feeds$reuters <- "https://news.google.com/rss/search?q=site:reuters.com+business&hl=en"

  feeds
}


# ── Parse a single RSS feed ──────────────────────────────────

parse_rss_feed <- function(url, source_name = "Unknown",
                           timeout_seconds = 8) {
  tryCatch({
    resp <- httr::GET(url,
      httr::timeout(timeout_seconds),
      httr::user_agent("Mozilla/5.0 (compatible; BloombergTerminalLight/2.0)")
    )
    if (httr::status_code(resp) != 200) return(data.frame())

    raw <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_xml(raw)

    # Try RSS 2.0 items
    items <- xml2::xml_find_all(doc, ".//item")
    if (length(items) == 0) {
      # Try Atom entries
      items <- xml2::xml_find_all(doc, ".//entry")
    }
    if (length(items) == 0) return(data.frame())

    titles <- vapply(items, function(x) {
      t <- xml2::xml_find_first(x, ".//title")
      if (inherits(t, "xml_missing")) "" else xml2::xml_text(t)
    }, character(1))

    descriptions <- vapply(items, function(x) {
      d <- xml2::xml_find_first(x, ".//description")
      if (inherits(d, "xml_missing")) {
        d <- xml2::xml_find_first(x, ".//summary")
      }
      if (inherits(d, "xml_missing")) "" else xml2::xml_text(d)
    }, character(1))

    dates <- vapply(items, function(x) {
      d <- xml2::xml_find_first(x, ".//pubDate")
      if (inherits(d, "xml_missing")) {
        d <- xml2::xml_find_first(x, ".//published")
      }
      if (inherits(d, "xml_missing")) {
        d <- xml2::xml_find_first(x, ".//updated")
      }
      if (inherits(d, "xml_missing")) "" else xml2::xml_text(d)
    }, character(1))

    links <- vapply(items, function(x) {
      l <- xml2::xml_find_first(x, ".//link")
      if (inherits(l, "xml_missing")) "" else {
        href <- xml2::xml_attr(l, "href")
        if (is.na(href)) xml2::xml_text(l) else href
      }
    }, character(1))

    data.frame(
      title       = titles,
      description = descriptions,
      date        = dates,
      link        = links,
      source      = source_name,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame()
  })
}


# ── Score sentiment of a text string ─────────────────────────

score_text <- function(text) {
  if (is.null(text) || is.na(text) || nchar(text) == 0) {
    return(list(score = 0, positive = 0, negative = 0, total = 0))
  }

  # Strip HTML tags and normalize
  clean <- gsub("<[^>]+>", " ", text)
  clean <- gsub("[^a-zA-ZäöüÄÖÜß ]", " ", clean)
  words <- tolower(strsplit(trimws(clean), "\\s+")[[1]])
  words <- words[nchar(words) > 2]

  pos <- sum(words %in% POSITIVE_WORDS)
  neg <- sum(words %in% NEGATIVE_WORDS)
  total <- length(words)

  # Score: range roughly -1 to +1
  if (total == 0) {
    score <- 0
  } else {
    score <- (pos - neg) / sqrt(total)  # normalized
    score <- max(-1, min(1, score))     # clamp
  }

  list(score = score, positive = pos, negative = neg, total = total)
}


# ── Analyse sentiment for a ticker ───────────────────────────

analyse_sentiment <- function(ticker = NULL, max_articles = 40) {
  feeds <- build_feed_urls(ticker)

  all_articles <- data.frame()
  for (name in names(feeds)) {
    src <- switch(name,
      yahoo_ticker = paste0("Google News (", ticker, ")"),
      de_ticker    = paste0("Google News DE (", ticker, ")"),
      ft_markets   = "Financial Times Markets",
      ft_companies = "Financial Times Companies",
      hb_finanzen  = "Handelsblatt Finanzen",
      hb_boerse    = "Handelsblatt Boerse",
      reuters      = "Reuters Business",
      name
    )
    arts <- parse_rss_feed(feeds[[name]], source_name = src)
    if (nrow(arts) > 0) all_articles <- rbind(all_articles, arts)
  }

  if (nrow(all_articles) == 0) {
    return(list(
      ticker       = ticker,
      overall      = 0,
      label        = "NEUTRAL",
      colour       = "#f5a623",
      articles     = data.frame(),
      by_source    = data.frame(),
      pos_count    = 0,
      neg_count    = 0,
      neutral_count = 0,
      total        = 0,
      timestamp    = Sys.time()
    ))
  }

  # Limit to recent articles
  all_articles <- head(all_articles, max_articles)

  # Score each article (combine title + description)
  scores <- lapply(seq_len(nrow(all_articles)), function(i) {
    combined <- paste(all_articles$title[i],
                      all_articles$description[i])
    s <- score_text(combined)
    s$idx <- i
    s
  })

  all_articles$sentiment <- vapply(scores, function(s) s$score, numeric(1))
  all_articles$pos_words <- vapply(scores, function(s) s$positive, numeric(1))
  all_articles$neg_words <- vapply(scores, function(s) s$negative, numeric(1))

  # Classify
  all_articles$label <- ifelse(
    all_articles$sentiment > 0.05, "POSITIVE",
    ifelse(all_articles$sentiment < -0.05, "NEGATIVE", "NEUTRAL")
  )

  # Overall score: weighted mean (recent articles count more)
  n <- nrow(all_articles)
  weights <- rev(seq_len(n)) / sum(seq_len(n)) * n
  overall <- weighted.mean(all_articles$sentiment, weights)

  # Sentiment by source
  by_source <- aggregate(sentiment ~ source, data = all_articles, FUN = mean)
  by_source$count <- as.numeric(table(all_articles$source)[by_source$source])
  by_source <- by_source[order(-by_source$sentiment), ]

  # Counts
  pos_count     <- sum(all_articles$label == "POSITIVE")
  neg_count     <- sum(all_articles$label == "NEGATIVE")
  neutral_count <- sum(all_articles$label == "NEUTRAL")

  # Label & colour
  if (overall > 0.15) {
    lbl <- "BULLISH"; clr <- "#00c853"
  } else if (overall > 0.05) {
    lbl <- "POSITIVE"; clr <- "#69f0ae"
  } else if (overall < -0.15) {
    lbl <- "BEARISH"; clr <- "#ff1744"
  } else if (overall < -0.05) {
    lbl <- "NEGATIVE"; clr <- "#ff8a80"
  } else {
    lbl <- "NEUTRAL"; clr <- "#f5a623"
  }

  list(
    ticker        = ticker,
    overall       = round(overall, 3),
    label         = lbl,
    colour        = clr,
    articles      = all_articles,
    by_source     = by_source,
    pos_count     = pos_count,
    neg_count     = neg_count,
    neutral_count = neutral_count,
    total         = n,
    timestamp     = Sys.time()
  )
}
