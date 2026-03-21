# Bloomberg Terminal Light

A **Bloomberg-inspired** Shiny app for private portfolio management, watchlist tracking, fundamental analysis, and news sentiment scoring.

![Bloomberg Terminal Light](https://img.shields.io/badge/R-Shiny-blue?style=flat-square&logo=r)
![Theme](https://img.shields.io/badge/Theme-Bloomberg%20Dark-orange?style=flat-square)

---

## Features

| Module | Description |
|--------|-------------|
| **Dashboard** | Portfolio overview with KPIs (total value, P&L, day change), allocation donut, and quick snapshot tables |
| **Portfolio** | Add, edit, and remove positions. Live pricing via Yahoo Finance. P&L tracking per position and aggregate |
| **Watchlist** | Track stocks you're watching with target prices, live quotes, and mini price charts |
| **Analysis** | Deep fundamental analysis (15 metrics) via GuruFocus scraping or demo data — valuation, financials, quality tabs |
| **Sentiment** | News sentiment scoring using Financial Times, Handelsblatt, and Reuters RSS feeds with a Loughran-McDonald–inspired financial lexicon |

---

## Bloomberg Aesthetic

- **Dark terminal theme** — pitch-black background, amber/orange accents
- **Monospace data fonts** (JetBrains Mono) for a Bloomberg Professional feel
- **Dense, information-rich layouts** with minimal whitespace
- **Colour-coded P&L** — green for gains, red for losses
- **Real-time clock** in the header bar

---

## Quick Start

```bash
# 1. Clone the repo
git clone https://github.com/gupa1012/Stock_analyzer.git
cd Stock_analyzer

# 2. Install R packages
Rscript install.R

# 3. Launch the app
Rscript -e "shiny::runApp('app.R', port = 3838, launch.browser = TRUE)"
```

---

## Requirements

| Requirement | Notes |
|-------------|-------|
| R ≥ 4.0 | [Download R](https://cran.r-project.org/) |
| Google Chrome | Only needed for GuruFocus analysis scraping |
| `chromedriver` | Version must match Chrome; install via `wdman` or system package |
| Internet | Required for Yahoo Finance quotes and RSS news feeds |

---

## Project Structure

```
Stock_analyzer/
├── app.R                  # Main Shiny app (Bloomberg Terminal)
├── install.R              # One-time package installer
├── R/
│   ├── data_manager.R     # Portfolio & watchlist CSV persistence
│   ├── market_data.R      # Yahoo Finance price fetching (quantmod)
│   ├── sentiment.R        # News sentiment analysis engine
│   ├── mod_dashboard.R    # Dashboard module (overview)
│   ├── mod_portfolio.R    # Portfolio management module
│   ├── mod_watchlist.R    # Watchlist module
│   ├── mod_analysis.R     # Fundamental analysis module
│   ├── mod_sentiment.R    # Sentiment display module
│   ├── scraper.R          # GuruFocus web scraper (RSelenium)
│   ├── charts.R           # Plotly chart builders
│   ├── utils.R            # Formatting helpers & colour palette
│   └── demo_data.R        # Demo data generator
├── data/
│   └── sp500_tickers.csv  # S&P 500 ticker list (242 companies)
├── user_data/             # Created at runtime (gitignored)
│   ├── portfolio.csv      # Your portfolio positions
│   └── watchlist.csv      # Your watchlist
└── www/
    └── custom.css         # Bloomberg dark theme
```

---

## How It Works

### Portfolio & Watchlist
Portfolio positions and watchlist entries are stored in local CSV files (`user_data/`). Live prices are fetched from Yahoo Finance via the `quantmod` package (with a direct API fallback).

### Fundamental Analysis
The analysis module uses RSelenium + rvest to scrape 15 fundamental metrics from GuruFocus.com. A demo mode generates realistic sample data when live scraping is unavailable.

### News Sentiment
The sentiment engine fetches RSS feeds from:
- **Financial Times** (Markets, Companies)
- **Handelsblatt** (Finanzen, Börse)
- **Reuters** (Business)
- **Google News** (ticker-specific, EN + DE)

Articles are scored using a curated financial sentiment lexicon (Loughran-McDonald inspired) with support for both English and German financial terms. The overall sentiment is displayed as a gauge (−1 bearish to +1 bullish) with per-source breakdowns and a scrollable headline feed.

---

## Notes

- Portfolio data is stored locally — no cloud sync.
- GuruFocus scraping requires Chrome/chromedriver.
- Sentiment analysis uses free RSS feeds — no API keys needed.
- Rate limiting is applied to both GuruFocus (0.8 s) and Yahoo Finance (0.3 s) requests.
- The app is designed for personal/research use.
