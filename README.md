# Bloomberg Terminal Light

A **Bloomberg-inspired** Shiny app for private portfolio management, watchlist tracking, fundamental analysis, and news sentiment scoring.

![Bloomberg Terminal Light](https://img.shields.io/badge/R-Shiny-blue?style=flat-square&logo=r)
![Theme](https://img.shields.io/badge/Theme-Bloomberg%20Dark-orange?style=flat-square)

---

## Features

| Module | Description |
|--------|-------------|
| **Portfolio** | Combined start page with KPIs, add/remove position controls, allocation, a full holdings table, and an **ETF X-Ray** look-through panel. When X-Ray is on, the main holdings list expands with ETF-derived underlying exposures |
| **Watchlist** | Track stocks you're watching with target prices, live quotes, and mini price charts |
| **Analysis** | Deep fundamental analysis (15+ metrics) via Alpha Vantage API or Yahoo Finance — valuation, financials, quality tabs, 5-year historicals. Demo mode included |
| **Sentiment** | News sentiment scoring using Financial Times, Handelsblatt, and Reuters RSS feeds with a Loughran-McDonald–inspired financial lexicon |

## Mobile Layout

The app now supports a dedicated responsive layout for smartphone usage, which works well on shinyapps.io without maintaining a separate codebase.

- **Auto detection**: the client checks viewport width and mobile user agent and switches into mobile mode automatically.
- **Manual override**: the header contains a `LAYOUT` selector with `Auto`, `Desktop`, and `Mobile`.
- **Persistent choice**: manual selection is stored in the browser via `localStorage`.
- **URL override**: add `?mode=mobile`, `?mode=desktop`, or `?mode=auto` to the app URL for testing or explicit routing.

This is especially useful on shinyapps.io, where the same deployed app can adapt its layout per device without needing a second deployment.

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
| Internet | Required for Yahoo Finance quotes, Alpha Vantage API, and RSS news feeds |
| Alpha Vantage API key | Optional but recommended. Free key at [alphavantage.co](https://www.alphavantage.co/support/#api-key) (25 req/day free). Set env var `alpha_vantage_api_key` |

> **No Chrome or chromedriver required.** The previous GuruFocus/RSelenium scraper has been replaced with direct API calls.

---

## Project Structure

```
Stock_analyzer/
├── app.R                  # Main Shiny app (Bloomberg Terminal)
├── install.R              # One-time package installer
├── R/
│   ├── data_manager.R     # Portfolio & watchlist CSV persistence
│   ├── market_data.R      # Yahoo Finance price + FX fetching (v8 chart API)
│   ├── sentiment.R        # News sentiment analysis engine
│   ├── mod_dashboard.R    # Combined portfolio dashboard / home tab
│   ├── mod_portfolio.R    # Legacy portfolio module (not the primary entry tab)
│   ├── mod_watchlist.R    # Watchlist module
│   ├── mod_analysis.R     # Fundamental analysis module
│   ├── mod_sentiment.R    # Sentiment display module
│   ├── scraper.R          # Fundamental data fetcher (Alpha Vantage + Yahoo Finance)
│   ├── etf_xray.R         # ETF look-through (X-Ray) — iShares CSV + justETF.com
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

### Deployment on shinyapps.io

The mobile variant is implemented inside the same Shiny app rather than as a separate app. On shinyapps.io this means:

- one deployment serves both desktop and mobile clients
- device detection happens in the browser, so no server-side user-agent routing is required
- users can still override the automatically detected mode from the header if they prefer the desktop or mobile layout

### Portfolio & Watchlist
Portfolio positions and watchlist entries are stored in local CSV files (`user_data/`). Live prices and FX rates are fetched from the Yahoo Finance v8 chart endpoint, cached for 30 seconds, and reused across modules.

### Base Currency
Portfolio, watchlist table values, and X-Ray effective values are shown in **EUR**. For non-EUR securities the app fetches Yahoo FX pairs dynamically and converts price / market value / P&L into EUR.

Currently required FX pairs for the sample portfolio:
- `USDEUR=X`
- `SGDEUR=X`

### ETF X-Ray
The main Portfolio tab has an **ETF X-Ray** toggle. When enabled:
- the right-hand panel shows aggregated look-through details across all ETF positions that hold the same company
- the main holdings table is expanded with ETF-derived underlying positions
- if the same company appears in multiple ETFs, those look-through rows are aggregated into one combined exposure in the main holdings table
- if you already hold the same company directly, the expanded holdings table also merges the direct line with the ETF look-through exposure when the portfolio note contains the company name

In the X-Ray detail table, repeated companies are also merged into a single row. The `ETF` column lists all contributing ETFs, while effective value is summed across those ETF sleeves. The displayed `Portfolioanteil` in that right-hand table is relative to the ETF sleeve only, whereas the main holdings table continues to show total-portfolio weights.

The X-Ray table is sorted by **effective portfolio weight** so the biggest underlying exposures rise to the top.

For `VGWD.DE`, the local Vanguard XLSX source is used as the primary source and contributes the **full holdings list** from the export file, not just the top 10.

Data sources (dispatcher tries in this order per ETF):
1. **Local XLSX** — manually exported holdings file dropped into `user_data/` (full fund). The newest file matching the ETF's glob pattern is used automatically, so simply drop a new file to refresh.
2. **iShares CSV** — direct download from ishares.com (full fund, live). Used for SXR8, IS3Q, QDVW.
3. **justETF.com** — SSR HTML scrape (public, no API key). Top-10 only. Fallback for XSMI and any new ETF without a file/CSV URL.

Extend by adding entries to `ETF_ISINS` (required), `LOCAL_XLSX_PATTERNS` (local file), and/or `ISHARES_CSV_URLS` (iShares) in `R/etf_xray.R`.

Currently mapped:
| Ticker | Name | ISIN | Source |
|--------|------|------|--------|
| VGWD.DE | Vanguard FTSE AW High Dividend Yield UCITS ETF | IE00B8GKDB10 | Local XLSX → justETF |
| XSMI.DE | Xtrackers MSCI Switzerland UCITS ETF | LU0274221281 | justETF |
| SXR8.DE | iShares Core S&P 500 UCITS ETF (Acc) | IE00B5BMR087 | iShares CSV |
| IS3Q.DE | iShares Edge MSCI World Quality Factor UCITS ETF (Acc) | IE00BP3QZ601 | iShares CSV |
| QDVW.DE | iShares MSCI World Quality Dividend ESG UCITS ETF (Dist) | IE00BYYHSQ67 | iShares CSV |

### Fundamental Analysis
The analysis module fetches fundamental data from two sources (in priority order):

1. **Alpha Vantage** (if API key is set via env var `alpha_vantage_api_key`) — 4 API calls per analysis (`OVERVIEW`, `INCOME_STATEMENT`, `BALANCE_SHEET`, `CASH_FLOW`). Provides P/S ratio, EV/EBITDA, TTM margins, and up to 5 years of annual historicals. Uses the `alphavantager` R package.
2. **Yahoo Finance** (fallback, no key needed) — fetches via the unofficial JSON API. Covers revenue, EPS, FCF, P/E, P/B, margins, ROE/ROA, debt ratios.
3. **Demo mode** — generates realistic synthetic data when both live sources fail.

No browser, Chrome, or chromedriver required.

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
- No Chrome/chromedriver needed — Selenium dependency fully removed.
- Alpha Vantage free tier: 25 requests/day (~6 full analyses/day).
- Without an API key the app automatically falls back to Yahoo Finance.
- Sentiment analysis uses free RSS feeds — no API keys needed.
- Portfolio and watchlist quotes are cached for 30 seconds to avoid duplicate requests across tabs.
- Mobile and desktop are served by the same app; layout mode can be forced with the header selector or a `?mode=` query parameter.
- The app is designed for personal/research use.
