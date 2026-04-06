# Bloomberg Terminal Light

A **Bloomberg-inspired** Shiny app for private portfolio management, watchlist tracking, fundamental analysis, and news sentiment scoring.

![Bloomberg Terminal Light](https://img.shields.io/badge/R-Shiny-blue?style=flat-square&logo=r)
![Theme](https://img.shields.io/badge/Theme-Bloomberg%20Dark-orange?style=flat-square)

---

## Features

| Module | Description |
|--------|-------------|
| **Portfolio** | Combined start page with KPIs, add/remove position controls, allocation, a full holdings table, a **5Y portfolio performance chart** with optional benchmark overlay, an **ETF X-Ray** look-through panel, plus a **Correlation & Diversification** scan with benchmark links, factor proxies, risk-reducer ideas, and similar-stock matches |
| **Watchlist** | Track stocks you're watching with target prices, daily move, 52-week range, inline mini charts, and a click-linked detail chart |
| **Analysis** | Valuation-first fundamental dashboard with Alpha Vantage overview-first loading, on-demand statement history, SEC EDGAR fallback for US tickers, Yahoo fallback, and demo mode |
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
│   ├── portfolio_analytics.R # Portfolio correlation / factor / diversification analytics
│   ├── sentiment.R        # News sentiment analysis engine
│   ├── mod_dashboard.R    # Combined portfolio dashboard / home tab
│   ├── mod_portfolio.R    # Legacy portfolio module (not the primary entry tab)
│   ├── mod_watchlist.R    # Watchlist module
│   ├── mod_analysis.R     # Fundamental analysis module
│   ├── mod_sentiment.R    # Sentiment display module
│   ├── scraper.R          # Fundamental data fetcher (Alpha Vantage + SEC EDGAR + Yahoo Finance)
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

All four primary tabs now show a compact **Data Sources** hint near the top so you can see which feeds currently drive the screen in front of you.

`user_data/watchlist.csv` uses the columns `ticker,date_added,target_price,notes`. Empty `target_price` or `notes` fields are normalized when read, which makes one-off imports from exported watchlists safe even when the source file has no targets or notes yet.

The Watchlist table uses `reactable` and now has a Bloomberg-style dark theme. It shows price, a stacked daily move cell, a 52-week range slider with the current price marker inside the stock's own range, and an inline sparkline per ticker. Clicking a row updates the price chart on the right, so there is no separate chart ticker selector.

The top Portfolio area also includes a **5Y performance chart** for the current holdings mix. It backcasts today's portfolio composition through the last five years and can optionally overlay:
- **MSCI World** via the Yahoo Finance ETF proxy `URTH`
- **S&P 500** via `SPY`

The chart is EUR-based where FX history is available and is meant as a holdings-mix comparison, not as a transaction-accurate reconstruction of past deposits, withdrawals, or historical position sizes.

### Correlation & Diversification Scan
The Portfolio dashboard now includes an on-demand **Correlation & Diversification** block below the allocation and P&L charts.

It is designed to answer questions like:
- how strongly the portfolio correlates with **Gold**, the **S&P 500**, long-duration bonds, defensive sectors, and other benchmark sleeves
- which additional names or ETFs could lower the portfolio's annualized volatility if a small weight is added
- which listed stocks move very similarly to existing direct holdings
- whether the portfolio currently shows stronger market, quality, momentum, low-volatility, or gold-hedge behavior via simple **CAPM / factor proxy** regressions

The scan is **on demand** to avoid unnecessary network traffic. It uses:
- **Yahoo Finance adjusted daily history** for held names, benchmarks, factor-proxy ETFs, and candidate stocks
- **Yahoo FX history** to convert non-EUR assets into EUR return series before portfolio-level comparisons are calculated
- the local `data/sp500_tickers.csv` universe for similar-stock and diversification candidate searches

Outputs in the dashboard:
- a correlation heatmap for the portfolio, top holdings, and selected benchmarks
- a benchmark table showing correlation, beta, annual return, and annualized volatility versus the portfolio
- a **Potential Risk Reducers** table that simulates adding a candidate at a configurable test weight (default `10%`) and ranks names that reduce overall portfolio volatility
- a **Similar Stocks** table that finds highly price-correlated S&P 500 names for each direct stock position
- a factor table with CAPM beta plus ETF-based proxy factors such as value, momentum, quality, and low-volatility

Related portfolio charting on the start page:
- a **5Y indexed holdings-mix chart** with optional `MSCI World` or `S&P 500` overlay for quick long-horizon context before drilling into correlations

This feature does **not** use Alpha Vantage and therefore does not consume the free-tier request budget.

### Base Currency
Portfolio, watchlist table values, X-Ray effective values, and portfolio analytics return series are shown or evaluated in **EUR**. For non-EUR securities the app fetches Yahoo FX pairs dynamically and converts price / market value / P&L into EUR.

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
The analysis module fetches fundamental data in an overview-first flow to reduce Alpha Vantage usage:

1. **Persistent cache first** — fundamentals payloads are stored under `user_data/cache/fundamentals/` as `.rds` files with fetch timestamps. Before any live request, the app checks this cache first and reuses data for up to 30 days.
2. **Alpha Vantage Overview first** (if API key is set via env var `alpha_vantage_api_key`) — the `ANALYZE` button loads only `OVERVIEW` initially, so opening the analysis tab does not immediately consume all free-tier calls.
3. **SEC EDGAR first for US statement history** — when a US ticker opens `Earnings`, `Balance Sheet`, or `Cash Flow`, the app prefers SEC `company_tickers.json` plus `companyfacts` XBRL data for annual revenue, operating income, net income, cash, debt, operating cash flow, free cash flow, buybacks, and derived leverage/liquidity ratios. This preserves Alpha Vantage statement calls for international names.
4. **Alpha Vantage statements on demand for non-US or missing SEC coverage** — if SEC is not applicable or does not provide strong enough coverage, the app fetches `INCOME_STATEMENT`, `BALANCE_SHEET`, and `CASH_FLOW` on demand. Requests are throttled to respect the free-tier `1 request / second` guidance.
5. **Yahoo Finance** (fallback, no key needed) — fetches via the unofficial JSON API. Covers a reduced set of revenue, operating income, net income, EPS, FCF, margins, and leverage metrics so the analysis tab still renders when no API key is available. If SEC or Alpha Vantage still leave gaps, the app supplements the missing histories from Yahoo instead of failing the full analysis.
6. **Demo mode** — generates realistic synthetic data when both live sources fail.

The analysis page also shows a local Alpha Vantage usage estimate. Alpha Vantage does **not** expose an official endpoint for “remaining requests today”, so the app logs its own live Alpha Vantage calls and displays an estimated `used / 25` count for the current day.

The analysis tab is organized into four views:
- **Overview** — company description, valuation tiles, valuation snapshot, and profitability/growth snapshot
- **Earnings** — 10-year revenue, operating income, net income, plus margin/return context
- **Balance Sheet** — cash vs debt history with leverage and liquidity charts
- **Cash Flow** — operating cash flow, free cash flow, buybacks, plus standalone FCF / EPS trend cards

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
- Alpha Vantage free tier: 25 requests/day. The app now checks a 30-day on-disk cache first, uses only the `OVERVIEW` call on the first `ANALYZE` click, and prefers SEC statement data for US tickers before spending Alpha Vantage statement requests.
- The Alpha Vantage counter shown in the Analysis tab is a **local estimate** of live calls made by this app, not an official quota value from Alpha Vantage.
- Without an API key the app automatically falls back to Yahoo Finance.
- For US tickers, SEC EDGAR is now the preferred source for statement history; Alpha Vantage statements are used mainly for international tickers or when SEC coverage is insufficient.
- Sentiment analysis uses free RSS feeds — no API keys needed.
- Portfolio and watchlist quotes are cached for 30 seconds to avoid duplicate requests across tabs.
- Historical Yahoo price series used by the portfolio performance and correlation scans are also cached in-session by ticker, lookback window, and field (`close` vs `adjusted`).
- The portfolio correlation scan uses Yahoo Finance plus the local S&P 500 universe and does not spend Alpha Vantage requests.
- Mobile and desktop are served by the same app; layout mode can be forced with the header selector or a `?mode=` query parameter.
- The app is designed for personal/research use.
