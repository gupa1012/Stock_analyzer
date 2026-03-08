# Stock Analyzer — Shiny Fundamental Analysis Dashboard

A **Shiny R** web application that crawls **[GuruFocus.com](https://www.gurufocus.com)** using **RSelenium** to retrieve live fundamental data for any stock, then displays interactive charts and key financial ratios.

---

## Features

| Section | Content |
|---------|---------|
| **Overview** | KPI cards (P/E, P/B, ROE, margins …) + quality radar chart |
| **Valuation** | Historical P/E, P/B, P/S and EV/EBITDA trend lines |
| **Financials** | Annual Revenue, Diluted EPS, Free Cash Flow bar charts |
| **Quality** | Profit margins, ROE/ROA/ROIC, Debt/Equity, Current Ratio |
| **Raw Data** | Full table of all scraped metrics |

- Searchable **S&P 500 ticker list** (242 companies) or enter any custom ticker.
- Dark-themed dashboard built with `shinydashboard` + custom CSS.
- All charts are interactive (zoom, hover, export) via `plotly`.

---

## Requirements

| Requirement | Notes |
|-------------|-------|
| R ≥ 4.0 | [Download R](https://cran.r-project.org/) |
| Google Chrome | Must be installed on the server/machine |
| `chromedriver` | Version must match Chrome; install via `wdman` or system package |
| Java (optional) | Required only if using Selenium standalone server |

---

## Quick Start

```bash
# 1. Clone the repo
git clone https://github.com/gupa1012/Stock_analyzer.git
cd Stock_analyzer

# 2. Install R packages
Rscript install.R

# 3. Launch the app
Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

The app opens in your default browser. Select a ticker, click **Analyze**, and wait
~20–30 seconds while GuruFocus pages are crawled.

---

## Project Structure

```
Stock_analyzer/
├── app.R                  # Main Shiny app (UI + server)
├── install.R              # One-time package installer
├── R/
│   ├── scraper.R          # RSelenium / rvest scraping logic
│   ├── charts.R           # Plotly chart builders
│   └── utils.R            # Formatting helpers & colour palette
├── data/
│   └── sp500_tickers.csv  # S&P 500 ticker list (ticker, company, sector)
└── www/
    └── custom.css         # Dark-theme dashboard styles
```

---

## How It Works

1. On startup the app launches a **headless Chrome** via `RSelenium`.
2. When you click **Analyze**, the scraper navigates to  
   `https://www.gurufocus.com/term/summary/{TICKER}` for summary metrics, then  
   to individual term pages (e.g. `/term/pe/AAPL`) for 10–20 years of history.
3. The extracted data is stored in reactive values and rendered as plotly charts.

---

## Notes

- **GuruFocus rate limiting**: The scraper introduces small delays between page  
  loads. Do not scrape dozens of tickers in rapid succession.
- **ChromeDriver version**: If Chrome auto-updates, `chromedriver` may need to  
  be updated too. Run `wdman::chrome(version = "latest")` to auto-manage it.
- The app is designed for personal/research use in compliance with  
  GuruFocus's terms of service.
