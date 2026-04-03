# Copilot Instructions – Stock Analyzer

## R Environment

- **R Version:** 4.4.2 (installed at `C:\Program Files\R\R-4.4.2`)
- **Rscript.exe:** `C:\Program Files\R\R-4.4.2\bin\Rscript.exe`
- **R.exe:** `C:\Program Files\R\R-4.4.2\bin\R.exe`
- R is **not** in the system PATH. Always use the full path when invoking R from the terminal.
- When running R commands in PowerShell, quote the path:
  ```powershell
  & "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" -e "print('hello')"
  ```
- **Library paths:**
  - User library: `C:/Users/patgs/AppData/Local/R/win-library/4.4`
  - System library: `C:/Program Files/R/R-4.4.2/library`
- An older R 4.2.1 also exists but should not be used.

## Project Conventions

- This is an R Shiny app (`app.R` is the entry point).
- Launch with: `& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" -e "shiny::runApp('app.R', port=3838, launch.browser=TRUE)"`
- Install packages with: `& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" install.R`
- Alpha Vantage API key env variable name: `alpha_vantage_api_key`
- Syntax-check the app (without running it) with: `& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" --vanilla -e "tryCatch({ source('app.R'); cat('OK\n') }, error=function(e) cat('ERROR:', conditionMessage(e), '\n'))" 2>&1 | Select-String "OK|ERROR"`

## Architecture Notes

- **Portfolio module** (`R/mod_portfolio.R`): uses `DT::renderDataTable` for the main table and `renderUI + DT::dataTableOutput` for the X-Ray sub-table. Never put `DT::renderDataTable()` directly inside `renderUI` — use a separate `output$xxx <-` pair.
- **Dashboard module** (`R/mod_dashboard.R`): this is the primary portfolio home tab. It combines the old dashboard + portfolio pages: KPIs, add/remove position controls, the main holdings table, allocation chart, and the X-Ray side panel all live here. When X-Ray is enabled, the right panel shows ETF details and the main holdings table expands with ETF-derived underlying rows. If the same company appears in multiple ETFs, aggregate those look-through rows into one combined exposure in the main holdings table. Sort the main holdings table by hidden numeric value columns rather than formatted currency strings.
- **Direct + X-Ray merge** (`R/mod_dashboard.R` + `R/utils.R`): in expanded holdings mode, direct positions are also merged with ETF look-through rows when the portfolio `notes` field contains the same company name. The direct row keeps its share/price/change fields where possible, while value and portfolio weight reflect total combined exposure.
- **X-Ray detail aggregation** (`R/etf_xray.R` + dashboard panel): `build_xray_table()` now also merges the same company across multiple ETFs in the right-hand X-Ray table. The displayed ETF column becomes a joined source list like `SXR8.DE + IS3Q.DE`, while effective EUR value is summed. The visible `Portfolioanteil` in that detail table is relative to the ETF sleeve only; a hidden `total_portfolio_pct_num` is used when the main holdings table needs total-portfolio weights.
- **ETF X-Ray** (`R/etf_xray.R`): three-source architecture. Dispatcher `fetch_etf_holdings(ticker)` tries in order: (1) local XLSX in `user_data/` matched by `LOCAL_XLSX_PATTERNS`, (2) iShares CSV via `ISHARES_CSV_URLS`, (3) justETF.com SSR HTML fallback. Triggered by `materialSwitch` toggle in portfolio UI. Holdings cached in `rv$xray_holdings` (named list) with timestamp `rv$xray_fetched_at`. Add new ETFs by extending `ETF_ISINS` (required) plus optionally `LOCAL_XLSX_PATTERNS` and/or `ISHARES_CSV_URLS`.
- **X-Ray sorting**: `build_xray_table()` returns hidden numeric columns (`eff_value_num`, `portfolio_pct_num`, `total_portfolio_pct_num`) so DT tables can sort by ETF-sleeve weight in the detail table while the expanded holdings table still uses total-portfolio weight instead of lexicographically sorting formatted strings.
- **Local XLSX format** (Vanguard DE export): 5 header rows to skip (`skip=5`), then `Ticker | Wertpapiere | % der Assets | Sektor | Region | Marktwert | Anteile`. Weight column format: `"1,7313 %"` with non-breaking spaces and German decimals. Drop newest file in `user_data/` — newest-mtime file matching the glob wins automatically. VGWD uses the full XLSX holdings list, not a top-10 cut.
- **iShares CSV format**: German locale — comma as decimal separator, periods as thousands separator, all values double-quoted. Header: `Emittententicker,Name,Sektor,Anlageklasse,Marktwert,Gewichtung (%),...`. Parsed by `fetch_ishares_holdings()` with custom `parse_de_num()`.
- **Price + FX data** (`R/market_data.R`): use Yahoo Finance `v8/finance/chart` directly; do not use `quantmod::getQuote()` because it fails in Germany due to Yahoo GDPR crumb requirements. Quotes are fetched in parallel, cached for 30 seconds in `.quote_cache`, and reused across dashboard/portfolio/watchlist. EUR conversion uses Yahoo FX pairs like `USDEUR=X` and `SGDEUR=X`.
- **Watchlist persistence** (`R/data_manager.R`): `read_watchlist()` coerces imported CSV columns back to the expected types (`ticker/date_added/notes` as character, `target_price` as numeric). This matters for bulk imports where empty CSV fields would otherwise come back as all-`NA` logical columns.
- **Watchlist module** (`R/mod_watchlist.R`): the table uses `reactable`, not DT. It has a dark Bloomberg-style theme, shows price, a stacked daily change cell, a 52-week range slider with the current price marker inside the range, target comparison, and an inline sparkline. The right-hand price chart is driven by row clicks from the table; there is no separate ticker selector for that chart.
- **Analysis module** (`R/mod_analysis.R`): now centers the Alpha Vantage `OVERVIEW` endpoint. The top of the tab is valuation-first (market cap, price, trailing/forward P/E, dividend yield, P/B, EV/EBITDA, target upside). The Overview tab combines company profile text with valuation/profitability snapshots. The historical tabs are now `Earnings`, `Balance Sheet`, and `Cash Flow` instead of the older valuation/quality layout. With an Alpha Vantage key, `ANALYZE` loads only Overview data first; the full statement package is fetched lazily when a detail tab is opened.
- **Page-level source notices** (`R/utils.R` + module UIs): Portfolio, Watchlist, Analysis, and Sentiment each render a compact `DATA SOURCES` notice near the top of the tab. Keep these notices aligned with the actual active feeds on screen, not just generic documentation text.
- **Persistent fundamentals cache** (`R/scraper.R`): live fundamentals payloads are cached on disk under `user_data/cache/fundamentals/` as `.rds` files with a fetch timestamp. Cache TTL is 30 days. The app checks this persistent cache before hitting Alpha Vantage or SEC endpoints, so previously fetched tickers like `V` should normally avoid repeat live requests inside that window.
- **Alpha Vantage fundamentals fetch** (`R/scraper.R`): use the documented raw JSON endpoints directly, not the `alphavantager` wrapper, because the documented payload shape is needed for annual reports and buyback fields. `fetch_av_overview_data()` handles the cheap first pass, while `fetch_av_fundamentals()` uses Alpha Vantage `OVERVIEW` plus on-demand statement endpoints when needed. Alpha Vantage requests are throttled to about 1.2 seconds between calls.
- **Alpha Vantage quota display** (`R/scraper.R` + analysis notice): Alpha Vantage does not provide an official endpoint for daily remaining requests. The app therefore keeps a local log of live Alpha Vantage calls under `user_data/cache/fundamentals/` and shows an estimated `used / 25` counter in the Analysis tab. Treat it as an app-local estimate, not an authoritative quota API response.
- **SEC EDGAR statement source** (`R/scraper.R`): for US tickers, `fetch_sec_fundamentals()` is now the preferred source for statement history before Alpha Vantage statement endpoints are used. It uses `https://www.sec.gov/files/company_tickers.json` to map ticker→CIK and `https://data.sec.gov/api/xbrl/companyfacts/CIK##########.json` to backfill annual revenue, operating income, net income, cash, debt, operating cash flow, free cash flow, buybacks, ROE/ROA, debt/equity, and current ratio. SEC requests require a User-Agent header but no API key.
- **Data sources**: Prices + FX = Yahoo Finance v8 chart API (free, unlimited). Fundamentals = persistent cache first, then Alpha Vantage overview-first (key required, 25/day free), SEC EDGAR first for US statement data, Alpha Vantage statements mainly for international or missing SEC coverage, and Yahoo Finance as the broader fallback. ETF holdings = iShares CSV + justETF.com.
- **PowerShell quoting**: Multi-line or complex R one-liners in `-e` break due to PowerShell quote rules. Always write to a temp `.R` file and run `Rscript path/to/file.R` instead.
- **Mobile layout** (`app.R` + `www/custom.css`): mobile support is handled in the same Shiny deployment. Client-side JS applies `bb-mobile-mode` or `bb-desktop-mode` based on viewport/user agent, with manual override from the header `LAYOUT` selector. Manual choice is persisted in `localStorage`, and `?mode=mobile|desktop|auto` can force the layout for testing or links.

## Known ETF ISINs (in portfolio)
| Ticker | ISIN | Source | Notes |
|--------|------|--------|-------|
| VGWD.DE | IE00B8GKDB10 | Local XLSX (glob: "vanguard") → justETF fallback | Vanguard FTSE AW High Dividend Yield |
| XSMI.DE | LU0274221281 | justETF | Xtrackers MSCI Switzerland |
| SXR8.DE | IE00B5BMR087 | iShares CSV | iShares Core S&P 500 (Acc) · WKN A0YEDG |
| IS3Q.DE | IE00BP3QZ601 | iShares CSV | iShares MSCI World Quality Factor (Acc) · WKN A12ATE |
| QDVW.DE | IE00BYYHSQ67 | iShares CSV | iShares MSCI World Quality Dividend ESG (Dist) · WKN A2DRG5 |

## iShares CSV URL Pattern
```
https://www.ishares.com/de/privatanleger/de/produkte/{PRODUCT_ID}/{FUND_SLUG}/1478358465952.ajax?fileType=csv&fileName={TICKER}_holdings&dataType=fund
```
- `1478358465952` is a fixed token across all iShares DE pages.
- Product IDs: 253743 (SXR8), 270054 (IS3Q), 288147 (QDVW).
- To add a new iShares ETF: find its product page URL on ishares.com/de, extract the product ID and ticker, then add the URL to `ISHARES_CSV_URLS` in `R/etf_xray.R`.

## Documentation Rules

- **Always update `README.md`** when features, modules, dependencies, or behaviour change — do this as part of the same task, not as a follow-up.
- **Always update this file (`copilot-instructions.md`)** whenever something worth knowing for future sessions is discovered: new env variables, quirks of the R setup, installed packages, architectural decisions, naming conventions, or lessons learned.
- Both updates are mandatory parts of any non-trivial code change, not optional extras.
