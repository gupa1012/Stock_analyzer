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
- **ETF X-Ray** (`R/etf_xray.R`): three-source architecture. Dispatcher `fetch_etf_holdings(ticker)` tries in order: (1) local XLSX in `user_data/` matched by `LOCAL_XLSX_PATTERNS`, (2) iShares CSV via `ISHARES_CSV_URLS`, (3) justETF.com SSR HTML fallback. Triggered by `materialSwitch` toggle in portfolio UI. Holdings cached in `rv$xray_holdings` (named list) with timestamp `rv$xray_fetched_at`. Add new ETFs by extending `ETF_ISINS` (required) plus optionally `LOCAL_XLSX_PATTERNS` and/or `ISHARES_CSV_URLS`.
- **X-Ray sorting**: `build_xray_table()` returns hidden numeric columns (`eff_value_num`, `portfolio_pct_num`) so DT tables can sort by effective portfolio weight instead of lexicographically sorting formatted strings.
- **Local XLSX format** (Vanguard DE export): 5 header rows to skip (`skip=5`), then `Ticker | Wertpapiere | % der Assets | Sektor | Region | Marktwert | Anteile`. Weight column format: `"1,7313 %"` with non-breaking spaces and German decimals. Drop newest file in `user_data/` — newest-mtime file matching the glob wins automatically. VGWD uses the full XLSX holdings list, not a top-10 cut.
- **iShares CSV format**: German locale — comma as decimal separator, periods as thousands separator, all values double-quoted. Header: `Emittententicker,Name,Sektor,Anlageklasse,Marktwert,Gewichtung (%),...`. Parsed by `fetch_ishares_holdings()` with custom `parse_de_num()`.
- **Price + FX data** (`R/market_data.R`): use Yahoo Finance `v8/finance/chart` directly; do not use `quantmod::getQuote()` because it fails in Germany due to Yahoo GDPR crumb requirements. Quotes are fetched in parallel, cached for 30 seconds in `.quote_cache`, and reused across dashboard/portfolio/watchlist. EUR conversion uses Yahoo FX pairs like `USDEUR=X` and `SGDEUR=X`.
- **Data sources**: Prices + FX = Yahoo Finance v8 chart API (free, unlimited). Fundamentals = Alpha Vantage (key required, 25/day free) with Yahoo Finance fallback. ETF holdings = iShares CSV + justETF.com.
- **PowerShell quoting**: Multi-line or complex R one-liners in `-e` break due to PowerShell quote rules. Always write to a temp `.R` file and run `Rscript path/to/file.R` instead.

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
