# ──────────────────────────────────────────────────────────────
#  mod_analysis.R – Stock analysis Shiny module (refactored)
# ──────────────────────────────────────────────────────────────

analysisUI <- function(id) {
  ns <- NS(id)

  has_widgets <- requireNamespace("shinyWidgets", quietly = TRUE)

  # ── Ticker data ──
  tickers_file <- file.path("data", "sp500_tickers.csv")
  if (file.exists(tickers_file)) {
    sp500 <- read.csv(tickers_file, stringsAsFactors = FALSE)
    ticker_choices <- setNames(sp500$ticker,
                               paste0(sp500$ticker, " – ", sp500$company))
  } else {
    ticker_choices <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")
  }

  div(class = "bb-page bb-analysis-page",
    fluidRow(
      class = "bb-section-header",
      column(12, h3(icon("search-dollar"), "FUNDAMENTAL ANALYSIS",
                    class = "bb-title"))
    ),

    fluidRow(
      column(12, uiOutput(ns("data_source_notice")))
    ),

    # ── Controls ──
    fluidRow(
      class = "bb-form-row",
      column(12,
        div(class = "bb-panel",
          fluidRow(
            column(4,
              if (has_widgets) {
                shinyWidgets::pickerInput(
                  ns("sel_ticker"), "S&P 500 Ticker",
                  choices  = ticker_choices,
                  selected = "AAPL",
                  options  = list(`live-search` = TRUE,
                                  size = 10,
                                  style = "btn-dark")
                )
              } else {
                selectInput(ns("sel_ticker"), "S&P 500 Ticker",
                            choices = ticker_choices, selected = "AAPL")
              }
            ),
            column(3,
              textInput(ns("custom_ticker"), "Custom Ticker",
                        placeholder = "e.g. TSLA")
            ),
            column(2, div(style = "margin-top: 25px;",
              actionButton(ns("btn_analyze"), "ANALYZE",
                           class = "bb-btn-primary",
                           icon = icon("chart-line"))
            )),
            column(2, div(style = "margin-top: 25px;",
              actionButton(ns("btn_demo"), "DEMO DATA",
                           class = "bb-btn-secondary",
                           icon = icon("flask"))
            )),
            column(1, div(style = "margin-top: 30px;",
              uiOutput(ns("status_badge"))
            ))
          ),
          # ── API Key row ──
          fluidRow(
            column(5,
              div(style = "padding-top: 6px;",
                passwordInput(
                  ns("av_api_key"),
                  label = tags$span(
                    icon("key", style = "color:#f5a623;"),
                    " Alpha Vantage API Key"
                  ),
                  value       = Sys.getenv("alpha_vantage_api_key"),
                  placeholder = "Paste key for richer data (optional)"
                )
              )
            ),
            column(7,
              div(style = "padding-top: 32px; color:#6c757d; font-size:12px;",
                icon("info-circle"), " With key: Alpha Vantage Overview first; US statement history prefers SEC EDGAR and cached data before new API calls.",
                tags$br(),
                "Without key: Yahoo Finance fallback with a reduced dataset.  ",
                tags$a(
                  href   = "https://www.alphavantage.co/support/#api-key",
                  target = "_blank",
                  style  = "color:#f5a623;",
                  icon("external-link-alt"), " Get free key"
                ),
                " (international detail tabs can still use Alpha Vantage statement calls when no fresh cache exists)."
              )
            )
          )
        )
      )
    ),

    # ── Conditional: Welcome vs Data ──
    conditionalPanel(
      condition = paste0("!output['", ns("has_data"), "']"),
      fluidRow(
        column(12,
          div(class = "bb-panel", style = "text-align:center; padding:60px 20px;",
            icon("chart-line", class = "fa-3x",
                 style = "color:#f5a623; margin-bottom:20px;"),
            h3("Select a ticker and click ANALYZE",
               style = "color:#ecf0f1;"),
            p("or use DEMO DATA to explore with sample data",
              style = "color:#6c757d;")
          )
        )
      )
    ),

    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "']"),

      # ── KPI row ──
      fluidRow(class = "bb-kpi-row",
        column(12, uiOutput(ns("kpi_row")))
      ),

      # ── Tabs ──
      div(class = "bb-analysis-tabs",
        tabsetPanel(
          id = ns("analysis_tabs"),
          type = "pills",

          tabPanel("Overview",
            fluidRow(
              column(7, div(class = "bb-panel",
                uiOutput(ns("company_overview_panel"))
              )),
              column(5, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_valuation_snapshot"), height = "330px")
              ))
            ),
            fluidRow(
              column(6, div(class = "bb-panel",
                uiOutput(ns("valuation_tile_grid"))
              )),
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_profitability_snapshot"), height = "330px")
              ))
            )
          ),

          tabPanel("Earnings",
            fluidRow(
              column(12, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_earnings_history"), height = "360px")
              ))
            ),
            fluidRow(
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_margins"), height = "300px")
              )),
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_returns"), height = "300px")
              ))
            )
          ),

          tabPanel("Balance Sheet",
            fluidRow(
              column(12, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_cash_vs_debt"), height = "360px")
              ))
            ),
            fluidRow(
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_debt"), height = "300px")
              )),
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_liquidity"), height = "300px")
              ))
            )
          ),

          tabPanel("Cash Flow",
            fluidRow(
              column(12, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_cashflow_allocation"), height = "360px")
              ))
            ),
            fluidRow(
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_fcf"), height = "300px")
              )),
              column(6, div(class = "bb-panel",
                plotly::plotlyOutput(ns("chart_eps"), height = "300px")
              ))
            )
          ),

          tabPanel("Raw Data",
            fluidRow(
              column(12, div(class = "bb-panel",
                div(class = "bb-table-scroll",
                  DT::dataTableOutput(ns("tbl_raw"))
                )
              ))
            )
          )
        )
      )
    )
  )
}


analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metric_card <- function(label, value, subtitle = NULL) {
      div(class = "bb-analysis-metric-card",
        div(class = "bb-analysis-metric-label", label),
        div(class = "bb-analysis-metric-value", value),
        if (!is.null(subtitle)) div(class = "bb-analysis-metric-sub", subtitle)
      )
    }

    rv <- reactiveValues(
      data           = NULL,
      loading        = FALSE,
      error          = NULL,
      detail_pending = FALSE
    )

    maybe_load_detail_data <- function(selected_tab = input$analysis_tabs) {
      if (is.null(rv$data) || isTRUE(rv$data$demo_mode) || !isTRUE(rv$detail_pending) || isTRUE(rv$loading)) {
        return(invisible(NULL))
      }

      if (is.null(selected_tab) || !selected_tab %in% c("Earnings", "Balance Sheet", "Cash Flow")) {
        return(invisible(NULL))
      }

      api_key <- trimws(input$av_api_key %||% "")
      if (nchar(api_key) == 0) {
        rv$detail_pending <- FALSE
        return(invisible(NULL))
      }

      rv$loading <- TRUE
      withProgress(message = paste("Loading detailed statements for", rv$data$ticker, "..."), {
        setProgress(0.2, detail = "Fetching statements and fallbacks ...")
        detailed_result <- tryCatch(
          fetch_av_fundamentals(rv$data$ticker, api_key),
          error = function(e) {
            rv$error <- e$message
            NULL
          }
        )

        if (!is.null(detailed_result)) {
          rv$data <- detailed_result
          rv$detail_pending <- FALSE
          rv$error <- NULL
          setProgress(0.95, detail = "Done.")
        } else {
          showNotification(
            paste0("Detailed statements for ", rv$data$ticker,
                   " could not be loaded right now. Overview remains available."),
            type = "warning", duration = 6
          )
        }
      })
      rv$loading <- FALSE
      invisible(NULL)
    }

    # ── Active ticker ──
    active_ticker <- reactive({
      ct <- input$custom_ticker
      custom <- if (is.null(ct)) "" else trimws(ct)
      if (nchar(custom) > 0) toupper(custom) else input$sel_ticker
    })

    # ── Analyze button ──
    observeEvent(input$btn_analyze, {
      ticker <- active_ticker()
      req(nchar(ticker) > 0)
      rv$loading <- TRUE
      rv$error   <- NULL

      withProgress(message = paste("Analyzing", ticker, "..."), {
        api_key <- trimws(input$av_api_key %||% "")

        result <- NULL

        # 1. Try Alpha Vantage if the user supplied a key
        if (nchar(api_key) > 0) {
          setProgress(0.2, detail = "Calling Alpha Vantage Overview …")
          result <- tryCatch(
            fetch_av_overview_data(ticker, api_key),
            error = function(e) { rv$error <- e$message; NULL }
          )
          if (!is.null(result)) {
            rv$detail_pending <- TRUE
            setProgress(0.9, detail = "Done.")
          } else {
            rv$detail_pending <- FALSE
            showNotification(
              paste0("Alpha Vantage fetch failed for ", ticker,
                     " – trying Yahoo Finance …"),
              type = "warning", duration = 6
            )
          }
        }

        # 2. Fall back to Yahoo Finance
        if (is.null(result)) {
          setProgress(0.4, detail = "Calling Yahoo Finance …")
          rv$detail_pending <- FALSE
          result <- tryCatch(
            fetch_yahoo_fundamentals(ticker),
            error = function(e) { rv$error <- e$message; NULL }
          )
        }

        if (!is.null(result)) {
          rv$data <- result
          if (isTRUE(rv$detail_pending)) {
            showNotification(
              paste0("Loaded Overview for ", ticker,
                     ". Statement history will load when you open a detail tab."),
              type = "message", duration = 5
            )
          }
        } else {
          rv$data <- generate_demo_data(ticker)
          rv$error <- NULL
          rv$detail_pending <- FALSE
          showNotification(
            paste0("All live sources failed – showing demo data for ", ticker, "."),
            type = "warning", duration = 8
          )
        }
      })
      rv$loading <- FALSE
    })

    # ── Demo button ──
    observeEvent(input$btn_demo, {
      ticker <- active_ticker()
      rv$data           <- generate_demo_data(ticker)
      rv$error          <- NULL
      rv$loading        <- FALSE
      rv$detail_pending <- FALSE
    })

    observeEvent(input$analysis_tabs, {
      maybe_load_detail_data(input$analysis_tabs)
    }, ignoreInit = TRUE)

    observeEvent(rv$data, {
      maybe_load_detail_data(input$analysis_tabs)
    }, ignoreInit = TRUE)

    # ── Has data flag ──
    output$has_data <- reactive(!is.null(rv$data))
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # ── Status badge ──
    output$status_badge <- renderUI({
      if (rv$loading) {
        span(class = "bb-badge bb-badge-warning", icon("spinner",
             class = "fa-spin"), "LOADING")
      } else if (!is.null(rv$error)) {
        span(class = "bb-badge bb-badge-danger", icon("exclamation-triangle"),
             "ERROR")
      } else if (!is.null(rv$data)) {
        if (isTRUE(rv$data$demo_mode)) {
          span(class = "bb-badge bb-badge-info", icon("flask"), "DEMO")
        } else {
          span(class = "bb-badge bb-badge-success", icon("signal"), "LIVE")
        }
      } else {
        span(class = "bb-badge bb-badge-muted", "READY")
      }
    })

    output$data_source_notice <- renderUI({
      d <- rv$data
      api_key <- trimws(input$av_api_key %||% "")
      av_status <- if (nchar(api_key) > 0) get_av_usage_status() else NULL

      items <- if (is.null(d)) {
        c(
          "Flow" = "30-day disk cache -> Alpha Vantage OVERVIEW -> SEC EDGAR first for US statements -> Yahoo fallback",
          "Cache" = "user_data/cache/fundamentals"
        )
      } else if (isTRUE(d$demo_mode)) {
        c(
          "Current View" = "Demo dataset",
          "Live Sources" = "Alpha Vantage, SEC EDGAR, Yahoo Finance",
          "Cache" = "user_data/cache/fundamentals"
        )
      } else {
        c(
          d$metadata$source_breakdown,
          "Loaded" = format_notice_timestamp(d$timestamp),
          "Cache" = "user_data/cache/fundamentals"
        )
      }

      if (!is.null(av_status)) {
        items <- c(
          items,
          "Alpha Vantage" = paste0(
            "No official remaining-requests endpoint; local live calls today ",
            av_status$live_requests_today,
            "/",
            av_status$limit,
            ", est. left ",
            av_status$estimated_remaining
          )
        )
      } else {
        items <- c(items, "Alpha Vantage" = "No API key configured")
      }

      bb_data_source_notice(
        items,
        footer = "Alpha Vantage does not expose an official daily remaining-request counter; the shown value is this app's local live-call estimate."
      )
    })

    # ── KPI row ──
    output$kpi_row <- renderUI({
      d <- rv$data
      if (is.null(d)) return(NULL)

      s <- d$summary
      tagList(
        fluidRow(
          column(3, bb_kpi("Market Cap",
                           get_metric(s, "market cap", "--"),
                           subtitle = "Capitalization",
                           colour = "#f5a623")),
          column(3, bb_kpi("Current Price",
                           get_metric(s, "current price", "--"),
                           subtitle = get_metric(s, "reported currency", "--"),
                           colour = "#4da3ff")),
          column(3, bb_kpi("Trailing P/E",
                           get_metric(s, "trailing pe|pe ratio", "--"),
                           subtitle = "TTM",
                           colour = "#44d7b6")),
          column(3, bb_kpi("Forward P/E",
                           get_metric(s, "forward pe", "--"),
                           subtitle = "Consensus",
                           colour = "#ffd166"))
        ),
        fluidRow(
          column(3, bb_kpi("Dividend Yield",
                           get_metric(s, "dividend yield", "--"),
                           subtitle = "Current",
                           colour = "#ff6b6b")),
          column(3, bb_kpi("P/B",
                           get_metric(s, "price to book|pb ratio", "--"),
                           subtitle = "Book multiple",
                           colour = "#8e7dff")),
          column(3, bb_kpi("EV/EBITDA",
                           get_metric(s, "ev/ebitda", "--"),
                           subtitle = "Enterprise value",
                           colour = "#00c853")),
          column(3, bb_kpi("Target Upside",
                           get_metric(s, "target upside", "--"),
                           subtitle = "vs analyst target",
                           colour = "#ff8f3d"))
        )
      )
    })

    output$company_overview_panel <- renderUI({
      d <- rv$data
      if (is.null(d)) return(NULL)

      s <- d$summary
      company_name <- get_metric(s, "company name", d$ticker)
      source_label <- get_metric(s, "source", if (isTRUE(d$demo_mode)) "Demo" else "Live")
      description <- get_metric(s, "description", "No company description available.")

      div(class = "bb-analysis-company",
        div(class = "bb-analysis-company-header",
          div(
            div(class = "bb-analysis-company-name", company_name),
            div(class = "bb-analysis-company-symbol", d$ticker)
          ),
          div(class = "bb-analysis-company-badges",
            span(class = "bb-badge bb-badge-info", get_metric(s, "exchange", "--")),
            span(class = "bb-badge bb-badge-muted", source_label)
          )
        ),
        div(class = "bb-analysis-chip-row",
          span(class = "bb-analysis-chip", get_metric(s, "sector", "--")),
          span(class = "bb-analysis-chip", get_metric(s, "industry", "--")),
          span(class = "bb-analysis-chip", get_metric(s, "asset type", "--"))
        ),
        tags$p(class = "bb-analysis-company-desc", description),
        div(class = "bb-analysis-detail-grid",
          metric_card("52W Range", get_metric(s, "52w range", "--")),
          metric_card("Beta", get_metric(s, "beta", "--")),
          metric_card("Revenue TTM", get_metric(s, "revenue \\(ttm\\)|revenue", "--")),
          metric_card("EBITDA TTM", get_metric(s, "ebitda", "--")),
          metric_card("Cash", get_metric(s, "cash", "--")),
          metric_card("Debt", get_metric(s, "debt", "--"))
        )
      )
    })

    output$valuation_tile_grid <- renderUI({
      d <- rv$data
      if (is.null(d)) return(NULL)

      s <- d$summary
      div(class = "bb-analysis-metric-grid",
        metric_card("Trailing P/E", get_metric(s, "trailing pe|pe ratio", "--"), "TTM multiple"),
        metric_card("Forward P/E", get_metric(s, "forward pe", "--"), "Next twelve months"),
        metric_card("PEG", get_metric(s, "peg", "--"), "Growth-adjusted"),
        metric_card("Dividend Yield", get_metric(s, "dividend yield", "--"), "Cash return"),
        metric_card("P/S", get_metric(s, "price to sales|ps ratio", "--"), "Revenue multiple"),
        metric_card("P/B", get_metric(s, "price to book|pb ratio", "--"), "Book multiple"),
        metric_card("EV/Revenue", get_metric(s, "ev/revenue", "--"), "Enterprise multiple"),
        metric_card("EV/EBITDA", get_metric(s, "ev/ebitda", "--"), "Operating cash proxy")
      )
    })

    # ── Chart renderers ──
    output$chart_valuation_snapshot <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_valuation_snapshot(d$summary)
    })
    output$chart_profitability_snapshot <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_profitability_snapshot(d$summary)
    })
    output$chart_earnings_history <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_earnings_history(d$history)
    })
    output$chart_eps <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_eps(d$history)
    })
    output$chart_fcf <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_fcf(d$history)
    })
    output$chart_margins <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_margins(d$history)
    })
    output$chart_returns <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_returns(d$history)
    })
    output$chart_debt <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_debt(d$history)
    })
    output$chart_liquidity <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_liquidity(d$history)
    })
    output$chart_cash_vs_debt <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_cash_vs_debt(d$history)
    })
    output$chart_cashflow_allocation <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_cashflow_allocation(d$history)
    })

    # ── Raw data table ──
    output$tbl_raw <- DT::renderDataTable({
      d <- rv$data; if (is.null(d)) return(NULL)
      s <- d$summary
      if (is.null(s) || length(s) == 0) return(NULL)
      df <- data.frame(
        Metric = names(s),
        Value  = unlist(s),
        stringsAsFactors = FALSE
      )
      DT::datatable(df,
        options = list(dom = "ft", pageLength = 50),
        rownames = FALSE, class = "cell-border compact"
      )
    })
  })
}
