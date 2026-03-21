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

  tagList(
    fluidRow(
      class = "bb-section-header",
      column(12, h3(icon("search-dollar"), "FUNDAMENTAL ANALYSIS",
                    class = "bb-title"))
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
      tabsetPanel(
        id = ns("analysis_tabs"),
        type = "pills",

        tabPanel("Overview",
          fluidRow(
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_radar"), height = "350px")
            )),
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_revenue_overview"),
                                  height = "350px")
            ))
          )
        ),

        tabPanel("Valuation",
          fluidRow(
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_pe"), height = "300px")
            )),
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_pb"), height = "300px")
            ))
          ),
          fluidRow(
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_ps"), height = "300px")
            )),
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_ev_ebitda"), height = "300px")
            ))
          )
        ),

        tabPanel("Financials",
          fluidRow(
            column(4, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_revenue"), height = "300px")
            )),
            column(4, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_eps"), height = "300px")
            )),
            column(4, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_fcf"), height = "300px")
            ))
          )
        ),

        tabPanel("Quality",
          fluidRow(
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_margins"), height = "300px")
            )),
            column(6, div(class = "bb-panel",
              plotly::plotlyOutput(ns("chart_returns"), height = "300px")
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

        tabPanel("Raw Data",
          fluidRow(
            column(12, div(class = "bb-panel",
              DT::dataTableOutput(ns("tbl_raw"))
            ))
          )
        )
      )
    )
  )
}


analysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      data     = NULL,
      selenium = NULL,
      loading  = FALSE,
      error    = NULL
    )

    # ── Selenium lifecycle ──
    start_selenium <- function() {
      if (is.null(rv$selenium)) {
        tryCatch({
          rv$selenium <- start_selenium_driver()
        }, error = function(e) {
          rv$error <- paste("Selenium error:", e$message)
        })
      }
    }

    onStop(function() {
      if (!is.null(rv$selenium)) {
        tryCatch(stop_selenium_driver(rv$selenium), error = function(e) NULL)
      }
    })

    # ── Active ticker ──
    active_ticker <- reactive({
      custom <- trimws(input$custom_ticker %||% "")
      if (nchar(custom) > 0) toupper(custom) else input$sel_ticker
    })

    # ── Analyze button ──
    observeEvent(input$btn_analyze, {
      ticker <- active_ticker()
      req(nchar(ticker) > 0)
      rv$loading <- TRUE
      rv$error   <- NULL

      withProgress(message = paste("Analyzing", ticker, "..."), {
        start_selenium()
        if (!is.null(rv$selenium)) {
          tryCatch({
            rv$data <- scrape_gurufocus(rv$selenium$driver, ticker)
          }, error = function(e) {
            rv$error <- e$message
            rv$data  <- NULL
          })
        } else {
          rv$error <- "Could not start Selenium"
        }
      })
      rv$loading <- FALSE
    })

    # ── Demo button ──
    observeEvent(input$btn_demo, {
      ticker <- active_ticker()
      rv$data    <- generate_demo_data(ticker)
      rv$error   <- NULL
      rv$loading <- FALSE
    })

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

    # ── KPI row ──
    output$kpi_row <- renderUI({
      d <- rv$data
      if (is.null(d)) return(NULL)

      s <- d$summary
      fluidRow(
        column(2, bb_kpi("P/E Ratio",
                         get_metric(s, "p/e|pe ratio", "--"),
                         colour = "#f5a623")),
        column(2, bb_kpi("P/B Ratio",
                         get_metric(s, "p/b|pb ratio", "--"),
                         colour = "#ff6f00")),
        column(2, bb_kpi("ROE",
                         get_metric(s, "roe|return on equity", "--"),
                         colour = "#00c853")),
        column(2, bb_kpi("Debt/Eq",
                         get_metric(s, "debt.?to.?equity|d/e", "--"),
                         colour = "#ff1744")),
        column(2, bb_kpi("Revenue",
                         get_metric(s, "revenue", "--"),
                         colour = "#0091ea")),
        column(2, bb_kpi("Net Margin",
                         get_metric(s, "net.?margin|net margin", "--"),
                         colour = "#aa00ff"))
      )
    })

    # ── Chart renderers ──
    output$chart_radar <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_radar(d$summary, d$history)
    })
    output$chart_revenue_overview <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_revenue(d$history)
    })
    output$chart_pe <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_pe(d$history)
    })
    output$chart_pb <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_pb(d$history)
    })
    output$chart_ps <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_ps(d$history)
    })
    output$chart_ev_ebitda <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_ev_ebitda(d$history)
    })
    output$chart_revenue <- plotly::renderPlotly({
      d <- rv$data; if (is.null(d)) return(NULL)
      chart_revenue(d$history)
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
