# app.R – Stock Fundamental Analyzer
# Shiny dashboard that crawls GuruFocus.com with RSelenium and renders
# interactive charts for any S&P 500 (or custom) ticker.

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(htmltools)
library(DT)
library(plotly)
library(dplyr)
if (requireNamespace("shinyWidgets", quietly = TRUE)) library(shinyWidgets)

# Source helpers
source("R/utils.R")
source("R/scraper.R")
source("R/charts.R")
source("R/demo_data.R")

# ── Load S&P 500 ticker list ───────────────────────────────────────────────────
sp500 <- tryCatch(
  read.csv("data/sp500_tickers.csv", stringsAsFactors = FALSE),
  error = function(e) data.frame(ticker = "AAPL", company = "Apple Inc.", sector = "Technology")
)
sp500 <- sp500[!duplicated(sp500$ticker), ]
ticker_choices <- setNames(
  sp500$ticker,
  paste0(sp500$ticker, " – ", sp500$company)
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/fluency/20/stock-share.png",
               style = "margin-right:8px;"),
      "Stock Analyzer"
    ),
    titleWidth = 240
  ),

  # Sidebar
  dashboardSidebar(
    width = 240,
    tags$div(style = "padding: 12px 10px 0;",
      if (requireNamespace("shinyWidgets", quietly = TRUE)) {
        shinyWidgets::pickerInput(
          inputId  = "ticker",
          label    = "Select Stock Ticker",
          choices  = ticker_choices,
          selected = "AAPL",
          options  = shinyWidgets::pickerOptions(
            liveSearch      = TRUE,
            liveSearchStyle = "contains",
            size            = 12,
            title           = "Search tickers…"
          )
        )
      } else {
        selectInput(
          inputId  = "ticker",
          label    = "Select Stock Ticker",
          choices  = ticker_choices,
          selected = "AAPL",
          selectize = TRUE,
          width    = "100%"
        )
      },
      # Custom ticker entry
      textInput("custom_ticker", "Or type any ticker:", placeholder = "e.g. TSLA"),

      actionButton(
        "analyze", "  Analyze",
        icon  = icon("chart-line"),
        class = "btn-primary btn-lg",
        width = "100%"
      ),
      tags$div(
        style = "margin-top:6px;",
        actionButton(
          "demo", "  Demo Data",
          icon  = icon("flask"),
          class = "btn-default btn-sm",
          width = "100%",
          title = "Load sample data to explore the charts without live scraping"
        )
      ),
      tags$hr(style = "border-color:rgba(255,255,255,0.1);"),
      uiOutput("status_ui"),
      tags$hr(style = "border-color:rgba(255,255,255,0.1);"),
      tags$p(
        HTML("Data sourced from <a href='https://www.gurufocus.com' target='_blank' style='color:#3498db;'>GuruFocus.com</a>"),
        style = "font-size:11px;color:#7f8c8d;padding:5px 15px;"
      )
    )
  ),

  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),

    # Welcome screen (shown when no data loaded)
    conditionalPanel(
      condition = "output.has_data === false",
      fluidRow(
        column(12,
          div(class = "welcome-box",
            HTML('<i class="fa fa-chart-bar" style="font-size:64px;color:#2c3e50;display:block;margin-bottom:20px;"></i>'),
            h2("Stock Fundamental Analyzer"),
            p("Select a ticker from the sidebar and click ", tags$b("Analyze"),
              " to scrape live data from GuruFocus."),
            p(style = "font-size:13px;",
              "Charts include Valuation ratios (P/E, P/B, EV/EBITDA), ",
              "Financials (Revenue, EPS, FCF), and Quality metrics (ROE, ROA, margins).")
          )
        )
      )
    ),

    # Main dashboard (shown when data is available)
    conditionalPanel(
      condition = "output.has_data === true",
      uiOutput("stock_header_ui"),
      tabBox(
        width = 12,
        id = "main_tabs",

        # ── Tab 1: Overview ────────────────────────────────────────────────────
        tabPanel(
          "Overview",
          icon = icon("tachometer-alt"),
          uiOutput("kpi_cards"),
          fluidRow(
            column(6,
              withSpinner(
                plotlyOutput("radar_chart", height = "380px"),
                color = "#3498db"
              )
            ),
            column(6,
              withSpinner(
                plotlyOutput("revenue_overview_chart", height = "380px"),
                color = "#3498db"
              )
            )
          )
        ),

        # ── Tab 2: Valuation ───────────────────────────────────────────────────
        tabPanel(
          "Valuation",
          icon = icon("dollar-sign"),
          fluidRow(
            column(6,
              withSpinner(plotlyOutput("pe_chart",       height = "320px"), color = "#3498db")
            ),
            column(6,
              withSpinner(plotlyOutput("pb_chart",       height = "320px"), color = "#3498db")
            )
          ),
          fluidRow(
            column(6,
              withSpinner(plotlyOutput("ps_chart",       height = "320px"), color = "#3498db")
            ),
            column(6,
              withSpinner(plotlyOutput("ev_ebitda_chart",height = "320px"), color = "#3498db")
            )
          )
        ),

        # ── Tab 3: Financials ──────────────────────────────────────────────────
        tabPanel(
          "Financials",
          icon = icon("money-bill-wave"),
          fluidRow(
            column(12,
              withSpinner(plotlyOutput("revenue_chart", height = "320px"), color = "#3498db")
            )
          ),
          fluidRow(
            column(6,
              withSpinner(plotlyOutput("eps_chart", height = "320px"), color = "#3498db")
            ),
            column(6,
              withSpinner(plotlyOutput("fcf_chart", height = "320px"), color = "#3498db")
            )
          )
        ),

        # ── Tab 4: Quality ─────────────────────────────────────────────────────
        tabPanel(
          "Quality",
          icon = icon("star"),
          fluidRow(
            column(6,
              withSpinner(plotlyOutput("margin_chart",    height = "320px"), color = "#3498db")
            ),
            column(6,
              withSpinner(plotlyOutput("returns_chart",   height = "320px"), color = "#3498db")
            )
          ),
          fluidRow(
            column(6,
              withSpinner(plotlyOutput("debt_chart",      height = "320px"), color = "#3498db")
            ),
            column(6,
              withSpinner(plotlyOutput("liquidity_chart", height = "320px"), color = "#3498db")
            )
          )
        ),

        # ── Tab 5: Raw Data ────────────────────────────────────────────────────
        tabPanel(
          "Raw Data",
          icon = icon("table"),
          tags$p(
            style = "color:#95a5a6;font-size:12px;padding:10px 0 0;",
            "Summary metrics scraped from GuruFocus.com"
          ),
          DTOutput("raw_data_table")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    data     = NULL,
    selenium = NULL,
    loading  = FALSE,
    error    = NULL
  )

  # ── Selenium lifecycle ───────────────────────────────────────────────────────
  # Initialise a shared Selenium session once when the Shiny session starts.
  # We wrap it in a try so that the app still loads even when Chrome is absent.
  observe({
    isolate({
      if (!is.null(rv$selenium)) return()
      tryCatch({
        rv$selenium <- start_selenium_driver()
      }, error = function(e) {
        rv$error <- paste("Selenium could not start:", conditionMessage(e),
                          "\nCheck that Chrome and chromedriver are installed.")
      })
    })
  })

  session$onSessionEnded(function() {
    tryCatch(stop_selenium_driver(rv$selenium), error = function(e) NULL)
  })

  # ── Reactive: active ticker ──────────────────────────────────────────────────
  active_ticker <- reactive({
    ct <- trimws(toupper(input$custom_ticker))
    if (nchar(ct) >= 1 && nchar(ct) <= 6) ct else input$ticker
  })

  # ── Analyze button ───────────────────────────────────────────────────────────
  observeEvent(input$analyze, {
    ticker <- active_ticker()
    if (is.null(ticker) || nchar(ticker) == 0) {
      showNotification("Please select or enter a ticker.", type = "warning")
      return()
    }

    if (is.null(rv$selenium)) {
      showNotification(
        paste("Selenium is not available.", rv$error),
        type     = "error",
        duration = 10
      )
      return()
    }

    rv$loading <- TRUE
    rv$error   <- NULL

    notif_id <- showNotification(
      paste("Fetching data for", ticker, "…"),
      duration = NULL,
      type     = "message"
    )

    # Run scraping in the same R process (RSelenium is not thread-safe)
    withProgress(message = paste("Analysing", ticker), value = 0, {
      tryCatch({
        setProgress(0.1, detail = "Connecting to GuruFocus…")
        result <- scrape_gurufocus(rv$selenium$driver, ticker)
        setProgress(0.9, detail = "Processing data…")
        rv$data    <- result
        rv$loading <- FALSE
        removeNotification(notif_id)
        showNotification(
          paste("✓ Data loaded for", ticker),
          duration = 4,
          type     = "message"
        )
      }, error = function(e) {
        rv$loading <- FALSE
        rv$error   <- conditionMessage(e)
        removeNotification(notif_id)
        showNotification(
          paste("Error:", conditionMessage(e)),
          duration = 8,
          type     = "error"
        )
      })
    })
  })

  # ── Demo button ──────────────────────────────────────────────────────────────
  observeEvent(input$demo, {
    ticker <- active_ticker()
    if (is.null(ticker) || nchar(ticker) == 0) ticker <- "AAPL"
    rv$data    <- generate_demo_data(ticker)
    rv$loading <- FALSE
    rv$error   <- NULL
    showNotification(
      paste0("Demo data loaded for ", ticker,
             " — charts show illustrative (not live) figures."),
      duration = 6,
      type     = "message"
    )
  })

  # ── output$has_data (used by conditionalPanel) ───────────────────────────────
  output$has_data <- reactive({
    !is.null(rv$data) && length(rv$data$history) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  # ── Status sidebar ───────────────────────────────────────────────────────────
  output$status_ui <- renderUI({
    if (!is.null(rv$error) && nchar(rv$error) > 0) {
      div(style = "padding:8px 15px;",
          tags$small(style = "color:#e74c3c;", icon("exclamation-triangle"), rv$error))
    } else if (rv$loading) {
      div(style = "padding:8px 15px;",
          tags$small(style = "color:#f39c12;", icon("spinner", class = "fa-spin"), " Loading…"))
    } else if (!is.null(rv$data)) {
      is_demo <- isTRUE(rv$data$demo_mode)
      badge_style <- if (is_demo) "color:#f39c12;" else "color:#27ae60;"
      badge_icon  <- if (is_demo) "flask" else "check-circle"
      badge_label <- if (is_demo) {
        paste(" DEMO –", rv$data$ticker)
      } else {
        paste(" Live –", rv$data$ticker, "at", format(rv$data$timestamp, "%H:%M:%S"))
      }
      div(style = "padding:8px 15px;",
          tags$small(style = badge_style, icon(badge_icon), badge_label))
    }
  })

  # ── Stock header ─────────────────────────────────────────────────────────────
  output$stock_header_ui <- renderUI({
    req(rv$data)
    d  <- rv$data
    tk <- d$ticker
    sm <- d$summary

    company_row <- sp500[sp500$ticker == tk, , drop = FALSE]
    company_name <- if (nrow(company_row) > 0) company_row$company[1] else tk
    sector       <- if (nrow(company_row) > 0) company_row$sector[1]  else ""

    mktcap  <- get_metric(sm, "Market Cap",     "N/A")
    price   <- get_metric(sm, "Current Price|Price", "N/A")

    demo_badge <- if (isTRUE(d$demo_mode)) {
      tags$span(
        style = paste(
          "background:#f39c12;color:#000;font-size:10px;font-weight:700;",
          "padding:2px 7px;border-radius:4px;margin-left:8px;",
          "vertical-align:middle;letter-spacing:0.5px;"
        ),
        "DEMO"
      )
    } else NULL

    div(class = "stock-header",
      fluidRow(
        column(8,
          tags$h2(paste(tk, "–", company_name), demo_badge),
          tags$div(class = "stock-meta",
                   icon("industry"), " ", sector, " | ",
                   icon("clock"),    " Updated: ",
                   format(d$timestamp, "%Y-%m-%d %H:%M"))
        ),
        column(4, style = "text-align:right;",
          tags$div(class = "stock-price", price),
          tags$div(style = "color:#95a5a6;font-size:13px;",
                   "Market Cap: ", mktcap)
        )
      )
    )
  })

  # ── KPI cards ────────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    req(rv$data)
    sm  <- rv$data$summary
    his <- rv$data$history

    get_last <- function(nm) {
      df <- his[[nm]]
      if (is.null(df) || nrow(df) == 0) return("N/A")
      v <- tail(df$value[!is.na(df$value)], 1)
      if (length(v) == 0) "N/A" else round(v, 2)
    }

    cards <- list(
      list(title = "P/E Ratio",     value = get_last("pe"),           colour = APP_COLOURS$primary),
      list(title = "P/B Ratio",     value = get_last("pb"),           colour = APP_COLOURS$warning),
      list(title = "P/S Ratio",     value = get_last("ps"),           colour = APP_COLOURS$info),
      list(title = "EV/EBITDA",     value = get_last("ev_ebitda"),    colour = APP_COLOURS$secondary),
      list(title = "ROE (%)",       value = get_last("roe"),          colour = APP_COLOURS$success),
      list(title = "Net Margin (%)",value = get_last("net_margin"),   colour = APP_COLOURS$danger),
      list(title = "D/E Ratio",     value = get_last("debt_equity"),  colour = "#c0392b"),
      list(title = "Current Ratio", value = get_last("current_ratio"),colour = "#16a085")
    )

    n <- length(cards)
    cols_per_row <- 4
    rows <- split(cards, ceiling(seq_along(cards) / cols_per_row))

    row_tags <- lapply(rows, function(row_cards) {
      col_tags <- lapply(row_cards, function(card) {
        column(3,
          kpi_card(card$title, as.character(card$value), colour = card$colour)
        )
      })
      do.call(fluidRow, col_tags)
    })

    do.call(tagList, row_tags)
  })

  # ── Charts ────────────────────────────────────────────────────────────────────
  hist_data <- reactive({
    req(rv$data)
    rv$data$history
  })

  output$radar_chart          <- renderPlotly({ chart_radar(hist_data(), rv$data$ticker) })
  output$revenue_overview_chart <- renderPlotly({ chart_revenue(hist_data()) })

  output$pe_chart             <- renderPlotly({ chart_pe(hist_data()) })
  output$pb_chart             <- renderPlotly({ chart_pb(hist_data()) })
  output$ps_chart             <- renderPlotly({ chart_ps(hist_data()) })
  output$ev_ebitda_chart      <- renderPlotly({ chart_ev_ebitda(hist_data()) })

  output$revenue_chart        <- renderPlotly({ chart_revenue(hist_data()) })
  output$eps_chart            <- renderPlotly({ chart_eps(hist_data()) })
  output$fcf_chart            <- renderPlotly({ chart_fcf(hist_data()) })

  output$margin_chart         <- renderPlotly({ chart_margins(hist_data()) })
  output$returns_chart        <- renderPlotly({ chart_returns(hist_data()) })
  output$debt_chart           <- renderPlotly({ chart_debt(hist_data()) })
  output$liquidity_chart      <- renderPlotly({ chart_liquidity(hist_data()) })

  # ── Raw data table ────────────────────────────────────────────────────────────
  output$raw_data_table <- renderDT({
    req(rv$data)
    sm <- rv$data$summary
    if (length(sm) == 0) {
      df <- data.frame(Message = "No summary data was scraped.", stringsAsFactors = FALSE)
    } else {
      df <- data.frame(
        Metric = names(sm),
        Value  = unlist(sm),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
    datatable(df,
      rownames = FALSE,
      options  = list(
        pageLength = 25,
        dom        = "ftp",
        scrollX    = TRUE
      ),
      class = "display nowrap"
    )
  })
}

# ── Run ─────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
