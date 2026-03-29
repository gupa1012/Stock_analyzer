# ──────────────────────────────────────────────────────────────
#  mod_portfolio.R – Portfolio management Shiny module
# ──────────────────────────────────────────────────────────────

portfolioUI <- function(id) {
  ns <- NS(id)

  tagList(
    # ── Header row ──
    fluidRow(
      class = "bb-section-header",
      column(12, h3(icon("briefcase"), "PORTFOLIO",
                    class = "bb-title"))
    ),

    # ── Summary cards (filled by server) ──
    fluidRow(
      class = "bb-kpi-row",
      column(3, uiOutput(ns("kpi_total_value"))),
      column(3, uiOutput(ns("kpi_day_pnl"))),
      column(3, uiOutput(ns("kpi_total_pnl"))),
      column(3, uiOutput(ns("kpi_positions")))
    ),

    # ── Add position form ──
    fluidRow(
      class = "bb-form-row",
      column(12,
        div(class = "bb-panel",
          h4(icon("plus-circle"), "ADD POSITION", class = "bb-panel-title"),
          fluidRow(
            column(3, textInput(ns("add_ticker"), "Ticker", placeholder = "AAPL")),
            column(2, numericInput(ns("add_shares"), "Shares", value = 0, min = 0)),
            column(2, numericInput(ns("add_cost"), "Avg Cost (native)", value = 0, min = 0, step = 0.01)),
            column(3, textInput(ns("add_notes"), "Notes", placeholder = "Optional")),
            column(2, div(style = "margin-top: 25px;",
              actionButton(ns("btn_add"), "ADD",
                           class = "bb-btn-primary",
                           icon = icon("plus"))
            ))
          )
        )
      )
    ),

    # ── Portfolio table ──
    fluidRow(
      column(8,
        div(class = "bb-panel",
          div(style = "display:flex; align-items:center; justify-content:space-between;",
            h4(icon("table"), "HOLDINGS", class = "bb-panel-title",
               style = "margin-bottom:0;"),
            div(style = "display:flex; align-items:center; gap:12px;",
              shinyWidgets::materialSwitch(
                inputId = ns("xray_on"),
                label   = HTML("<span style='font-size:11px;letter-spacing:1px;color:#f5a623;'>ETF X-RAY</span>"),
                value   = FALSE,
                status  = "warning",
                inline  = TRUE
              ),
              conditionalPanel(
                condition = paste0("input['" , ns("xray_on"), "'] == true"),
                actionButton(ns("btn_refresh_xray"), "",
                             icon  = icon("sync-alt"),
                             class = "btn-xs bb-btn-secondary",
                             title = "ETF-Positionen neu abrufen")
              )
            )
          ),
          DT::dataTableOutput(ns("tbl_holdings")),
          uiOutput(ns("xray_panel"))
        )
      ),
      column(4,
        div(class = "bb-panel",
          h4(icon("chart-pie"), "ALLOCATION", class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_allocation"), height = "350px")
        ),
        div(class = "bb-panel",
          h4(icon("trash-alt"), "MANAGE", class = "bb-panel-title"),
          selectInput(ns("sel_remove"), "Select Position to Remove:", choices = NULL),
          actionButton(ns("btn_remove"), "REMOVE POSITION",
                       class = "bb-btn-danger", icon = icon("times"))
        )
      )
    )
  )
}


portfolioServer <- function(id, trigger_refresh) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      portfolio       = read_portfolio(),
      quotes          = NULL,
      xray_holdings   = list(),   # named list: ticker -> data.frame
      xray_fetched_at = NULL      # POSIXct timestamp of last fetch
    )

    # ── Refresh quotes when portfolio changes or trigger fires ──
    observe({
      trigger_refresh()
      rv$portfolio <- read_portfolio()
    })

    observe({
      pf <- rv$portfolio
      if (nrow(pf) == 0) {
        rv$quotes <- NULL
        return()
      }
      tickers <- unique(c(pf$ticker, required_fx_tickers(pf$currency)))
      rv$quotes <- tryCatch(fetch_quotes(tickers), error = function(e) NULL)
    })

    # ── Add position ──
    observeEvent(input$btn_add, {
      req(nchar(trimws(input$add_ticker)) > 0, input$add_shares > 0)
      rv$portfolio <- add_position(
        ticker   = input$add_ticker,
        shares   = input$add_shares,
        avg_cost = input$add_cost,
        notes    = input$add_notes
      )
      updateTextInput(session, "add_ticker", value = "")
      updateNumericInput(session, "add_shares", value = 0)
      updateNumericInput(session, "add_cost", value = 0)
      updateTextInput(session, "add_notes", value = "")
      showNotification("Position added", type = "message")
    })

    # ── Remove position ──
    observe({
      pf <- rv$portfolio
      if (nrow(pf) == 0) {
        choices <- c("No positions" = "")
      } else {
        choices <- setNames(pf$id,
                            paste(pf$ticker, "-", pf$shares, "shares"))
      }
      updateSelectInput(session, "sel_remove", choices = choices)
    })

    observeEvent(input$btn_remove, {
      req(nchar(input$sel_remove) > 0)
      rv$portfolio <- remove_position(input$sel_remove)
      showNotification("Position removed", type = "warning")
    })

    # ── Build display table ──
    holdings_display <- reactive({
      pf <- rv$portfolio
      if (nrow(pf) == 0) return(data.frame())
      qt <- rv$quotes

      pf$current_price <- NA_real_
      pf$market_value  <- NA_real_
      pf$pnl           <- NA_real_
      pf$pnl_pct       <- NA_real_
      pf$day_change    <- NA_real_
      pf$avg_cost_eur  <- NA_real_

      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) {
            pf$current_price[i] <- convert_amount_to_base(row_q$price[1], pf$currency[i], qt)
            pf$market_value[i]  <- convert_amount_to_base(row_q$price[1] * pf$shares[i], pf$currency[i], qt)
            pf$avg_cost_eur[i]  <- convert_amount_to_base(pf$avg_cost[i], pf$currency[i], qt)
            cost_basis <- convert_amount_to_base(pf$avg_cost[i] * pf$shares[i], pf$currency[i], qt)
            pf$pnl[i]      <- pf$market_value[i] - cost_basis
            pf$pnl_pct[i]  <- if (cost_basis > 0)
              round((pf$market_value[i] / cost_basis - 1) * 100, 2) else NA
            pf$day_change[i] <- convert_amount_to_base(row_q$change[1] * pf$shares[i], pf$currency[i], qt)
          }
        }
      }

      data.frame(
        Ticker       = pf$ticker,
        Shares       = pf$shares,
        `Avg Cost`   = ifelse(is.na(pf$avg_cost_eur), "--",
                              format_base_currency(pf$avg_cost_eur, digits = 2)),
        `Price`      = ifelse(is.na(pf$current_price), "--",
                              format_base_currency(pf$current_price, digits = 2)),
        `Mkt Value`  = ifelse(is.na(pf$market_value), "--",
                              format_base_currency(pf$market_value, digits = 0)),
        `P&L`        = ifelse(is.na(pf$pnl), "--",
                              format_signed_base_currency(pf$pnl, digits = 0)),
        `P&L %`      = ifelse(is.na(pf$pnl_pct), "--",
                              paste0(ifelse(pf$pnl_pct >= 0, "+", ""),
                                     pf$pnl_pct, "%")),
        `Day Chg`    = ifelse(is.na(pf$day_change), "--",
                              format_signed_base_currency(pf$day_change, digits = 0)),
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )
    })

    # ── Render table ──
    output$tbl_holdings <- DT::renderDataTable({
      df <- holdings_display()
      DT::datatable(df,
        options = list(
          dom = "t",
          pageLength = 50,
          ordering = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = 2:7)
          )
        ),
        rownames = FALSE,
        class = "cell-border compact"
      )
    })

    # ── KPI cards ──
    output$kpi_total_value <- renderUI({
      pf <- rv$portfolio
      qt <- rv$quotes
      total <- 0
      if (nrow(pf) > 0 && !is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) {
            total <- total + convert_amount_to_base(
              row_q$price[1] * pf$shares[i],
              pf$currency[i],
              qt
            )
          }
        }
      }
      bb_kpi("TOTAL VALUE",
             format_base_currency(total, digits = 0),
             colour = "#f5a623")
    })

    output$kpi_day_pnl <- renderUI({
      pf <- rv$portfolio
      qt <- rv$quotes
      day_pnl <- 0
      if (nrow(pf) > 0 && !is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0 && !is.na(row_q$change[1]))
            day_pnl <- day_pnl + convert_amount_to_base(
              row_q$change[1] * pf$shares[i],
              pf$currency[i],
              qt
            )
        }
      }
      clr <- if (day_pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("DAY P&L",
             format_signed_base_currency(day_pnl, digits = 0),
             colour = clr)
    })

    output$kpi_total_pnl <- renderUI({
      pf <- rv$portfolio
      qt <- rv$quotes
      total_pnl <- 0
      if (nrow(pf) > 0 && !is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0 && !is.na(row_q$price[1])) {
            mv <- convert_amount_to_base(
              row_q$price[1] * pf$shares[i],
              pf$currency[i],
              qt
            )
            cost <- convert_amount_to_base(
              pf$avg_cost[i] * pf$shares[i],
              pf$currency[i],
              qt
            )
            total_pnl <- total_pnl + (mv - cost)
          }
        }
      }
      clr <- if (total_pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("TOTAL P&L",
             format_signed_base_currency(total_pnl, digits = 0),
             colour = clr)
    })

    output$kpi_positions <- renderUI({
      bb_kpi("POSITIONS",
             as.character(nrow(rv$portfolio)),
             colour = "#f5a623")
    })

    # ── Allocation pie chart ──
    output$chart_allocation <- plotly::renderPlotly({
      pf <- rv$portfolio
      qt <- rv$quotes
      if (nrow(pf) == 0) return(bb_empty_chart("No positions yet"))

      pf$value <- 0
      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0 && !is.na(row_q$price[1])) {
            pf$value[i] <- convert_amount_to_base(
              row_q$price[1] * pf$shares[i],
              pf$currency[i],
              qt
            )
          }
        }
      }
      pf$value[is.na(pf$value)] <- 0
      if (sum(pf$value, na.rm = TRUE) == 0) {
        pf$value <- mapply(convert_amount_to_base,
                           pf$shares * pf$avg_cost,
                           pf$currency,
                           MoreArgs = list(quotes_df = qt))
      }

      agg <- aggregate(value ~ ticker, data = pf, FUN = sum)
      agg <- agg[order(-agg$value), ]

      plotly::plot_ly(agg,
        labels = ~ticker, values = ~value,
        type = "pie",
        hole = 0.55,
        textinfo = "label+percent",
        textfont = list(color = "#ecf0f1", size = 11,
                        family = "JetBrains Mono, Consolas, monospace"),
        marker = list(
          colors = c("#f5a623", "#ff6f00", "#ff8f00", "#ffab00",
                     "#ffd600", "#aeea00", "#64dd17", "#00c853",
                     "#00bfa5", "#0091ea"),
          line = list(color = "#0a0a14", width = 1)
        )
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          showlegend = FALSE,
          margin = list(l = 10, r = 10, t = 10, b = 10)
        )
    })

    # ── X-Ray: fetch holdings when toggle turns on or refresh button clicked ──
    do_fetch_xray <- function() {
      pf_tickers <- rv$portfolio$ticker
      etf_tickers <- intersect(pf_tickers, names(ETF_ISINS))
      if (length(etf_tickers) == 0) return()

      showNotification("ETF X-Ray: Lade Positionen …",
                       id = "xray_loading", duration = NULL, type = "message")
      on.exit(removeNotification("xray_loading"))

      cache <- list()
      for (tkr in etf_tickers) {
        hld  <- tryCatch(fetch_etf_holdings(tkr), error = function(e) NULL)
        if (!is.null(hld) && nrow(hld) > 0) {
          cache[[tkr]] <- hld
        } else {
          showNotification(paste0("X-Ray: Keine Daten für ", tkr),
                           type = "warning", duration = 5)
        }
      }
      rv$xray_holdings   <- cache
      rv$xray_fetched_at <- Sys.time()
    }

    # Auto-fetch the first time the toggle is switched on
    observeEvent(input$xray_on, {
      req(input$xray_on)
      if (length(rv$xray_holdings) == 0) do_fetch_xray()
    })

    # Manual refresh button
    observeEvent(input$btn_refresh_xray, {
      do_fetch_xray()
    })

    # ── X-Ray panel output ────────────────────────────────────
    output$xray_table <- DT::renderDataTable({
      req(input$xray_on)
      tbl <- build_xray_table(rv$portfolio, rv$quotes, rv$xray_holdings)
      if (is.null(tbl)) return(data.frame())
      DT::datatable(tbl,
        options = list(
          dom        = "t",
          pageLength = 50,
          ordering   = TRUE,
          order      = list(list(6, "desc")),
          columnDefs = list(
            list(className = "dt-right",
                 targets   = c(2, 3, 4)),
            list(visible = FALSE,
                 targets = c(5, 6))
          )
        ),
        rownames = FALSE,
        class    = "cell-border compact"
      )
    })

    output$xray_panel <- renderUI({
      if (!isTRUE(input$xray_on)) return(NULL)

      ts_str <- if (!is.null(rv$xray_fetched_at))
        paste0("Stand: ", format(rv$xray_fetched_at, "%d.%m.%Y %H:%M"))
      else "Noch nicht geladen"

      tbl <- build_xray_table(rv$portfolio, rv$quotes, rv$xray_holdings)
      if (is.null(tbl)) {
        return(div(
          style = "padding:10px; color:#f5a623; font-size:11px;",
          icon("info-circle"),
          " Keine ETF-Positionen oder Daten ausstehend."
        ))
      }

      tagList(
        hr(style = "border-color:#2a2a3e; margin:12px 0 8px;"),
        div(style = "display:flex; align-items:center; margin-bottom:6px;",
          tags$span(icon("search"), " ETF X-RAY LOOK-THROUGH",
            style = "font-size:11px; letter-spacing:1px; color:#f5a623; font-weight:bold;"),
          tags$span(ts_str,
            style = "font-size:10px; color:#888; margin-left:8px;")
        ),
        DT::dataTableOutput(ns("xray_table"))
      )
    })

    return(reactive(rv$portfolio))
  })
}
