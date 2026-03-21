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
            column(2, numericInput(ns("add_cost"), "Avg Cost ($)", value = 0, min = 0, step = 0.01)),
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
          h4(icon("table"), "HOLDINGS", class = "bb-panel-title"),
          DT::dataTableOutput(ns("tbl_holdings"))
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
      portfolio = read_portfolio(),
      quotes    = NULL
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
      tickers <- unique(pf$ticker)
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
      pf$day_change     <- NA_real_

      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) {
            pf$current_price[i] <- row_q$price[1]
            pf$market_value[i]  <- row_q$price[1] * pf$shares[i]
            cost_basis <- pf$avg_cost[i] * pf$shares[i]
            pf$pnl[i]      <- pf$market_value[i] - cost_basis
            pf$pnl_pct[i]  <- if (cost_basis > 0)
              round((pf$market_value[i] / cost_basis - 1) * 100, 2) else NA
            pf$day_change[i] <- row_q$change[1] * pf$shares[i]
          }
        }
      }

      data.frame(
        Ticker       = pf$ticker,
        Shares       = pf$shares,
        `Avg Cost`   = sprintf("$%.2f", pf$avg_cost),
        `Price`      = ifelse(is.na(pf$current_price), "--",
                              sprintf("$%.2f", pf$current_price)),
        `Mkt Value`  = ifelse(is.na(pf$market_value), "--",
                              sprintf("$%s", formatC(pf$market_value,
                                      format = "f", digits = 0,
                                      big.mark = ","))),
        `P&L`        = ifelse(is.na(pf$pnl), "--",
                              sprintf("%s$%s",
                                      ifelse(pf$pnl >= 0, "+", "-"),
                                      formatC(abs(pf$pnl), format = "f",
                                              digits = 0, big.mark = ","))),
        `P&L %`      = ifelse(is.na(pf$pnl_pct), "--",
                              paste0(ifelse(pf$pnl_pct >= 0, "+", ""),
                                     pf$pnl_pct, "%")),
        `Day Chg`    = ifelse(is.na(pf$day_change), "--",
                              sprintf("%+.0f", pf$day_change)),
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
          if (nrow(row_q) > 0) total <- total + row_q$price[1] * pf$shares[i]
        }
      }
      bb_kpi("TOTAL VALUE",
             paste0("$", formatC(total, format = "f", digits = 0,
                                 big.mark = ",")),
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
            day_pnl <- day_pnl + row_q$change[1] * pf$shares[i]
        }
      }
      clr <- if (day_pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("DAY P&L",
             sprintf("%+.0f", day_pnl),
             colour = clr)
    })

    output$kpi_total_pnl <- renderUI({
      pf <- rv$portfolio
      qt <- rv$quotes
      total_pnl <- 0
      if (nrow(pf) > 0 && !is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) {
            mv   <- row_q$price[1] * pf$shares[i]
            cost <- pf$avg_cost[i] * pf$shares[i]
            total_pnl <- total_pnl + (mv - cost)
          }
        }
      }
      clr <- if (total_pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("TOTAL P&L",
             sprintf("%+.0f", total_pnl),
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
          if (nrow(row_q) > 0) pf$value[i] <- row_q$price[1] * pf$shares[i]
        }
      }
      if (sum(pf$value) == 0) {
        pf$value <- pf$shares * pf$avg_cost
      }

      agg <- aggregate(value ~ ticker, data = pf, FUN = sum)
      agg <- agg[order(-agg$value), ]

      plotly::plot_ly(agg,
        labels = ~ticker, values = ~value,
        type = "pie",
        hole = 0.55,
        textinfo = "label+percent",
        textfont = list(color = "#ecf0f1", size = 11,
                        family = "Consolas, monospace"),
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

    return(reactive(rv$portfolio))
  })
}
