# ──────────────────────────────────────────────────────────────
#  mod_watchlist.R – Watchlist management Shiny module
# ──────────────────────────────────────────────────────────────

watchlistUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      class = "bb-section-header",
      column(12, h3(icon("eye"), "WATCHLIST",
                    class = "bb-title"))
    ),

    # ── Add to watchlist ──
    fluidRow(
      class = "bb-form-row",
      column(12,
        div(class = "bb-panel",
          h4(icon("plus-circle"), "ADD TO WATCHLIST", class = "bb-panel-title"),
          fluidRow(
            column(3, textInput(ns("add_ticker"), "Ticker",
                                placeholder = "MSFT")),
            column(3, numericInput(ns("add_target"), "Target Price ($)",
                                   value = NA, min = 0, step = 0.01)),
            column(4, textInput(ns("add_notes"), "Notes",
                                placeholder = "Optional")),
            column(2, div(style = "margin-top: 25px;",
              actionButton(ns("btn_add"), "ADD",
                           class = "bb-btn-primary",
                           icon = icon("plus"))
            ))
          )
        )
      )
    ),

    # ── Watchlist table + mini chart ──
    fluidRow(
      column(8,
        div(class = "bb-panel",
          h4(icon("list"), "WATCHED STOCKS", class = "bb-panel-title"),
          DT::dataTableOutput(ns("tbl_watchlist"))
        )
      ),
      column(4,
        div(class = "bb-panel",
          h4(icon("chart-line"), "PRICE CHART", class = "bb-panel-title"),
          selectInput(ns("sel_chart_ticker"), "Ticker", choices = NULL),
          plotly::plotlyOutput(ns("chart_mini"), height = "250px")
        ),
        div(class = "bb-panel",
          h4(icon("trash-alt"), "MANAGE", class = "bb-panel-title"),
          selectInput(ns("sel_remove"), "Remove Ticker:", choices = NULL),
          actionButton(ns("btn_remove"), "REMOVE",
                       class = "bb-btn-danger", icon = icon("times"))
        )
      )
    )
  )
}


watchlistServer <- function(id, trigger_refresh) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      watchlist = read_watchlist(),
      quotes    = NULL
    )

    observe({
      trigger_refresh()
      rv$watchlist <- read_watchlist()
    })

    observe({
      wl <- rv$watchlist
      if (nrow(wl) == 0) {
        rv$quotes <- NULL
        return()
      }
      rv$quotes <- tryCatch(fetch_quotes(wl$ticker), error = function(e) NULL)
    })

    # ── Add ──
    observeEvent(input$btn_add, {
      req(nchar(trimws(input$add_ticker)) > 0)
      rv$watchlist <- add_to_watchlist(
        ticker       = input$add_ticker,
        target_price = input$add_target,
        notes        = input$add_notes
      )
      updateTextInput(session, "add_ticker", value = "")
      updateNumericInput(session, "add_target", value = NA)
      updateTextInput(session, "add_notes", value = "")
      showNotification("Added to watchlist", type = "message")
    })

    # ── Remove ──
    observe({
      wl <- rv$watchlist
      choices <- if (nrow(wl) == 0) c("Empty" = "") else
        setNames(wl$ticker, wl$ticker)
      updateSelectInput(session, "sel_remove", choices = choices)
      updateSelectInput(session, "sel_chart_ticker", choices = choices)
    })

    observeEvent(input$btn_remove, {
      req(nchar(input$sel_remove) > 0)
      rv$watchlist <- remove_from_watchlist(input$sel_remove)
      showNotification("Removed from watchlist", type = "warning")
    })

    # ── Display table ──
    output$tbl_watchlist <- DT::renderDataTable({
      wl <- rv$watchlist
      qt <- rv$quotes
      if (nrow(wl) == 0) return(DT::datatable(data.frame(), rownames = FALSE))

      wl$price      <- NA_real_
      wl$change     <- NA_real_
      wl$change_pct <- NA_real_

      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(wl))) {
          row_q <- qt[qt$ticker == wl$ticker[i], ]
          if (nrow(row_q) > 0) {
            wl$price[i]      <- row_q$price[1]
            wl$change[i]     <- row_q$change[1]
            wl$change_pct[i] <- row_q$pct[1]
          }
        }
      }

      display <- data.frame(
        Ticker   = wl$ticker,
        Price    = ifelse(is.na(wl$price), "--",
                          sprintf("$%.2f", wl$price)),
        Change   = ifelse(is.na(wl$change), "--",
                          sprintf("%+.2f", wl$change)),
        `Chg %`  = ifelse(is.na(wl$change_pct), "--",
                          sprintf("%+.2f%%", wl$change_pct)),
        Target   = ifelse(is.na(wl$target_price), "--",
                          sprintf("$%.2f", wl$target_price)),
        `vs Target` = ifelse(is.na(wl$price) | is.na(wl$target_price), "--",
                             sprintf("%+.1f%%",
                                     (wl$price / wl$target_price - 1) * 100)),
        Added    = wl$date_added,
        Notes    = ifelse(is.na(wl$notes), "", wl$notes),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT::datatable(display,
        options = list(
          dom = "t",
          pageLength = 50,
          ordering = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = 1:5)
          )
        ),
        rownames = FALSE,
        class = "cell-border compact"
      )
    })

    # ── Mini price chart ──
    output$chart_mini <- plotly::renderPlotly({
      tkr <- input$sel_chart_ticker
      if (is.null(tkr) || nchar(tkr) == 0)
        return(bb_empty_chart("Select a ticker"))

      hist <- fetch_history(tkr)
      if (nrow(hist) == 0) return(bb_empty_chart("No data"))

      plotly::plot_ly(hist, x = ~date, y = ~close,
        type = "scatter", mode = "lines",
        line = list(color = "#f5a623", width = 1.5),
        fill = "tozeroy",
        fillcolor = "rgba(245,166,35,0.1)"
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          xaxis = list(
            color = "#6c757d",
            gridcolor = "rgba(255,255,255,0.05)",
            zeroline = FALSE
          ),
          yaxis = list(
            color = "#6c757d",
            gridcolor = "rgba(255,255,255,0.05)",
            zeroline = FALSE,
            title = ""
          ),
          margin = list(l = 40, r = 10, t = 10, b = 30),
          showlegend = FALSE
        )
    })

    return(reactive(rv$watchlist))
  })
}
