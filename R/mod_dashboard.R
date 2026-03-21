# ──────────────────────────────────────────────────────────────
#  mod_dashboard.R – Dashboard / overview Shiny module
# ──────────────────────────────────────────────────────────────

dashboardTabUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      class = "bb-section-header",
      column(8, h3(icon("tachometer-alt"), "DASHBOARD",
                    class = "bb-title")),
      column(4, div(style = "text-align:right; padding-top:15px;",
        actionButton(ns("btn_refresh"), "REFRESH",
                     class = "bb-btn-primary", icon = icon("sync")),
        span(style = "color:#6c757d; margin-left:10px; font-size:11px;",
             textOutput(ns("last_update"), inline = TRUE))
      ))
    ),

    # ── Portfolio KPIs ──
    fluidRow(
      class = "bb-kpi-row",
      column(2, uiOutput(ns("kpi_portfolio_value"))),
      column(2, uiOutput(ns("kpi_day_change"))),
      column(2, uiOutput(ns("kpi_total_return"))),
      column(2, uiOutput(ns("kpi_positions_count"))),
      column(2, uiOutput(ns("kpi_watchlist_count"))),
      column(2, uiOutput(ns("kpi_best_performer")))
    ),

    # ── Main dashboard content ──
    fluidRow(
      # Portfolio mini-table
      column(6,
        div(class = "bb-panel",
          h4(icon("briefcase"), "PORTFOLIO SNAPSHOT", class = "bb-panel-title"),
          DT::dataTableOutput(ns("tbl_portfolio_snap"))
        )
      ),
      # Watchlist mini-table
      column(6,
        div(class = "bb-panel",
          h4(icon("eye"), "WATCHLIST SNAPSHOT", class = "bb-panel-title"),
          DT::dataTableOutput(ns("tbl_watchlist_snap"))
        )
      )
    ),

    fluidRow(
      column(6,
        div(class = "bb-panel",
          h4(icon("chart-pie"), "ALLOCATION", class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_alloc"), height = "300px")
        )
      ),
      column(6,
        div(class = "bb-panel",
          h4(icon("chart-bar"), "PORTFOLIO P&L BY POSITION",
             class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_pnl_bar"), height = "300px")
        )
      )
    )
  )
}


dashboardTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(refresh = 0)

    observeEvent(input$btn_refresh, {
      rv$refresh <- rv$refresh + 1
    })

    # ── Gather data ──
    all_data <- reactive({
      rv$refresh
      pf <- read_portfolio()
      wl <- read_watchlist()

      pf_quotes <- NULL
      wl_quotes <- NULL

      if (nrow(pf) > 0) {
        pf_quotes <- tryCatch(fetch_quotes(unique(pf$ticker)),
                              error = function(e) NULL)
      }
      if (nrow(wl) > 0) {
        wl_quotes <- tryCatch(fetch_quotes(wl$ticker),
                              error = function(e) NULL)
      }

      list(pf = pf, wl = wl, pf_q = pf_quotes, wl_q = wl_quotes,
           time = Sys.time())
    })

    output$last_update <- renderText({
      d <- all_data()
      format(d$time, "%H:%M:%S")
    })

    # ── KPI cards ──
    output$kpi_portfolio_value <- renderUI({
      d <- all_data()
      total <- compute_portfolio_value(d$pf, d$pf_q)
      bb_kpi("PORTFOLIO",
             paste0("$", formatC(total, format = "f", digits = 0,
                                 big.mark = ",")),
             colour = "#f5a623")
    })

    output$kpi_day_change <- renderUI({
      d <- all_data()
      day_chg <- compute_day_pnl(d$pf, d$pf_q)
      clr <- if (day_chg >= 0) "#00c853" else "#ff1744"
      bb_kpi("DAY P&L",
             sprintf("%+.0f", day_chg),
             colour = clr)
    })

    output$kpi_total_return <- renderUI({
      d <- all_data()
      pnl <- compute_total_pnl(d$pf, d$pf_q)
      clr <- if (pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("TOTAL P&L",
             sprintf("%+.0f", pnl),
             colour = clr)
    })

    output$kpi_positions_count <- renderUI({
      d <- all_data()
      bb_kpi("POSITIONS", as.character(nrow(d$pf)), colour = "#f5a623")
    })

    output$kpi_watchlist_count <- renderUI({
      d <- all_data()
      bb_kpi("WATCHING", as.character(nrow(d$wl)), colour = "#f5a623")
    })

    output$kpi_best_performer <- renderUI({
      d <- all_data()
      best <- find_best_performer(d$pf, d$pf_q)
      bb_kpi("TOP MOVER", best$label, colour = best$colour)
    })

    # ── Portfolio snapshot table ──
    output$tbl_portfolio_snap <- DT::renderDataTable({
      d <- all_data()
      df <- build_portfolio_snapshot(d$pf, d$pf_q)
      DT::datatable(df,
        options = list(dom = "t", pageLength = 10, ordering = TRUE,
                       columnDefs = list(
                         list(className = "dt-right", targets = 1:4)
                       )),
        rownames = FALSE, class = "cell-border compact"
      )
    })

    # ── Watchlist snapshot table ──
    output$tbl_watchlist_snap <- DT::renderDataTable({
      d <- all_data()
      df <- build_watchlist_snapshot(d$wl, d$wl_q)
      DT::datatable(df,
        options = list(dom = "t", pageLength = 10, ordering = TRUE,
                       columnDefs = list(
                         list(className = "dt-right", targets = 1:3)
                       )),
        rownames = FALSE, class = "cell-border compact"
      )
    })

    # ── Allocation donut ──
    output$chart_alloc <- plotly::renderPlotly({
      d <- all_data()
      pf <- d$pf; qt <- d$pf_q
      if (nrow(pf) == 0) return(bb_empty_chart("No positions"))

      pf$value <- pf$shares * pf$avg_cost
      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) pf$value[i] <- row_q$price[1] * pf$shares[i]
        }
      }
      agg <- aggregate(value ~ ticker, data = pf, FUN = sum)
      agg <- agg[order(-agg$value), ]

      plotly::plot_ly(agg, labels = ~ticker, values = ~value,
        type = "pie", hole = 0.6,
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
          showlegend = TRUE,
          legend = list(font = list(color = "#adb5bd", size = 10)),
          margin = list(l = 10, r = 10, t = 10, b = 10)
        )
    })

    # ── P&L bar chart ──
    output$chart_pnl_bar <- plotly::renderPlotly({
      d <- all_data()
      pf <- d$pf; qt <- d$pf_q
      if (nrow(pf) == 0) return(bb_empty_chart("No positions"))

      pf$pnl <- 0
      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(pf))) {
          row_q <- qt[qt$ticker == pf$ticker[i], ]
          if (nrow(row_q) > 0) {
            mv   <- row_q$price[1] * pf$shares[i]
            cost <- pf$avg_cost[i] * pf$shares[i]
            pf$pnl[i] <- mv - cost
          }
        }
      }

      agg <- aggregate(pnl ~ ticker, data = pf, FUN = sum)
      agg <- agg[order(agg$pnl), ]
      agg$colour <- ifelse(agg$pnl >= 0, "#00c853", "#ff1744")

      plotly::plot_ly(agg, x = ~ticker, y = ~pnl,
        type = "bar",
        marker = list(color = agg$colour)
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          xaxis = list(color = "#6c757d",
                       gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(color = "#6c757d",
                       gridcolor = "rgba(255,255,255,0.05)",
                       title = "P&L ($)",
                       zeroline = TRUE,
                       zerolinecolor = "rgba(255,255,255,0.2)"),
          margin = list(l = 50, r = 10, t = 10, b = 40)
        )
    })

    return(reactive(rv$refresh))
  })
}


# ── Helper functions for dashboard ───────────────────────────

compute_portfolio_value <- function(pf, qt) {
  total <- 0
  if (nrow(pf) == 0) return(total)
  if (!is.null(qt) && nrow(qt) > 0) {
    for (i in seq_len(nrow(pf))) {
      row_q <- qt[qt$ticker == pf$ticker[i], ]
      if (nrow(row_q) > 0) total <- total + row_q$price[1] * pf$shares[i]
      else total <- total + pf$avg_cost[i] * pf$shares[i]
    }
  } else {
    total <- sum(pf$avg_cost * pf$shares)
  }
  total
}

compute_day_pnl <- function(pf, qt) {
  pnl <- 0
  if (nrow(pf) == 0 || is.null(qt)) return(pnl)
  for (i in seq_len(nrow(pf))) {
    row_q <- qt[qt$ticker == pf$ticker[i], ]
    if (nrow(row_q) > 0 && !is.na(row_q$change[1]))
      pnl <- pnl + row_q$change[1] * pf$shares[i]
  }
  pnl
}

compute_total_pnl <- function(pf, qt) {
  pnl <- 0
  if (nrow(pf) == 0 || is.null(qt)) return(pnl)
  for (i in seq_len(nrow(pf))) {
    row_q <- qt[qt$ticker == pf$ticker[i], ]
    if (nrow(row_q) > 0) {
      mv   <- row_q$price[1] * pf$shares[i]
      cost <- pf$avg_cost[i] * pf$shares[i]
      pnl  <- pnl + (mv - cost)
    }
  }
  pnl
}

find_best_performer <- function(pf, qt) {
  if (nrow(pf) == 0 || is.null(qt) || nrow(qt) == 0)
    return(list(label = "--", colour = "#6c757d"))

  best_pct <- -Inf
  best_tkr <- "--"
  for (i in seq_len(nrow(pf))) {
    row_q <- qt[qt$ticker == pf$ticker[i], ]
    if (nrow(row_q) > 0 && !is.na(row_q$pct[1]) &&
        row_q$pct[1] > best_pct) {
      best_pct <- row_q$pct[1]
      best_tkr <- pf$ticker[i]
    }
  }
  clr <- if (best_pct >= 0) "#00c853" else "#ff1744"
  label <- if (best_tkr == "--") "--" else
    paste0(best_tkr, " ", sprintf("%+.1f%%", best_pct))
  list(label = label, colour = clr)
}

build_portfolio_snapshot <- function(pf, qt) {
  if (nrow(pf) == 0) {
    return(data.frame(Ticker = character(), Price = character(),
                      Value = character(), `P&L` = character(),
                      `Chg%` = character(), check.names = FALSE,
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(nrow(pf)), function(i) {
    price <- NA; chg <- NA
    if (!is.null(qt) && nrow(qt) > 0) {
      row_q <- qt[qt$ticker == pf$ticker[i], ]
      if (nrow(row_q) > 0) { price <- row_q$price[1]; chg <- row_q$pct[1] }
    }
    mv <- if (!is.na(price)) price * pf$shares[i] else pf$avg_cost[i] * pf$shares[i]
    pnl <- if (!is.na(price)) mv - pf$avg_cost[i] * pf$shares[i] else 0
    data.frame(
      Ticker = pf$ticker[i],
      Price  = if (is.na(price)) "--" else sprintf("$%.2f", price),
      Value  = sprintf("$%s", formatC(mv, format = "f", digits = 0, big.mark = ",")),
      `P&L`  = sprintf("%+.0f", pnl),
      `Chg%` = if (is.na(chg)) "--" else sprintf("%+.1f%%", chg),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_watchlist_snapshot <- function(wl, qt) {
  if (nrow(wl) == 0) {
    return(data.frame(Ticker = character(), Price = character(),
                      Change = character(), `Chg%` = character(),
                      check.names = FALSE, stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(nrow(wl)), function(i) {
    price <- NA; chg <- NA; pct <- NA
    if (!is.null(qt) && nrow(qt) > 0) {
      row_q <- qt[qt$ticker == wl$ticker[i], ]
      if (nrow(row_q) > 0) {
        price <- row_q$price[1]
        chg   <- row_q$change[1]
        pct   <- row_q$pct[1]
      }
    }
    data.frame(
      Ticker = wl$ticker[i],
      Price  = if (is.na(price)) "--" else sprintf("$%.2f", price),
      Change = if (is.na(chg)) "--" else sprintf("%+.2f", chg),
      `Chg%` = if (is.na(pct)) "--" else sprintf("%+.1f%%", pct),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
