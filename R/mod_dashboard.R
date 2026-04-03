# ──────────────────────────────────────────────────────────────
#  mod_dashboard.R – Combined portfolio dashboard / home tab
# ──────────────────────────────────────────────────────────────

dashboardTabUI <- function(id) {
  ns <- NS(id)

  div(class = "bb-page bb-dashboard-page",
    fluidRow(
      class = "bb-section-header",
      column(8, h3(icon("briefcase"), "PORTFOLIO DASHBOARD",
                    class = "bb-title")),
      column(4, div(class = "bb-header-actions", style = "text-align:right; padding-top:15px;",
        actionButton(ns("btn_refresh"), "REFRESH",
                     class = "bb-btn-primary", icon = icon("sync")),
        span(style = "color:#6c757d; margin-left:10px; font-size:11px;",
             textOutput(ns("last_update"), inline = TRUE))
      ))
    ),

    fluidRow(
      column(12, uiOutput(ns("data_source_notice")))
    ),

    fluidRow(
      class = "bb-kpi-row",
      column(2, uiOutput(ns("kpi_portfolio_value"))),
      column(2, uiOutput(ns("kpi_day_change"))),
      column(2, uiOutput(ns("kpi_total_return"))),
      column(2, uiOutput(ns("kpi_positions_count"))),
      column(2, uiOutput(ns("kpi_etf_count"))),
      column(2, uiOutput(ns("kpi_best_performer")))
    ),

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

    fluidRow(
      column(8,
        div(class = "bb-panel",
          div(class = "bb-panel-toolbar", style = "display:flex; align-items:center; justify-content:space-between;",
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
                condition = paste0("input['", ns("xray_on"), "'] == true"),
                actionButton(ns("btn_refresh_xray"), "",
                             icon  = icon("sync-alt"),
                             class = "btn-xs bb-btn-secondary",
                             title = "ETF-Positionen neu abrufen")
              )
            )
          ),
          div(style = "font-size:10px; color:#888; margin:6px 0 10px;",
              textOutput(ns("holdings_hint"), inline = TRUE)),
          div(class = "bb-table-scroll",
            DT::dataTableOutput(ns("tbl_holdings"))
          )
        )
      ),
      column(4,
        div(class = "bb-panel",
          h4(icon("search"), "ETF X-RAY", class = "bb-panel-title"),
          uiOutput(ns("xray_panel"))
        ),
        div(class = "bb-panel",
          h4(icon("trash-alt"), "MANAGE", class = "bb-panel-title"),
          selectInput(ns("sel_remove"), "Select Position to Remove:", choices = NULL),
          actionButton(ns("btn_remove"), "REMOVE POSITION",
                       class = "bb-btn-danger", icon = icon("times"))
        )
      )
    ),

    fluidRow(
      column(6,
        div(class = "bb-panel",
          h4(icon("chart-pie"), "ALLOCATION", class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_alloc"), height = "320px")
        )
      ),
      column(6,
        div(class = "bb-panel",
          h4(icon("chart-bar"), "PORTFOLIO P&L BY POSITION",
             class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_pnl_bar"), height = "320px")
        )
      )
    )
  )
}


dashboardTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      refresh = 0,
      portfolio = read_portfolio(),
      quotes = NULL,
      xray_holdings = list(),
      xray_fetched_at = NULL,
      quotes_fetched_at = NULL
    )

    observeEvent(input$btn_refresh, {
      rv$refresh <- rv$refresh + 1
      rv$portfolio <- read_portfolio()
    })

    observe({
      rv$refresh
      pf <- rv$portfolio
      if (nrow(pf) == 0) {
        rv$quotes <- NULL
        rv$quotes_fetched_at <- Sys.time()
        return()
      }

      tickers <- unique(c(pf$ticker, required_fx_tickers(pf$currency)))
      rv$quotes <- tryCatch(fetch_quotes(tickers), error = function(e) NULL)
      rv$quotes_fetched_at <- Sys.time()
    })

    observe({
      pf <- rv$portfolio
      if (nrow(pf) == 0) {
        updateSelectInput(session, "sel_remove", choices = c("No positions" = ""))
        return()
      }

      choices <- setNames(pf$id, paste(pf$ticker, "-", pf$shares, "shares"))
      updateSelectInput(session, "sel_remove", choices = choices)
    })

    observeEvent(input$btn_add, {
      req(nchar(trimws(input$add_ticker)) > 0, input$add_shares > 0)
      rv$portfolio <- add_position(
        ticker = input$add_ticker,
        shares = input$add_shares,
        avg_cost = input$add_cost,
        notes = input$add_notes
      )
      rv$refresh <- rv$refresh + 1
      updateTextInput(session, "add_ticker", value = "")
      updateNumericInput(session, "add_shares", value = 0)
      updateNumericInput(session, "add_cost", value = 0)
      updateTextInput(session, "add_notes", value = "")
      showNotification("Position added", type = "message")
    })

    observeEvent(input$btn_remove, {
      req(nchar(input$sel_remove) > 0)
      rv$portfolio <- remove_position(input$sel_remove)
      rv$xray_holdings <- rv$xray_holdings[intersect(names(rv$xray_holdings), rv$portfolio$ticker)]
      rv$refresh <- rv$refresh + 1
      showNotification("Position removed", type = "warning")
    })

    do_fetch_xray <- function() {
      pf_tickers <- rv$portfolio$ticker
      etf_tickers <- intersect(pf_tickers, names(ETF_ISINS))
      if (length(etf_tickers) == 0) return()

      showNotification("ETF X-Ray: Lade Positionen …",
                       id = "dash_xray_loading", duration = NULL, type = "message")
      on.exit(removeNotification("dash_xray_loading"))

      cache <- rv$xray_holdings
      for (tkr in etf_tickers) {
        hld <- tryCatch(fetch_etf_holdings(tkr), error = function(e) NULL)
        if (!is.null(hld) && nrow(hld) > 0) {
          cache[[tkr]] <- hld
        } else {
          showNotification(paste0("X-Ray: Keine Daten für ", tkr),
                           type = "warning", duration = 5)
        }
      }

      rv$xray_holdings <- cache
      rv$xray_fetched_at <- Sys.time()
    }

    observeEvent(input$xray_on, {
      req(input$xray_on)
      if (length(rv$xray_holdings) == 0) do_fetch_xray()
    })

    observeEvent(input$btn_refresh_xray, {
      do_fetch_xray()
    })

    all_data <- reactive({
      list(
        pf = rv$portfolio,
        pf_q = rv$quotes,
        time = if (!is.null(rv$quotes_fetched_at)) rv$quotes_fetched_at else Sys.time()
      )
    })

    output$last_update <- renderText({
      format(all_data()$time, "%H:%M:%S")
    })

    output$data_source_notice <- renderUI({
      active_etfs <- rv$portfolio$ticker[rv$portfolio$ticker %in% names(rv$xray_holdings)]
      active_etfs <- unique(active_etfs)

      xray_value <- if (!isTRUE(input$xray_on)) {
        "Off on current screen"
      } else if (length(active_etfs) == 0) {
        "Enabled, waiting for ETF holdings"
      } else {
        parts <- vapply(active_etfs, function(tkr) {
          src <- attr(rv$xray_holdings[[tkr]], "source_label")
          paste0(tkr, " -> ", if (!is.null(src) && nzchar(src)) src else "unknown source")
        }, character(1), USE.NAMES = FALSE)
        paste(parts, collapse = " | ")
      }

      bb_data_source_notice(c(
        "Portfolio" = "Local CSV: user_data/portfolio.csv",
        "Prices / FX" = paste0(
          "Yahoo Finance v8 chart API",
          if (!is.null(rv$quotes_fetched_at)) {
            paste0(" · refreshed ", format_notice_timestamp(rv$quotes_fetched_at))
          } else {
            ""
          }
        ),
        "ETF X-Ray" = xray_value
      ))
    })

    output$holdings_hint <- renderText({
      if (isTRUE(input$xray_on)) {
        "Direkte Positionen plus ETF-Look-through-Positionen, sortiert nach Wert in EUR"
      } else {
        "Direkte Positionen, sortiert nach Wert in EUR"
      }
    })

    output$kpi_portfolio_value <- renderUI({
      d <- all_data()
      total <- compute_portfolio_value(d$pf, d$pf_q)
      bb_kpi("PORTFOLIO", format_base_currency(total, digits = 0), colour = "#f5a623")
    })

    output$kpi_day_change <- renderUI({
      d <- all_data()
      day_chg <- compute_day_pnl(d$pf, d$pf_q)
      clr <- if (day_chg >= 0) "#00c853" else "#ff1744"
      bb_kpi("DAY P&L", format_signed_base_currency(day_chg, digits = 0), colour = clr)
    })

    output$kpi_total_return <- renderUI({
      d <- all_data()
      pnl <- compute_total_pnl(d$pf, d$pf_q)
      clr <- if (pnl >= 0) "#00c853" else "#ff1744"
      bb_kpi("TOTAL P&L", format_signed_base_currency(pnl, digits = 0), colour = clr)
    })

    output$kpi_positions_count <- renderUI({
      bb_kpi("POSITIONS", as.character(nrow(rv$portfolio)), colour = "#f5a623")
    })

    output$kpi_etf_count <- renderUI({
      etf_count <- sum(rv$portfolio$ticker %in% names(ETF_ISINS))
      bb_kpi("ETF POS.", as.character(etf_count), colour = "#f5a623")
    })

    output$kpi_best_performer <- renderUI({
      d <- all_data()
      best <- find_best_performer(d$pf, d$pf_q)
      bb_kpi("TOP MOVER", best$label, colour = best$colour)
    })

    holdings_display <- reactive({
      build_dashboard_holdings_table(
        portfolio_df = rv$portfolio,
        quotes_df = rv$quotes,
        holdings_cache = rv$xray_holdings,
        include_xray = isTRUE(input$xray_on)
      )
    })

    output$tbl_holdings <- DT::renderDataTable({
      df <- holdings_display()
      DT::datatable(df,
        options = list(
          dom = "t",
          pageLength = 100,
          ordering = TRUE,
          order = list(list(7, "desc")),
          columnDefs = list(
            list(className = "dt-right", targets = c(2, 3, 4, 5, 6)),
            list(visible = FALSE, targets = c(7, 8))
          )
        ),
        rownames = FALSE,
        class = "cell-border compact"
      )
    })

    output$xray_table <- DT::renderDataTable({
      req(input$xray_on)
      tbl <- build_xray_table(rv$portfolio, rv$quotes, rv$xray_holdings)
      if (is.null(tbl)) return(data.frame())

      DT::datatable(tbl,
        options = list(
          dom = "t",
          pageLength = 25,
          ordering = TRUE,
          order = list(list(6, "desc")),
          columnDefs = list(
            list(className = "dt-right", targets = c(2, 3, 4)),
            list(visible = FALSE, targets = c(5, 6, 7))
          )
        ),
        rownames = FALSE,
        class = "cell-border compact"
      )
    })

    output$xray_panel <- renderUI({
      if (!isTRUE(input$xray_on)) {
        return(div(
          style = "padding:10px 0; color:#888; font-size:11px;",
          icon("info-circle"),
          " X-Ray einschalten, um rechts die ETF-Look-through-Ansicht zu sehen."
        ))
      }

      tbl <- build_xray_table(rv$portfolio, rv$quotes, rv$xray_holdings)
      if (is.null(tbl)) {
        return(div(
          style = "padding:10px 0; color:#f5a623; font-size:11px;",
          icon("info-circle"),
          " Keine ETF-Positionen oder Daten ausstehend."
        ))
      }

      ts_str <- if (!is.null(rv$xray_fetched_at)) {
        paste0("Stand: ", format(rv$xray_fetched_at, "%d.%m.%Y %H:%M"))
      } else {
        "Noch nicht geladen"
      }

      tagList(
        div(style = "display:flex; align-items:center; margin:4px 0 8px;",
          tags$span(ts_str,
            style = "font-size:10px; color:#888; margin-right:8px;"),
          tags$span("Einzelpositionen je ETF in EUR · Portfolioanteil relativ zum ETF-Teil des Portfolios",
            style = "font-size:10px; color:#888;")
        ),
        div(class = "bb-table-scroll",
          DT::dataTableOutput(ns("xray_table"))
        )
      )
    })

    output$chart_alloc <- plotly::renderPlotly({
      pf <- rv$portfolio
      qt <- rv$quotes
      if (nrow(pf) == 0) return(bb_empty_chart("No positions"))

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
        type = "pie", hole = 0.55,
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

    output$chart_pnl_bar <- plotly::renderPlotly({
      pf <- rv$portfolio
      qt <- rv$quotes
      if (nrow(pf) == 0) return(bb_empty_chart("No positions"))

      pf$pnl <- 0
      if (!is.null(qt) && nrow(qt) > 0) {
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
                       title = "P&L (EUR)",
                       zeroline = TRUE,
                       zerolinecolor = "rgba(255,255,255,0.2)"),
          margin = list(l = 50, r = 10, t = 10, b = 40)
        )
    })

    return(reactive(rv$portfolio))
  })
}


compute_portfolio_value <- function(pf, qt) {
  total <- 0
  if (nrow(pf) == 0) return(total)
  if (!is.null(qt) && nrow(qt) > 0) {
    for (i in seq_len(nrow(pf))) {
      row_q <- qt[qt$ticker == pf$ticker[i], ]
      if (nrow(row_q) > 0 && !is.na(row_q$price[1])) {
        total <- total + convert_amount_to_base(
          row_q$price[1] * pf$shares[i],
          pf$currency[i],
          qt
        )
      } else {
        total <- total + convert_amount_to_base(
          pf$avg_cost[i] * pf$shares[i],
          pf$currency[i],
          qt
        )
      }
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
    if (nrow(row_q) > 0 && !is.na(row_q$change[1])) {
      pnl <- pnl + convert_amount_to_base(
        row_q$change[1] * pf$shares[i],
        pf$currency[i],
        qt
      )
    }
  }
  pnl
}

compute_total_pnl <- function(pf, qt) {
  pnl <- 0
  if (nrow(pf) == 0 || is.null(qt)) return(pnl)
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
      pnl <- pnl + (mv - cost)
    }
  }
  pnl
}

find_best_performer <- function(pf, qt) {
  if (nrow(pf) == 0 || is.null(qt) || nrow(qt) == 0) {
    return(list(label = "--", colour = "#6c757d"))
  }

  best_pct <- -Inf
  best_tkr <- "--"
  for (i in seq_len(nrow(pf))) {
    row_q <- qt[qt$ticker == pf$ticker[i], ]
    if (nrow(row_q) > 0 && !is.na(row_q$pct[1]) && row_q$pct[1] > best_pct) {
      best_pct <- row_q$pct[1]
      best_tkr <- pf$ticker[i]
    }
  }

  clr <- if (best_pct >= 0) "#00c853" else "#ff1744"
  label <- if (best_tkr == "--") "--" else paste0(best_tkr, " ", sprintf("%+.1f%%", best_pct))
  list(label = label, colour = clr)
}

build_dashboard_holdings_table <- function(portfolio_df, quotes_df, holdings_cache,
                                           include_xray = FALSE) {
  if (is.null(portfolio_df) || nrow(portfolio_df) == 0) {
    return(data.frame(
      Position = character(), Quelle = character(), Shares = character(),
      Price = character(), Value = character(), `Portf.%` = character(),
      `Chg%` = character(), value_num = numeric(), portfolio_pct_num = numeric(),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  total_mv <- compute_portfolio_value(portfolio_df, quotes_df)
  base_rows <- if (include_xray) {
    portfolio_df[!portfolio_df$ticker %in% names(ETF_ISINS), , drop = FALSE]
  } else {
    portfolio_df
  }

  direct_company_names <- extract_company_name_from_notes(base_rows$notes)

  direct_rows <- lapply(seq_len(nrow(base_rows)), function(i) {
    row_q <- if (!is.null(quotes_df) && nrow(quotes_df) > 0) {
      quotes_df[quotes_df$ticker == base_rows$ticker[i], ]
    } else {
      NULL
    }

    price_eur <- NA_real_
    mv <- NA_real_
    chg <- NA_real_

    if (!is.null(row_q) && nrow(row_q) > 0 && !is.na(row_q$price[1])) {
      price_eur <- convert_amount_to_base(row_q$price[1], base_rows$currency[i], quotes_df)
      mv <- convert_amount_to_base(row_q$price[1] * base_rows$shares[i], base_rows$currency[i], quotes_df)
      chg <- row_q$pct[1]
    } else {
      mv <- convert_amount_to_base(base_rows$avg_cost[i] * base_rows$shares[i], base_rows$currency[i], quotes_df)
    }

    port_pct <- if (!is.na(mv) && total_mv > 0) mv / total_mv * 100 else NA_real_
    company_name <- direct_company_names[i]
    entity_label <- if (!is.na(company_name) && nzchar(company_name)) {
      canonical_company_label(company_name)
    } else {
      base_rows$ticker[i]
    }

    data.frame(
      Position = base_rows$ticker[i],
      Quelle = "Direkt",
      Shares = formatC(base_rows$shares[i], format = "f", digits = 4),
      Price = if (is.na(price_eur)) "--" else format_base_currency(price_eur, digits = 2),
      Value = if (is.na(mv)) "--" else format_base_currency(mv, digits = 0),
      `Portf.%` = if (is.na(port_pct)) "--" else paste0(formatC(port_pct, digits = 2, format = "f"), " %"),
      `Chg%` = if (is.na(chg)) "--" else sprintf("%+.1f%%", chg),
      value_num = if (is.na(mv)) NA_real_ else mv,
      portfolio_pct_num = if (is.na(port_pct)) NA_real_ else port_pct,
      entity_key = canonical_company_label(entity_label),
      entity_label = entity_label,
      source_type = "direct",
      shares_num = base_rows$shares[i],
      price_num = price_eur,
      chg_pct_num = chg,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  out <- if (length(direct_rows) == 0) {
    data.frame(
      Position = character(), Quelle = character(), Shares = character(),
      Price = character(), Value = character(), `Portf.%` = character(),
      `Chg%` = character(), value_num = numeric(), portfolio_pct_num = numeric(),
      entity_key = character(), entity_label = character(), source_type = character(),
      shares_num = numeric(), price_num = numeric(), chg_pct_num = numeric(),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, direct_rows)
  }

  if (include_xray) {
    xray_tbl <- build_xray_table(portfolio_df, quotes_df, holdings_cache)
    if (!is.null(xray_tbl) && nrow(xray_tbl) > 0) {
      xray_rows <- data.frame(
        Position = xray_tbl$Unternehmen,
        Quelle = paste0("via ", xray_tbl$ETF),
        Shares = "--",
        Price = "--",
        Value = xray_tbl$`Eff. Wert`,
        `Portf.%` = ifelse(
          is.na(xray_tbl$total_portfolio_pct_num),
          "--",
          paste0(formatC(xray_tbl$total_portfolio_pct_num, digits = 2, format = "f"), " %")
        ),
        `Chg%` = "--",
        value_num = xray_tbl$eff_value_num,
        portfolio_pct_num = xray_tbl$total_portfolio_pct_num,
        entity_key = canonical_company_label(xray_tbl$Unternehmen),
        entity_label = canonical_company_label(xray_tbl$Unternehmen),
        source_type = "xray",
        shares_num = NA_real_,
        price_num = NA_real_,
        chg_pct_num = NA_real_,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      out <- rbind(out, xray_rows)
    }

    if (nrow(out) > 0) {
      grouped_rows <- split(out, out$entity_key)
      out <- do.call(rbind, lapply(grouped_rows, function(part) {
        direct_part <- part[part$source_type == "direct", , drop = FALSE]
        xray_part <- part[part$source_type == "xray", , drop = FALSE]

        total_value <- if (all(is.na(part$value_num))) NA_real_ else sum(part$value_num, na.rm = TRUE)
        total_pct <- if (all(is.na(part$portfolio_pct_num))) NA_real_ else sum(part$portfolio_pct_num, na.rm = TRUE)

        display_label <- if (nrow(xray_part) > 0) {
          xray_part$entity_label[1]
        } else if (nrow(direct_part) > 0 && !all(is.na(direct_part$entity_label)) &&
                   any(nzchar(direct_part$entity_label))) {
          direct_part$entity_label[1]
        } else {
          part$Position[1]
        }

        unique_sources <- unique(part$Quelle)

        shares_display <- "--"
        price_display <- "--"
        chg_display <- "--"

        if (nrow(direct_part) == 1) {
          shares_display <- direct_part$Shares[1]
          price_display <- direct_part$Price[1]
          chg_display <- direct_part$`Chg%`[1]
        } else if (nrow(direct_part) > 1) {
          total_shares <- sum(direct_part$shares_num, na.rm = TRUE)
          weighted_price <- if (total_shares > 0) {
            sum(direct_part$price_num * direct_part$shares_num, na.rm = TRUE) / total_shares
          } else {
            NA_real_
          }
          weighted_chg <- if (all(is.na(direct_part$chg_pct_num)) || all(is.na(direct_part$value_num))) {
            NA_real_
          } else {
            sum(direct_part$chg_pct_num * direct_part$value_num, na.rm = TRUE) /
              sum(direct_part$value_num[!is.na(direct_part$chg_pct_num)], na.rm = TRUE)
          }

          shares_display <- formatC(total_shares, format = "f", digits = 4)
          price_display <- if (is.na(weighted_price)) "--" else format_base_currency(weighted_price, digits = 2)
          chg_display <- if (is.na(weighted_chg)) "--" else sprintf("%+.1f%%", weighted_chg)
        }

        data.frame(
          Position = display_label,
          Quelle = paste(unique_sources, collapse = " + "),
          Shares = shares_display,
          Price = price_display,
          Value = if (is.na(total_value)) "--" else format_base_currency(total_value, digits = 0),
          `Portf.%` = if (is.na(total_pct)) "--" else paste0(formatC(total_pct, digits = 2, format = "f"), " %"),
          `Chg%` = chg_display,
          value_num = total_value,
          portfolio_pct_num = total_pct,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }))
    }
  }

  out <- out[, c("Position", "Quelle", "Shares", "Price", "Value",
                 "Portf.%", "Chg%", "value_num", "portfolio_pct_num"),
             drop = FALSE]
  out[order(-out$value_num, -out$portfolio_pct_num), ]
}
