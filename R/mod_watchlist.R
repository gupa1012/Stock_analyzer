# ──────────────────────────────────────────────────────────────
#  mod_watchlist.R – Watchlist management Shiny module
# ──────────────────────────────────────────────────────────────

build_watchlist_sparkline <- function(history_df, change_pct = NA_real_) {
  if (is.null(history_df) || nrow(history_df) == 0 || all(is.na(history_df$close))) {
    return('<div class="bb-watchlist-sparkline-empty">--</div>')
  }

  closes <- history_df$close
  closes <- closes[is.finite(closes)]
  if (length(closes) < 2) {
    return('<div class="bb-watchlist-sparkline-empty">--</div>')
  }

  y_min <- min(closes)
  y_max <- max(closes)
  if (isTRUE(all.equal(y_min, y_max))) {
    y_min <- y_min - 1
    y_max <- y_max + 1
  }

  points_x <- seq(0, 100, length.out = length(closes))
  points_y <- 100 - ((closes - y_min) / (y_max - y_min)) * 100
  points <- paste(sprintf("%.2f,%.2f", points_x, points_y), collapse = " ")

  line_colour <- if (is.na(change_pct)) {
    "#f5a623"
  } else if (change_pct >= 0) {
    "#00c853"
  } else {
    "#ff1744"
  }

  paste0(
    '<div class="bb-watchlist-sparkline-shell">',
    '<svg class="bb-watchlist-sparkline" viewBox="0 0 100 32" preserveAspectRatio="none" aria-hidden="true">',
    '<line x1="0" y1="16" x2="100" y2="16" stroke="rgba(255,255,255,0.08)" stroke-width="1" stroke-dasharray="2 2" />',
    '<polyline fill="none" stroke="', line_colour, '" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round" points="', points, '" />',
    '</svg>',
    '</div>'
  )
}

build_watchlist_range_cell <- function(low, high, current) {
  if (is.na(low) || is.na(high) || is.na(current) || high <= low) {
    return('<div class="bb-watchlist-range-empty">--</div>')
  }

  position <- min(max((current - low) / (high - low), 0), 1)

  paste0(
    '<div class="bb-watchlist-range-cell">',
    '<div class="bb-watchlist-range-current">', format_base_currency(current, digits = 2), '</div>',
    '<div class="bb-watchlist-range-slider">',
    '<div class="bb-watchlist-range-track">',
    '<span class="bb-watchlist-range-dot" style="left:', sprintf("%.1f%%", position * 100), ';"></span>',
    '</div>',
    '<div class="bb-watchlist-range-scale">',
    '<span class="bb-watchlist-range-low">', format_base_currency(low, digits = 0), '</span>',
    '<span class="bb-watchlist-range-high">', format_base_currency(high, digits = 0), '</span>',
    '</div>',
    '</div>',
    '</div>'
  )
}

build_watchlist_daily_cell <- function(change_value, change_pct) {
  if (is.na(change_value) && is.na(change_pct)) {
    return('<div class="bb-watchlist-daily-empty">--</div>')
  }

  cell_class <- if (!is.na(change_pct) && change_pct < 0) {
    "bb-watchlist-negative"
  } else if (!is.na(change_pct) && change_pct > 0) {
    "bb-watchlist-positive"
  } else if (!is.na(change_value) && change_value < 0) {
    "bb-watchlist-negative"
  } else {
    "bb-watchlist-positive"
  }

  value_label <- if (is.na(change_value)) {
    "--"
  } else {
    format_signed_base_currency(change_value, digits = 2)
  }

  pct_label <- if (is.na(change_pct)) {
    ""
  } else {
    sprintf("%+.2f%%", change_pct)
  }

  paste0(
    '<div class="bb-watchlist-daily-cell ', cell_class, '">',
    '<span class="bb-watchlist-daily-value">', value_label, '</span>',
    '<span class="bb-watchlist-daily-pct">', pct_label, '</span>',
    '</div>'
  )
}

watchlistUI <- function(id) {
  ns <- NS(id)

  div(class = "bb-page bb-watchlist-page",
    fluidRow(
      class = "bb-section-header",
      column(12, h3(icon("eye"), "WATCHLIST",
                    class = "bb-title"))
    ),

    fluidRow(
      column(12, uiOutput(ns("data_source_notice")))
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
            column(3, numericInput(ns("add_target"), "Target Price (native)",
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
          div(class = "bb-watchlist-hint", "Click a row to update the chart on the right"),
          div(class = "bb-table-scroll",
            reactable::reactableOutput(ns("tbl_watchlist"))
          )
        )
      ),
      column(4,
        div(class = "bb-panel",
          h4(icon("chart-line"), "PRICE CHART", class = "bb-panel-title"),
          htmlOutput(ns("chart_selection_label")),
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
      quotes    = NULL,
      quotes_fetched_at = NULL
    )

    selected_ticker <- reactive({
      wl <- rv$watchlist
      if (nrow(wl) == 0) return(NULL)

      clicked_ticker_raw <- input$selected_ticker
      clicked_ticker <- if (is.null(clicked_ticker_raw)) {
        ""
      } else {
        toupper(trimws(as.character(clicked_ticker_raw)))
      }

      if (nzchar(clicked_ticker) && clicked_ticker %in% wl$ticker) {
        return(clicked_ticker)
      }

      wl$ticker[[1]]
    })

    watchlist_rows <- reactive({
      wl <- rv$watchlist
      qt <- rv$quotes

      if (nrow(wl) == 0) {
        return(data.frame(
          ticker = character(),
          price = numeric(),
          daily_change = numeric(),
          daily_pct = numeric(),
          low_52w = numeric(),
          high_52w = numeric(),
          range_position = numeric(),
          target_price = numeric(),
          vs_target = numeric(),
          added = character(),
          trend = character(),
          notes = character(),
          stringsAsFactors = FALSE
        ))
      }

      wl$price <- NA_real_
      wl$change <- NA_real_
      wl$change_pct <- NA_real_
      wl$low_52w <- NA_real_
      wl$high_52w <- NA_real_

      if (!is.null(qt) && nrow(qt) > 0) {
        for (i in seq_len(nrow(wl))) {
          row_q <- qt[qt$ticker == wl$ticker[i], ]
          if (nrow(row_q) > 0) {
            wl$price[i] <- convert_amount_to_base(row_q$price[1], row_q$currency[1], qt)
            wl$change[i] <- convert_amount_to_base(row_q$change[1], row_q$currency[1], qt)
            wl$change_pct[i] <- row_q$pct[1]
            wl$low_52w[i] <- convert_amount_to_base(row_q$low_52w[1], row_q$currency[1], qt)
            wl$high_52w[i] <- convert_amount_to_base(row_q$high_52w[1], row_q$currency[1], qt)
          }
        }
      }

      wl$target_price_eur <- ifelse(
        is.na(wl$target_price),
        NA_real_,
        vapply(seq_len(nrow(wl)), function(i) {
          row_q <- if (!is.null(qt) && nrow(qt) > 0) qt[qt$ticker == wl$ticker[i], ] else NULL
          if (is.null(row_q) || nrow(row_q) == 0) return(NA_real_)
          convert_amount_to_base(wl$target_price[i], row_q$currency[1], qt)
        }, numeric(1))
      )

      vs_target <- ifelse(
        is.na(wl$price) | is.na(wl$target_price_eur) | wl$target_price_eur == 0,
        NA_real_,
        (wl$price / wl$target_price_eur - 1) * 100
      )

      range_position <- ifelse(
        is.na(wl$price) | is.na(wl$low_52w) | is.na(wl$high_52w) | wl$high_52w <= wl$low_52w,
        NA_real_,
        (wl$price - wl$low_52w) / (wl$high_52w - wl$low_52w)
      )

      sparkline_html <- vapply(seq_len(nrow(wl)), function(i) {
        hist <- fetch_history(wl$ticker[i], period = "3mo")
        build_watchlist_sparkline(hist, wl$change_pct[i])
      }, character(1))

      data.frame(
        ticker = wl$ticker,
        price = wl$price,
        daily_change = wl$change,
        daily_pct = wl$change_pct,
        low_52w = wl$low_52w,
        high_52w = wl$high_52w,
        range_position = range_position,
        target_price = wl$target_price_eur,
        vs_target = vs_target,
        added = wl$date_added,
        trend = sparkline_html,
        notes = ifelse(is.na(wl$notes), "", wl$notes),
        stringsAsFactors = FALSE
      )
    })

    observe({
      trigger_refresh()
      rv$watchlist <- read_watchlist()
    })

    observe({
      wl <- rv$watchlist
      if (nrow(wl) == 0) {
        rv$quotes <- NULL
        rv$quotes_fetched_at <- NULL
        return()
      }

      qt <- tryCatch(fetch_quotes(wl$ticker), error = function(e) NULL)
      fx_tickers <- if (!is.null(qt) && nrow(qt) > 0) {
        required_fx_tickers(unique(qt$currency))
      } else {
        character(0)
      }

      rv$quotes <- if (length(fx_tickers) > 0) {
        tryCatch(fetch_quotes(c(wl$ticker, fx_tickers)), error = function(e) qt)
      } else {
        qt
      }
      rv$quotes_fetched_at <- Sys.time()
    })

    output$data_source_notice <- renderUI({
      bb_data_source_notice(c(
        "Watchlist" = "Local CSV: user_data/watchlist.csv",
        "Price / Daily / 52W" = paste0(
          "Yahoo Finance v8 chart API",
          if (!is.null(rv$quotes_fetched_at)) {
            paste0(" · refreshed ", format_notice_timestamp(rv$quotes_fetched_at))
          } else {
            ""
          }
        ),
        "Mini Charts" = "Yahoo Finance price history (3 months in table, full history on right chart, converted to EUR when needed)"
      ))
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

      current_ticker <- selected_ticker()
      if (!is.null(current_ticker) && nzchar(current_ticker)) {
        updateSelectInput(session, "sel_remove", selected = current_ticker)
      }
    })

    observeEvent(input$btn_remove, {
      req(nchar(input$sel_remove) > 0)
      rv$watchlist <- remove_from_watchlist(input$sel_remove)
      showNotification("Removed from watchlist", type = "warning")
    })

    # ── Display table ──
    output$tbl_watchlist <- reactable::renderReactable({
      values <- watchlist_rows()
      if (nrow(values) == 0) {
        return(reactable::reactable(data.frame()))
      }

      selected_row_ticker <- selected_ticker()
      header_style <- list(
        background = "#0e0e1e",
        color = "#f5a623",
        borderBottom = "1px solid #2a2a3e",
        fontFamily = "'JetBrains Mono', 'Consolas', 'Courier New', monospace",
        fontSize = "10px",
        fontWeight = "600",
        letterSpacing = "1px",
        textTransform = "uppercase",
        padding = "9px 10px"
      )
      cell_style <- list(
        color = "#e0e0e0",
        borderBottom = "1px solid rgba(255,255,255,0.05)",
        fontFamily = "'JetBrains Mono', 'Consolas', 'Courier New', monospace",
        fontSize = "12px",
        padding = "10px 10px",
        background = "transparent"
      )

      reactable::reactable(
        values,
        searchable = FALSE,
        sortable = TRUE,
        highlight = FALSE,
        striped = FALSE,
        bordered = FALSE,
        compact = TRUE,
        pagination = FALSE,
        wrap = FALSE,
        class = "bb-watchlist-reactable",
        style = list(
          background = "#141422",
          border = "1px solid #2a2a3e",
          borderRadius = "4px",
          overflow = "hidden",
          boxShadow = "inset 0 1px 0 rgba(255,255,255,0.02)"
        ),
        theme = reactable::reactableTheme(
          color = "#e0e0e0",
          backgroundColor = "#141422",
          borderColor = "#2a2a3e",
          cellPadding = "10px 10px",
          style = list(fontFamily = "'JetBrains Mono', 'Consolas', 'Courier New', monospace"),
          headerStyle = header_style,
          cellStyle = cell_style,
          rowStyle = list(background = "#141422"),
          rowStripedStyle = list(background = "#1a1a2e")
        ),
        defaultColDef = reactable::colDef(
          headerStyle = header_style,
          style = cell_style,
          vAlign = "center"
        ),
        onClick = reactable::JS(sprintf(
          "function(rowInfo) {
             if (!rowInfo) return;
             if (window.Shiny && window.Shiny.setInputValue) {
               window.Shiny.setInputValue('%s', rowInfo.values.ticker, { priority: 'event' });
             }
           }",
          ns("selected_ticker")
        )),
        rowClass = function(index) {
          if (!is.null(selected_row_ticker) && identical(values$ticker[[index]], selected_row_ticker)) {
            "bb-watchlist-row bb-watchlist-row-selected"
          } else {
            "bb-watchlist-row"
          }
        },
        rowStyle = function(index) {
          is_selected <- !is.null(selected_row_ticker) && identical(values$ticker[[index]], selected_row_ticker)
          base_background <- if (index %% 2 == 0) "#171728" else "#141422"

          if (is_selected) {
            list(
              background = "linear-gradient(90deg, rgba(245,166,35,0.18), rgba(245,166,35,0.07))",
              boxShadow = "inset 3px 0 0 #f5a623"
            )
          } else {
            list(background = base_background)
          }
        },
        columns = list(
          ticker = reactable::colDef(
            name = "Ticker",
            minWidth = 110,
            style = c(cell_style, list(fontWeight = "700", color = "#f4f1ea"))
          ),
          price = reactable::colDef(
            name = "Price",
            minWidth = 110,
            align = "right",
            cell = function(value) {
              if (is.na(value)) return("--")
              format_base_currency(value, digits = 2)
            }
          ),
          daily_change = reactable::colDef(
            name = "Daily",
            minWidth = 130,
            align = "right",
            html = TRUE,
            cell = function(value, index) {
              htmltools::HTML(build_watchlist_daily_cell(value, values$daily_pct[[index]]))
            }
          ),
          daily_pct = reactable::colDef(show = FALSE),
          low_52w = reactable::colDef(show = FALSE),
          high_52w = reactable::colDef(show = FALSE),
          range_position = reactable::colDef(
            name = "52W Range",
            minWidth = 220,
            align = "left",
            html = TRUE,
            cell = function(value, index) {
              htmltools::HTML(build_watchlist_range_cell(
                values$low_52w[[index]],
                values$high_52w[[index]],
                values$price[[index]]
              ))
            }
          ),
          target_price = reactable::colDef(
            name = "Target",
            minWidth = 95,
            align = "right",
            cell = function(value) {
              if (is.na(value)) return("--")
              format_base_currency(value, digits = 2)
            }
          ),
          vs_target = reactable::colDef(
            name = "vs Target",
            minWidth = 95,
            align = "right",
            cell = function(value) {
              if (is.na(value)) return("--")
              sprintf("%+.1f%%", value)
            }
          ),
          added = reactable::colDef(
            name = "Added",
            minWidth = 112,
            style = c(cell_style, list(color = "#b7a98a"))
          ),
          trend = reactable::colDef(
            name = "Mini",
            minWidth = 120,
            sortable = FALSE,
            html = TRUE,
            cell = function(value) htmltools::HTML(value)
          ),
          notes = reactable::colDef(
            name = "Notes",
            minWidth = 240,
            cell = function(value) {
              if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return("--")
              value
            },
            style = c(cell_style, list(color = "#c9c2b4"))
          )
        )
      )
    })

    output$chart_selection_label <- renderUI({
      tkr <- selected_ticker()
      label <- if (is.null(tkr) || !nzchar(tkr)) {
        "Click a row in the table to load the chart"
      } else {
        paste("Selected:", tkr, "· click another row to switch")
      }

      div(class = "bb-watchlist-selection-label", label)
    })

    # ── Mini price chart ──
    output$chart_mini <- plotly::renderPlotly({
      tkr <- selected_ticker()
      if (is.null(tkr) || nchar(tkr) == 0)
        return(bb_empty_chart("Select a ticker"))

      hist <- fetch_history(tkr)
      if (nrow(hist) == 0) return(bb_empty_chart("No data"))

      chart_title <- ""
      if (!is.null(rv$quotes) && nrow(rv$quotes) > 0) {
        row_q <- rv$quotes[rv$quotes$ticker == tkr, ]
        if (nrow(row_q) > 0) {
          fx_ticker <- fx_pair_ticker(row_q$currency[1])
          if (!is.na(fx_ticker)) {
            fx_hist <- fetch_history(fx_ticker)
            if (nrow(fx_hist) > 0) {
              names(fx_hist)[names(fx_hist) == "close"] <- "fx_close"
              hist <- merge(hist, fx_hist, by = "date", all.x = TRUE)
              hist$fx_close <- zoo::na.locf(hist$fx_close, na.rm = FALSE)
              hist$fx_close <- zoo::na.locf(hist$fx_close, fromLast = TRUE, na.rm = FALSE)
              hist$close <- hist$close * hist$fx_close
              chart_title <- "EUR"
            }
          } else {
            chart_title <- "EUR"
          }
        }
      }

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
            title = chart_title
          ),
          margin = list(l = 40, r = 10, t = 10, b = 30),
          showlegend = FALSE
        )
    })

    return(reactive(rv$watchlist))
  })
}
