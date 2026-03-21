# ──────────────────────────────────────────────────────────────
#  mod_sentiment.R – News sentiment Shiny module
#
#  Bloomberg-style sentiment dashboard with gauge, headline
#  feed, source breakdown, and sentiment distribution.
# ──────────────────────────────────────────────────────────────

sentimentUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      class = "bb-section-header",
      column(8, h3(icon("newspaper"), "NEWS SENTIMENT",
                    class = "bb-title")),
      column(4, div(style = "text-align:right; padding-top:10px;",
        textInput(ns("ticker"), NULL, placeholder = "Ticker (e.g. AAPL)",
                  width = "140px"),
        actionButton(ns("btn_scan"), "SCAN",
                     class = "bb-btn-primary",
                     icon = icon("search")),
        actionButton(ns("btn_market"), "MARKET OVERVIEW",
                     class = "bb-btn-secondary",
                     icon = icon("globe"))
      ))
    ),

    # ── Sentiment gauge row ──
    fluidRow(
      class = "bb-kpi-row",
      column(3, uiOutput(ns("kpi_overall"))),
      column(3, uiOutput(ns("kpi_positive"))),
      column(3, uiOutput(ns("kpi_negative"))),
      column(3, uiOutput(ns("kpi_articles")))
    ),

    # ── Main content ──
    fluidRow(
      # Gauge + source breakdown
      column(4,
        div(class = "bb-panel",
          h4(icon("tachometer-alt"), "SENTIMENT GAUGE",
             class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_gauge"), height = "260px")
        ),
        div(class = "bb-panel",
          h4(icon("sitemap"), "BY SOURCE",
             class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_sources"), height = "220px")
        )
      ),
      # Headlines feed
      column(5,
        div(class = "bb-panel bb-headline-panel",
          h4(icon("rss"), "LATEST HEADLINES",
             class = "bb-panel-title"),
          uiOutput(ns("headline_feed"))
        )
      ),
      # Distribution + word cloud placeholder
      column(3,
        div(class = "bb-panel",
          h4(icon("chart-bar"), "DISTRIBUTION",
             class = "bb-panel-title"),
          plotly::plotlyOutput(ns("chart_dist"), height = "200px")
        ),
        div(class = "bb-panel",
          h4(icon("info-circle"), "METHODOLOGY",
             class = "bb-panel-title"),
          div(class = "bb-method-info",
            p("Sentiment is scored using a ",
              strong("Loughran-McDonald"),
              " inspired financial lexicon."),
            p("Sources: Financial Times, Handelsblatt,",
              " Reuters, Google News (EN + DE)."),
            p("Score range: -1 (very bearish) to",
              " +1 (very bullish)."),
            tags$hr(style = "border-color:rgba(255,255,255,0.1);"),
            p(class = "bb-disclaimer",
              "Sentiment analysis is for informational",
              " purposes only. Not investment advice.")
          )
        )
      )
    )
  )
}


sentimentServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(result = NULL)

    # ── Scan ticker ──
    observeEvent(input$btn_scan, {
      tkr <- toupper(trimws(input$ticker))
      req(nchar(tkr) > 0)

      withProgress(
        message = paste("Scanning news for", tkr, "..."),
        value = 0.3, {
          rv$result <- analyse_sentiment(tkr)
          setProgress(1)
        }
      )
      showNotification(
        paste("Sentiment scan complete for", tkr),
        type = "message", duration = 4
      )
    })

    # ── Market overview (no ticker) ──
    observeEvent(input$btn_market, {
      withProgress(
        message = "Scanning market sentiment...",
        value = 0.3, {
          rv$result <- analyse_sentiment(NULL)
          setProgress(1)
        }
      )
      showNotification("Market sentiment scan complete",
                       type = "message", duration = 4)
    })

    # ── KPI cards ──
    output$kpi_overall <- renderUI({
      r <- rv$result
      if (is.null(r)) return(bb_kpi("SENTIMENT", "--", colour = "#6c757d"))
      lbl <- paste0(r$label, " (", sprintf("%+.2f", r$overall), ")")
      bb_kpi("SENTIMENT", lbl, colour = r$colour)
    })

    output$kpi_positive <- renderUI({
      r <- rv$result
      n <- if (is.null(r)) 0 else r$pos_count
      bb_kpi("POSITIVE", as.character(n), colour = "#00c853")
    })

    output$kpi_negative <- renderUI({
      r <- rv$result
      n <- if (is.null(r)) 0 else r$neg_count
      bb_kpi("NEGATIVE", as.character(n), colour = "#ff1744")
    })

    output$kpi_articles <- renderUI({
      r <- rv$result
      n <- if (is.null(r)) 0 else r$total
      bb_kpi("ARTICLES", as.character(n), colour = "#f5a623")
    })

    # ── Gauge chart ──
    output$chart_gauge <- plotly::renderPlotly({
      r <- rv$result
      val <- if (is.null(r)) 0 else r$overall
      clr <- if (is.null(r)) "#6c757d" else r$colour

      plotly::plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = val,
        number = list(
          font = list(color = "#ecf0f1", size = 28,
                      family = "Consolas, monospace"),
          suffix = "",
          valueformat = "+.2f"
        ),
        gauge = list(
          axis = list(
            range = list(-1, 1),
            tickwidth = 1,
            tickcolor = "#6c757d",
            tickfont = list(color = "#6c757d", size = 10)
          ),
          bar = list(color = clr, thickness = 0.75),
          bgcolor = "#1e1e2e",
          bordercolor = "#2a2a3e",
          steps = list(
            list(range = c(-1, -0.15), color = "rgba(255,23,68,0.2)"),
            list(range = c(-0.15, -0.05), color = "rgba(255,138,128,0.15)"),
            list(range = c(-0.05, 0.05), color = "rgba(245,166,35,0.15)"),
            list(range = c(0.05, 0.15), color = "rgba(105,240,174,0.15)"),
            list(range = c(0.15, 1), color = "rgba(0,200,83,0.2)")
          ),
          threshold = list(
            line = list(color = "#ecf0f1", width = 2),
            thickness = 0.8,
            value = val
          )
        )
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          margin = list(l = 30, r = 30, t = 40, b = 10),
          font = list(color = "#ecf0f1")
        )
    })

    # ── Source breakdown ──
    output$chart_sources <- plotly::renderPlotly({
      r <- rv$result
      if (is.null(r) || nrow(r$by_source) == 0)
        return(bb_empty_chart("No data"))

      bs <- r$by_source
      bs$colour <- ifelse(bs$sentiment > 0.05, "#00c853",
                          ifelse(bs$sentiment < -0.05, "#ff1744", "#f5a623"))
      # Truncate long source names
      bs$short <- substr(bs$source, 1, 22)

      plotly::plot_ly(bs,
        y = ~reorder(short, sentiment),
        x = ~sentiment,
        type = "bar",
        orientation = "h",
        marker = list(color = bs$colour),
        text = ~paste0("n=", count),
        textposition = "auto",
        textfont = list(color = "#ecf0f1", size = 10)
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          xaxis = list(
            color = "#6c757d",
            gridcolor = "rgba(255,255,255,0.05)",
            title = "Avg Sentiment",
            range = c(-1, 1),
            zeroline = TRUE,
            zerolinecolor = "rgba(255,255,255,0.2)"
          ),
          yaxis = list(
            color = "#6c757d",
            title = ""
          ),
          margin = list(l = 130, r = 10, t = 10, b = 30),
          showlegend = FALSE
        )
    })

    # ── Distribution ──
    output$chart_dist <- plotly::renderPlotly({
      r <- rv$result
      if (is.null(r) || r$total == 0) return(bb_empty_chart("No data"))

      cats   <- c("Positive", "Neutral", "Negative")
      counts <- c(r$pos_count, r$neutral_count, r$neg_count)
      clrs   <- c("#00c853", "#f5a623", "#ff1744")

      plotly::plot_ly(
        x = cats, y = counts,
        type = "bar",
        marker = list(color = clrs)
      ) |>
        plotly::layout(
          paper_bgcolor = "transparent",
          plot_bgcolor  = "transparent",
          xaxis = list(color = "#6c757d",
                       gridcolor = "rgba(255,255,255,0.05)"),
          yaxis = list(color = "#6c757d",
                       gridcolor = "rgba(255,255,255,0.05)",
                       title = "Count"),
          margin = list(l = 40, r = 10, t = 10, b = 30),
          showlegend = FALSE
        )
    })

    # ── Headline feed (HTML) ──
    output$headline_feed <- renderUI({
      r <- rv$result
      if (is.null(r) || nrow(r$articles) == 0) {
        return(div(class = "bb-empty-feed",
          icon("newspaper", class = "fa-2x"),
          p("Click SCAN or MARKET OVERVIEW to load headlines")
        ))
      }

      arts <- r$articles
      # Sort by sentiment (most extreme first for interest)
      arts <- arts[order(-abs(arts$sentiment)), ]
      arts <- head(arts, 25)

      headline_tags <- lapply(seq_len(nrow(arts)), function(i) {
        a <- arts[i, ]
        sent_clr <- if (a$sentiment > 0.05) "#00c853"
                    else if (a$sentiment < -0.05) "#ff1744"
                    else "#f5a623"
        sent_icon <- if (a$sentiment > 0.05) "arrow-up"
                     else if (a$sentiment < -0.05) "arrow-down"
                     else "minus"

        div(class = "bb-headline-item",
          div(class = "bb-headline-score",
              style = paste0("color:", sent_clr, ";"),
              icon(sent_icon),
              sprintf(" %+.2f", a$sentiment)
          ),
          div(class = "bb-headline-content",
            if (nchar(a$link) > 0) {
              tags$a(href = a$link, target = "_blank",
                     class = "bb-headline-title", a$title)
            } else {
              span(class = "bb-headline-title", a$title)
            },
            div(class = "bb-headline-meta",
              span(class = "bb-source-tag", a$source),
              if (nchar(a$date) > 0) span(" | ", a$date)
            )
          )
        )
      })

      do.call(tagList, headline_tags)
    })
  })
}
