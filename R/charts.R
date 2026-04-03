# R/charts.R
# Plotly chart builders for the Stock Analyzer app

library(plotly)
library(dplyr)

# Shared layout template
base_layout <- list(
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  font          = list(color = "#ecf0f1", family = "Roboto, sans-serif"),
  xaxis         = list(
    gridcolor    = "rgba(255,255,255,0.08)",
    zerolinecolor= "rgba(255,255,255,0.15)",
    tickfont     = list(color = "#bdc3c7")
  ),
  yaxis         = list(
    gridcolor    = "rgba(255,255,255,0.08)",
    zerolinecolor= "rgba(255,255,255,0.15)",
    tickfont     = list(color = "#bdc3c7")
  ),
  legend = list(
    bgcolor     = "rgba(0,0,0,0.3)",
    bordercolor = "rgba(255,255,255,0.1)",
    borderwidth = 1
  ),
  margin = list(l = 50, r = 20, t = 40, b = 40)
)

#' Apply the base layout to a plotly object.
apply_layout <- function(p, title = NULL, yaxis_fmt = NULL) {
  ly <- base_layout
  if (!is.null(title)) ly$title <- list(text = title, font = list(size = 14, color = "#ecf0f1"))
  if (!is.null(yaxis_fmt)) ly$yaxis$tickformat <- yaxis_fmt
  do.call(layout, c(list(p), ly))
}

# ── Generic time-series helpers ────────────────────────────────────────────────

#' Line chart for a history data.frame (cols: year, value).
make_line_chart <- function(df, title = "", colour = "#3498db",
                             yaxis_title = "", yaxis_fmt = NULL) {
  if (is.null(df) || nrow(df) == 0) return(empty_chart(title))

  p <- plot_ly(df,
    x = ~year, y = ~value,
    type  = "scatter", mode = "lines+markers",
    line  = list(color = colour, width = 2.5),
    marker= list(color = colour, size = 6),
    hovertemplate = paste0("<b>%{x}</b><br>", yaxis_title, ": %{y:.2f}<extra></extra>")
  )
  p <- apply_layout(p, title, yaxis_fmt)
  p <- layout(p, yaxis = list(title = yaxis_title))
  p
}

#' Bar chart for a history data.frame (cols: year, value).
make_bar_chart <- function(df, title = "", colour = "#2ecc71",
                            yaxis_title = "", yaxis_fmt = NULL) {
  if (is.null(df) || nrow(df) == 0) return(empty_chart(title))

  bar_colours <- ifelse(df$value >= 0, colour, "#e74c3c")

  p <- plot_ly(df,
    x     = ~year, y = ~value,
    type  = "bar",
    marker= list(color = bar_colours),
    hovertemplate = paste0("<b>%{x}</b><br>", yaxis_title, ": %{y:.2f}<extra></extra>")
  )
  p <- apply_layout(p, title, yaxis_fmt)
  p <- layout(p, yaxis = list(title = yaxis_title))
  p
}

#' Dual-axis combo chart (bars for revenue, line for margin).
make_combo_chart <- function(df_bar, df_line, title = "",
                              bar_name = "Revenue",
                              line_name = "Margin %",
                              bar_colour = "#3498db",
                              line_colour = "#e74c3c") {
  if (is.null(df_bar) || nrow(df_bar) == 0) return(empty_chart(title))

  p <- plot_ly()

  p <- add_bars(p,
    x = df_bar$year, y = df_bar$value,
    name   = bar_name,
    marker = list(color = bar_colour, opacity = 0.8),
    hovertemplate = paste0("<b>%{x}</b><br>", bar_name, ": %{y:.2f}<extra></extra>")
  )

  if (!is.null(df_line) && nrow(df_line) > 0) {
    p <- add_lines(p,
      x = df_line$year, y = df_line$value,
      name   = line_name,
      yaxis  = "y2",
      line   = list(color = line_colour, width = 2.5),
      marker = list(color = line_colour, size = 5),
      hovertemplate = paste0("<b>%{x}</b><br>", line_name, ": %{y:.2f}%<extra></extra>")
    )
    p <- apply_layout(p, title)
    p <- layout(p,
      yaxis2 = list(
        overlaying = "y",
        side       = "right",
        title      = line_name,
        tickfont   = list(color = "#bdc3c7"),
        gridcolor  = "rgba(255,255,255,0)",
        ticksuffix = "%"
      )
    )
  } else {
    p <- apply_layout(p, title)
  }
  p
}

#' Grouped bar chart for multiple annual series.
make_grouped_bar_chart <- function(series_list, title = "", yaxis_title = "") {
  valid_series <- Filter(function(df) !is.null(df) && nrow(df) > 0, series_list)
  if (length(valid_series) == 0) return(empty_chart(title))

  palette <- c("#44d7b6", "#f5a623", "#ff6b6b", "#4da3ff", "#8e7dff")
  p <- plot_ly()

  for (idx in seq_along(valid_series)) {
    series_name <- names(valid_series)[idx]
    df <- valid_series[[idx]]
    p <- add_bars(
      p,
      x = df$year,
      y = df$value,
      name = series_name,
      marker = list(color = palette[((idx - 1) %% length(palette)) + 1]),
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        series_name,
        ": %{y:,.3s}<extra></extra>"
      )
    )
  }

  p <- apply_layout(p, title)
  layout(p, barmode = "group", yaxis = list(title = yaxis_title))
}

#' Horizontal bar chart for a current snapshot of metrics.
make_snapshot_bar_chart <- function(metrics_df, title = "") {
  if (is.null(metrics_df) || nrow(metrics_df) == 0) return(empty_chart(title))

  metrics_df <- metrics_df[order(metrics_df$value), , drop = FALSE]
  p <- plot_ly(
    metrics_df,
    x = ~value,
    y = ~metric,
    type = "bar",
    orientation = "h",
    marker = list(color = metrics_df$colour),
    text = ~label,
    textposition = "outside",
    hovertemplate = "<b>%{y}</b><br>%{text}<extra></extra>"
  )
  p <- apply_layout(p, title)
  layout(p, yaxis = list(title = "", automargin = TRUE), xaxis = list(title = ""))
}

#' Placeholder chart when no data is available.
empty_chart <- function(title = "") {
  p <- plot_ly(type = "scatter", mode = "text") %>%
    add_trace(
      x = 0, y = 0,
      text      = "No data available",
      textfont  = list(size = 16, color = "#95a5a6"),
      mode      = "text",
      showlegend= FALSE
    )
  p <- apply_layout(p, title)
  layout(p,
    xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
  )
}

# ── Radar / spider chart ───────────────────────────────────────────────────────

#' Radar chart showing normalised quality metrics.
#' scores: named numeric vector (values 0–100).
make_radar_chart <- function(scores, title = "Quality Radar") {
  if (length(scores) == 0) return(empty_chart(title))

  labels <- names(scores)
  values <- as.numeric(scores)
  # close the polygon
  labels <- c(labels, labels[1])
  values <- c(values, values[1])

  p <- plot_ly(
    type = "scatterpolar",
    r    = values,
    theta= labels,
    fill = "toself",
    fillcolor = "rgba(52,152,219,0.3)",
    line = list(color = "#3498db", width = 2),
    hovertemplate = "<b>%{theta}</b><br>Score: %{r:.1f}<extra></extra>"
  )
  layout(p,
    title      = list(text = title, font = list(color = "#ecf0f1", size = 14)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    polar  = list(
      bgcolor  = "rgba(0,0,0,0)",
      radialaxis = list(
        visible = TRUE, range = c(0, 100),
        gridcolor = "rgba(255,255,255,0.15)",
        tickfont  = list(color = "#bdc3c7")
      ),
      angularaxis = list(
        gridcolor = "rgba(255,255,255,0.15)",
        tickfont  = list(color = "#ecf0f1")
      )
    ),
    margin = list(l = 60, r = 60, t = 50, b = 40),
    showlegend = FALSE
  )
}

# ── Specific chart builders ────────────────────────────────────────────────────

chart_revenue <- function(history) {
  make_bar_chart(history[["revenue"]], "Annual Revenue",
    colour = "#3498db", yaxis_title = "Revenue (USD)")
}

chart_eps <- function(history) {
  make_bar_chart(history[["eps_diluted"]], "Diluted EPS",
    colour = "#2ecc71", yaxis_title = "EPS (USD)")
}

chart_fcf <- function(history) {
  make_bar_chart(history[["fcf"]], "Free Cash Flow",
    colour = "#9b59b6", yaxis_title = "FCF (USD)")
}

chart_pe <- function(history) {
  make_line_chart(history[["pe"]], "P/E Ratio History",
    colour = "#e67e22", yaxis_title = "P/E Ratio")
}

chart_pb <- function(history) {
  make_line_chart(history[["pb"]], "P/B Ratio History",
    colour = "#1abc9c", yaxis_title = "P/B Ratio")
}

chart_ps <- function(history) {
  make_line_chart(history[["ps"]], "P/S Ratio History",
    colour = "#e74c3c", yaxis_title = "P/S Ratio")
}

chart_ev_ebitda <- function(history) {
  make_line_chart(history[["ev_ebitda"]], "EV/EBITDA History",
    colour = "#f39c12", yaxis_title = "EV/EBITDA")
}

chart_margins <- function(history) {
  df_gross <- history[["gross_margin"]]
  df_oper  <- history[["oper_margin"]]
  df_net   <- history[["net_margin"]]

  if (is.null(df_gross) && is.null(df_oper) && is.null(df_net)) {
    return(empty_chart("Profit Margins"))
  }

  p <- plot_ly()
  add_series <- function(p, df, name, colour) {
    if (!is.null(df) && nrow(df) > 0) {
      add_lines(p, x = df$year, y = df$value,
        name = name,
        line = list(color = colour, width = 2.5),
        marker = list(color = colour, size = 5),
        hovertemplate = paste0("<b>%{x}</b><br>", name, ": %{y:.1f}%<extra></extra>")
      )
    } else p
  }

  p <- add_series(p, df_gross, "Gross Margin",  "#2ecc71")
  p <- add_series(p, df_oper,  "Oper. Margin",  "#3498db")
  p <- add_series(p, df_net,   "Net Margin",    "#e74c3c")

  apply_layout(p, "Profit Margins (%)")
}

chart_returns <- function(history) {
  df_roe  <- history[["roe"]]
  df_roa  <- history[["roa"]]
  df_roic <- history[["roic"]]

  if (is.null(df_roe) && is.null(df_roa) && is.null(df_roic)) {
    return(empty_chart("Return Metrics"))
  }

  p <- plot_ly()
  add_series <- function(p, df, name, colour) {
    if (!is.null(df) && nrow(df) > 0) {
      add_lines(p, x = df$year, y = df$value,
        name   = name,
        line   = list(color = colour, width = 2.5),
        marker = list(color = colour, size = 5),
        hovertemplate = paste0("<b>%{x}</b><br>", name, ": %{y:.1f}%<extra></extra>")
      )
    } else p
  }

  p <- add_series(p, df_roe,  "ROE",  "#e74c3c")
  p <- add_series(p, df_roa,  "ROA",  "#3498db")
  p <- add_series(p, df_roic, "ROIC", "#2ecc71")

  apply_layout(p, "Return Metrics (%)")
}

chart_debt <- function(history) {
  make_line_chart(history[["debt_equity"]], "Debt / Equity Ratio",
    colour = "#e74c3c", yaxis_title = "D/E")
}

chart_liquidity <- function(history) {
  make_line_chart(history[["current_ratio"]], "Current Ratio",
    colour = "#1abc9c", yaxis_title = "Current Ratio")
}

#' Build quality radar from history data.frames.
chart_radar <- function(history, ticker) {
  score <- function(df, higher_is_better = TRUE, scale = 1) {
    if (is.null(df) || nrow(df) == 0) return(50)
    recent <- tail(df$value[!is.na(df$value)], 5)
    if (length(recent) == 0) return(50)
    val <- mean(recent) * scale
    # normalise to 0-100 (rough heuristic)
    raw <- if (higher_is_better) pmin(pmax(val, 0), 100) else pmin(pmax(100 - val, 0), 100)
    round(raw, 1)
  }

  scores <- c(
    "ROE"        = score(history[["roe"]]),
    "ROA"        = score(history[["roa"]]),
    "ROIC"       = score(history[["roic"]]),
    "Net Margin" = score(history[["net_margin"]]),
    "Gross Mrgn" = score(history[["gross_margin"]])
  )

  make_radar_chart(scores, title = paste(ticker, "– Quality Radar"))
}

chart_valuation_snapshot <- function(summary) {
  metrics <- data.frame(
    metric = c("Trailing P/E", "Forward P/E", "PEG", "P/B", "P/S", "EV/Revenue", "EV/EBITDA", "Dividend Yield %"),
    value = c(
      get_metric_num(summary, "trailing pe|pe ratio"),
      get_metric_num(summary, "forward pe"),
      get_metric_num(summary, "peg"),
      get_metric_num(summary, "price to book|pb ratio"),
      get_metric_num(summary, "price to sales|ps ratio"),
      get_metric_num(summary, "ev/revenue"),
      get_metric_num(summary, "ev/ebitda"),
      get_metric_num(summary, "dividend yield")
    ),
    colour = c("#f5a623", "#ff8f3d", "#ffd166", "#4da3ff", "#2dd4bf", "#22c55e", "#8b5cf6", "#ff6b6b"),
    stringsAsFactors = FALSE
  )
  metrics <- metrics[!is.na(metrics$value), , drop = FALSE]
  if (nrow(metrics) == 0) return(empty_chart("Valuation Snapshot"))
  metrics$label <- ifelse(metrics$metric == "Dividend Yield %",
                          paste0(round(metrics$value, 2), "%"),
                          sprintf("%.2f", metrics$value))
  make_snapshot_bar_chart(metrics, "Valuation Snapshot")
}

chart_profitability_snapshot <- function(summary) {
  metrics <- data.frame(
    metric = c("ROE %", "ROA %", "Gross Margin %", "Operating Margin %", "Net Margin %", "Target Upside %", "Rev Growth %", "EPS Growth %"),
    value = c(
      get_metric_num(summary, "roe"),
      get_metric_num(summary, "roa"),
      get_metric_num(summary, "gross margin"),
      get_metric_num(summary, "operating margin|oper margin"),
      get_metric_num(summary, "profit margin|net margin"),
      get_metric_num(summary, "target upside"),
      get_metric_num(summary, "quarterly revenue growth"),
      get_metric_num(summary, "quarterly earnings growth")
    ),
    colour = c("#44d7b6", "#4da3ff", "#7dd3fc", "#22c55e", "#f59e0b", "#8b5cf6", "#f97316", "#ef4444"),
    stringsAsFactors = FALSE
  )
  metrics <- metrics[!is.na(metrics$value), , drop = FALSE]
  if (nrow(metrics) == 0) return(empty_chart("Profitability & Growth"))
  metrics$label <- paste0(round(metrics$value, 2), "%")
  make_snapshot_bar_chart(metrics, "Profitability & Growth")
}

chart_earnings_history <- function(history) {
  make_grouped_bar_chart(
    list(
      "Revenue" = history[["revenue"]],
      "Operating Income" = history[["operating_income"]],
      "Net Income" = history[["net_income"]]
    ),
    title = "10Y Earnings History",
    yaxis_title = "USD"
  )
}

chart_cash_vs_debt <- function(history) {
  make_grouped_bar_chart(
    list(
      "Cash" = history[["cash"]],
      "Debt" = history[["debt"]]
    ),
    title = "Cash vs Debt",
    yaxis_title = "USD"
  )
}

chart_cashflow_allocation <- function(history) {
  buybacks_df <- history[["buybacks"]]
  if (!is.null(buybacks_df) && nrow(buybacks_df) > 0) {
    buybacks_df <- buybacks_df |> mutate(value = -abs(value))
  }

  make_grouped_bar_chart(
    list(
      "Operating Cash Flow" = history[["operating_cashflow"]],
      "Free Cash Flow" = history[["free_cashflow"]],
      "Buybacks" = buybacks_df
    ),
    title = "Cash Flow + Buybacks",
    yaxis_title = "USD"
  )
}
