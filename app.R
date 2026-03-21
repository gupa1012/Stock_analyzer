# ──────────────────────────────────────────────────────────────
#  app.R – Bloomberg Terminal Light
#  Private portfolio management Shiny app with Bloomberg-style
#  dark UI, portfolio & watchlist management, fundamental
#  analysis, and news sentiment scoring.
# ──────────────────────────────────────────────────────────────

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(htmltools)
library(DT)
library(plotly)
library(dplyr)
if (requireNamespace("shinyWidgets", quietly = TRUE)) library(shinyWidgets)

# Source helpers & modules
source("R/utils.R")
source("R/scraper.R")
source("R/charts.R")
source("R/demo_data.R")
source("R/data_manager.R")
source("R/market_data.R")
source("R/sentiment.R")
source("R/mod_dashboard.R")
source("R/mod_portfolio.R")
source("R/mod_watchlist.R")
source("R/mod_analysis.R")
source("R/mod_sentiment.R")

# ── Bloomberg-style KPI card builder ─────────────────────────
bb_kpi <- function(title, value, subtitle = NULL, colour = "#f5a623") {
  div(class = "bb-kpi",
    div(class = "bb-kpi-title", title),
    div(class = "bb-kpi-value", style = paste0("color:", colour, ";"), value),
    if (!is.null(subtitle)) div(class = "bb-kpi-subtitle", subtitle)
  )
}

# ── Empty chart placeholder ──────────────────────────────────
bb_empty_chart <- function(msg = "No data") {
  plotly::plot_ly() |>
    plotly::layout(
      title = list(text = msg, font = list(color = "#6c757d", size = 14)),
      paper_bgcolor = "transparent",
      plot_bgcolor  = "transparent",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",

  # ── Header ──
  dashboardHeader(
    title = tags$span(
      class = "bb-logo",
      tags$span(class = "bb-logo-icon", "BB"),
      tags$span(class = "bb-logo-text", "TERMINAL")
    ),
    titleWidth = 220,
    tags$li(class = "dropdown",
      tags$div(class = "bb-header-ticker",
        textOutput("header_clock", inline = TRUE)
      )
    )
  ),

  # ── Sidebar ──
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Dashboard",  tabName = "tab_dashboard",
               icon = icon("tachometer-alt")),
      menuItem("Portfolio",  tabName = "tab_portfolio",
               icon = icon("briefcase")),
      menuItem("Watchlist",  tabName = "tab_watchlist",
               icon = icon("eye")),
      menuItem("Analysis",   tabName = "tab_analysis",
               icon = icon("search-dollar")),
      menuItem("Sentiment",  tabName = "tab_sentiment",
               icon = icon("newspaper"))
    ),
    tags$hr(style = "border-color:rgba(245,166,35,0.2); margin:10px 15px;"),
    tags$div(class = "bb-sidebar-info",
      tags$div(class = "bb-sidebar-label", "SYSTEM"),
      tags$div(class = "bb-sidebar-status",
        icon("circle", class = "bb-status-dot"),
        textOutput("system_status", inline = TRUE)
      ),
      tags$div(class = "bb-sidebar-version",
        "Bloomberg Terminal Light v2.0"
      )
    )
  ),

  # ── Body ──
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300;400;500;700&family=Inter:wght@300;400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$meta(name = "viewport",
                content = "width=device-width, initial-scale=1")
    ),

    tabItems(
      tabItem(tabName = "tab_dashboard",  dashboardTabUI("dash")),
      tabItem(tabName = "tab_portfolio",  portfolioUI("portfolio")),
      tabItem(tabName = "tab_watchlist",  watchlistUI("watchlist")),
      tabItem(tabName = "tab_analysis",   analysisUI("analysis")),
      tabItem(tabName = "tab_sentiment",  sentimentUI("sentiment"))
    )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Header clock ──
  output$header_clock <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%Y-%m-%d  %H:%M:%S")
  })

  # ── System status ──
  output$system_status <- renderText({
    invalidateLater(30000, session)
    "ONLINE"
  })

  # ── Shared refresh trigger ──
  refresh_trigger <- reactiveVal(0)

  # ── Module servers ──
  dash_refresh <- dashboardTabServer("dash")
  pf_data      <- portfolioServer("portfolio", trigger_refresh = refresh_trigger)
  wl_data      <- watchlistServer("watchlist", trigger_refresh = refresh_trigger)
  analysisServer("analysis")
  sentimentServer("sentiment")

  # When portfolio or watchlist changes, bump the refresh trigger
  observe({
    pf_data()
    isolate(refresh_trigger(refresh_trigger() + 1))
  })
  observe({
    wl_data()
    isolate(refresh_trigger(refresh_trigger() + 1))
  })
}

# ── Run ──────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
