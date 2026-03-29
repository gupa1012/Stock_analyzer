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
source("R/etf_xray.R")
source("R/mod_dashboard.R")
source("R/mod_portfolio.R")
source("R/mod_watchlist.R")
source("R/mod_analysis.R")
source("R/mod_sentiment.R")

app_mode_control_ui <- function() {
  tags$li(class = "dropdown bb-mode-dropdown",
    tags$div(class = "bb-mode-control",
      tags$span(class = "bb-mode-label", "LAYOUT"),
      tags$select(
        id = "app_mode_select",
        class = "bb-mode-select",
        tags$option(value = "auto", "Auto"),
        tags$option(value = "desktop", "Desktop"),
        tags$option(value = "mobile", "Mobile")
      )
    )
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
    app_mode_control_ui(),
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
      menuItem("Portfolio",  tabName = "tab_dashboard",
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
      tags$div(class = "bb-sidebar-layout",
        textOutput("app_layout_status", inline = TRUE)
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
                content = "width=device-width, initial-scale=1"),
      tags$script(HTML("(function () {
        function getQueryMode() {
          try {
            var params = new URLSearchParams(window.location.search || '');
            var mode = (params.get('mode') || '').toLowerCase();
            if (mode === 'auto' || mode === 'desktop' || mode === 'mobile') {
              return mode;
            }
          } catch (err) {}
          return null;
        }

        function detectMobile() {
          var userAgent = navigator.userAgent || '';
          var mobileAgent = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(userAgent);
          var narrowViewport = window.innerWidth <= 768;
          return mobileAgent || narrowViewport;
        }

        function resolveChoice() {
          var queryMode = getQueryMode();
          if (queryMode) {
            return queryMode;
          }
          try {
            var saved = window.localStorage.getItem('bbLayoutMode');
            if (saved === 'auto' || saved === 'desktop' || saved === 'mobile') {
              return saved;
            }
          } catch (err) {}
          return 'auto';
        }

        function publishState(choice, effective) {
          if (!(window.Shiny && window.Shiny.setInputValue)) {
            return;
          }

          window.Shiny.setInputValue('app_mode_choice', choice, { priority: 'event' });
          window.Shiny.setInputValue('app_mode_effective', effective, { priority: 'event' });
          window.Shiny.setInputValue('app_device_info', {
            width: window.innerWidth,
            height: window.innerHeight,
            user_agent: navigator.userAgent || '',
            touch: 'ontouchstart' in window || navigator.maxTouchPoints > 0
          }, { priority: 'event' });
        }

        function applyMode(choice) {
          var effective = choice === 'auto'
            ? (detectMobile() ? 'mobile' : 'desktop')
            : choice;
          var body = document.body;
          if (!body) {
            return;
          }

          body.classList.remove('bb-mobile-mode', 'bb-desktop-mode', 'sidebar-open');
          body.classList.add(effective === 'mobile' ? 'bb-mobile-mode' : 'bb-desktop-mode');
          body.classList.toggle('sidebar-collapse', effective === 'mobile');
          body.setAttribute('data-layout-choice', choice);
          body.setAttribute('data-layout-effective', effective);

          var select = document.getElementById('app_mode_select');
          if (select && select.value !== choice) {
            select.value = choice;
          }

          try {
            window.localStorage.setItem('bbLayoutMode', choice);
          } catch (err) {}

          publishState(choice, effective);
        }

        function bindControl() {
          var select = document.getElementById('app_mode_select');
          if (!select || select.dataset.bound === '1') {
            return;
          }
          select.dataset.bound = '1';
          select.addEventListener('change', function (event) {
            applyMode((event.target.value || 'auto').toLowerCase());
          });
        }

        document.addEventListener('DOMContentLoaded', function () {
          bindControl();
          applyMode(resolveChoice());
        });

        document.addEventListener('shiny:connected', function () {
          bindControl();
          applyMode(resolveChoice());
        });

        window.addEventListener('resize', function () {
          var select = document.getElementById('app_mode_select');
          var choice = select ? (select.value || 'auto') : resolveChoice();
          if (choice === 'auto') {
            applyMode('auto');
          } else {
            publishState(choice, choice);
          }
        });
      })();"))
    ),

    div(class = "bb-app-shell",
      tabItems(
        tabItem(tabName = "tab_dashboard",  dashboardTabUI("dash")),
        tabItem(tabName = "tab_watchlist",  watchlistUI("watchlist")),
        tabItem(tabName = "tab_analysis",   analysisUI("analysis")),
        tabItem(tabName = "tab_sentiment",  sentimentUI("sentiment"))
      )
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

  output$app_layout_status <- renderText({
    choice <- input$app_mode_choice
    effective <- input$app_mode_effective

    if (is.null(choice) || !nzchar(choice)) choice <- "auto"
    if (is.null(effective) || !nzchar(effective)) effective <- "desktop"

    if (identical(choice, "auto")) {
      paste("LAYOUT AUTO ·", toupper(effective))
    } else {
      paste("LAYOUT FIXED ·", toupper(effective))
    }
  })

  # ── Shared refresh trigger ──
  refresh_trigger <- reactiveVal(0)

  # ── Module servers ──
  dash_data <- dashboardTabServer("dash")
  wl_data   <- watchlistServer("watchlist", trigger_refresh = refresh_trigger)
  analysisServer("analysis")
  sentimentServer("sentiment")

  # When watchlist changes, bump the refresh trigger
  observe({
    wl_data()
    isolate(refresh_trigger(refresh_trigger() + 1))
  })
}

# ── Run ──────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
