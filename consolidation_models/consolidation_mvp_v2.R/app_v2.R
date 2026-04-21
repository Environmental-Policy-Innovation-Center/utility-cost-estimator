# ============================================================
#  Water System Consolidation Tool  —  v3 (simplified)
#  Dependencies: shiny, shinydashboard, shinyjs, leaflet,
#                aws.s3, tidyverse, sf, stringr, DT, plotly, scales
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(aws.s3)
library(tidyverse)
library(sf)
library(stringr)
library(DT)
library(plotly)
library(scales)

source("mod_landing.R")

# ── 0. Static lookups --------------------------------------------------------

owner_types <- c("All", "Federal", "Local", "Native American",
                 "Private", "Public", "State")

state_choices <- c(
  "AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID",
  "IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
  "ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD",
  "TN","TX","UT","VA","VT","WA","WI","WV","WY"
)

# Approximate bounding boxes: c(lng_min, lat_min, lng_max, lat_max)
state_bbox <- list(
  AK = c(-179.1, 51.2, -129.9, 71.4), AL = c(-88.5, 30.1, -84.9, 35.0),
  AR = c(-94.6,  33.0, -89.6,  36.5), AZ = c(-114.8, 31.3, -109.0, 37.0),
  CA = c(-124.4, 32.5, -114.1, 42.0), CO = c(-109.1, 36.9, -102.0, 41.0),
  CT = c(-73.7,  40.9, -71.8,  42.1), DC = c(-77.1,  38.8, -76.9,  39.0),
  DE = c(-75.8,  38.4, -75.0,  39.8), FL = c(-87.6,  24.4, -80.0,  31.0),
  GA = c(-85.6,  30.4, -80.8,  35.0), HI = c(-160.2, 18.9, -154.8, 22.2),
  IA = c(-96.6,  40.4, -90.1,  43.5), ID = c(-117.2, 42.0, -111.0, 49.0),
  IL = c(-91.5,  36.9, -87.0,  42.5), IN = c(-88.1,  37.8, -84.8,  41.8),
  KS = c(-102.1, 36.9, -94.6,  40.0), KY = c(-89.6,  36.5, -81.9,  39.1),
  LA = c(-94.0,  28.9, -88.8,  33.0), MA = c(-73.5,  41.2, -69.9,  42.9),
  MD = c(-79.5,  37.9, -75.0,  39.7), ME = c(-71.1,  43.0, -67.0,  47.5),
  MI = c(-90.4,  41.7, -82.4,  48.3), MN = c(-97.2,  43.5, -89.5,  49.4),
  MO = c(-95.8,  35.9, -89.1,  40.6), MS = c(-91.7,  30.1, -88.1,  35.0),
  MT = c(-116.1, 44.4, -104.0, 49.0), NC = c(-84.3,  33.8, -75.5,  36.6),
  ND = c(-104.0, 45.9, -96.5,  49.0), NE = c(-104.1, 40.0, -95.3,  43.0),
  NH = c(-72.6,  42.7, -70.7,  45.3), NJ = c(-75.6,  38.9, -73.9,  41.4),
  NM = c(-109.1, 31.3, -103.0, 37.0), NV = c(-120.0, 35.0, -114.0, 42.0),
  NY = c(-79.8,  40.5, -71.9,  45.0), OH = c(-84.8,  38.4, -80.5,  42.3),
  OK = c(-103.0, 33.6, -94.4,  37.0), OR = c(-124.6, 42.0, -116.5, 46.3),
  PA = c(-80.5,  39.7, -74.7,  42.3), RI = c(-71.9,  41.1, -71.1,  42.0),
  SC = c(-83.4,  32.0, -78.5,  35.2), SD = c(-104.1, 42.5, -96.4,  45.9),
  TN = c(-90.3,  34.9, -81.6,  36.7), TX = c(-106.6, 25.8, -93.5,  36.5),
  UT = c(-114.1, 37.0, -109.0, 42.0), VA = c(-83.7,  36.5, -75.2,  39.5),
  VT = c(-73.4,  42.7, -71.5,  45.0), WA = c(-124.7, 45.5, -116.9, 49.0),
  WI = c(-92.9,  42.5, -86.8,  47.1), WV = c(-82.6,  37.2, -77.7,  40.6),
  WY = c(-111.1, 40.9, -104.1, 45.0)
)

# ── 1. Model functions -------------------------------------------------------

load_state_data <- function(state) {
  state_lc <- tolower(state)

  neighbors <- s3read_using(
    read.csv,
    object = sprintf("s3://tech-team-data/consolidation/mvp/state_data/%s_neighbors.csv", state_lc)
  )

  sys_geo <- s3read_using(
    st_read,
    object = sprintf("s3://tech-team-data/consolidation/mvp/state_data/%s_sys_geo.geojson", state_lc),
    quiet  = TRUE
  ) %>% st_transform(4326)

  list(neighbors = neighbors, sys_geo = sys_geo)
}

filter_pairs <- function(neighbors, cons_cfg, rec_cfg) {
  neighbors %>%
    # ── Consolidating side ──
    filter(as.numeric(health_viols_10yr) >= cons_cfg$health_viols_10yr |
             is.na(as.numeric(health_viols_10yr))) %>%
    filter(open_health_viol          == cons_cfg$open_health_viol) %>%
    filter(population_served_count   <= cons_cfg$pop_served) %>%
    { if (cons_cfg$owner_type != "All") filter(., owner_type == cons_cfg$owner_type) else . } %>%
    # ── Receiving side ──
    filter(as.numeric(rec_health_viols_10yr) <= rec_cfg$health_viols_10yr |
             is.na(as.numeric(rec_health_viols_10yr))) %>%
    filter(rec_open_health_viol        == rec_cfg$open_health_viol) %>%
    filter(rec_population_served_count >= rec_cfg$pop_served) %>%
    { if (rec_cfg$owner_type != "All") filter(., rec_owner_type == rec_cfg$owner_type) else . } %>%
    # ── Distance cutoff ──
    filter(rec_travel_distance <= rec_cfg$cutoff | rec_overlap == TRUE)
}

get_costs <- function(pairs,
                      cost_per_mile, connection_fee, service_line_fee,
                      admin_cost, contingency_const, planning_constuction_const,
                      engineering_services_const, inflation_const,
                      regional_multiplier_const) {
  pairs %>%
    mutate(
      new_source_cost   = ifelse(rec_num_facilities > 0, 0, 1238933),
      pipe_line_cost    = rec_travel_distance * cost_per_mile,
      connection_fees   = service_connections_count * connection_fee,
      service_line_cost = service_connections_count * service_line_fee,
      admin_costs       = (pipe_line_cost + service_line_cost) * admin_cost,
      CEQA_cost         = ifelse(rec_overlap == TRUE, 25000, 100000)
    ) %>%
    rowwise() %>%
    mutate(
      total_capital_costs  = sum(c_across(new_source_cost:CEQA_cost)),
      contingency          = total_capital_costs * contingency_const,
      planning_constuction = total_capital_costs * planning_constuction_const,
      engineering_services = total_capital_costs * engineering_services_const,
      inflation            = total_capital_costs * inflation_const,
      regional_multiplier  = total_capital_costs * regional_multiplier_const
    ) %>%
    rowwise() %>%
    mutate(
      total_markup       = sum(c_across(contingency:regional_multiplier)),
      total_project_cost = total_markup + total_capital_costs
    ) %>%
    ungroup()
}

# ── 2. UI --------------------------------------------------------------------

ui <- dashboardPage(

  # ── Header (disabled — replaced by custom top nav) ──
  dashboardHeader(disable = TRUE),

  # ── Sidebar (hidden — kept for tab routing only) ──
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",       tabName = "home"),
      menuItem("Estimator",  tabName = "tool")
    )
  ),

  # ── Body ──
  dashboardBody(
    useShinyjs(),

    tags$head(
      includeCSS("www/styles.css")
    ),

    # ── Custom top navbar ──
    tags$nav(
      class = "topnav",
      tags$a(
        id = "nav-home", href = "#", class = "topnav-brand",
        HTML('<svg width="24" height="24" viewBox="0 0 24 24" fill="none">
               <circle cx="12" cy="12" r="11" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
               <path d="M5 17c2-6 4-10 7-10s5 4 7 10" stroke="#0e8a7d" stroke-width="2" stroke-linecap="round"/>
               <circle cx="12" cy="6" r="2" fill="#0e8a7d"/>
               <path d="M8 20h8" stroke="rgba(255,255,255,0.4)" stroke-width="1.5" stroke-linecap="round"/>
             </svg>'),
        "Physical Consolidation Cost Estimate Tool"
      ),
      tags$div(
        class = "topnav-links",
        tags$a(id = "nav-how",      href = "#", "How It Works"),
        tags$a(id = "nav-models",   href = "#", "Models"),
        tags$a(id = "nav-method",   href = "#", "Methodology"),
        tags$a(id = "nav-launch",   href = "#", class = "topnav-btn", "Launch Tool")
      )
    ),

    # Tab content
    tabItems(
      tabItem(tabName = "home", landingUI("landing")),

      tabItem(tabName = "tool",
        div(class = "tool-content",
          sidebarLayout(
            sidebarPanel(
              width = 3,

              # ── Sticky Run Example banner ──
              div(class = "example-sticky",
                actionButton(
                  "btn_example",
                  tagList(icon("play"), "Run Example"),
                  class = "btn-example-run"
                ),
                div(class = "example-sub", "California \u00b7 default settings")
              ),

              # ── Wizard step indicator ──
              uiOutput("wizard_header"),

              # ── Step 1: Select State ──
              div(id = "wizard-step-1",
                div(class = "filter-row",
                  tags$label("State"),
                  div(class = "filter-input",
                    selectInput("state", NULL, choices = state_choices, selected = "CA"))
                )
              ),

              # ── Step 2: Define Systems ──
              shinyjs::hidden(div(id = "wizard-step-2",
                div(class = "filter-section-label", "Joining System"),
                div(class = "filter-row",
                  tags$label("Owner Type"),
                  div(class = "filter-input",
                    selectInput("cons_owner", NULL, choices = owner_types, selected = "All"))),
                div(class = "filter-row",
                  tags$label("Min Health Violations (10yr)"),
                  div(class = "filter-input",
                    numericInput("cons_viols", NULL, value = 1, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Max Population Served"),
                  div(class = "filter-input",
                    numericInput("cons_max_pop", NULL, value = 1000, min = 0, step = 100))),
                div(class = "filter-row",
                  tags$label("Open Health Violation"),
                  div(class = "filter-input",
                    selectInput("cons_open_viol", NULL, choices = c("Yes","No"), selected = "No"))),

                tags$hr(style = "margin: 8px 0;"),
                div(class = "filter-section-label", "Receiving System"),
                div(class = "filter-row",
                  tags$label("Owner Type"),
                  div(class = "filter-input",
                    selectInput("rec_owner", NULL, choices = owner_types, selected = "All"))),
                div(class = "filter-row",
                  tags$label("Max Health Violations (10yr)"),
                  div(class = "filter-input",
                    numericInput("rec_viols", NULL, value = 1, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Min Population Served"),
                  div(class = "filter-input",
                    numericInput("rec_min_pop", NULL, value = 10000, min = 0, step = 500))),
                div(class = "filter-row",
                  tags$label("Open Health Violation"),
                  div(class = "filter-input",
                    selectInput("rec_open_viol", NULL, choices = c("Yes","No"), selected = "No"))),

                tags$hr(style = "margin: 8px 0;"),
                div(class = "filter-row",
                  tags$label("Distance Cutoff (miles)"),
                  div(class = "filter-input",
                    numericInput("cutoff", NULL, value = 3, min = .1, max = 5)))
              )),

              # ── Step 3: Cost Parameters ──
              shinyjs::hidden(div(id = "wizard-step-3",
                div(class = "filter-row",
                  tags$label("Cost per Mile ($)"),
                  div(class = "filter-input",
                    numericInput("cost_per_mile", NULL, value = 1000000, min = 0))),
                div(class = "filter-row",
                  tags$label("Per Customer Connection Fee ($)"),
                  div(class = "filter-input",
                    numericInput("connection_fee", NULL, value = 4000, min = 0))),
                div(class = "filter-row",
                  tags$label("Service Line Fee ($)"),
                  div(class = "filter-input",
                    numericInput("service_line", NULL, value = 6200, min = 0))),
                div(class = "filter-row",
                  tags$label("Admin Cost (%)"),
                  div(class = "filter-input",
                    numericInput("admin_cost", NULL, value = 15, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Contingency (%)"),
                  div(class = "filter-input",
                    numericInput("contingency", NULL, value = 20, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Planning & Const. (%)"),
                  div(class = "filter-input",
                    numericInput("planning", NULL, value = 10, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Engineering (%)"),
                  div(class = "filter-input",
                    numericInput("engineering", NULL, value = 15, min = 0, step = 1))),
                div(class = "filter-row",
                  tags$label("Inflation (%)"),
                  div(class = "filter-input",
                    numericInput("inflation", NULL, value = 3.1, min = 0, step = 0.1))),
                div(class = "filter-row",
                  tags$label("Regional Multiplier (%)"),
                  div(class = "filter-input",
                    numericInput("regional", NULL, value = 10, min = 0, step = 1)))
              )),

              # ── Wizard navigation ──
              uiOutput("wizard_nav"),

              uiOutput("status_ui")
            ),

            mainPanel(
              width = 9,
              leafletOutput("map", height = "420px"),
              br(),
              tabsetPanel(
                id = "tabs",
                tabPanel("Potential Joining and Receiving Systems",        br(), DTOutput("results_table")),
                tabPanel("Estimated Cost Chart",   br(), uiOutput("chart_controls_ui"), plotlyOutput("cost_chart", height = "360px")),
                tabPanel("Estimated Cost Summary", br(), uiOutput("cost_summary_ui"))
              )
            )
          )
        )
      )
    ),

    # Fixed footer banner
    tags$div(class = "app-footer",
      tags$span(class = "footer-icon", icon("droplet")),
      tags$span(
        tags$span(class = "footer-label", "Source | "),
        tags$span(class = "footer-source", "EPA SDWIS & EPIC Engineering Cost Model"),
        tags$span(class = "footer-label", " | Water System Consolidation Estimator")
      )
    )
  )
)

# ── 3. Server ----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    neighbors     = NULL,
    sys_geo       = NULL,
    filtered      = NULL,
    costs         = NULL,
    selected_cons = NULL,
    selected_pair = NULL
  )

  # Landing page module
  landingServer("landing", parent_session = session)

  # ── Run Example: CA + defaults ─────────────────────────────────────────────
  observeEvent(input$btn_example, {
    updateTabItems(session, "sidebar", "tool")

    # Reset all inputs to example defaults
    updateSelectInput(session,  "state",         selected = "CA")
    updateSelectInput(session,  "cons_owner",    selected = "All")
    updateNumericInput(session, "cons_viols",    value = 1)
    updateNumericInput(session, "cons_max_pop",  value = 1000)
    updateSelectInput(session,  "cons_open_viol",selected = "No")
    updateSelectInput(session,  "rec_owner",     selected = "All")
    updateNumericInput(session, "rec_viols",     value = 1)
    updateNumericInput(session, "rec_min_pop",   value = 10000)
    updateSelectInput(session,  "rec_open_viol", selected = "No")
    updateNumericInput(session, "cutoff",        value = 3)
    updateNumericInput(session, "cost_per_mile", value = 1000000)
    updateNumericInput(session, "connection_fee",value = 4000)
    updateNumericInput(session, "service_line",  value = 6200)
    updateNumericInput(session, "admin_cost",    value = 15)
    updateNumericInput(session, "contingency",   value = 20)
    updateNumericInput(session, "planning",      value = 10)
    updateNumericInput(session, "engineering",   value = 15)
    updateNumericInput(session, "inflation",     value = 3.1)
    updateNumericInput(session, "regional",      value = 10)

    withProgress(message = "Running example: loading California...", value = 0.1, {
      tryCatch({
        dat <- load_state_data("CA")
        rv$neighbors <- dat$neighbors
        rv$sys_geo   <- dat$sys_geo
        setProgress(0.4, message = "Filtering pairs...")

        cons_cfg <- list(open_health_viol = "No", health_viols_10yr = 1,
                         owner_type = "All", pop_served = 1000)
        rec_cfg  <- list(open_health_viol = "No", health_viols_10yr = 1,
                         owner_type = "All", pop_served = 10000, cutoff = 3)
        filtered <- filter_pairs(rv$neighbors, cons_cfg, rec_cfg)

        if (nrow(filtered) == 0) {
          showNotification("Example: no pairs matched — try relaxing filters.", type = "warning")
          return()
        }

        rv$filtered <- filtered
        setProgress(0.7, message = "Calculating costs...")

        rv$costs <- get_costs(
          filtered,
          cost_per_mile = 1000000, connection_fee = 4000, service_line_fee = 6200,
          admin_cost = 0.15, contingency_const = 0.20, planning_constuction_const = 0.10,
          engineering_services_const = 0.15, inflation_const = 0.031,
          regional_multiplier_const = 0.10
        )
        rv$selected_cons <- unique(rv$costs$pwsid)[1]
        rv$selected_pair <- NULL
        setProgress(1)
        goto_step(3)
        showNotification(
          sprintf("Example loaded: %d pairs across %d systems in CA.",
                  nrow(rv$costs), n_distinct(rv$costs$pwsid)),
          type = "message", duration = 4
        )
      }, error = function(e) {
        showNotification(paste("Example failed:", e$message), type = "error", duration = 8)
      })
    })
  })

  # ── Top-navbar link handlers ──
  scroll_to_section <- function(section_id) {
    shinyjs::runjs(sprintf("
      (function() {
        var homeTab = document.querySelector('a[data-value=\"home\"]');
        if (homeTab && !homeTab.parentElement.classList.contains('active')) {
          homeTab.click();
          setTimeout(function() {
            var el = document.getElementById('%s');
            if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
          }, 300);
        } else {
          var el = document.getElementById('%s');
          if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
        }
      })();
    ", section_id, section_id))
  }

  shinyjs::onclick("nav-home", {
    updateTabItems(session, "sidebar", "home")
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })

  shinyjs::onclick("nav-how",    scroll_to_section("landing-how_section"))
  shinyjs::onclick("nav-models", scroll_to_section("landing-models_section"))
  shinyjs::onclick("nav-method", scroll_to_section("landing-methodology_anchor"))
  shinyjs::onclick("nav-launch", updateTabItems(session, "sidebar", "tool"))

  # ── Wizard state ──────────────────────────────────────────────────────────
  wizard_step <- reactiveVal(1)

  step_titles <- c("Select State", "Define Systems", "Cost Parameters")

  output$wizard_header <- renderUI({
    s <- wizard_step()
    div(class = "wizard-header",
      div(class = "wizard-dots",
        lapply(1:3, function(i) {
          cls <- if (i == s) "wizard-dot active" else if (i < s) "wizard-dot done" else "wizard-dot"
          div(class = cls)
        })
      ),
      div(class = "wizard-title", step_titles[s])
    )
  })

  output$wizard_nav <- renderUI({
    s <- wizard_step()
    div(class = "wizard-nav",
      if (s > 1) actionButton("btn_back", "\u2190 Back",   class = "btn-secondary btn-sm wizard-btn"),
      if (s < 3) actionButton("btn_next", "Next \u2192",   class = "btn-primary   btn-sm wizard-btn"),
      if (s == 3) actionButton("btn_run", "Run Analysis",  class = "btn-danger    btn-sm wizard-btn")
    )
  })

  goto_step <- function(s) {
    shinyjs::hide(paste0("wizard-step-", wizard_step()))
    shinyjs::show(paste0("wizard-step-", s))
    wizard_step(s)
  }

  observeEvent(input$btn_back, {
    goto_step(wizard_step() - 1)
  })

  # Next on step 1: load state then advance
  observeEvent(input$btn_next, {
    if (wizard_step() == 1) {
      withProgress(message = sprintf("Loading %s data from S3...", input$state), value = 0.2, {
        tryCatch({
          dat <- load_state_data(input$state)
          rv$neighbors     <- dat$neighbors
          rv$sys_geo       <- dat$sys_geo
          rv$filtered      <- NULL
          rv$costs         <- NULL
          rv$selected_cons <- NULL
          rv$selected_pair <- NULL
          setProgress(1)
          showNotification(
            sprintf("Loaded %d candidate pairs for %s.", nrow(dat$neighbors), input$state),
            type = "message", duration = 3
          )
          leafletProxy("map") %>% clearShapes() %>% clearControls()
          goto_step(2)
          # Delay fitBounds until after goto_step's layout reflow settles
          local({
            st <- input$state
            shinyjs::delay(200, {
              bb <- state_bbox[[st]]
              if (!is.null(bb)) leafletProxy("map") %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
            })
          })
        }, error = function(e) {
          showNotification(paste("S3 load failed:", e$message), type = "error", duration = 8)
        })
      })

    } else if (wizard_step() == 2) {
      req(rv$neighbors)
      withProgress(message = "Filtering pairs...", {
        cons_cfg <- list(
          open_health_viol  = input$cons_open_viol,
          health_viols_10yr = input$cons_viols,
          owner_type        = input$cons_owner,
          pop_served        = input$cons_max_pop
        )
        rec_cfg <- list(
          open_health_viol  = input$rec_open_viol,
          health_viols_10yr = input$rec_viols,
          owner_type        = input$rec_owner,
          pop_served        = input$rec_min_pop,
          cutoff            = input$cutoff
        )
        filtered <- filter_pairs(rv$neighbors, cons_cfg, rec_cfg)
        if (nrow(filtered) == 0) {
          showNotification("No pairs match — try relaxing filters.", type = "warning")
          return()
        }
        rv$filtered      <- filtered
        rv$costs         <- NULL
        rv$selected_cons <- NULL
        rv$selected_pair <- NULL
        showNotification(
          sprintf("%d joining | %d receiving systems (%d pairs).",
                  n_distinct(filtered$pwsid), n_distinct(filtered$rec_pwsid), nrow(filtered)),
          type = "message", duration = 4
        )
        goto_step(3)
      })
    }
  })

  # ── Step 3: Apply costs ────────────────────────────────────────────────────
  observeEvent(input$btn_run, {
    req(rv$filtered)
    withProgress(message = "Calculating costs...", value = 0.3, {
      rv$costs <- get_costs(
        rv$filtered,
        cost_per_mile              = input$cost_per_mile,
        connection_fee             = input$connection_fee,
        service_line_fee           = input$service_line,
        admin_cost                 = input$admin_cost        / 100,
        contingency_const          = input$contingency       / 100,
        planning_constuction_const = input$planning          / 100,
        engineering_services_const = input$engineering       / 100,
        inflation_const            = input$inflation         / 100,
        regional_multiplier_const  = input$regional          / 100
      )
      rv$selected_cons <- unique(rv$costs$pwsid)[1]
      rv$selected_pair <- NULL
      setProgress(1)
      showNotification(
        sprintf("Done! %d pairs across %d consolidating systems.",
                nrow(rv$costs), n_distinct(rv$costs$pwsid)),
        type = "message", duration = 5
      )
    })
  })

  # ── Status badge ───────────────────────────────────────────────────────────
  output$status_ui <- renderUI({
    if (!is.null(rv$costs) && "total_project_cost" %in% names(rv$costs)) {
      div(style = "color:green; font-size:12px; margin-top:6px;",
          icon("check-circle"), sprintf(" %d pairs ready", nrow(rv$costs)))
    } else if (!is.null(rv$filtered)) {
      div(style = "color:orange; font-size:12px; margin-top:6px;",
          icon("hourglass-half"),
          sprintf(" %d pairs filtered — run costs", nrow(rv$filtered)))
    } else if (!is.null(rv$neighbors)) {
      div(style = "color:#555; font-size:12px; margin-top:6px;",
          icon("map"), sprintf(" %d raw pairs loaded", nrow(rv$neighbors)))
    }
  })

  # ── Map base tile (rendered once) ──────────────────────────────────────────
  output$map <- renderLeaflet({
    bb <- state_bbox[["CA"]]
    leaflet() %>%
      addMapPane("receiving",     zIndex = 410) %>%
      addMapPane("consolidating", zIndex = 420) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  })

  # ── Zoom to selected state (no data load) ──────────────────────────────────
  observeEvent(input$state, {
    bb <- state_bbox[[input$state]]
    req(bb)
    leafletProxy("map") %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  }, ignoreInit = TRUE)

  # ── Draw ALL base polygons once when costs are computed ────────────────────
  observeEvent(rv$costs, {
    req(rv$costs, rv$sys_geo)
    costs <- rv$costs

    cons_ids <- unique(costs$pwsid)
    rec_ids  <- unique(costs$rec_pwsid)

    cons_info <- costs %>%
      distinct(pwsid, pws_name, population_served_count,
               service_connections_count, owner_type, health_viols_10yr)

    rec_info <- costs %>%
      distinct(rec_pwsid, rec_pws_name, rec_population_served_count,
               rec_owner_type, rec_health_viols_10yr)

    cons_sf <- rv$sys_geo %>%
      filter(pwsid %in% cons_ids) %>%
      left_join(cons_info, by = "pwsid")

    rec_sf <- rv$sys_geo %>%
      filter(pwsid %in% rec_ids) %>%
      left_join(rec_info, by = c("pwsid" = "rec_pwsid"))

    bbox <- st_bbox(bind_rows(
      cons_sf %>% select(geometry)#,
    #  rec_sf  %>% select(geometry)
    ))

    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = cons_sf, group = "base",
        fillColor = "green", fillOpacity = 0.45,
        color = "darkgreen", weight = 1.5,
        options = pathOptions(pane = "consolidating"),
        layerId = ~pwsid,
        popup = ~paste0(
          "<b>", pws_name, "</b><br>", pwsid,
          "<br>Pop: ", scales::comma(population_served_count),
          "<br>Owner: ", owner_type,
          "<br>Violations (10yr): ", health_viols_10yr,
          "<br><i>Consolidating</i>"
        )
      ) %>%
      addPolygons(
        data = rec_sf, group = "base",
        fillColor = "steelblue", fillOpacity = 0.35,
        color = "navy", weight = 1.5,
        options = pathOptions(pane = "receiving"),
        layerId = ~pwsid,
        popup = ~paste0(
          "<b>", rec_pws_name, "</b><br>", pwsid,
          "<br>Pop: ", scales::comma(rec_population_served_count),
          "<br>Owner: ", rec_owner_type,
          "<br>Violations (10yr): ", rec_health_viols_10yr,
          "<br><i>Receiving</i>"
        )
      ) %>%
      addLegend("bottomright",
                colors  = c("green", "steelblue"),
                labels  = c("Consolidating", "Receiving"),
                opacity = 0.7) %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })

  # ── Highlight selected system + its partners ───────────────────────────────
  observe({
    req(rv$costs, rv$sys_geo, rv$selected_cons)
    costs  <- rv$costs
    sel_id <- rv$selected_cons

    sel_info <- costs %>%
      filter(pwsid == sel_id) %>%
      slice(1) %>%
      select(pwsid, pws_name, population_served_count, owner_type, health_viols_10yr)

    sel_sf <- rv$sys_geo %>%
      filter(pwsid == sel_id) %>%
      left_join(sel_info, by = "pwsid")

    partner_ids <- costs %>% filter(pwsid == sel_id) %>% pull(rec_pwsid) %>% unique()

    partner_info <- costs %>%
      filter(pwsid == sel_id) %>%
      distinct(rec_pwsid, rec_pws_name, rec_population_served_count,
               rec_owner_type, rec_health_viols_10yr)

    partner_sf <- rv$sys_geo %>%
      filter(pwsid %in% partner_ids) %>%
      left_join(partner_info, by = c("pwsid" = "rec_pwsid"))

    bbox <- st_bbox(bind_rows(
      sel_sf     %>% select(geometry)#,
     # partner_sf %>% select(geometry)
    ))

    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = partner_sf, group = "highlight",
        fillColor = "#1e90ff", fillOpacity = 0.7,
        color = "darkblue", weight = 2.5,
        options = pathOptions(pane = "receiving"),
        popup = ~paste0(
          "<b>", rec_pws_name, "</b><br>", pwsid,
          "<br>Pop: ", scales::comma(rec_population_served_count),
          "<br>Owner: ", rec_owner_type,
          "<br><i>Receiving</i>"
        )
      ) %>%
      addPolygons(
        data = sel_sf, group = "highlight",
        fillColor = "#2ecc71", fillOpacity = 0.85,
        color = "#145a32", weight = 3,
        options = pathOptions(pane = "consolidating"),
        popup = ~paste0(
          "<b>", pws_name, "</b><br>", pwsid,
          "<br>Pop: ", scales::comma(population_served_count),
          "<br>Owner: ", owner_type,
          "<br><i>Consolidating (selected)</i>"
        )
      ) %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })

  # ── Map click → update selection ───────────────────────────────────────────
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    req(id, rv$costs)
    if (id %in% rv$costs$pwsid) {
      rv$selected_cons <- id
      rv$selected_pair <- NULL
    }
  })

  # ── Results table ──────────────────────────────────────────────────────────
  selected_pairs_df <- reactive({
    req(rv$costs, "total_project_cost" %in% names(rv$costs))
    rv$costs %>%
      select(
        pwsid, pws_name,
        rec_pwsid, rec_pws_name,
        rec_centroid_distance, rec_travel_distance, rec_overlap,
        service_connections_count,
        total_capital_costs, total_markup, total_project_cost
      )
  })

  output$results_table <- renderDT({
    req(selected_pairs_df())
    selected_pairs_df() %>%
      mutate(
        across(c(total_capital_costs, total_markup, total_project_cost), dollar),
        across(c(rec_centroid_distance, rec_travel_distance), ~round(., 2))
      ) %>%
      rename(
        "Joining System ID"        = pwsid,
        "Joining System Name"         = pws_name,
        "Receving System ID"         = rec_pwsid,
        "Receiving System Name"          = rec_pws_name,
        "Est. Total Cost"         = total_project_cost,
        "Distance Center to Center (mi)" = rec_centroid_distance,
        "Travel Dist (mi)"   = rec_travel_distance,
        "Do they overlap?"            = rec_overlap,
        "Number of Service Connections"        = service_connections_count,
        "Est. Consolidating Capital Costs"      = total_capital_costs,
        "Est. Markup"             = total_markup
      ) %>%
      datatable(selection = list(mode = "single", selected = 1), rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  observeEvent(input$results_table_rows_selected, {
    req(rv$costs, rv$sys_geo)
    row_idx  <- input$results_table_rows_selected
    pair_row <- selected_pairs_df()[row_idx, ]

    rv$selected_cons <- pair_row$pwsid
    rv$selected_pair <- pair_row$rec_pwsid
  })

  # ── Cost chart controls ────────────────────────────────────────────────────
  output$chart_controls_ui <- renderUI({
    req(rv$costs, rv$selected_cons)
    pairs   <- rv$costs %>% filter(pwsid == rv$selected_cons)
    n_pairs <- length(unique(pairs$rec_pwsid))
    if (n_pairs <= 1) return(NULL)

    pair_choices <- pairs %>%
      distinct(rec_pwsid, rec_pws_name) %>%
      { setNames(.$rec_pwsid, .$rec_pws_name) }

    div(
      style = "display:flex; align-items:center; gap:20px; margin-bottom:6px;",
      radioButtons("chart_view", NULL,
                   choices  = c("All Pairs" = "all", "Single Pair" = "single"),
                   selected = "all", inline = TRUE),
      conditionalPanel(
        condition = "input.chart_view === 'single'",
        selectInput("chart_pair", NULL, choices = pair_choices, width = "280px")
      )
    )
  })

  # ── Cost chart ─────────────────────────────────────────────────────────────
  output$cost_chart <- renderPlotly({
    req(rv$costs, rv$selected_cons, "total_project_cost" %in% names(rv$costs))

    cost_cols <- c("new_source_cost", "pipe_line_cost", "connection_fees",
                   "service_line_cost", "admin_costs", "CEQA_cost",
                   "contingency", "planning_constuction", "engineering_services",
                   "inflation", "regional_multiplier")

    component_labels <- c(
      new_source_cost      = "New Source",
      pipe_line_cost       = "Pipeline",
      connection_fees      = "Connections",
      service_line_cost    = "Service Lines",
      admin_costs          = "Admin",
      CEQA_cost            = "Permits/CEQA",
      contingency          = "Contingency",
      planning_constuction = "Planning & CM",
      engineering_services = "Engineering",
      inflation            = "Inflation",
      regional_multiplier  = "Regional Adj."
    )

    view_mode <- if (!is.null(input$chart_view)) input$chart_view else "all"
    pair_id   <- if (!is.null(input$chart_pair)) input$chart_pair else NULL

    plot_df <- rv$costs %>%
      filter(pwsid == rv$selected_cons) %>%
      { if (view_mode == "single" && !is.null(pair_id)) filter(., rec_pwsid == pair_id) else . } %>%
      select(rec_pws_name, all_of(cost_cols)) %>%
      pivot_longer(-rec_pws_name, names_to = "component", values_to = "cost") %>%
      mutate(
        component    = factor(component_labels[component], levels = component_labels),
        rec_pws_name = stringr::str_wrap(rec_pws_name, 20)
      )

    p <- ggplot(
      plot_df,
      aes(
        x    = component,
        y    = cost,
        fill = rec_pws_name,
        text = paste0(rec_pws_name, "\n", component, ": ", scales::dollar(cost))
      )
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      scale_y_continuous(
        labels = scales::label_dollar(scale_cut = scales::cut_short_scale()),
        expand = expansion(mult = c(0, 0.06))
      ) +
      scale_fill_brewer(palette = "Blues", direction = 1) +
      labs(x = NULL, y = "Cost", fill = "Receiving System") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(angle = 35, hjust = 1, size = 9),
        legend.position    = "right",
        legend.key.size    = unit(0.45, "cm"),
        legend.text        = element_text(size = 8),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.background   = element_rect(fill = "white", color = NA)
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(font = list(size = 10)),
        margin = list(b = 80, r = 20)
      )
  })

  # ── Summary panel ──────────────────────────────────────────────────────────
  output$cost_summary_ui <- renderUI({
    req(rv$costs, rv$selected_cons, "total_project_cost" %in% names(rv$costs))

    pairs <- rv$costs %>%
      filter(pwsid == rv$selected_cons) %>%
      arrange(total_project_cost)

    header <- div(
      style = "background:#1a5276; color:white; padding:10px 14px;
                border-radius:6px 6px 0 0; margin-bottom:0;",
      tags$b(pairs$pws_name[1]), " — ", tags$small(rv$selected_cons),
      tags$span(style = "float:right;",
                sprintf("%d potential receiving system(s)", nrow(pairs)))
    )

    rows <- lapply(seq_len(nrow(pairs)), function(i) {
      p <- pairs[i, ]
      div(
        style = "border:1px solid #dee2e6; border-radius:4px;
                  padding:10px; margin-bottom:8px; background:#fff;",
        fluidRow(
          column(4,
                 tags$b(p$rec_pws_name), br(),
                 tags$small(p$rec_pwsid), br(),
                 tags$small(sprintf("Travel: %.2f mi | Overlap: %s",
                                    p$rec_travel_distance,
                                    ifelse(p$rec_overlap, "Yes", "No")))
          ),
          column(8,
                 fluidRow(
                   column(4, div(style="font-size:11px;color:#555;","New Source"),
                          div(style="font-weight:600;", dollar(p$new_source_cost))),
                   column(4, div(style="font-size:11px;color:#555;","Pipeline"),
                          div(style="font-weight:600;", dollar(p$pipe_line_cost))),
                   column(4, div(style="font-size:11px;color:#555;","Connections"),
                          div(style="font-weight:600;", dollar(p$connection_fees)))
                 ),
                 fluidRow(
                   column(4, div(style="font-size:11px;color:#555;","Service Line"),
                          div(style="font-weight:600;", dollar(p$service_line_cost))),
                   column(4, div(style="font-size:11px;color:#555;","Admin"),
                          div(style="font-weight:600;", dollar(p$admin_costs))),
                   column(4, div(style="font-size:11px;color:#555;","CEQA"),
                          div(style="font-weight:600;", dollar(p$CEQA_cost)))
                 ),
                 hr(style="margin:6px 0;"),
                 fluidRow(
                   column(4, div(style="font-size:11px;color:#555;","Capital Total"),
                          div(style="font-weight:700;color:#c0392b;",
                              dollar(p$total_capital_costs))),
                   column(4, div(style="font-size:11px;color:#555;","Markup"),
                          div(style="font-weight:700;color:#e67e22;",
                              dollar(p$total_markup))),
                   column(4, div(style="font-size:11px;color:#555;","PROJECT TOTAL"),
                          div(style="font-weight:700;font-size:15px;color:#1a5276;",
                              dollar(p$total_project_cost)))
                 )
          )
        )
      )
    })

    # agg <- div(
    #   style = "background:#eaf2ff; border:1px solid #aed6f1; border-radius:4px;
    #             padding:10px; margin-top:4px;",
    #   tags$b("Aggregate across all pairs"), br(),
    #   fluidRow(
    #     column(3, "Min:",    tags$b(dollar(min(pairs$total_project_cost)))),
    #     column(3, "Median:", tags$b(dollar(median(pairs$total_project_cost)))),
    #     column(3, "Mean:",   tags$b(dollar(mean(pairs$total_project_cost)))),
    #     column(3, "Total:",  tags$b(dollar(sum(pairs$total_project_cost))))
    #   )
    # )

    tagList(header, br(), rows)
  })
}

shinyApp(ui, server, options = list(port = 8888, launch.browser = TRUE))
