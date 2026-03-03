# ============================================================
#  Water System Consolidation Tool  —  v3 (simplified)
#  Dependencies: shiny, leaflet, aws.s3, tidyverse, sf,
#                stringr, DT, plotly, scales
# ============================================================

library(shiny)
library(leaflet)
library(aws.s3)
library(tidyverse)
library(sf)
library(stringr)
library(DT)
library(plotly)
library(scales)

# ── 0. Static lookups --------------------------------------------------------

owner_types <- c("All", "Federal", "Local", "Native American",
                 "Private", "Public", "State")

state_choices <- c(
  "AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID",
  "IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
  "ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD",
  "TN","TX","UT","VA","VT","WA","WI","WV","WY"
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

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 13px; }
    .sidebar-section { background: #f8f9fa; border-radius: 6px;
                        padding: 10px 12px; margin-bottom: 10px; }
    .sidebar-section h5 { margin-top: 0; font-weight: 600; color: #333; }
    .step-badge { display: inline-block; background: #0d6efd; color: #fff;
                  border-radius: 50%; width: 20px; height: 20px;
                  text-align: center; line-height: 20px; font-size: 11px;
                  margin-right: 6px; }
    .btn-step { width: 100%; margin-top: 6px; }
    #map { border-radius: 6px; }
    .filter-row { display: flex; align-items: center; margin-bottom: 4px; }
    .filter-row label { flex: 0 0 48%; font-size: 12px; color: #444;
                         margin: 0; padding-right: 8px; line-height: 1.2; }
    .filter-row .filter-input { flex: 1; }
    .filter-row .filter-input .form-group { margin-bottom: 0; }
    .filter-row .filter-input input,
    .filter-row .filter-input select { height: 28px; padding: 2px 6px; font-size: 12px; }
    .filter-section-label { font-size: 11px; font-weight: 700; color: #666;
                             text-transform: uppercase; letter-spacing: .5px;
                             margin: 6px 0 4px; }
  "))),
  
  titlePanel("Water System Consolidation Tool — v3"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Step 1
      div(class = "sidebar-section",
          h5(HTML('<span class="step-badge">1</span>Select State')),
          div(class = "filter-row",
              tags$label("State"),
              div(class = "filter-input",
                  selectInput("state", NULL, choices = state_choices, selected = "HI"))
          ),
          actionButton("btn_state", "Load State", class = "btn-primary btn-step btn-sm")
      ),
      
      # Step 2
      div(class = "sidebar-section",
          h5(HTML('<span class="step-badge">2</span>Define Systems')),
          
          div(class = "filter-section-label", "Consolidating System Characteristics"),
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
          div(class = "filter-section-label", "Receiving System Characteristics"),
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
                  numericInput("cutoff", NULL, value = 25, min = 1))),
          
          actionButton("btn_define", "Define Systems", class = "btn-success btn-step btn-sm")
      ),
      
      # Step 3
      div(class = "sidebar-section",
          h5(HTML('<span class="step-badge">3</span>Cost Parameters')),
          div(class = "filter-row",
              tags$label("Cost per Mile ($)"),
              div(class = "filter-input",
                  numericInput("cost_per_mile", NULL, value = 1000000, min = 0))),
          div(class = "filter-row",
              tags$label("Connection Fee ($)"),
              div(class = "filter-input",
                  numericInput("connection_fee", NULL, value = 4000, min = 0))),
          div(class = "filter-row",
              tags$label("Service Line Fee ($)"),
              div(class = "filter-input",
                  numericInput("service_line", NULL, value = 6200, min = 0))),
          div(class = "filter-row",
              tags$label("Admin Cost (%)"),
              div(class = "filter-input",
                  numericInput("admin_cost", NULL, value = 0.15, step = 0.01))),
          div(class = "filter-row",
              tags$label("Contingency (%)"),
              div(class = "filter-input",
                  numericInput("contingency", NULL, value = 0.20, step = 0.01))),
          div(class = "filter-row",
              tags$label("Planning & Const. (%)"),
              div(class = "filter-input",
                  numericInput("planning", NULL, value = 0.10, step = 0.01))),
          div(class = "filter-row",
              tags$label("Engineering (%)"),
              div(class = "filter-input",
                  numericInput("engineering", NULL, value = 0.15, step = 0.01))),
          div(class = "filter-row",
              tags$label("Inflation (%)"),
              div(class = "filter-input",
                  numericInput("inflation", NULL, value = 0.031, step = 0.001))),
          div(class = "filter-row",
              tags$label("Regional Multiplier (%)"),
              div(class = "filter-input",
                  numericInput("regional", NULL, value = 0.10, step = 0.01))),
          actionButton("btn_run", "Run Analysis", class = "btn-danger btn-step btn-sm")
      ),
      
      uiOutput("status_ui")
    ),
    
    mainPanel(
      width = 8,
      leafletOutput("map", height = "420px"),
      br(),
      tabsetPanel(
        id = "tabs",
        tabPanel("Consolidation Pairs",        br(), DTOutput("results_table")),
        tabPanel("Consolidating Cost Chart",   br(), plotlyOutput("cost_chart", height = "320px")),
        tabPanel("Consolidation Cost Summary", br(), uiOutput("cost_summary_ui"))
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
    selected_cons = NULL,   # pwsid of clicked consolidating system
    selected_pair = NULL    # rec_pwsid from table row click
  )
  
  # ── Step 1: Load state ─────────────────────────────────────────────────────
  observeEvent(input$btn_state, {
    withProgress(message = sprintf("Loading %s data from S3...", input$state),
                 detail = "This may take a moment for large states.", value = 0.2, {
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
                     bbox <- st_bbox(dat$sys_geo)
                     leafletProxy("map") %>%
                       clearShapes() %>%
                       clearControls() %>%
                       fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
                   }, error = function(e) {
                     showNotification(paste("S3 load failed:", e$message), type = "error", duration = 8)
                   })
                 })
  })
  
  # ── Step 2: Filter pairs ───────────────────────────────────────────────────
  observeEvent(input$btn_define, {
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
        sprintf("%d consolidating | %d receiving systems identified (%d pairs).",
                n_distinct(filtered$pwsid), n_distinct(filtered$rec_pwsid), nrow(filtered)),
        type = "message", duration = 4
      )
    })
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
        admin_cost                 = input$admin_cost,
        contingency_const          = input$contingency,
        planning_constuction_const = input$planning,
        engineering_services_const = input$engineering,
        inflation_const            = input$inflation,
        regional_multiplier_const  = input$regional
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
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 37, zoom = 4)
  })
  
  # ── Draw ALL base polygons once when costs are computed ────────────────────
  # These stay on the map permanently until a new filter/cost run replaces them.
  observeEvent(rv$costs, {
    req(rv$costs, rv$sys_geo)
    costs <- rv$costs
    
    # Build one-row-per-system lookup tables directly from costs
    cons_ids <- unique(costs$pwsid)
    rec_ids  <- unique(costs$rec_pwsid)
    
    cons_info <- costs %>%
      distinct(pwsid, pws_name, population_served_count,
               service_connections_count, owner_type, health_viols_10yr)
    
    rec_info <- costs %>%
      distinct(rec_pwsid, rec_pws_name, rec_population_served_count,
               rec_owner_type, rec_health_viols_10yr)
    
    # Join geometry
    cons_sf <- rv$sys_geo %>%
      filter(pwsid %in% cons_ids) %>%
      left_join(cons_info, by = "pwsid")
    
    rec_sf <- rv$sys_geo %>%
      filter(pwsid %in% rec_ids) %>%
      left_join(rec_info, by = c("pwsid" = "rec_pwsid"))
    
    bbox <- st_bbox(bind_rows(
      cons_sf %>% select(geometry),
      rec_sf  %>% select(geometry)
    ))
    
    # Draw base layer — group = "base" so we can clear it on re-run
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data = cons_sf, group = "base",
        fillColor = "green", fillOpacity = 0.45,
        color = "darkgreen", weight = 1.5,
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
  # This ONLY touches the "highlight" group. Base polygons are never removed.
  observe({
    req(rv$costs, rv$sys_geo, rv$selected_cons)
    costs  <- rv$costs
    sel_id <- rv$selected_cons
    
    # The selected consolidating system's info (one row)
    sel_info <- costs %>%
      filter(pwsid == sel_id) %>%
      slice(1) %>%
      select(pwsid, pws_name, population_served_count, owner_type, health_viols_10yr)
    
    sel_sf <- rv$sys_geo %>%
      filter(pwsid == sel_id) %>%
      left_join(sel_info, by = "pwsid")
    
    # Its receiving partners
    partner_ids <- costs %>% filter(pwsid == sel_id) %>% pull(rec_pwsid) %>% unique()
    
    partner_info <- costs %>%
      filter(pwsid == sel_id) %>%
      distinct(rec_pwsid, rec_pws_name, rec_population_served_count,
               rec_owner_type, rec_health_viols_10yr)
    
    partner_sf <- rv$sys_geo %>%
      filter(pwsid %in% partner_ids) %>%
      left_join(partner_info, by = c("pwsid" = "rec_pwsid"))
    
    bbox <- st_bbox(bind_rows(
      sel_sf     %>% select(geometry),
      partner_sf %>% select(geometry)
    ))
    
    # Only clear and redraw the highlight group — base stays untouched
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data = partner_sf, group = "highlight",
        fillColor = "#1e90ff", fillOpacity = 0.7,
        color = "darkblue", weight = 2.5,
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
        "Cons. PWSID"        = pwsid,
        "Cons. Name"         = pws_name,
        "Rec. PWSID"         = rec_pwsid,
        "Rec. Name"          = rec_pws_name,
        "Centroid Dist (mi)" = rec_centroid_distance,
        "Travel Dist (mi)"   = rec_travel_distance,
        "Overlap"            = rec_overlap,
        "Connections"        = service_connections_count,
        "Capital Costs"      = total_capital_costs,
        "Markup"             = total_markup,
        "Total Cost"         = total_project_cost
      ) %>%
      datatable(selection = "single", rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })
  
  # Table row click → zoom to that pair
  observeEvent(input$results_table_rows_selected, {
    req(rv$costs, rv$sys_geo)
    row_idx  <- input$results_table_rows_selected
    pair_row <- selected_pairs_df()[row_idx, ]
    
    rv$selected_cons <- pair_row$pwsid
    rv$selected_pair <- pair_row$rec_pwsid
  })
  
  # ── Cost chart ─────────────────────────────────────────────────────────────
  output$cost_chart <- renderPlotly({
    req(rv$costs, rv$selected_cons, "total_project_cost" %in% names(rv$costs))
    
    cost_cols <- c("new_source_cost", "pipe_line_cost", "connection_fees",
                   "service_line_cost", "admin_costs", "CEQA_cost",
                   "contingency", "planning_constuction", "engineering_services",
                   "inflation", "regional_multiplier")
    
    plot_df <- rv$costs %>%
      filter(pwsid == rv$selected_cons) %>%
      { if (!is.null(rv$selected_pair)) filter(., rec_pwsid == rv$selected_pair) else . } %>%
      select(rec_pws_name, all_of(cost_cols)) %>%
      pivot_longer(-rec_pws_name, names_to = "component", values_to = "cost") %>%
      mutate(component = str_replace_all(component, "_", " ") %>% str_to_title())
    
    plot_ly(plot_df, x = ~rec_pws_name, y = ~cost, color = ~component,
            type = "bar", text = ~dollar(cost), textposition = "none",
            hovertemplate = "%{x}<br>%{data.name}: %{y:$,.0f}<extra></extra>") %>%
      layout(
        barmode = "stack",
        xaxis   = list(title = "Receiving System"),
        yaxis   = list(title = "Cost ($)", tickformat = "$,.0f"),
        legend  = list(orientation = "h", y = -0.25),
        margin  = list(b = 100)
      )
  })
  
  # ── Summary panel ──────────────────────────────────────────────────────────
  output$cost_summary_ui <- renderUI({
    req(rv$costs, rv$selected_cons, "total_project_cost" %in% names(rv$costs))
    
    pairs <- rv$costs %>%
      filter(pwsid == rv$selected_cons) %>%
      arrange(total_project_cost)
    
    header <- div(
      style = "background:#2c3e50; color:white; padding:10px 14px;
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
    
    agg <- div(
      style = "background:#eaf2ff; border:1px solid #aed6f1; border-radius:4px;
                padding:10px; margin-top:4px;",
      tags$b("Aggregate across all pairs"), br(),
      fluidRow(
        column(3, "Min:",    tags$b(dollar(min(pairs$total_project_cost)))),
        column(3, "Median:", tags$b(dollar(median(pairs$total_project_cost)))),
        column(3, "Mean:",   tags$b(dollar(mean(pairs$total_project_cost)))),
        column(3, "Total:",  tags$b(dollar(sum(pairs$total_project_cost))))
      )
    )
    
    tagList(header, br(), rows, agg)
  })
}

shinyApp(ui, server, options = list(port = 8888, launch.browser = TRUE))