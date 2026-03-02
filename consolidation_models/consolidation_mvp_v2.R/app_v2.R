# ============================================================
#  Water System Consolidation Tool  —  v2
#  Dependencies: shiny, leaflet, aws.s3, tidyverse, sf,
#                stringr, osrm, DT, plotly, scales
# ============================================================

library(shiny)
library(leaflet)
library(aws.s3)
library(tidyverse)
library(sf)
library(stringr)
library(osrm)
library(DT)
library(plotly)
library(scales)

# ── 0. Data ------------------------------------------------------------------

national_water_system <- s3read_using(
  readRDS,
  object = "s3://tech-team-data/national-dw-tool/clean/national/national_water_system.RData"
)

pwsid_huc <- s3read_using(
  read.csv,
  object = "s3://tech-team-data/national-dw-tool/test-staged/pwsid_npdes_usts_rmps_imp.csv"
) %>%
  select(pwsid, num_facilities) %>%
  group_by(pwsid) %>%
  summarize(num_facilities = sum(num_facilities, na.rm = TRUE))

epa_sabs <- national_water_system[[1]] %>%
  st_as_sf() %>%
  left_join(pwsid_huc, by = "pwsid")

sdwis <- national_water_system[[2]] %>%
  select(pwsid, owner_type, health_viols_10yr, open_health_viol)

owner_types <- c("All", sort(unique(sdwis$owner_type)))
state_choices <- sort(unique(
  str_extract_all(epa_sabs$epic_states_intersect, "[A-Z]{2}") %>%
    unlist()
))

# ── 1. Model functions -------------------------------------------------------

get_sabs <- function(ids, size = "small") {
  if (size == "small") {
    epa_sabs %>%
      filter(pwsid %in% ids) %>%
      select(pwsid, pws_name, epic_states_intersect,
             service_connections_count, population_served_count, num_facilities)
  } else {
    epa_sabs %>% filter(pwsid %in% ids)
  }
}

filter_sabs <- function(sabs, cons_sys_config, rec_sys_config) {
  base <- sabs %>% left_join(sdwis, by = "pwsid")
  
  owner_type_con <- cons_sys_config$owner_type
  owner_type_rec <- rec_sys_config$owner_type
  
  cons_sabs <- base %>%
    filter(health_viols_10yr >= cons_sys_config$health_viols_10yr) %>%
    filter(open_health_viol  == cons_sys_config$open_health_viol) %>%
    { if (owner_type_con != "All") filter(., owner_type == owner_type_con) else . } %>%
    filter(population_served_count <= cons_sys_config$pop_served) %>%
    pull(pwsid)
  
  rec_sabs <- base %>%
    filter(health_viols_10yr <= rec_sys_config$health_viols_10yr) %>%
    filter(open_health_viol  == rec_sys_config$open_health_viol) %>%
    { if (owner_type_rec != "All") filter(., owner_type == owner_type_rec) else . } %>%
    filter(population_served_count >= rec_sys_config$pop_served) %>%
    pull(pwsid)
  
  list(cons = cons_sabs, rec = rec_sabs)
}

get_neighbors <- function(sabs, cutoff, cons_pwsids, rec_pwsids, progress) {
  message("Calculating centroids...")
  centroids <- st_centroid(sabs$geometry)
  centroid_distance_matrix <- st_distance(centroids, centroids)
  
  message("Processing ", nrow(sabs), " pwsids...")
  
  df <- sabs %>%
    mutate(rec = map(1:n(), function(i) {
      if (i %% 10 == 0 && !is.null(progress)) {
        shiny::withReactiveDomain(progress, {
          setProgress(i / nrow(sabs), detail = sprintf("System %d of %d", i, nrow(sabs)))
        })
      }
      
      this_pwsid <- pwsid[i]
      
      if (!(this_pwsid %in% cons_pwsids)) {
        return(tibble(pwsid = character(), centroid_distance = numeric(),
                      travel_distance = numeric(), overlap = logical()))
      }
      
      parent_geom   <- sabs$geometry[i]
      candidate_idx <- which(pwsid != this_pwsid & pwsid %in% rec_pwsids)
      
      if (length(candidate_idx) == 0) {
        return(tibble(pwsid = character(), centroid_distance = numeric(),
                      travel_distance = numeric(), overlap = logical()))
      }
      
      centroid_distances <- as.numeric(centroid_distance_matrix[i, candidate_idx]) * 0.000621371
      candidate_pwsids   <- pwsid[candidate_idx]
      child_geoms        <- sabs$geometry[candidate_idx]
      overlaps           <- st_overlaps(parent_geom, child_geoms, sparse = FALSE)[1, ]
      
      matches <- tibble(
        pwsid             = candidate_pwsids,
        centroid_distance = centroid_distances,
        overlap           = overlaps
      ) %>%
        filter(centroid_distance <= cutoff | overlap)
      
      if (nrow(matches) == 0) {
        return(tibble(pwsid = character(), centroid_distance = numeric(),
                      travel_distance = numeric(), overlap = logical()))
      }
      
      matched_geoms <- sabs$geometry[sabs$pwsid %in% matches$pwsid]
      
      travel_table <- tryCatch(
        osrmTable(
          src     = st_centroid(parent_geom),
          dst     = st_centroid(matched_geoms),
          measure = "distance"
        ),
        error = function(e) {
          message("  OSRM error for pwsid ", i, ": ", e$message, " — falling back")
          NULL
        }
      )
      
      matches %>%
        mutate(
          travel_distance = if (!is.null(travel_table)) {
            as.numeric(travel_table$distances[1, ]) * 0.000621371
          } else {
            centroid_distance
          },
          overlap = st_overlaps(parent_geom, matched_geoms, sparse = FALSE)[1, ]
        ) %>%
        filter(travel_distance <= cutoff | overlap)
    })) %>%
    mutate(rec_num_neighbors = map_int(rec, nrow))
  
  message("Unnesting...")
  df <- unnest(df, rec, names_sep = "_")
  
  df %>%
    left_join(
      sabs %>% st_drop_geometry() %>%
        select(pwsid, num_facilities) %>%
        rename(rec_pwsid = pwsid, rec_num_facilities = num_facilities),
      by = "rec_pwsid"
    ) %>%
    mutate(rec_num_facilities = replace_na(rec_num_facilities, 0))
}

get_costs <- function(neighbors,
                      cost_per_mile, connection_fee, service_line_fee,
                      admin_cost, contingency_const, planning_constuction_const,
                      engineering_services_const, inflation_const,
                      regional_multiplier_const) {
  neighbors %>%
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
      total_capital_costs = sum(c_across(new_source_cost:CEQA_cost)),
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
    /* Compact paired-row filter table */
    .filter-row { display: flex; align-items: center; margin-bottom: 4px; }
    .filter-row label { flex: 0 0 48%; font-size: 12px; color: #444;
                         margin: 0; padding-right: 8px; line-height: 1.2; }
    .filter-row .filter-input { flex: 1; }
    .filter-row .filter-input .form-group { margin-bottom: 0; }
    .filter-row .filter-input input,
    .filter-row .filter-input select { height: 28px; padding: 2px 6px;
                                        font-size: 12px; }
    .filter-section-label { font-size: 11px; font-weight: 700; color: #666;
                             text-transform: uppercase; letter-spacing: .5px;
                             margin: 6px 0 4px; }
  "))),
  
  titlePanel("Water System Consolidation Tool — v2"),
  
  sidebarLayout(
    # ── Sidebar ──────────────────────────────────────────────────────────────
    sidebarPanel(
      width = 4,
      
      # helper to render a paired label + input row
      # (defined inline via local so it's available at UI build time)
      
      # Step 1 — State
      div(class = "sidebar-section",
          h5(HTML('<span class="step-badge">1</span>Select State')),
          div(class = "filter-row",
              tags$label("State"),
              div(class = "filter-input",
                  selectInput("state", NULL, choices = state_choices, selected = "MD"))
          ),
          actionButton("btn_state", "Load State", class = "btn-primary btn-step btn-sm")
      ),
      
      # Step 2 — System filters
      div(class = "sidebar-section",
          h5(HTML('<span class="step-badge">2</span>Define Systems')),
          
          div(class = "filter-section-label", "Consolidating System Characteristics"),
          
          div(class = "filter-row",
              tags$label("Owner Type"),
              div(class = "filter-input",
                  selectInput("cons_owner", NULL, choices = owner_types, selected = "Private"))),
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
                  selectInput("rec_owner", NULL, choices = owner_types, selected = "Local"))),
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
                  numericInput("cutoff", NULL, value = 5, min = 1))),
          
          actionButton("btn_define", "Define Systems", class = "btn-success btn-step btn-sm")
      ),
      
      # Step 3 — Costs
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
    
    # ── Main panel ───────────────────────────────────────────────────────────
    mainPanel(
      width = 8,
      # Map takes top 2/3
      leafletOutput("map", height = "420px"),
      br(),
      # Tabs take bottom 1/3
      tabsetPanel(
        id = "tabs",
        tabPanel("Consolidation Pairs",
                 br(),
                 DTOutput("results_table")
        ),
        tabPanel("Consolidating Cost Chart",
                 br(),
                 plotlyOutput("cost_chart", height = "320px")
        ),
        tabPanel("Consolidation Cost Summary",
                 br(),
                 uiOutput("cost_summary_ui")
        )
      )
    )
  )
)

# ── 3. Server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive state
  rv <- reactiveValues(
    sabs          = NULL,   # full state sabs (small)
    filtered_ids  = NULL,   # list(cons, rec)
    sabs_filtered = NULL,   # sabs subset for analysis
    costs         = NULL,   # cost df
    selected_cons = NULL,   # currently highlighted cons pwsid
    selected_pair = NULL    # currently highlighted rec pwsid (from table click)
  )
  
  # ── Step 1: Load state ─────────────────────────────────────────────────────
  observeEvent(input$btn_state, {
    withProgress(message = "Loading state systems...", {
      ids <- epa_sabs %>%
        filter(str_detect(epic_states_intersect, input$state)) %>%
        pull(pwsid)
      rv$sabs <- get_sabs(ids, "small")
      rv$filtered_ids  <- NULL
      rv$sabs_filtered <- NULL
      rv$costs         <- NULL
      rv$selected_cons <- NULL
      showNotification(
        sprintf("Loaded %d systems for %s.", nrow(rv$sabs), input$state),
        type = "message", duration = 3
      )
    })
  })
  
  # ── Step 2: Filter systems ─────────────────────────────────────────────────
  observeEvent(input$btn_define, {
    req(rv$sabs)
    withProgress(message = "Filtering systems...", {
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
        pop_served        = input$rec_min_pop
      )
      rv$filtered_ids <- filter_sabs(rv$sabs, cons_cfg, rec_cfg)
      rv$sabs_filtered <- rv$sabs %>%
        filter(pwsid %in% c(rv$filtered_ids$cons, rv$filtered_ids$rec))
      rv$costs <- NULL
      
      showNotification(
        sprintf("%d consolidating | %d receiving systems identified.",
                length(rv$filtered_ids$cons), length(rv$filtered_ids$rec)),
        type = "message", duration = 4
      )
      
      if (length(rv$filtered_ids$cons) == 0 || length(rv$filtered_ids$rec) == 0)
        showNotification("No systems match — try relaxing filters.", type = "warning")
    })
  })
  
  # ── Step 3: Run cost analysis ──────────────────────────────────────────────
  observeEvent(input$btn_run, {
    req(rv$sabs_filtered, rv$filtered_ids)
    req(length(rv$filtered_ids$cons) > 0, length(rv$filtered_ids$rec) > 0)
    
    withProgress(message = "Running analysis...", value = 0, {
      setProgress(0.1, detail = "Finding neighbors (OSRM)...")
      
      neighbors <- get_neighbors(
        sabs        = rv$sabs_filtered,
        cutoff      = input$cutoff,
        cons_pwsids = rv$filtered_ids$cons,
        rec_pwsids  = rv$filtered_ids$rec,
        progress    = shiny::getDefaultReactiveDomain()
      )
      
      if (nrow(neighbors) == 0) {
        showNotification("No pairs found. Try relaxing distance or filters.",
                         type = "warning")
        return()
      }
      
      setProgress(0.85, detail = "Calculating costs...")
      rv$costs <- get_costs(
        neighbors,
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
      
      # Default selection = first cons system
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
    if (!is.null(rv$costs)) {
      div(style = "color:green; font-size:12px; margin-top:6px;",
          icon("check-circle"),
          sprintf(" %d pairs ready", nrow(rv$costs)))
    } else if (!is.null(rv$sabs_filtered)) {
      div(style = "color:orange; font-size:12px; margin-top:6px;",
          icon("hourglass-half"),
          sprintf(" %d cons / %d rec systems — run costs",
                  length(rv$filtered_ids$cons), length(rv$filtered_ids$rec)))
    } else if (!is.null(rv$sabs)) {
      div(style = "color:#555; font-size:12px; margin-top:6px;",
          icon("map"), sprintf(" %d systems loaded", nrow(rv$sabs)))
    }
  })
  
  # ── Map base ───────────────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 37, zoom = 4)
  })
  
  # Render all polygons once costs are ready
  observe({
    req(rv$costs, rv$sabs_filtered)
    costs <- rv$costs
    sabs  <- rv$sabs_filtered
    
    cons_sf <- sabs %>%
      filter(pwsid %in% unique(costs$pwsid)) %>%
      st_transform(4326)
    rec_sf  <- sabs %>%
      filter(pwsid %in% unique(costs$rec_pwsid)) %>%
      st_transform(4326)
    
    bbox <- st_bbox(st_transform(
      sabs %>% filter(pwsid %in% c(unique(costs$pwsid), unique(costs$rec_pwsid))), 4326
    ))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        data        = cons_sf,
        fillColor   = "green", fillOpacity = 0.45,
        color       = "darkgreen", weight = 1.5,
        layerId     = ~pwsid,
        popup       = ~paste0("<b>", pws_name, "</b><br>",
                              pwsid, "<br><i>Consolidating</i>")
      ) %>%
      addPolygons(
        data        = rec_sf,
        fillColor   = "steelblue", fillOpacity = 0.35,
        color       = "navy", weight = 1.5,
        popup       = ~paste0("<b>", pws_name, "</b><br>",
                              pwsid, "<br><i>Receiving</i>")
      ) %>%
      addLegend("bottomright",
                colors = c("green", "steelblue"),
                labels = c("Consolidating", "Receiving"),
                opacity = 0.7
      ) %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })
  
  # Highlight selected cons + its pairs
  observe({
    req(rv$costs, rv$sabs_filtered, rv$selected_cons)
    sel_id   <- rv$selected_cons
    costs    <- rv$costs
    sabs     <- rv$sabs_filtered
    
    sel_sf   <- sabs %>% filter(pwsid == sel_id) %>% st_transform(4326)
    pairs_sf <- sabs %>%
      filter(pwsid %in% filter(costs, pwsid == sel_id)$rec_pwsid) %>%
      st_transform(4326)
    
    bbox <- st_bbox(bind_rows(sel_sf, pairs_sf) %>% st_transform(4326))
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(
        data        = pairs_sf, group = "highlight",
        fillColor   = "#1e90ff", fillOpacity = 0.7,
        color       = "darkblue", weight = 2.5,
        popup       = ~paste0("<b>", pws_name, "</b><br>",
                              pwsid, "<br><i>Receiving</i>")
      ) %>%
      addPolygons(
        data        = sel_sf, group = "highlight",
        fillColor   = "#2ecc71", fillOpacity = 0.85,
        color       = "#145a32", weight = 3,
        layerId     = ~pwsid,   # preserve click-to-select behaviour
        popup       = ~paste0("<b>", pws_name, "</b><br>",
                              pwsid, "<br><i>Consolidating</i>")
      )%>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })
  
  # Map click → update selected cons
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    req(id, rv$costs)
    if (id %in% rv$costs$pwsid) {
      rv$selected_cons <- id
      rv$selected_pair <- NULL
    }
  })
  
  # ── Results table ──────────────────────────────────────────────────────────
  # Show only rows for selected cons system
  selected_pairs_df <- reactive({
    req(rv$costs)
    rv$costs %>%
      st_drop_geometry() %>%
      left_join(
        rv$sabs_filtered %>% st_drop_geometry() %>%
          select(pwsid, pws_name) %>%
          rename(rec_pwsid = pwsid, rec_name = pws_name),
        by = "rec_pwsid"
      ) %>%
      select(
        pwsid, pws_name,
        rec_pwsid, rec_name,
        rec_centroid_distance, rec_travel_distance, rec_overlap,
        service_connections_count,
        total_capital_costs, total_markup, total_project_cost
      )
  })
  
  
  output$results_table <- renderDT({
    req(selected_pairs_df())
    df <- selected_pairs_df() %>%
      mutate(
        across(c(total_capital_costs, total_markup, total_project_cost), dollar),
        across(c(rec_centroid_distance, rec_travel_distance), ~round(., 2))
      ) %>%
      rename(
        "Cons. PWSID"        = pwsid,
        "Cons. Name"         = pws_name,
        "Rec. PWSID"         = rec_pwsid,
        "Rec. Name"          = rec_name,
        "Centroid Dist (mi)" = rec_centroid_distance,
        "Travel Dist (mi)"   = rec_travel_distance,
        "Overlap"            = rec_overlap,
        "Connections"        = service_connections_count,
        "Capital Costs"      = total_capital_costs,
        "Markup"             = total_markup,
        "Total Cost"         = total_project_cost
      )
    datatable(df,
              selection = "single",
              rownames  = FALSE,
              options   = list(pageLength = 8, scrollX = TRUE, dom = "tip")
    )
  })
  
  # Table row click → zoom map to that pair
  observeEvent(input$results_table_rows_selected, {
    req(rv$costs, rv$sabs_filtered)
    row_idx <- input$results_table_rows_selected
    req(row_idx)
    
    pair_row <- selected_pairs_df()[row_idx, ]
    cons_id  <- pair_row$pwsid
    rec_id   <- pair_row$rec_pwsid
    
    rv$selected_cons <- cons_id
    rv$selected_pair <- rec_id
    
    cons_sf <- rv$sabs_filtered %>% filter(pwsid == cons_id) %>% st_transform(4326)
    rec_sf  <- rv$sabs_filtered %>% filter(pwsid == rec_id)  %>% st_transform(4326)
    bbox    <- st_bbox(bind_rows(cons_sf, rec_sf))
    
    leafletProxy("map") %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
  })
  
  # ── Cost chart ─────────────────────────────────────────────────────────────
  output$cost_chart <- renderPlotly({
    req(selected_pairs_df())
    df <- selected_pairs_df()
    
    # If a row is selected focus on that, else show all pairs for this cons
    if (!is.null(rv$selected_pair)) {
      df <- df %>% filter(rec_pwsid == rv$selected_pair)
    }
    
    # Long format cost components
    cost_cols <- c("new_source_cost", "pipe_line_cost", "connection_fees",
                   "service_line_cost", "admin_costs", "CEQA_cost",
                   "contingency", "planning_constuction", "engineering_services",
                   "inflation", "regional_multiplier")
    
    plot_df <- rv$costs %>%
      st_drop_geometry() %>%
      filter(pwsid == rv$selected_cons) %>%
      { if (!is.null(rv$selected_pair)) filter(., rec_pwsid == rv$selected_pair) else . } %>%
      left_join(
        rv$sabs_filtered %>% st_drop_geometry() %>%
          select(pwsid, pws_name) %>% rename(rec_pwsid = pwsid, rec_name = pws_name),
        by = "rec_pwsid"
      ) %>%
      select(rec_name, all_of(cost_cols)) %>%
      pivot_longer(-rec_name, names_to = "component", values_to = "cost") %>%
      mutate(component = str_replace_all(component, "_", " ") %>% str_to_title())
    
    plot_ly(plot_df, x = ~rec_name, y = ~cost, color = ~component,
            type = "bar", text = ~dollar(cost), textposition = "none",
            hovertemplate = "%{x}<br>%{data.name}: %{y:$,.0f}<extra></extra>") %>%
      layout(
        barmode  = "stack",
        xaxis    = list(title = "Receiving System"),
        yaxis    = list(title = "Cost ($)", tickformat = "$,.0f"),
        legend   = list(orientation = "h", y = -0.25),
        margin   = list(b = 100)
      )
  })
  
  # ── Summary panel ──────────────────────────────────────────────────────────
  output$cost_summary_ui <- renderUI({
    req(rv$costs, rv$selected_cons)
    
    pairs <- rv$costs %>%
      st_drop_geometry() %>%
      filter(pwsid == rv$selected_cons) %>%
      left_join(
        rv$sabs_filtered %>% st_drop_geometry() %>%
          select(pwsid, pws_name) %>% rename(rec_pwsid = pwsid, rec_name = pws_name),
        by = "rec_pwsid"
      ) %>%
      arrange(total_project_cost)
    
    cons_name <- pairs$pws_name[1]
    
    header <- div(
      style = "background:#2c3e50; color:white; padding:10px 14px;
                border-radius:6px 6px 0 0; margin-bottom:0;",
      tags$b(cons_name), " — ", tags$small(rv$selected_cons),
      tags$span(style = "float:right;",
                sprintf("%d potential receiving system(s)", nrow(pairs)))
    )
    
    rows <- lapply(seq_len(nrow(pairs)), function(i) {
      p <- pairs[i, ]
      div(
        style = "border: 1px solid #dee2e6; border-radius:4px;
                  padding:10px; margin-bottom:8px; background:#fff;",
        fluidRow(
          column(4,
                 tags$b(p$rec_name), br(),
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