library(shiny)
library(leaflet)
library(aws.s3)
library(tidyverse)
library(sf)
library(stringr)
library(osrm)
library(DT)

# --- Data -------------------------------------------------------------------
# national_water_system <- s3read_using(readRDS, object = "s3://...")
# pwsid_huc <- s3read_using(read.csv, object = "s3://...") %>%
#   select(pwsid, num_facilities) %>%
#   group_by(pwsid) %>%
#   summarize(num_facilities = sum(num_facilities, na.rm = TRUE))

epa_sabs <- national_water_system[[1]] %>%
  st_as_sf() %>%
  left_join(pwsid_huc)

sdwis <- national_water_system[[2]] %>%
  select(pwsid, owner_type, health_viols_10yr, open_health_viol)

owner_types <- sort(unique(sdwis$owner_type))

# --- Helper functions -------------------------------------------------------

filter_sabs <- function(sabs, cons_config, rec_config) {
  base <- sabs %>% left_join(sdwis, by = "pwsid")
  list(
    cons = base %>%
      filter(health_viols_10yr >= cons_config$min_viols,
             owner_type == cons_config$owner_type,
             population_served_count <= cons_config$max_pop) %>%
      pull(pwsid),
    rec = base %>%
      filter(health_viols_10yr <= rec_config$max_viols,
             open_health_viol == "No",
             owner_type == rec_config$owner_type,
             population_served_count >= rec_config$min_pop) %>%
      pull(pwsid)
  )
}

get_neighbors <- function(sabs, cutoff) {
  centroids <- st_centroid(sabs$geometry)
  dist_mat  <- st_distance(centroids, centroids)
  
  sabs %>%
    mutate(distance_match = map(seq_len(n()), function(i) {
      dists   <- as.numeric(dist_mat[i, ]) * 0.000621371
      matches <- tibble(pwsid = pwsid[-i], centroid_distance = dists[-i]) %>%
        filter(centroid_distance <= cutoff)
      
      if (nrow(matches) == 0)
        return(tibble(pwsid = character(), centroid_distance = numeric(),
                      travel_distance = numeric(), overlap = logical()))
      
      parent <- sabs$geometry[i]
      kids   <- sabs$geometry[sabs$pwsid %in% matches$pwsid]
      routes <- osrmTable(src = st_centroid(parent), dst = st_centroid(kids),
                          measure = "distance")
      
      matches %>%
        mutate(travel_distance = as.numeric(routes$distances) * 0.000621371,
               overlap = as.vector(st_overlaps(parent, kids, sparse = FALSE))) %>%
        filter(travel_distance <= cutoff)
    })) %>%
    unnest(distance_match, names_sep = "_")
}

get_costs <- function(neighbors, params) {
  neighbors %>%
    mutate(
      new_source_cost     = ifelse(distance_match_num_facilities > 0, 0, 1238933),
      pipe_line_cost      = distance_match_travel_distance * params$cost_per_mile,
      connection_fees     = service_connections_count * params$connection_fee,
      service_line_cost   = service_connections_count * params$service_line_fee,
      admin_costs         = (pipe_line_cost + service_line_cost) * params$admin_cost,
      CEQA_cost           = ifelse(distance_match_overlap, 25000, 100000),
      total_capital_costs = new_source_cost + pipe_line_cost + connection_fees +
        service_line_cost + admin_costs + CEQA_cost,
      contingency         = total_capital_costs * params$contingency,
      planning_const      = total_capital_costs * params$planning,
      engineering         = total_capital_costs * params$engineering,
      inflation           = total_capital_costs * params$inflation,
      regional_multiplier = total_capital_costs * params$regional,
      total_markup        = contingency + planning_const + engineering +
        inflation + regional_multiplier,
      total_project_cost  = total_capital_costs + total_markup
    )
}

# --- UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Water System Consolidation Cost Estimator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      selectInput("state", "State:",
                  choices = sort(unique(str_extract(epa_sabs$epic_states_intersect, "^[A-Z]{2}"))),
                  selected = "RI"),
      numericInput("cutoff", "Distance Cutoff (miles):", value = 3, min = 1, max = 50),
      
      hr(), h4("Consolidating System"),
      helpText("Systems to be absorbed."),
      selectInput("cons_owner", "Owner Type:", choices = owner_types, selected = "Local"),
      numericInput("cons_min_viols", "Min Health Violations (10yr):", value = 1, min = 0),
      numericInput("cons_max_pop",   "Max Population Served:", value = 5000, min = 0, step = 500),
      
      hr(), h4("Receiving System"),
      helpText("Systems that absorb others."),
      selectInput("rec_owner",    "Owner Type:", choices = owner_types, selected = "Local"),
      numericInput("rec_max_viols", "Max Health Violations (10yr):", value = 1, min = 0),
      numericInput("rec_min_pop",   "Min Population Served:", value = 10000, min = 0, step = 500),
      
      hr(), h4("Cost Parameters"),
      numericInput("cost_per_mile",  "Cost per Mile ($):",      value = 1000000, min = 0),
      numericInput("connection_fee", "Connection Fee ($):",     value = 4000,    min = 0),
      numericInput("service_line",   "Service Line Fee ($):",   value = 6200,    min = 0),
      numericInput("admin_cost",     "Admin Cost (%):",         value = 0.15,    min = 0, max = 1, step = 0.01),
      numericInput("contingency",    "Contingency (%):",        value = 0.20,    min = 0, max = 1, step = 0.01),
      numericInput("planning",       "Planning & Const. (%):",  value = 0.10,    min = 0, max = 1, step = 0.01),
      numericInput("engineering",    "Engineering (%):",        value = 0.15,    min = 0, max = 1, step = 0.01),
      numericInput("inflation",      "Inflation (%):",          value = 0.031,   min = 0, max = 1, step = 0.001),
      numericInput("regional",       "Regional Multiplier (%):",value = 0.10,    min = 0, max = 1, step = 0.01),
      
      hr(),
      actionButton("run", "Run Analysis", class = "btn-primary btn-lg btn-block")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Map",     leafletOutput("map", height = "600px")),
        tabPanel("Results", br(), DTOutput("results_table")),
        tabPanel("Summary", br(), verbatimTextOutput("cost_summary"))
      )
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  costs_data <- reactiveVal(NULL)
  sabs_data  <- reactiveVal(NULL)
  selected   <- reactiveVal(NULL)
  
  # Run analysis
  observeEvent(input$run, {
    withProgress(message = "Running analysis...", value = 0, {
      
      setProgress(0.1, detail = "Loading systems...")
      state_ids <- epa_sabs %>%
        filter(str_detect(epic_states_intersect, input$state)) %>%
        pull(pwsid)
      
      sabs <- epa_sabs %>%
        filter(pwsid %in% state_ids) %>%
        select(pwsid, pws_name, epic_states_intersect,
               service_connections_count, population_served_count, num_facilities)
      
      setProgress(0.2, detail = "Filtering systems...")
      roles <- filter_sabs(
        sabs,
        cons_config = list(min_viols  = input$cons_min_viols,
                           owner_type = input$cons_owner,
                           max_pop    = input$cons_max_pop),
        rec_config  = list(max_viols  = input$rec_max_viols,
                           owner_type = input$rec_owner,
                           min_pop    = input$rec_min_pop)
      )
      
      if (length(roles$cons) == 0 || length(roles$rec) == 0) {
        showNotification("No systems match filters — try relaxing criteria.", type = "warning")
        return()
      }
      
      sabs_filtered <- sabs %>% filter(pwsid %in% c(roles$cons, roles$rec))
      
      setProgress(0.3, detail = "Finding neighbors (OSRM)...")
      neighbors <- get_neighbors(sabs_filtered, input$cutoff) %>%
        filter(pwsid %in% roles$cons,
               distance_match_pwsid %in% roles$rec) %>%
        left_join(
          sabs_filtered %>% st_drop_geometry() %>%
            select(pwsid, num_facilities) %>%
            rename(distance_match_pwsid = pwsid,
                   distance_match_num_facilities = num_facilities),
          by = "distance_match_pwsid"
        ) %>%
        mutate(distance_match_num_facilities =
                 replace_na(distance_match_num_facilities, 0))
      
      setProgress(0.85, detail = "Calculating costs...")
      if (nrow(neighbors) == 0) {
        showNotification("No consolidation pairs found. Try relaxing filters or distance cutoff.",
                         type = "warning")
        return()
      }
      
      costs <- get_costs(neighbors, list(
        cost_per_mile  = input$cost_per_mile,
        connection_fee = input$connection_fee,
        service_line_fee = input$service_line,
        admin_cost     = input$admin_cost,
        contingency    = input$contingency,
        planning       = input$planning,
        engineering    = input$engineering,
        inflation      = input$inflation,
        regional       = input$regional
      ))
      
      costs_data(costs)
      sabs_data(sabs_filtered)
      selected(unique(costs$pwsid)[1])
      
      setProgress(1)
      showNotification(
        sprintf("Done! %d pairs across %d consolidating systems.",
                nrow(costs), n_distinct(costs$pwsid)),
        type = "message", duration = 5
      )
    })
  })
  
  # Map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -95, lat = 37, zoom = 4)
  })
  
  observe({
    req(costs_data(), sabs_data())
    costs <- costs_data()
    sabs  <- sabs_data()
    
    cons_ids <- unique(costs$pwsid)
    rec_ids  <- unique(costs$distance_match_pwsid)
    
    cons_sf <- st_transform(sabs %>% filter(pwsid %in% cons_ids), 4326)
    rec_sf  <- st_transform(sabs %>% filter(pwsid %in% rec_ids),  4326)
    bbox    <- st_bbox(st_transform(sabs %>%
                                      filter(pwsid %in% c(cons_ids, rec_ids)), 4326))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = rec_sf,  fillColor = "green",  fillOpacity = 0.5,
                  color = "darkgreen", weight = 2,
                  popup = ~paste0("<b>", pws_name, "</b><br>PWSID: ", pwsid,
                                  "<br>Receiving System")) %>%
      addPolygons(data = cons_sf, fillColor = "grey", fillOpacity = 0.5,
                  color = "darkgrey", weight = 2, layerId = ~pwsid,
                  popup = ~paste0("<b>", pws_name, "</b><br>PWSID: ", pwsid,
                                  "<br><i>Click to highlight</i>")) %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
  
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!is.null(id)) selected(id)
  })
  
  observe({
    req(selected(), sabs_data(), costs_data())
    sel_id  <- selected()
    sabs    <- sabs_data()
    costs   <- costs_data()
    
    sel_sf      <- st_transform(sabs %>% filter(pwsid == sel_id), 4326)
    neighbor_sf <- st_transform(sabs %>%
                                  filter(pwsid %in% filter(costs, pwsid == sel_id)$distance_match_pwsid), 4326)
    
    leafletProxy("map") %>%
      clearGroup("highlight") %>%
      addPolygons(data = neighbor_sf, group = "highlight",
                  fillColor = "yellow", fillOpacity = 0.7,
                  color = "orange", weight = 2) %>%
      addPolygons(data = sel_sf, group = "highlight",
                  fillColor = "blue", fillOpacity = 0.7,
                  color = "darkblue", weight = 3)
  })
  
  # Results table — ALL pairs
  output$results_table <- renderDT({
    req(costs_data())
    costs_data() %>%
      st_drop_geometry() %>%
      select(pwsid, pws_name,
             distance_match_pwsid,
             distance_match_centroid_distance,
             distance_match_travel_distance,
             distance_match_overlap,
             service_connections_count,
             total_capital_costs, total_markup, total_project_cost) %>%
      mutate(across(c(total_capital_costs, total_markup, total_project_cost), scales::dollar),
             across(c(distance_match_centroid_distance, distance_match_travel_distance),
                    ~round(., 2))) %>%
      rename("Cons. PWSID"        = pwsid,
             "Cons. Name"         = pws_name,
             "Rec. PWSID"         = distance_match_pwsid,
             "Centroid Dist (mi)" = distance_match_centroid_distance,
             "Travel Dist (mi)"   = distance_match_travel_distance,
             "Overlap"            = distance_match_overlap,
             "Connections"        = service_connections_count,
             "Capital Costs"      = total_capital_costs,
             "Markup"             = total_markup,
             "Total Cost"         = total_project_cost)
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # Summary — scoped to selected system
  output$cost_summary <- renderPrint({
    req(costs_data(), selected())
    sel   <- selected()
    pairs <- costs_data() %>% st_drop_geometry() %>% filter(pwsid == sel)
    
    if (nrow(pairs) == 0) { cat("Select a system on the map.\n"); return() }
    
    cat("═══════════════════════════════════════════════════════════\n")
    cat("  COST SUMMARY —", pairs$pws_name[1], "\n")
    cat("  PWSID:", sel, "| Partners:", nrow(pairs), "\n")
    cat("═══════════════════════════════════════════════════════════\n\n")
    
    for (i in seq_len(nrow(pairs))) {
      p <- pairs[i, ]
      cat(sprintf("Partner %d: %s\n", i, p$distance_match_pwsid))
      cat(sprintf("  Travel distance:    %.2f mi\n",  p$distance_match_travel_distance))
      cat(sprintf("  Overlap:            %s\n",       p$distance_match_overlap))
      cat(sprintf("  New source cost:    %s\n",       scales::dollar(p$new_source_cost)))
      cat(sprintf("  Pipeline cost:      %s\n",       scales::dollar(p$pipe_line_cost)))
      cat(sprintf("  Connection fees:    %s\n",       scales::dollar(p$connection_fees)))
      cat(sprintf("  Service line:       %s\n",       scales::dollar(p$service_line_cost)))
      cat(sprintf("  Admin costs:        %s\n",       scales::dollar(p$admin_costs)))
      cat(sprintf("  CEQA cost:          %s\n",       scales::dollar(p$CEQA_cost)))
      cat("  ───────────────────────────────────────────────────\n")
      cat(sprintf("  Total capital:      %s\n",       scales::dollar(p$total_capital_costs)))
      cat(sprintf("  Total markup:       %s\n",       scales::dollar(p$total_markup)))
      cat(sprintf("  TOTAL PROJECT:      %s\n\n",     scales::dollar(p$total_project_cost)))
    }
    
    cat("AGGREGATE\n")
    cat("─────────────────────────────────────────────────────────\n")
    cat(sprintf("  Min:    %s\n",    scales::dollar(min(pairs$total_project_cost))))
    cat(sprintf("  Median: %s\n",    scales::dollar(median(pairs$total_project_cost))))
    cat(sprintf("  Mean:   %s\n",    scales::dollar(mean(pairs$total_project_cost))))
    cat(sprintf("  Max:    %s\n",    scales::dollar(max(pairs$total_project_cost))))
    cat(sprintf("  Total:  %s\n",    scales::dollar(sum(pairs$total_project_cost))))
    cat("═══════════════════════════════════════════════════════════\n")
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8888, launch.browser = TRUE))