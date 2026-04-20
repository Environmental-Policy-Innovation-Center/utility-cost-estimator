# Inputs Module
# System Design Interface

inputsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
# Add the overlay div
div(id = "loading-overlay",
  div(class = "loading-content",
    div(class = "spinner"),
    h4("Calculating..."),
    p("Please wait while we process your request")
  )
), 
    column(
      width = 8,

      # Contaminant Information Section
      box(
        title = NULL,
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        
        div(class = "section-header", "Step 1: Contaminant Information"),

        fluidRow(
          column(
            width = 12,
            selectInput(
              ns("contam_I"),
              "Contaminant Type:",
              choices = c("", get_contam_type()),
              selected = get_contam_type()[8],
              width = "100%"
            ),
            # ── "Other" Contaminant: required inputs ────────────────────────────
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,

              tags$hr(style = "margin: 6px 0 14px;"),

              # Contaminant Name
              textInput(
                ns("cont_name"),
                "Contaminant Name",
                width = "100%"
              ),

              # ── Carbon Life ─────────────────────────────────────────────────────
              tags$div(
                style = "margin-top: 14px;",
                tags$label("Carbon Life Input Type:", class = "control-label"),
                radioButtons(
                  ns("carbon_life_type"),
                  label    = NULL,
                  choices  = c(
                    "Bed Volumes"         = "bed_volumes",
                    "Months"              = "months",
                    "Freundlich Isotherm" = "freundlich"
                  ),
                  selected = "bed_volumes",
                  inline   = TRUE
                ),
                shiny::conditionalPanel(
                  condition = "input['carbon_life_type'] == 'bed_volumes'",
                  ns = ns,
                  sliderInput(
                    ns("carbon_life_bed_vol"),
                    "Typical Carbon Life (bed volumes)",
                    min = 5000, max = 65000, value = 40000, step = 1000,
                    width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input['carbon_life_type'] == 'months'",
                  ns = ns,
                  sliderInput(
                    ns("carbon_life_months"),
                    "Typical Carbon Life (months)",
                    min = 6, max = 12, value = 9, step = 1,
                    width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input['carbon_life_type'] == 'freundlich'",
                  ns = ns,
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("freund_kf"),
                        HTML("K<sub>f</sub> \u2014 (\u00b5g/g)(L/\u00b5g)<sup>1/n</sup>"),
                        value = NA_real_, min = 0,
                        width = "100%"
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("freund_1_n"),
                        "1/n (dimensionless)",
                        value = NA_real_, min = 0, max = 1, step = 0.01,
                        width = "100%"
                      )
                    )
                  )
                )
              ),

              # ── Contaminant Removal ─────────────────────────────────────────────
              tags$div(
                style = "margin-top: 14px;",
                tags$label("Contaminant Removal Input Type:", class = "control-label"),
                radioButtons(
                  ns("removal_input_type"),
                  label    = NULL,
                  choices  = c(
                    "EBCT"                               = "ebct",
                    "Contaminant Removal %"              = "removal_pct",
                    "Influent & Effluent Concentrations" = "conc"
                  ),
                  selected = "ebct",
                  inline   = TRUE
                ),
                shiny::conditionalPanel(
                  condition = "input['removal_input_type'] == 'ebct'",
                  ns = ns,
                  numericInput(
                    ns("ebct"),
                    "Empty Bed Contact Time (EBCT) (min)",
                    value = 7.5, min = 0.1, step = 0.5,
                    width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input['removal_input_type'] == 'removal_pct'",
                  ns = ns,
                  numericInput(
                    ns("removal_pct"),
                    "Contaminant Removal (enter as fraction, e.g. 0.95)",
                    value = 0.95, min = 0, max = 1, step = 0.01,
                    width = "100%"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input['removal_input_type'] == 'conc'",
                  ns = ns,
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("C_0"),
                        "Influent Concentration (mg/L)",
                        value = NA_real_, min = 0,
                        width = "100%"
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("C_b"),
                        "Effluent Target (mg/L)",
                        value = NA_real_, min = 0,
                        width = "100%"
                      )
                    )
                  )
                )
              ),

              # ── System Parameters ───────────────────────────────────────────────
              tags$div(
                style = "margin-top: 14px;",
                numericInput(
                  ns("number_contactors_series"),
                  "Min. Contactors in Series",
                  value = 1, min = 1, step = 1,
                  width = "100%"
                ),
                numericInput(
                  ns("backwash_interval"),
                  "Interval Between Backwashes (hrs)",
                  value = 72, min = 1, step = 1,
                  width = "100%"
                ),
                selectInput(
                  ns("spent_carbon_managment"),
                  "Spent Carbon Management",
                  choices = c(
                    "regeneration on-site",
                    "regeneration off-site (non-hazardous)",
                    "throwaway (non-hazardous)",
                    "regeneration off-site (hazardous)",
                    "throwaway (hazardous)",
                    "throwaway (radioactive)",
                    "throwaway (radioactive hazardous)"
                  ),
                  selected = "regeneration off-site (non-hazardous)",
                  width = "100%"
                ),
                selectInput(
                  ns("residuals_disposal"),
                  "Discharge Option for Spent Backwash",
                  choices = c(
                    "surface water",
                    "POTW",
                    "recycle",
                    "septic system",
                    "evaporation pond"
                  ),
                  selected = "POTW",
                  width = "100%"
                ),
                # Characteristics of solids — only required when evaporation pond selected
                shiny::conditionalPanel(
                  condition = "input['residuals_disposal'] == 'evaporation pond'",
                  ns = ns,
                  selectInput(
                    ns("solids_haz"),
                    "Characteristics of Evaporation Pond Solids",
                    choices = c(
                      "non-hazardous",
                      "hazardous",
                      "radioactive",
                      "radioactive hazardous"
                    ),
                    selected = "non-hazardous",
                    width = "100%"
                  )
                ),
                selectInput(
                  ns("gac_transfer_method"),
                  "GAC Transfer Method",
                  choices = c(
                    "manual transfer",
                    "eductors"
                  ),
                  selected = "manual transfer",
                  width = "100%"
                )
              )
            )
          )
        )
        # ,
        
        # fluidRow(
        #   column(
        #     width = 6,
        #     numericInput(
        #       ns("C_0"),
        #       "Influent Concentration (ng/L or µg/L):",
        #       value = 100,
        #       min = 0,
        #       max = 100000,
        #       step = 1,
        #       width = "100%"
        #     )
        #   ),
        #   column(
        #     width = 6,
        #     numericInput(
        #       ns("C_b"),
        #       "Effluent Target (ng/L or µg/L):",
        #       value = 10,
        #       min = 0,
        #       max = 10000,
        #       step = 0.1,
        #       width = "100%"
        #     )
        #   )
        # )
      ),
      
      # Design Type
      box(
        title = NULL,
        status = "primary",
        solidHeader = FALSE,
        width = 12,
        
        div(class = "section-header", "Step 2: Design Type"),
        
        selectInput(
          ns("design_type"),
          "Design Type",
          choices = c("", get_design_type()),
          selected = get_design_type()[1],
          width = "100%"
        )
      ),

      # Flow Rates Section
      box(
        title = NULL,
        status = "primary",
        solidHeader = FALSE,
        width = 12,

        div(class = "section-header", "Step 3: Flow Rates"),

        # Label row: "Design Flow Rate:" + info toggle link
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;",
          tags$label("Design Flow Rate:", style = "font-weight: 500; color: #212529; margin: 0;"),
          actionLink(
            ns("toggle_flow_ref"),
            label = tagList(icon("circle-info"), "ref"),
            style = paste0(
              "font-size: 12px; color: #0e8a7d; text-decoration: none;",
              "display: inline-flex; align-items: center; gap: 4px;"
            )
          )
        ),

        # Collapsible reference table
        shinyjs::hidden(
          tags$div(
            id = ns("flow_ref_table"),
            style = paste0(
              "background: #e8f6f4; border: 1px solid #b2dfdb; border-radius: 6px;",
              "padding: 10px 14px; margin-bottom: 10px; font-size: 12px;"
            ),
            tags$p(
              style = "font-weight: 600; color: #0a2540; margin: 0 0 2px 0;",
              "Typical system sizes"
            ),
            tags$p(
              style = "color: #555; font-size: 11px; margin: 0 0 8px 0;",
              HTML(paste0(
                "<strong>gpcd</strong> (gallons per capita per day) is the amount of water ",
                "in gallons that the average person in a region uses every day. ",
                "Population estimates here use two benchmarks: ",
                "<strong>82 gpcd</strong> (residential domestic use) and ",
                "<strong>139 gpcd</strong> (total public supply per capita, incl. commercial/industrial — ",
                "derived from 39,200 Mgal/d ÷ 282M people served; standard basis for treatment plant sizing). ",
                "Source: <a href=\"https://doi.org/10.3133/cir1441\" target=\"_blank\" rel=\"noopener\" style=\"color:#0a2540;\">",
                "Dieter et al., USGS Circular 1441 (2018)</a>."
              ))
            ),
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$thead(
                tags$tr(
                  lapply(
                    c("Scale", "MGD", "GPM",
                      "~Pop (82 gpcd, residential)",
                      "~Pop (139 gpcd, plant design)"),
                    function(h)
                      tags$th(h, style = paste0(
                        "text-align: left; color: #555; font-weight: 600;",
                        "padding: 2px 8px 4px 0; border-bottom: 1px solid #c5dff0;",
                        "white-space: nowrap;"
                      ))
                  )
                )
              ),
              tags$tbody(
                lapply(
                  list(
                    c("Very small",  "0.01",  "6.9",    "122",      "72"),
                    c("Small",       "0.1",   "69",     "1,220",    "719"),
                    c("Medium",      "1.0",   "694",    "12,195",   "7,194"),
                    c("Large",       "10.0",  "6,944",  "121,951",  "71,942"),
                    c("Very large",  "100.0", "69,444", "1.2M",     "719,424")
                  ),
                  function(row) {
                    tags$tr(
                      lapply(row, function(cell)
                        tags$td(cell, style = "padding: 3px 8px 3px 0; color: #212529; white-space: nowrap;")
                      )
                    )
                  }
                )
              )
            )
          )
        ),

        # Flow input + units side by side (no label — handled above)
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              ns("design_flow_I"),
              label    = NULL,
              choices  = c("", get_design_number()),
              selected = get_design_number()[1],
              options  = list(
                create      = TRUE,
                placeholder = "Select or type a value"
              ),
              width = "100%"
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("df_units"),
              label   = NULL,
              choices = c("", "MGD", "GPM"),
              selected = "MGD",
              width   = "100%"
            )
          )
        ),

        # Live conversion + population callout
        uiOutput(ns("flow_callout"))

      ),
      
      # Calculate Button
      box(
        title = NULL,
        status = "success",
        solidHeader = FALSE,
        width = 12,
        
        actionButton(
          ns("calculate"),
          "Calculate System Cost",
          icon = icon("calculator"),
          class = "btn-success btn-lg btn-block btn-calculate"
        )
      )

    )
  )
}

inputsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store standard inputs and track updates
    standard_inputs_data <- reactiveVal(NULL)
    suppress_updates <- reactiveVal(FALSE)
    
    # Observer for design flow units logic (predefined vs custom)
    observeEvent(input$design_flow_I, {
      #browser()

      if (!is.null(input$design_flow_I) && input$design_flow_I != "") {
        predefined_numbers <- get_design_number()
        
        if (input$design_flow_I %in% predefined_numbers) {
          # Pre-defined: force MGD
          updateSelectInput(
            session, 
            "df_units",
            choices = c("MGD"),
            selected = "MGD"
          )
        } else {
          # Custom: allow MGD or GPM
          updateSelectInput(
            session, 
            "df_units",
            choices = c("", "MGD", "GPM"),
            selected = ""
          )
        }
      }
    }, ignoreInit = TRUE)
    
    # Main observer to fetch and apply standard inputs when key parameters change
    observeEvent(
      list(input$contam_I, input$design_type, input$design_flow_I), 
      {
        #browser()

        # Only proceed if all three inputs are selected
        req(input$contam_I, input$design_type, input$design_flow_I)
        
        # Skip if any value is empty string
        if (input$contam_I == "" || input$design_type == "" || input$design_flow_I == "") {
          return()
        }
        
        # Fetch standard inputs from Google Sheets
        tryCatch({
          
          std_inputs <- get_standard_inputs(
            contam_selection = which(get_contam_type() == input$contam_I),
            design_type = which(get_design_type() == input$design_type),
            design_number = which(get_design_number() == as.numeric(input$design_flow_I))
          )
          
          if (!is.null(std_inputs)) {
            # Store the data
            standard_inputs_data(std_inputs)
            
            # Show success notification
            showNotification(
              "Standard inputs loaded successfully!",
              type = "message",
              duration = 3
            )
            
          } else {
            # No matching standard inputs found
            showNotification(
              "No standard inputs found for this combination. Using default values.",
              type = "warning",
              duration = 4
            )
          }
          
        }, error = function(e) {
          # Error fetching standard inputs
          showNotification(
            paste("Error loading standard inputs:", e$message),
            type = "error",
            duration = 5
          )
        })
        
      }
      # , 
      # ignoreInit = TRUE
    )
    
    # ── Flow reference table toggle ──────────────────────────────────────────
    observeEvent(input$toggle_flow_ref, {
      shinyjs::toggle("flow_ref_table", anim = TRUE, animType = "slide", time = 0.2)
    }, ignoreNULL = TRUE)

    # ── Live flow conversion + population callout ─────────────────────────────
    output$flow_callout <- renderUI({
      flow_raw <- suppressWarnings(as.numeric(input$design_flow_I))
      units    <- input$df_units

      # Nothing to show until both fields are filled
      if (is.null(flow_raw) || is.na(flow_raw) || flow_raw <= 0 ||
          is.null(units) || units == "") return(NULL)

      # Convert to MGD and GPM
      MGD_PER_GPM <- 0.00144
      flow_mgd <- if (units == "MGD") flow_raw else flow_raw * MGD_PER_GPM
      flow_gpm <- if (units == "GPM") flow_raw else flow_raw / MGD_PER_GPM

      # Population equivalents — Dieter et al., USGS Circular 1441 (2018)
      # 82 gpcd  = domestic residential use            = 0.000082 MGD/person
      # 139 gpcd = total public supply per capita      = 0.000139 MGD/person
      #            (39,200 Mgal/d / 282M people served; basis for plant sizing)
      pop_res   <- flow_mgd / 0.000082
      pop_plant <- flow_mgd / 0.000139

      # Format helpers
      fmt_flow <- function(x) {
        if (x >= 1) formatC(x, format = "f", digits = 2, big.mark = ",")
        else formatC(x, format = "f", digits = 4)
      }
      fmt_pop <- function(x) {
        if (x >= 1e6)      paste0(formatC(x / 1e6, format = "f", digits = 2), "M")
        else if (x >= 1e3) paste0(formatC(x / 1e3, format = "f", digits = 1), "K")
        else                formatC(round(x), format = "d", big.mark = ",")
      }

      # Alternate unit label
      alt_label <- if (units == "MGD") {
        paste0(fmt_flow(flow_gpm), " GPM")
      } else {
        paste0(fmt_flow(flow_mgd), " MGD")
      }

      tags$div(
        style = "margin-top: 6px;",

        # Row 1: unit conversion
        tags$div(
          style = paste0(
            "display: flex; align-items: center; gap: 12px;",
            "background: #e8f6f4; border: 1px solid #b2dfdb;",
            "border-radius: 6px 6px 0 0; padding: 7px 14px;",
            "font-size: 13px; color: #0e8a7d;"
          ),
          tags$span(icon("arrows-left-right"), style = "opacity: 0.7;"),
          tags$span(tags$strong(alt_label), style = "white-space: nowrap;")
        ),

        # Row 2: residential population (82 gpcd)
        tags$div(
          style = paste0(
            "display: flex; align-items: center; gap: 12px;",
            "background: #e8f6f4; border: 1px solid #b2dfdb; border-top: none;",
            "padding: 7px 14px; font-size: 13px; color: #1a5276;"
          ),
          tags$span(icon("house"), style = "opacity: 0.7; font-size: 12px;"),
          tags$span(
            HTML(paste0(
              "~<strong>", fmt_pop(pop_res), "</strong> people served",
              " <span style='font-size:11px; opacity:0.65;'>residential use, 82 gpcd (gallons per capita per day) — <a href=\"https://doi.org/10.3133/cir1441\" target=\"_blank\" rel=\"noopener\" style=\"color:#1a5276;\">USGS Circular 1441</a></span>"
            )),
            style = "white-space: nowrap;"
          )
        ),

        # Row 3: total public supply population (139 gpcd)
        tags$div(
          style = paste0(
            "display: flex; align-items: center; gap: 12px;",
            "background: #e8f6f4; border: 1px solid #b2dfdb; border-top: none;",
            "border-radius: 0 0 6px 6px; padding: 7px 14px;",
            "font-size: 13px; color: #1a5276;"
          ),
          tags$span(icon("building"), style = "opacity: 0.7; font-size: 12px;"),
          tags$span(
            HTML(paste0(
              "~<strong>", fmt_pop(pop_plant), "</strong> people served",
              " <span style='font-size:11px; opacity:0.65;'>total public supply, 139 gpcd (gallons per capita per day) — <a href=\"https://doi.org/10.3133/cir1441\" target=\"_blank\" rel=\"noopener\" style=\"color:#1a5276;\">USGS Circular 1441</a></span>"
            )),
            style = "white-space: nowrap;"
          )
        )
      )
    })

    # Reactive value to track calculation trigger
    calc_trigger <- reactiveVal(0)
    
    # Observe calculate button
    observeEvent(input$calculate, {
      calc_trigger(calc_trigger() + 1)
    })
    
    # Return list of reactive functions
    list(
      calculate_trigger = reactive(calc_trigger()),
      
      # Return the stored standard inputs data
      get_standard_inputs_data = reactive(standard_inputs_data()),
      
      get_params = reactive({
        is_other <- isTRUE(input$contam_I == "Other")
        std      <- standard_inputs_data()

        # Resolve carbon life type index (matches workbook freund_type encoding:
        #   1 = months, 2 = Freundlich isotherm, 3 = bed volumes)
        other_freund_type <- if (is_other) {
          switch(input$carbon_life_type,
                 "bed_volumes" = 3L,
                 "months"      = 1L,
                 "freundlich"  = 2L,
                 3L)
        } else NULL

        # Resolve contaminant removal type index
        # (1 = contaminant removal %, 2 = influent/effluent conc, 3 = EBCT)
        other_ebct_type <- if (is_other) {
          switch(input$removal_input_type,
                 "ebct"        = 3L,
                 "removal_pct" = 1L,
                 "conc"        = 2L,
                 3L)
        } else NULL

        list(
          # Flow rates
          design_flow        = input$design_flow_I,
          design_flow_units  = input$df_units,
          average_flow       = std$average_flow,
          average_flow_units = std$average_flow_units,

          # Contaminant
          contaminant     = input$contam_I,
          cont_name       = if (is_other) input$cont_name else NULL,
          influent_conc   = if (is_other && isTRUE(input$removal_input_type == "conc")) input$C_0  else std$C_0,
          effluent_target = if (is_other && isTRUE(input$removal_input_type == "conc")) input$C_b  else std$C_b,

          # Design type
          design_type = input$design_type,

          # Design approach
          ebct_type = if (is_other) other_ebct_type else std$ebct_input_type,
          ebct      = if (is_other && isTRUE(input$removal_input_type == "ebct")) input$ebct else std$ebct,

          freund_type = if (is_other) other_freund_type else std$freund_type,
          freund_1    = if (is_other) {
            switch(input$carbon_life_type,
                   "bed_volumes" = input$carbon_life_bed_vol,
                   "months"      = input$carbon_life_months,
                   "freundlich"  = input$freund_kf,
                   input$carbon_life_bed_vol)
          } else std$freund_1,
          freund_2        = if (is_other && isTRUE(input$carbon_life_type == "freundlich")) input$freund_1_n else std$freund_2,
          bed_life_direct = std$bed_life_direct,

          # Contactor configuration
          tank_geometry            = std$tank_geom_I,
          num_trains               = NULL,  # always auto-calculated; Num_tanks_I is contactors-in-series
          num_contactors_in_series = if (is_other) input$number_contactors_series else std$Num_tanks_I,
          redundancy               = std$NRD_I,
          bed_depth                = std$bed_depth,
          vessel_diameter          = std$comm_diam,
          vessel_height_length     = std$comm_height_length,
          basin_length             = std$basin_length,
          basin_width              = std$basin_width,
          basin_depth              = std$basin_op_depth,

          # Backwash
          no_backwash       = std$no_backwash_I,
          backwash_interval = if (is_other) input$backwash_interval else std$back_interval_I,
          no_backwash_tank  = std$no_back_tank_I,
          regen_type        = if (is_other) input$spent_carbon_managment else std$regen_type_I,

          # Residuals
          residuals_disposal = if (is_other) input$residuals_disposal  else std$res_s2_opt_I,
          residuals_tank     = std$res_s1_opt_I,
          transfer_method    = if (is_other) input$gac_transfer_method  else std$transfer_method_I,
          solids_hazardous   = if (is_other && isTRUE(input$residuals_disposal == "evaporation pond")) input$solids_haz else std$solids_haz_I,

          # Pumps
          service_pumps   = std$lines_pump_I,
          backwash_pumps  = std$back_pumps_I,
          residuals_pumps = std$res_pumps_I,

          # Automation
          automation_level = std$component_level_I,
          manual_override  = std$manual_I,

          # Site
          include_buildings = std$include_buildings_I,
          include_hvac      = std$include_HVAC_I,
          include_land      = std$include_land_I,
          retrofit          = std$retrofit_I,

          # Standard inputs data (if available)
          standard_inputs = std
        )
      })
    )
  })
}
