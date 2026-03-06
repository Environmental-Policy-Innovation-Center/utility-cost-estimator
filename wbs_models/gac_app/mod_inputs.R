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
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              textInput(
                ns("cont_name"), "Contaminant Name"
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              sliderInput(
                ns("carbon_life_bed_vol"), "Typical Carbon Life (Bed Volumes)",
                min = 5000, max = 80000, value = 40000
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              sliderInput(
                ns("carbon_life_months"), "Typical Carbon Life (Months)",
                min = 6, max = 25, value = 12
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              numericInput(
                ns("ebct"), "EBCT (min)",
                value = 7.5,
                min = 7.5
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              numericInput(
                ns("number_contactors_series"), "Min Contactors in Series",
                value = 1  
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              numericInput(
                ns("backwash_interval"), "Interval Between Backwashes",
                value = 72
              )
            ),
            shiny::conditionalPanel(
              condition = "input['contam_I'] == 'Other'",
              ns = ns,
              selectInput(
                ns("spent_carbon_managment"), "Spent Carbon Management",
                c("Regeneration", "Disposal")
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
        
        fluidRow(
          column(
            width = 6,
            selectizeInput(
              ns("design_flow_I"),
              "Design Flow Rate:",
              choices = c("", get_design_number()),
              selected = get_design_number()[1],
              options = list(
                create = TRUE,
                placeholder = "Select or type a value"
              ),
              width = "100%"
            )
          ),
          column(
            width = 6,
            selectInput(
              ns("df_units"),
              "Units:",
              choices = c("", "MGD", "GPM"),
              selected = "MGD",
              width = "100%"
            )
          )
        )
      
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
        list(
          # Flow rates
          design_flow = input$design_flow_I,
          design_flow_units = input$df_units,
          average_flow = standard_inputs_data()$average_flow,
          average_flow_units = standard_inputs_data()$average_flow_units,
          
          # Contaminant
          contaminant = input$contam_I,
          influent_conc = standard_inputs_data()$C_0,
          effluent_target = standard_inputs_data()$C_b,
          
          # Design type
          design_type = input$design_type,
          
          # Design approach
          ebct_type = standard_inputs_data()$ebct_input_type,
          ebct = standard_inputs_data()$ebct,
          freund_type = standard_inputs_data()$freund_type,
          freund_1 = standard_inputs_data()$freund_1,
          freund_2 = standard_inputs_data()$freund_2,
          bed_life_direct = standard_inputs_data()$bed_life_direct,
          
          # Contactor configuration
          tank_geometry = standard_inputs_data()$tank_geom_I,
          num_trains = NULL,  # always auto-calculated from flow capacity; Num_tanks_I is contactors-in-series
          num_contactors_in_series = standard_inputs_data()$Num_tanks_I,
          redundancy = standard_inputs_data()$NRD_I,
          bed_depth = standard_inputs_data()$bed_depth,
          vessel_diameter = standard_inputs_data()$comm_diam,
          vessel_height_length = standard_inputs_data()$comm_height_length,
          basin_length = standard_inputs_data()$basin_length,
          basin_width = standard_inputs_data()$basin_width,
          basin_depth = standard_inputs_data()$basin_op_depth,
          
          # Backwash
          no_backwash = standard_inputs_data()$no_backwash_I,
          backwash_interval = standard_inputs_data()$back_interval_I,
          no_backwash_tank = standard_inputs_data()$no_back_tank_I,
          regen_type = standard_inputs_data()$regen_type_I,
          
          # Residuals
          residuals_disposal = standard_inputs_data()$res_s2_opt_I,
          residuals_tank = standard_inputs_data()$res_s1_opt_I,
          transfer_method = standard_inputs_data()$transfer_method_I,
          solids_hazardous = standard_inputs_data()$solids_haz_I,
          
          # Pumps
          service_pumps = standard_inputs_data()$lines_pump_I,
          backwash_pumps = standard_inputs_data()$back_pumps_I,
          residuals_pumps = standard_inputs_data()$res_pumps_I,
          
          # Automation
          automation_level = standard_inputs_data()$component_level_I,
          manual_override = standard_inputs_data()$manual_I,
          
          # Site
          include_buildings = standard_inputs_data()$include_buildings_I,
          include_hvac = standard_inputs_data()$include_HVAC_I,
          include_land = standard_inputs_data()$include_land_I,
          retrofit = standard_inputs_data()$retrofit_I,
          
          # Standard inputs data (if available)
          standard_inputs = standard_inputs_data()
        )
      })
    )
  })
}
