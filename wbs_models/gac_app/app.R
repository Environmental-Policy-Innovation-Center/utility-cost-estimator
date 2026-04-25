# setwd("wbs_models/gac_app/") # for deployment
# Source required libraries
source("load_libraries.R")

# Source modules
source("mod_landing.R")
source("mod_inputs.R")
source("mod_output_db.R")
source("mod_release_notes.R")

# Source utility functions
source("utils.R")
source("populate_wbs_table.R")
source("cost_equations.R")

# Cache google to reduce load times
googlesheets4::gs4_deauth()  # for public sheets, currently anyone with link has

message("Loading cost coefficients at startup...")
.gac_coeff_table <- load_cost_coefficients()
options(gac.coeff_table = .gac_coeff_table)
message("Cost coefficients cached.")

message("Caching Google Sheets reference data at startup...")
load_gac_sheet_cache()
load_critical_design_assumptions_sheet_cache()
message("Sheet cache ready.")

# Source calculation logic 
# AFTER caches are populated so that file-level
# assignments resolve against a live option rather than NULL.
source("calculations_headers.R")


# Define UI
ui <- dashboardPage(

  # ── Header (disabled — replaced by custom top nav) ──────────────────────────
  dashboardHeader(disable = TRUE),

  # ── Sidebar (hidden — kept for tab routing only) ───────────────────────────
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",            tabName = "home"),
      menuItem("System Design",   tabName = "inputs"),
      menuItem("Output Database",  tabName = "output_db")
    )
  ),

  # ── Body ────────────────────────────────────────────────────────────────────
  dashboardBody(
    shinyjs::useShinyjs(),

    # External stylesheet
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
               <path d="M7 16c1-4 3-8 5-8s4 4 5 8" stroke="#0e8a7d" stroke-width="2" stroke-linecap="round"/>
               <circle cx="12" cy="7" r="2" fill="#0e8a7d"/>
             </svg>'),
        "EPIC-Tech \u2014 Water Treatment Cost Estimator"
      ),
      tags$div(
        class = "topnav-links",
        tags$a(id = "nav-how",    href = "#", "How It Works"),
        tags$a(id = "nav-tech",   href = "#", "Technologies"),
        tags$a(id = "nav-method", href = "#", "Methodology"),
        tags$a(id = "nav-launch", href = "#", class = "topnav-btn", "Launch Tool")
      )
    ),

    # ── Sub-navigation bar (visible on tool pages) ──
    tags$div(
      id = "subnav",
      class = "subnav",
      tags$a(id = "subnav-inputs",    href = "#", class = "subnav-link subnav-active", "System Design"),
      tags$a(id = "subnav-output-db", href = "#", class = "subnav-link subnav-disabled", "Output Database")
    ),

    # Loading overlay
    div(id = "loading-overlay",
      div(class = "loading-content",
        div(class = "spinner"),
        h4("Calculating..."),
        p("Please wait while we process your GAC system design")
      )
    ),

    # Tab content
    tabItems(
      tabItem(tabName = "home",      landingUI("landing")),
      tabItem(tabName = "inputs",    inputsUI("inputs")),
      tabItem(tabName = "output_db", outputDbUI("output_db"))
    ),

    # Fixed footer banner
    tags$div(class = "app-footer",
      tags$span(class = "footer-icon", icon("droplet")),
      tags$span(
        tags$span(class = "footer-label", "Source | "),
        tags$a(
          href      = "https://www.epa.gov/sdwa/drinking-water-treatment-technology-unit-cost-models",
          target    = "_blank",
          rel       = "noopener noreferrer",
          class     = "footer-model",
          "Work Breakdown Structure-Based Cost Model Drinking Water Treatment"
        ),
        tags$span(class = "footer-label", "| "),
        uiOutput("footer_version_link", inline = TRUE)
    )
  )
))


# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store calculation results
  results <- reactiveValues(
    calculated = FALSE,
    data = NULL
  )
  
  # Landing page module server
  landingServer("landing", parent_session = session)

  # ── Top-navbar link handlers ──
  # Helper: switch to Home tab then smooth-scroll to a section
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

  # Brand / logo → Home tab (scroll to top)
  shinyjs::onclick("nav-home", {
    updateTabItems(session, "sidebar", "home")
    shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })

  # Section scroll links
  shinyjs::onclick("nav-how",    scroll_to_section("landing-how_section"))
  shinyjs::onclick("nav-tech",   scroll_to_section("landing-tech_section"))
  shinyjs::onclick("nav-method", scroll_to_section("landing-methodology_anchor"))

  # Launch Tool → System Design tab
  shinyjs::onclick("nav-launch", updateTabItems(session, "sidebar", "inputs"))

  # ── Sub-navigation logic ──
  # Show/hide sub-nav and manage active state when sidebar tab changes
  observeEvent(input$sidebar, {
    tab <- input$sidebar
    is_tool <- tab %in% c("inputs", "output_db")

    # Toggle sub-nav visibility
    if (is_tool) {
      shinyjs::runjs("
        document.getElementById('subnav').classList.add('active');
        document.querySelector('.content-wrapper').classList.add('subnav-visible');
      ")
    } else {
      shinyjs::runjs("
        document.getElementById('subnav').classList.remove('active');
        document.querySelector('.content-wrapper').classList.remove('subnav-visible');
      ")
    }

    # Update active sub-nav link
    if (tab == "inputs") {
      shinyjs::runjs("
        document.getElementById('subnav-inputs').classList.add('subnav-active');
        document.getElementById('subnav-output-db').classList.remove('subnav-active');
      ")
    } else if (tab == "output_db") {
      shinyjs::runjs("
        document.getElementById('subnav-output-db').classList.add('subnav-active');
        document.getElementById('subnav-inputs').classList.remove('subnav-active');
      ")
    }
  })

  # Sub-nav click handlers
  shinyjs::onclick("subnav-inputs", updateTabItems(session, "sidebar", "inputs"))
  shinyjs::onclick("subnav-output-db", {
    if (isTRUE(results$calculated)) {
      updateTabItems(session, "sidebar", "output_db")
    }
  })

  # Input module server
  input_data <- inputsServer("inputs")
  
  # Handle calculations with proper async pattern
  observeEvent(input_data$calculate_trigger(), {
    
    # Show spinner IMMEDIATELY with direct JavaScript
    shinyjs::runjs("document.getElementById('loading-overlay').classList.add('active');")
    
    # Use shinyjs::delay to allow browser to render before calculation
    shinyjs::delay(100, {
      
      # Get parameters
      params <- input_data$get_params()
      
      # Perform calculations with error handling
      calc_results <- withCallingHandlers(
        tryCatch({
          
          # Run calculation
          calculate_gac_system(params)
          
        }, error = function(e) {
          
          # Capture full call stack for debugging
          call_stack <- sys.calls()
          stack_str  <- paste(
            sapply(seq_along(call_stack), function(i) {
              paste0("  [", i, "] ", deparse(call_stack[[i]])[1])
            }),
            collapse = "\n"
          )
          
          debug_msg <- paste0(
            "ERROR: ", e$message, "\n",
            "CALL STACK:\n", stack_str
          )
          
          # Log to console (visible in R console / server logs)
          message(debug_msg)
          cat("\n========== CALCULATION ERROR ==========\n",
              debug_msg, "\n",
              "=======================================\n", sep = "")
          
          # Show notification with hint to check logs
          showNotification(
            paste("Error during calculation:", e$message,
                  "| Check server console for full stack trace."),
            type = "error",
            duration = 15
          )
          
          list(success = FALSE, errors = e$message, debug = debug_msg)
          
        }),
        warning = function(w) {
          message("CALC WARNING: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      
      # Process results
      if (!is.null(calc_results) && isTRUE(calc_results$success)) {
        
        # Store results
        results$data <- calc_results
        results$calculated <- TRUE

        # Unlock the Output Database sub-nav link
        shinyjs::runjs("document.getElementById('subnav-output-db').classList.remove('subnav-disabled');")

        # Use another delay for smooth transition
        shinyjs::delay(300, {
          
          # Hide spinner
          shinyjs::runjs("document.getElementById('loading-overlay').classList.remove('active');")
          
          # Another delay before tab switch
          shinyjs::delay(200, {
            
            # Switch to output tab
            updateTabItems(session, "sidebar", "output_db")
            
            # Delay before notification
            shinyjs::delay(100, {
              showNotification(
                "Calculation completed successfully!",
                type = "message",
                duration = 3
              )
            })
          })
        })
        
      } else if (!is.null(calc_results) && !isTRUE(calc_results$success)) {
        
        # Hide spinner
        shinyjs::runjs("document.getElementById('loading-overlay').classList.remove('active');")
        
        # Show error message
        error_msg <- paste(calc_results$errors, collapse = "\n")
        showNotification(
          paste("Calculation failed:", error_msg),
          type = "error",
          duration = 10
        )
        
      }
      
    })
  }, ignoreInit = TRUE)
  
  # Output Database module server
  outputDbServer("output_db", results)

  # Release notes module — capture return value for dynamic footer version
  rn_module <- releaseNotesServer("release_notes")

  # Render footer version label dynamically from the latest sheet entry
  output$footer_version_link <- renderUI({
    ver   <- tryCatch(rn_module$latest_version(), error = function(e) NA_character_)
    label <- if (!is.null(ver) && !is.na(ver) && nchar(ver) > 0) {
      paste("Version", ver)
    } else {
      "Release Notes"
    }
    actionLink("open_release_notes", label = label, class = "footer-version-link")
  })

  observeEvent(input$open_release_notes, {
    showModal(
      modalDialog(
        title = "Release Notes",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        releaseNotesUI("release_notes")
      )
    )
  })

 
  
}

# App
shinyApp(ui = ui, server = server)
