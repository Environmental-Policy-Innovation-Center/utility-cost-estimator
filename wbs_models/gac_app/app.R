library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(tidyverse)


#setwd("wbs_models/gac_app/") # for deployment

# Source modules
source("mod_inputs.R")
source("mod_output_db.R")

# Source utility functions
source("utils.R")
source("populate_wbs_table.R")
source("cost_equations.R")
source("calculations.R")

googlesheets4::gs4_deauth()  # for public sheets, currently anyone with link has 

# Define UI
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "GAC Treatment System Cost Estimator",
    titleWidth = 400
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("System Design", tabName = "inputs", icon = icon("sliders-h")),
      menuItem("Output Database", tabName = "output_db", icon = icon("chart-bar"))
    ),
    div(
      style = "position: absolute; bottom: 44px; width: 100%; padding: 10px 15px;
               border-top: 1px solid rgba(255,255,255,0.1); color: rgba(255,255,255,0.5);
               font-size: 14px;",
      paste0("Last updated: ", format(file.info("app.R")$mtime, "%B %d, %Y"))
      
    ),
    div(
      style = "position: absolute; bottom: 80px; width: 100%; padding: 10px 15px;
               border-top: 1px solid rgba(255,255,255,0.1); color: rgba(255,255,255,0.5);
               font-size: 14px;",
      "V0")
  ),
  
  # Body
  dashboardBody(
    shinyjs::useShinyjs(), 
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        #loading-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(0, 0, 0, 0.6);
          z-index: 99999;
          display: none;
          justify-content: center;
          align-items: center;
        }
        
        #loading-overlay.active {
          display: flex;
        }
        
        .loading-content {
          background: white;
          padding: 30px 40px;
          border-radius: 8px;
          text-align: center;
          box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
        }
        
        .spinner {
          border: 4px solid #f3f3f3;
          border-top: 4px solid #3c8dbc;
          border-radius: 50%;
          width: 50px;
          height: 50px;
          animation: spin 1s linear infinite;
          margin: 0 auto 15px auto;
        }
        
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .small-box {
          border-radius: 5px;
        }
        .nav-tabs-custom {
          margin-bottom: 20px;
        }
        .form-group {
          margin-bottom: 15px;
        }
        label {
          font-weight: 500;
          color: #333;
        }
        .sidebar-menu > li > a {
          padding: 12px 5px 12px 15px;
        }
        h3, h4 {
          color: #3c8dbc;
          font-weight: bold;
        }
        .section-header {
          background-color: #3c8dbc;
          color: white;
          padding: 10px 15px;
          margin: -15px -15px 15px -15px;
          font-weight: bold;
          font-size: 16px;
        }
        .help-block {
          color: #737373;
          font-size: 12px;
          margin-top: 5px;
        }
        .btn-calculate {
          margin-top: 20px;
          font-size: 16px;
          padding: 10px 20px;
        }
          /* ── App footer ── */
        .app-footer {
          position: fixed !important;
          bottom: 0 !important;
          left: 0 !important;
          width: 100% !important;
          z-index: 99998 !important;
          background-color: #922b21;
          color: #ffffff;
          font-size: 13px;
          font-weight: 500;
          padding: 10px 24px;
          display: flex !important;
          align-items: center;
          gap: 10px;
          box-shadow: 0 -3px 10px rgba(0,0,0,0.25);
          letter-spacing: 0.01em;
        }
        .app-footer .footer-icon {
          font-size: 16px;
          opacity: 0.85;
          flex-shrink: 0;
        }
        .app-footer .footer-label {
          opacity: 0.8;
          font-weight: 400;
        }
        .app-footer .footer-model {
          font-weight: 600;
          color: #aed6f1;
        }
        /* push content above footer so it never hides under it */
        .content-wrapper,
        .right-side {
          padding-bottom: 44px !important;
          min-height: calc(100vh - 44px) !important;
        }
        /* ensure footer clears the sidebar */
        @media (min-width: 768px) {
          .app-footer {
            left: 300px !important;
            width: calc(100% - 300px) !important;
          }
        }
      "))
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
      tabItem(tabName = "inputs", inputsUI("inputs")),
      tabItem(tabName = "output_db", outputDbUI("output_db"))
    ),

    # Fixed footer banner — always visible
    tags$div(class = "app-footer",
      tags$span(class = "footer-icon", icon("droplet")),
      tags$span(
        tags$span(class = "footer-label", "Source | "),
        tags$span(class = "footer-model",
          "Work Breakdown Structure-Based Cost Model: Granular Activated Carbon (GAC) Drinking Water Treatment"
        ),
        tags$span(class = "footer-label",
          HTML("&ensp;&bull;&ensp;US EPA Office of Water")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store calculation results
  results <- reactiveValues(
    calculated = FALSE,
    data = NULL
  )
  
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
        
  #browser()
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

 
  
}

# App
shinyApp(ui = ui, server = server)
