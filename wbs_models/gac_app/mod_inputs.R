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
  tags$div(
          class = "technology-header",
          h1("Technology: Granular Activated Carbon (GAC)")
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

              # Bootstrap popover initialisation (runs once when DOM is ready)
              tags$script(HTML("
                $(document).ready(function() {
                  $(document).on('click', '[data-toggle=\"popover\"]', function(e) {
                    e.stopPropagation();
                    $(this).popover('toggle');
                  });
                  $(document).on('click', function(e) {
                    if (!$(e.target).is('[data-toggle=\"popover\"]') &&
                        $(e.target).closest('.popover').length === 0) {
                      $('[data-toggle=\"popover\"]').popover('hide');
                    }
                  });
                });
              ")),

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
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
                  tags$label("Carbon Life Input Type:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Carbon Life Input Type",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>How GAC treatment capacity is characterized ",
                      "before carbon must be replaced or regenerated.</p>",
                      "<p style='margin:0 0 4px'><b>Bed Volumes (BV):</b> Total volume of water ",
                      "treated divided by the GAC bed volume. The preferred input when a ",
                      "site-specific or literature BV value is available.</p>",
                      "<ul style='padding-left:16px;margin:2px 0 6px'>",
                      "<li>VOCs (e.g., TCE/PCE): ~40,000\u201366,600 BV</li>",
                      "<li>HAA5 / TTHM: ~20,000\u201340,000 BV</li>",
                      "</ul>",
                      "<p style='margin:0 0 4px'><b>Months:</b> Direct service-life estimate ",
                      "when pilot data or operating records are available.</p>",
                      "<ul style='padding-left:16px;margin:2px 0 0'>",
                      "<li>HAA5 / TTHM: typically 6\u201318 months</li>",
                      "</ul>"
                    )
                  )
                ),
                radioButtons(
                  ns("carbon_life_type"),
                  label    = NULL,
                  choices  = c(
                    "Bed Volumes" = "bed_volumes",
                    "Months"      = "months"
                    # "Freundlich Isotherm" = "freundlich"  # not available for Other path
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
                )
                # Freundlich Isotherm inputs — commented out for Other path;
                # available for named contaminants via standard_inputs.
                # shiny::conditionalPanel(
                #   condition = "input['carbon_life_type'] == 'freundlich'",
                #   ns = ns,
                #   fluidRow(
                #     column(6, numericInput(ns("freund_kf"),
                #       HTML("K<sub>f</sub> \u2014 (\u00b5g/g)(L/\u00b5g)<sup>1/n</sup>"),
                #       value = NA_real_, min = 0, width = "100%")),
                #     column(6, numericInput(ns("freund_1_n"),
                #       "1/n (dimensionless)",
                #       value = NA_real_, min = 0, max = 1, step = 0.01, width = "100%"))
                #   )
                # )
              ),

              # ── Contaminant Removal ─────────────────────────────────────────────
              tags$div(
                style = "margin-top: 14px;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
                  tags$label("Contaminant Removal Input Type:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Empty Bed Contact Time (EBCT)",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>The theoretical time water spends passing ",
                      "through the GAC bed if the bed were empty of media. Longer EBCT ",
                      "provides greater treatment but requires larger or more vessels.</p>",
                      "<ul style='padding-left:16px;margin:0'>",
                      "<li>VOCs (TCE/PCE): typically 5\u201315 min</li>",
                      "<li>HAA5: typically 7.5\u201315 min</li>",
                      "<li>TTHM: typically 7.5\u201315 min</li>",
                      "</ul>"
                    )
                  )
                ),
                radioButtons(
                  ns("removal_input_type"),
                  label    = NULL,
                  choices  = c("EBCT" = "ebct"),
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
                )
              ),

              # ── Design Flow (Other path — uses Step 3 design_flow_I dropdown) ──
              # No separate design flow input here; the standard Step 3 dropdown
              # (design_flow_I) is used for "Other" contaminant as well.

              # ── System Parameters ───────────────────────────────────────────────
              tags$div(
                style = "margin-top: 14px;",

                # Contactors in series
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;",
                  tags$label("Min. Contactors in Series:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Contactors in Series",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>Number of GAC vessels that water flows through ",
                      "sequentially within each treatment train. Series arrangement provides ",
                      "lead\u2013lag operation, improving carbon utilization and providing a ",
                      "compliance safety margin when the lead vessel is exhausted.</p>",
                      "<ul style='padding-left:16px;margin:0'>",
                      "<li><b>1 in series:</b> standard for most small systems and single-pass designs</li>",
                      "<li><b>2 in series:</b> recommended for strict MCL compliance (e.g., VOCs, PFAS) ",
                      "where breakthrough in the lead vessel must be caught before treated water leaves the plant</li>",
                      "</ul>"
                    )
                  )
                ),
                numericInput(
                  ns("number_contactors_series"),
                  label = NULL,
                  value = 1, min = 1, step = 1,
                  width = "100%"
                ),

                # Backwash interval
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px; margin-top: 10px;",
                  tags$label("Interval Between Backwashes (hrs):", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Interval Between Backwashes",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>Hours between periodic backwash cycles. ",
                      "Backwashing removes suspended solids that accumulate and cause head loss; ",
                      "it does <em>not</em> restore GAC adsorption capacity.</p>",
                      "<p style='margin:0'>Typical range: 72\u2013168 hrs (3\u20137 days). ",
                      "Shorter intervals are used when turbidity or suspended solids loading ",
                      "is high.</p>"
                    )
                  )
                ),
                numericInput(
                  ns("backwash_interval"),
                  label = NULL,
                  value = 72, min = 1, step = 1,
                  width = "100%"
                ),

                # Spent Carbon Management
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px; margin-top: 10px;",
                  tags$label("Spent Carbon Management:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Spent Carbon Management",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>How exhausted GAC is handled once treatment ",
                      "capacity is depleted. Hazardous classification is determined by RCRA ",
                      "regulations based on contaminant type and concentration.</p>",
                      "<ul style='padding-left:16px;margin:0'>",
                      "<li><b>Regeneration off-site (non-hazardous):</b> spent GAC is ",
                      "thermally reactivated at a commercial facility and returned for reuse. ",
                      "Appropriate for HAA5 and TTHM, where spent carbon is not a listed or ",
                      "characteristic hazardous waste under RCRA.</li>",
                      "<li><b>Regeneration off-site (hazardous):</b> same process but the ",
                      "spent carbon qualifies as RCRA hazardous waste and requires a licensed ",
                      "hazardous waste transporter and facility. Typical for chlorinated VOCs ",
                      "(e.g., TCE, PCE) which are F-listed wastes.</li>",
                      "<li><b>Throwaway (hazardous):</b> one-time disposal at a permitted ",
                      "hazardous waste landfill without regeneration. May be cost-effective for ",
                      "very small flows of chlorinated VOCs where carbon volume is low.</li>",
                      "</ul>"
                    )
                  )
                ),
                selectInput(
                  ns("spent_carbon_managment"),
                  label = NULL,
                  choices = c(
                    "regeneration off-site (non-hazardous)",
                    "regeneration off-site (hazardous)",
                    "throwaway (hazardous)"
                    # "regeneration on-site"            — not applicable for Other path
                    # "throwaway (non-hazardous)"       — not applicable for Other path
                    # "throwaway (radioactive)"         — not applicable for Other path
                    # "throwaway (radioactive hazardous)"— not applicable for Other path
                  ),
                  selected = "regeneration off-site (non-hazardous)",
                  width = "100%"
                ),

                # Discharge Option for Spent Backwash
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px; margin-top: 10px;",
                  tags$label("Discharge Option for Spent Backwash:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "Discharge Option for Spent Backwash",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>Destination for backwash water generated during ",
                      "periodic bed-cleaning cycles.</p>",
                      "<p style='margin:0 0 6px'><b>POTW (Publicly Owned Treatment Works):</b> ",
                      "Backwash is discharged to the local sewerage system under a pretreatment ",
                      "permit. This is the standard disposal route for municipal GAC systems ",
                      "treating VOCs, HAA5, or TTHM, as the concentrations in backwash water are ",
                      "typically low enough to meet pretreatment standards.</p>",
                      "<p style='margin:0'>Alternative discharge routes (surface water, recycle, ",
                      "septic, evaporation pond) are not available in this simplified input mode.</p>"
                    )
                  )
                ),
                selectInput(
                  ns("residuals_disposal"),
                  label = NULL,
                  choices = c("POTW"),
                  selected = "POTW",
                  width = "100%"
                ),

                # Residuals holding tank — hardcoded default note
                tags$p(
                  style = "margin: 10px 0 0; font-size: 12px; color: #666; font-style: italic;",
                  tags$i(class = "fa fa-info-circle", style = "margin-right: 4px; color: #1a5276;"),
                  "Residuals holding tank: fixed at ",
                  tags$b("no holding tank"),
                  " for this calculation path. Direct discharge to POTW without intermediate storage is the standard assumption for VOCs, HAA5, and TTHM at the flow sizes most commonly modeled here. At design flows above 1 MGD a holding tank may be warranted, but it's not yet configurable."
                ),

                # GAC Transfer Method
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px; margin-top: 10px;",
                  tags$label("GAC Transfer Method:", class = "control-label",
                             style = "margin-bottom: 0;"),
                  tags$i(
                    class = "fa fa-info-circle",
                    style = "color: #1a5276; cursor: pointer; font-size: 14px;",
                    `data-toggle`    = "popover",
                    `data-placement` = "right",
                    `data-trigger`   = "click",
                    `data-html`      = "true",
                    `data-container` = "body",
                    title = "GAC Transfer Method",
                    `data-content`   = paste0(
                      "<p style='margin:0 0 6px'>How spent or virgin GAC is moved into and out ",
                      "of the contactors during changeout.</p>",
                      "<ul style='padding-left:16px;margin:0'>",
                      "<li><b>Manual transfer:</b> Carbon is sluiced or vacuum-transferred by ",
                      "an operator using portable equipment. Standard for small systems ",
                      "(\u22641 MGD) and the typical assumption for VOCs, HAA5, and TTHM at ",
                      "small-to-medium flows. Lower capital cost but higher labor per event.</li>",
                      "<li><b>Eductors:</b> Hydraulic eductors use water pressure to slurry ",
                      "the carbon through fixed piping to a transport vessel. Preferred for ",
                      "larger systems (generally \u22655 MGD) where frequent changeouts or high ",
                      "carbon volumes make manual handling impractical.</li>",
                      "</ul>"
                    )
                  )
                ),
                selectInput(
                  ns("gac_transfer_method"),
                  label = NULL,
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
        ),

        tags$p(
          style = "margin: 6px 0 0; font-size: 12px; color: #666; font-style: italic;",
          tags$i(class = "fa fa-info-circle", style = "margin-right: 4px; color: #1a5276;"),
          "Pressure vessel is the most widely used design type for GAC systems and has undergone the most extensive testing during development of this estimator. Gravity basin results should be reviewed carefully against workbook values before use."
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
            selectInput(
              ns("design_flow_I"),
              label    = NULL,
              choices  = c("", get_design_number()),
              selected = get_design_number()[1],
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
        uiOutput(ns("flow_callout")),

        # Average flow note — shown only when "Other" contaminant is selected
        uiOutput(ns("avg_flow_display_other"))

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
    
    # Observer for design flow units — all presets are MGD, always lock to MGD
    observeEvent(input$design_flow_I, {
      if (!is.null(input$design_flow_I) && input$design_flow_I != "") {
        updateSelectInput(session, "df_units", choices = c("MGD"), selected = "MGD")
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

        # "Other" uses UI inputs directly — clear any cached standard data and skip fetch
        if (input$contam_I == "Other") {
          standard_inputs_data(NULL)
          return()
        }
        
        # Fetch standard inputs from Google Sheets
        tryCatch({
          
          # Numeric-safe design_number lookup: convert sheet values to numeric before
          # comparing so that character "0.030" matches numeric 0.03 correctly.
          std_inputs <- get_standard_inputs(
            contam_selection = which(get_contam_type() == input$contam_I),
            design_type = which(get_design_type() == input$design_type),
            design_number = {
              dn_vals <- suppressWarnings(as.numeric(get_design_number()))
              df_val  <- suppressWarnings(as.numeric(input$design_flow_I))
              which(abs(dn_vals - df_val) < 1e-9)
            }
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

    # ── Average flow display for Other path ──────────────────────────────────
    output$avg_flow_display_other <- renderUI({
      req(input$contam_I == "Other")
      other_avg_lookup <- c(
        "0.030"  = 0.007,  "0.124"  = 0.035,  "0.305"  = 0.094,
        "0.740"  = 0.251,  "2.152"  = 0.819,  "7.365"  = 3.200,
        "22.614" = 11.066, "75.072" = 37.536
      )
      df_num <- suppressWarnings(as.numeric(input$design_flow_I %||% "0.124"))
      df <- sprintf("%.3f", df_num)
      af <- unname(other_avg_lookup[df])
      tags$p(
        style = "margin: 6px 0 0; font-size: 12px; color: #555;",
        tags$i(class = "fa fa-arrow-right", style = "margin-right: 4px; color: #1a5276;"),
        "Average flow: ",
        tags$b(sprintf("%.3f MGD", af)),
        tags$span(
          style = "color: #888; margin-left: 6px;",
          sprintf("(%.1f%% of design — from EPA WBS standard inputs)",
                  af / as.numeric(df) * 100)
        )
      )
    })

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

        # Resolve carbon life type index (calculation code encoding:
        #   1 = months, 2 = Freundlich isotherm, 3 = BDST, 4 = BV/EBCT)
        other_freund_type <- if (is_other) {
          switch(input$carbon_life_type,
                 "bed_volumes" = 4L,   # BV/EBCT — bed volumes divided by EBCT
                 "months"      = 1L,   # Direct months value
                 "freundlich"  = 2L,   # Freundlich isotherm
                 4L)
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
          # "Other" contaminant uses the same Step 3 design_flow_I dropdown;
          # average flow is auto-derived from the standard lookup table.
          design_flow        = input$design_flow_I,
          design_flow_units  = input$df_units,
          average_flow       = if (is_other) {
            # Auto-derive from the fixed design_flow → average_flow lookup table
            other_avg_lookup <- c(
              "0.030"  = 0.007,  "0.124"  = 0.035,  "0.305"  = 0.094,
              "0.740"  = 0.251,  "2.152"  = 0.819,  "7.365"  = 3.200,
              "22.614" = 11.066, "75.072" = 37.536
            )
            unname(other_avg_lookup[sprintf("%.3f", as.numeric(input$design_flow_I))])
          } else std$average_flow,
          average_flow_units = std$average_flow_units %||% "MGD",

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

          # Contactor configuration
          tank_geometry            = std$tank_geom_I,
          num_trains               = NULL,  # always auto-calculated; Num_tanks_I is contactors-in-series
          num_contactors_in_series = if (is_other) input$number_contactors_series else std$Num_tanks_I,
          redundancy               = std$NRD_I,
          # For "Other", pass NULL so calculate_gac_system() runs AutoSize
          bed_depth                = if (is_other) NULL else std$bed_depth,
          vessel_diameter          = if (is_other) NULL else std$comm_diam,
          vessel_height_length     = if (is_other) NULL else std$comm_height_length,
          basin_length             = if (is_other) NULL else std$basin_length,
          basin_width              = if (is_other) NULL else std$basin_width,
          basin_depth              = if (is_other) NULL else std$basin_op_depth,

          # Backwash
          no_backwash       = std$no_backwash_I,
          backwash_interval = if (is_other) input$backwash_interval else std$back_interval_I,
          no_backwash_tank  = std$no_back_tank_I,
          regen_type        = if (is_other) input$spent_carbon_managment else std$regen_type_I,

          # Residuals
          residuals_disposal = if (is_other) input$residuals_disposal  else std$res_s2_opt_I,
          # Other path: always "no holding tank" — holding tank sizing is not
          # implemented faithfully to the workbook and is not typical for VOCs,
          # HAA5, or TTHM at the flows this path is designed for.
          residuals_tank     = if (is_other) "no holding tank" else std$res_s1_opt_I,
          transfer_method    = if (is_other) input$gac_transfer_method  else std$transfer_method_I,
          solids_hazardous   = if (is_other && isTRUE(input$residuals_disposal == "evaporation pond")) input$solids_haz else std$solids_haz_I,

          # Pumps
          service_pumps   = std$lines_pump_I,
          backwash_pumps  = std$back_pumps_I,
          residuals_pumps = std$res_pumps_I,

          # Automation — pass NULL for "Other" so calculation uses its own defaults
          automation_level = if (is_other) NULL else std$component_level_I,
          manual_override  = if (is_other) NULL else std$manual_I,

          # Site
          include_buildings = std$include_buildings_I,
          include_hvac      = std$include_HVAC_I,
          include_land      = std$include_land_I,
          retrofit          = std$retrofit_I,

          # Add-on flag — 1 when GAC is added to an existing system (e.g. UVAOP Quench).
          # Suppresses inlet flow meter, PLC CPU/ethernet/interface, UPS, workstations,
          # printers, and yard piping in calculate_controls() / compile_capital_costs().
          # std$addon is populated by get_standard_inputs() from the addon_i sheet column.
          add_on = {
            # Sheet stores "add-on" text; also handle numeric 1/0 and "yes"/"true"
            ao_raw <- tolower(trimws(as.character(std$addon %||% "0")))
            av     <- suppressWarnings(as.numeric(ao_raw))
            if (!is.na(av)) as.integer(av)
            else if (ao_raw %in% c("add-on", "addon", "yes", "true")) 1L
            else 0L
          },

          # Standard inputs data (if available)
          standard_inputs = std
        )
      })
    )
  })
}
