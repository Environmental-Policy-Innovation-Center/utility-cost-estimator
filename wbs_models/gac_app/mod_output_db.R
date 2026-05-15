# Output Database Module
# Display calculated output_db range faithful to Excel structure

collapsible_section <- function(title, output_id, ns, start_open = FALSE) {
  btn_id  <- paste0("toggle_", output_id)
  body_id <- paste0("body_",   output_id)
  icon_id <- paste0("icon_",   output_id)

  tags$div(
    class = "collapsible-section",
    style = "margin-bottom: 8px;",

    tags$div(
      style = paste0(
        "display: flex; align-items: center; gap: 8px;",
        "border-bottom: 2px solid #1a5276; padding-bottom: 5px; margin-bottom: 4px;"
      ),
      actionLink(
        ns(btn_id),
        label = tagList(
          tags$span(
            id = ns(icon_id),
            style = "font-size: 11px; color: #1a5276;",
            if (start_open) "\u25BC" else "\u25B6"
          ),
          tags$span(
            title,
            style = "font-size: 16px; font-weight: 600; color: #1a5276; margin-left: 4px;"
          )
        ),
        style = "text-decoration: none; padding: 0;"
      )
    ),

    if (start_open) {
      tags$div(id = ns(body_id), style = "padding-top: 4px;",
        DTOutput(ns(output_id), width = "100%")
      )
    } else {
      shinyjs::hidden(
        tags$div(id = ns(body_id), style = "padding-top: 4px;",
          DTOutput(ns(output_id), width = "100%")
        )
      )
    }
  )
}


# ── Page TOC ─────────────────────────────────────────────────────────────────
# Wraps any UI element with a named anchor div for TOC scroll targets
with_anchor <- function(anchor_id, ...) {
  tagList(
    tags$div(id = anchor_id, style = "scroll-margin-top: 60px;"),
    ...
  )
}

output_db_toc <- function() {
  sections <- list(
    list(id = "anc-summary",    label = "High Level Summary"),
    list(id = "anc-costs",      label = "Cost Details"),
    list(id = "anc-breakdown",  label = "Cost Breakdown"),
    list(id = "anc-equipment",  label = "Equipment Quantities"),
    list(id = "anc-outputdb",   label = "Output Database")
  )

  buttons <- lapply(sections, function(s) {
    tags$a(
      href    = paste0("#", s$id),
      class   = "toc-btn",
      onclick = paste0(
        "event.preventDefault();",
        "var el = document.getElementById('", s$id, "');",
        "if(el){ var top = el.getBoundingClientRect().top + window.pageYOffset - 60;",
        "window.scrollTo({top: top, behavior: 'smooth'}); }"
      ),
      s$label
    )
  })

  tagList(
    # Floating TOC bar
    tags$div(
      id    = "output-toc",
      style = paste0(
        "position: sticky; top: 0; z-index: 900;",
        "background: #fff; border-bottom: 2px solid #1a5276;",
        "padding: 7px 16px; margin-bottom: 16px;",
        "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
        "box-shadow: 0 2px 6px rgba(0,0,0,0.08);"
      ),
      tags$span(
        "Jump to:",
        style = "font-size: 12px; font-weight: 700; color: #555; margin-right: 4px; white-space: nowrap;"
      ),
      tagList(buttons)
    ),
    # TOC button styles
    tags$style(HTML("
      .toc-btn {
        background: #1a5276;
        color: #fff !important;
        border: none;
        border-radius: 4px;
        padding: 4px 10px;
        font-size: 12px;
        cursor: pointer;
        white-space: nowrap;
        text-decoration: none !important;
        line-height: 1.6;
        display: inline-block;
      }
      .toc-btn:hover {
        background: #0a2540;
        color: #fff !important;
      }
    "))
  )
}

outputDbUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    output_db_toc(),

    ### HIGH LEVEL SUMMARY -----
    with_anchor("anc-summary",
    fluidRow(
      tags$div(
          class = "technology-header",
          h1("Technology: Granular Activated Carbon (GAC)")
      ),
      # Contaminant title
      uiOutput(ns("contaminant_title")),
      # Header Summary
      box(
        title = "High Level Summary",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        # Summary boxes — flex row with operators as flex items
        tags$div(
          class = "cost-summary-row",
          tags$div(class = "cost-summary-item", valueBoxOutput(ns("total_capital_summary"), width = NULL)),
          tags$div(class = "lp-step-num", "+"),
          tags$div(class = "cost-summary-item", valueBoxOutput(ns("total_indirect_summary"), width = NULL)),
          tags$div(class = "cost-summary-operator", "+"),
          tags$div(class = "cost-summary-item", valueBoxOutput(ns("total_add_on"), width = NULL)),
          tags$div(class = "cost-summary-operator", "="),
          tags$div(class = "cost-summary-item", valueBoxOutput(ns("total_project_summary"), width = NULL))
        )
      )
  )),  # end with_anchor("anc-summary")

    ### COST DETAILS -----
    with_anchor("anc-costs",
    fluidRow(
      box(
        title = "Cost Details",
        status = "info",
        solidHeader = TRUE,
        width = 12,

        shinyjs::useShinyjs(),        
        # h4("Total Direct Capital Cost", 
        #   style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        # DTOutput(ns("total_direct_table"), width = "100%"),
        # br(),
        
        # h4("Indirect Capital Cost Details", 
        #   style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        # DTOutput(ns("indirect_costs_table"), width = "100%"),
        # br(),

        # h4("Add-on Cost Details",
        #   style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        # DTOutput(ns("addon_costs_table"), width = "100%"),
        # br(),

        # h4("Grand Total Capital Cost", 
        #   style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        # DTOutput(ns("total_project_table"), width = "100%"),
        # br(),
        
        # h4("Annual Operating & Maintenance Costs Details", 
        #   style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        # DTOutput(ns("annual_om_table"), width = "100%")
      
        collapsible_section("Total Direct Capital Cost",             "total_direct_table",   ns, start_open = FALSE),
        collapsible_section("Indirect Capital Cost Details",         "indirect_costs_table", ns, start_open = FALSE),
        collapsible_section("Add-on Cost Details",                   "addon_costs_table",    ns, start_open = FALSE),
        collapsible_section("Grand Total Capital Cost",              "total_project_table",  ns, start_open = FALSE),
        collapsible_section("Annual Operating & Maintenance Costs",  "annual_om_table",      ns, start_open = FALSE)
      )
    )),  # end with_anchor("anc-costs")

### PLOTS -----
    with_anchor("anc-breakdown",
    fluidRow(
  # Cost Summary Chart
      box(
        title = "Cost Breakdown by Category",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        plotOutput(ns("cost_breakdown_chart"), height = 400)
      )
    )),  # end with_anchor("anc-breakdown")

  #### EQUIPMENT PLOT
    with_anchor("anc-equipment",
    fluidRow(
  # Equipment Summary Chart
      box(
        title = "Equipment Quantities",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        plotOutput(ns("equipment_chart"), height = 400)
      )
)),  # end with_anchor("anc-equipment")

    ### OUTPUT DATABASE -----
    with_anchor("anc-outputdb",
    fluidRow(
      # Main Output Database Table
      box(
        title = "Output Database",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
         # Input Parameters Section
        h4("Input Parameters", 
           style = "color: #1a5276; border-bottom: 2px solid #1a5276; padding-bottom: 5px;"),
        DTOutput(ns("inputs_table")),
        br(),

        h4("Direct Capital Costs Details"),
        uiOutput(ns("wbs_section_nav")),
        tags$div(id = "wbs-table-container",
          DTOutput(ns("test_table"))
        ),
        br()
      )
    ))  # end with_anchor("anc-outputdb")
  )
}

outputDbServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    
    # Extract output_db from results
    output_db <- reactive({
      req(results$calculated, results$data)
      
      if (!is.null(results$data$output_db)) {
        results$data$output_db
      } else if (!is.null(results$data$capital_costs)) {
        # Build output_db from calculation results if not already formatted
        build_output_db_from_results(results$data)
      } else {
        NULL
      }
    })
    

    output$contaminant_title <- renderUI({

      params <- output_db()$inputs

      is_other <- isTRUE(tolower(trimws(as.character(params$contaminant %||% ""))) == "other")
      custom_name <- trimws(as.character(params$cont_name %||% ""))
      display_name <- if (is_other && nchar(custom_name) > 0)
        paste0("Other — ", custom_name)
      else
        as.character(params$contaminant %||% "")

      tags$div(
        class = "target-contaminant-header",
        h2(paste0("Target Contaminant: ", display_name))
      )
    })

    # Helper function to build output_db structure from calculation results
    build_output_db_from_results <- function(data) {
      list(
        inputs = data$params,
        contactors = data$contactors,
        tanks = data$tanks,
        piping = data$piping,
        pumps = data$pumps,
        gac = data$gac,
        controls = data$controls,
        site = data$site,
        capital_costs = data$capital_costs,
        om_costs = data$om_costs,
        # Gravity basin counts (computed in calculate_gac_system)
        op_num_basins    = data$op_num_basins,
        total_num_basins = data$total_num_basins
      )
    }
    
    # ── Value Boxes ──────────────────────────────────────────────────────────────────
    output$total_capital_summary <- renderValueBox({
      req(output_db())
      
      value <- if (!is.null(output_db()$capital_costs$total_direct)) {
        scales::dollar(output_db()$capital_costs$total_direct, accuracy = 1)
      } else {
        "N/A"
      }

      valueBox(
        value = value,
        subtitle = "Total Direct Capital Cost",
        icon = icon("dollar-sign"),
        color = "teal"
      )
    })

    output$total_indirect_summary <- renderValueBox({
      req(output_db())

      value <- if (!is.null(output_db()$capital_costs$total_indirect)) {
        scales::dollar(output_db()$capital_costs$total_indirect, accuracy = 1)
      } else {
        "N/A"
      }

      valueBox(
        value = value,
        subtitle = "Total Indirect Cost",
        icon = icon("dollar-sign"),
        color = "teal"
      )
    })


    output$total_add_on <- renderValueBox({
      req(output_db())

      value <- if (!is.null(output_db()$capital_costs$addon_cost)) {
        scales::dollar(output_db()$capital_costs$addon_cost, accuracy = 1)
      } else {
        "N/A"
      }

      valueBox(
        value = value,
        subtitle = "Add-on Cost",
        icon = icon("dollar-sign"),
        color = "teal"
      )
    })

    output$total_project_summary <- renderValueBox({
      req(output_db())

      value <- if (!is.null(output_db()$capital_costs$total_project)) {
        scales::dollar(output_db()$capital_costs$total_project, accuracy = 1)
      } else {
        "N/A"
      }

      valueBox(
        value = value,
        subtitle = "Grand Total Capital Cost",
        icon = icon("money-bill-wave"),
        color = "green"
      )
    })

    output$annualized_om_summary <- renderValueBox({
      req(output_db())

      value <- if (!is.null(output_db()$om_costs$total_annual)) {
        scales::dollar(output_db()$om_costs$total_annual, accuracy = 1)
      } else {
        "N/A"
      }
      
      valueBox(
        value = value,
        subtitle = "Annual O&M Cost",
        icon = icon("dollar-sign"),
        color = "teal"
      )
    })
    
    
    # build_wbs_table now returns list(dt, sections).
    # Cache with a reactive so Google Sheets is only hit once per calculation.
    wbs_built <- reactive({
      req(output_db())
      build_wbs_table(output_db())
    })

    output$test_table <- renderDT({
      wbs_built()$dt
    })

    # ── Sticky section nav bar ────────────────────────────────────────────────
    # Reads section_names from build_wbs_table and builds one button per
    # RowGroup header. Button onclick scrolls to the <tr id="wbs-sec-...">
    # that initComplete stamps on each header row after the table draws.
    # The gsub() here MUST match the JS regex in utils.R initComplete exactly:
    #   JS: label.replace(/[^a-zA-Z0-9]+/g, '-').toLowerCase()
    #   R:  gsub("[^a-zA-Z0-9]+", "-", tolower(sec))
    output$wbs_section_nav <- renderUI({
      sections <- wbs_built()$sections
      req(length(sections) > 0)

      # ID pattern must match initComplete JS in utils.R exactly:
      #   JS:  label.replace(/[^a-zA-Z0-9]+/g, '-').toLowerCase()
      #   R:   gsub("[^a-zA-Z0-9]+", "-", tolower(sec))
      # The id is placed on the <tr class="dtrg-group"> row itself.
      # scrollIntoView() on the <tr> scrolls the page to that row.
      # 
      # STICKY FIX: shinydashboard boxes use overflow:hidden/auto which breaks
      # position:sticky on children. Instead we use a fixed-position bar that
      # appears only when the nav has been scrolled past, driven by a scroll
      # listener injected via tags$script.
      buttons <- lapply(sections, function(sec) {
        js_id <- paste0("wbs-sec-", gsub("[^a-zA-Z0-9]+", "-", tolower(sec)))
        tags$button(
          class = "wbs-nav-btn",
          `data-target` = js_id,
          sec,
          style = paste0(
            "background:#1a5276;color:#fff;border:none;border-radius:4px;",
            "padding:4px 10px;font-size:11px;cursor:pointer;white-space:nowrap;"
          )
        )
      })

      tagList(
        # Inline nav bar (always visible at its natural position in the page)
        tags$div(
          id = "wbs-nav-inline",
          style = paste0(
            "background:#fff;border:1px solid #b2dfdb;border-radius:6px;",
            "padding:7px 12px;margin-bottom:14px;",
            "display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
            "box-shadow:0 2px 5px rgba(0,0,0,0.07);"
          ),
          tags$span(
            style = "font-size:11px;font-weight:700;color:#6c757d;margin-right:4px;white-space:nowrap;",
            "Jump to section:"
          ),
          tagList(buttons)
        ),

        # Fixed clone that appears when the inline bar scrolls out of view
        tags$div(
          id = "wbs-nav-fixed",
          style = paste0(
            "display:none;position:fixed;top:50px;left:300px;",
            "right:0;z-index:9999;",
            "background:#fff;border-bottom:2px solid #1a5276;",
            "padding:7px 16px;",
            "display:none;align-items:center;gap:6px;flex-wrap:wrap;",
            "box-shadow:0 3px 8px rgba(0,0,0,0.15);"
          ),
          tags$span(
            style = "font-size:11px;font-weight:700;color:#6c757d;margin-right:4px;white-space:nowrap;",
            "Jump to section:"
          ),
          tagList(buttons)
        ),

        # Script: button clicks + show/hide fixed bar on scroll
        tags$script(HTML("
          (function() {
            // Click handler: scroll target row into view, offset by fixed nav height
            // Click handler: offset scroll by fixed nav height.
            // Uses window.pageYOffset so it works regardless of scroll container.
            $(document).off('click.wbsNav').on('click.wbsNav', '.wbs-nav-btn', function() {
              var targetId = $(this).data('target');
              var el = document.getElementById(targetId);
              if (!el) return;
              var fixedNav = document.getElementById('wbs-nav-fixed');
              var navHeight = (fixedNav && fixedNav.style.display !== 'none') ? fixedNav.offsetHeight : 0;
              var absoluteTop = el.getBoundingClientRect().top + window.pageYOffset;
              window.scrollTo({ top: absoluteTop - navHeight - 8, behavior: 'smooth' });
            });

            // Show fixed bar only when: inline bar is above viewport AND
            // the test_table is still (at least partially) in the viewport.
            // Hides when scrolled back up above inline, or past the table bottom.
            function updateFixedNav() {
              var inline  = document.getElementById('wbs-nav-inline');
              var fixed   = document.getElementById('wbs-nav-fixed');
              var tableEl = document.getElementById('wbs-table-container');
              if (!inline || !fixed) return;

              var inlineRect = inline.getBoundingClientRect();
              var aboveInline = inlineRect.bottom < 0;

              var belowTable = false;
              if (tableEl) {
                var tableRect = tableEl.getBoundingClientRect();
                belowTable = tableRect.bottom < 0;
              }

              if (aboveInline && !belowTable) {
                fixed.style.display = 'flex';
              } else {
                fixed.style.display = 'none';
              }
            }

            $(document).off('scroll.wbsNav').on('scroll.wbsNav', '.content-wrapper', updateFixedNav);
            $(window).off('scroll.wbsNav').on('scroll.wbsNav', updateFixedNav);
          })();
        "))
      )
    })
    
    # Input Parameters Table
    output$inputs_table <- renderDT({
      req(output_db())

      #browser()
      params    <- output_db()$inputs
      contactors <- output_db()$contactors
      controls  <- output_db()$controls
      gac       <- output_db()$gac

      # ── Design-type flag: use tank_geometry as the reliable signal ──────────
      # params$tank_geometry is set to "basin" by calculate_gac_system for gravity.
      # params$design_type may be numeric (1/2) or character ("Pressure"/"Gravity"/"basin")
      is_gravity <- isTRUE(tolower(params$tank_geometry %||% "") == "basin") ||
                    isTRUE(as.character(params$design_type %||% "1") %in% c("2","gravity","basin","open channel"))

      # ── Workbook OUTPUT row 9: # of treatment trains ─────────────────────────
      # Pressure: num_treat_lines  Gravity: op_num_basins
      trains_val <- if (is_gravity) {
        as.character(output_db()$op_num_basins %||% "")
      } else {
        as.character(params$num_trains %||% "")
      }

      # ── Workbook OUTPUT row 10: # of contactors in series ────────────────────
      # Always Num_tanks (contactors in series per train / basins in series)
      in_series_val <- as.character(params$num_contactors_in_series %||% 1)

      # ── Workbook OUTPUT row 11: # of contactors ──────────────────────────────
      # Pressure: final_num_tanks  Gravity: total_num_basins (op + NRD_g)
      # Both are carried in contactors$total_contactors, which calculate_contactors
      # computes using the correct workbook logic for each design type.
      total_contactors_val <- as.character(contactors$total_contactors %||% "")

      #browser()
      # ── Workbook OUTPUT row 13: EBCT per contactor ───────────────────────────
      # Pressure: EBCT / Num_tanks   Gravity: EBCT (per basin, no subdivision)
      ebct_per_contactor <- if (!is_gravity) {
        num_series <- as.numeric(params$num_contactors_in_series %||% 1)
        if (num_series > 0) as.numeric(params$ebct %||% 0) / num_series
        else as.numeric(params$ebct %||% 0)
      } else {
        as.numeric(params$ebct %||% 0)
      }

      # ── Workbook OUTPUT row 14: Carbon life ──────────────────────────────────
      # bed_life already computed correctly in calculate_gac_requirements
      carbon_life <- gac$bed_life_months

      # ── Workbook OUTPUT row 15: Bed depth ────────────────────────────────────
      # Pressure: bed_depth   Gravity: basin_op_depth (= params$basin_depth)
      # Workbook row 15: pressure=bed_depth, gravity=basin_op_depth (=params$basin_depth)
      bed_depth_val <- if (is_gravity) {
        as.character(round(as.numeric(params$basin_depth %||% 0), 1))
      } else {
        as.character(round(as.numeric(params$bed_depth   %||% 0), 1))
      }

      # ── Workbook OUTPUT rows 16-18: geometry ─────────────────────────────────
      # row 16 label always "Vessel geometry"; value: pressure=tank_geom_I, gravity="basins"
      # row 17 label always "Height (straight)"; value: pressure=comm_height_length, gravity=basin_width
      # row 18 label always "Diameter"; value: pressure=comm_diam, gravity=basin_length
      vessel_geometry_val <- if (is_gravity) "basins" else as.character(params$tank_geometry %||% "")
      height_val <- if (is_gravity) {
        as.character(params$basin_width %||% "")
      } else {
        as.character(params$vessel_height_length %||% "")
      }
      diameter_val <- if (is_gravity) {
        as.character(params$basin_length %||% "")
      } else {
        as.character(params$vessel_diameter %||% "")
      }

      # Convert params list to data frame — labels match OUTPUT sheet rows 3-22 exactly
      df <- data.frame(
        Parameter = c(
          "Contaminant",
          "System Size Category",
          "Technology",
          "Design Type",
          "Design Flow Rate",
          "Average Flow Rate",
          "# of treatment trains",
          "# of contactors in series",
          "# of contactors",
          "Total EBCT",
          "EBCT per contactor",
          "Carbon life",
          "Bed depth",
          "Vessel geometry",
          if (is_gravity) "Basin width"  else "Height (straight)",
          if (is_gravity) "Basin length" else "Diameter",
          "Component level",
          "System automation",
          "Retrofit (operational modification)?",
          "New carbon life after retrofit"
        ),
        Value = c(
          as.character(params$contaminant %||% ""),
          # Workbook OUTPUT C4: =IF(ss_cat2=1,"small","large")
          # ss_cat2 = IF(design_flow<1,1,IF(design_flow<10,2,3)) — HVAC!C5
          # Only two display labels: "small" (flow<1 MGD) or "large" (flow>=1 MGD)
          if (as.numeric(params$design_flow %||% 0) < 1) "small" else "large",
          "GAC",
          if (is_gravity) "Gravity" else "Pressure",
          as.character(params$design_flow %||% ""),
          as.character(params$average_flow %||% ""),
          trains_val,
          in_series_val,
          total_contactors_val,
          as.character(params$ebct %||% ""),
          as.character(round(ebct_per_contactor, 2)),
          as.character(round(carbon_life, 1)),
          bed_depth_val,
          vessel_geometry_val,
          height_val,
          diameter_val,
          as.character(contactors$component_level_name %||% ""),
          as.character(params$automation_level %||% ""),
          if (isTRUE(params$retrofit)) "yes" else "no",
          if (isTRUE(params$retrofit)) "TBD" else "not applicable"
        ),
        Units = c(
          "",
          "",
          "",
          "",
          as.character(paste0(params$design_flow_units %||% "MGD", " (excludes bypass flow)")),
          as.character(paste0(params$average_flow_units %||% "MGD", " (excludes bypass flow)")),
          "trains",
          "(i.e., parallel or series operation)",
          "(including redundancy)",
          "minutes at design flow",
          "minutes at design flow",
          "months at average flow",
          "feet",
          "",
          "feet",
          "feet",
          "",
          "",
          "",
          ""
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            dom = 't',
            ordering = FALSE,
            pageLength = 30
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
      
    })
    
    # GAC Contactors Table
    output$contactors_table <- renderDT({
      req(output_db())
      
      contactors <- output_db()$contactors
      
      df <- data.frame(
        Item = c(
          "Number of Contactors",
          "Volume per Contactor (ft³)",
          "GAC Volume per Contactor (ft³)",
          "Total GAC Volume (ft³)",
          "Vessel Diameter (feet)",
          "Bed Depth (feet)",
          "Unit Cost",
          "Total Cost"
        ),
        Quantity = c(
          contactors$total_contactors,
          round(contactors$volume_per_contactor_cf, 2),
          round(contactors$gac_volume_per_contactor, 2),
          round(contactors$total_gac_volume, 2),
          contactors$diameter,
          contactors$bed_depth,
          "",
          ""
        ),
        Cost = c(
          "",
          "",
          "",
          "",
          "",
          "",
          scales::dollar(contactors$unit_cost),
          scales::dollar(contactors$total_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Tanks Table
    output$tanks_table <- renderDT({
      req(output_db())
      
      tanks <- output_db()$tanks
      
      df <- data.frame(
        Item = c(
          "Backwash Tank",
          "Residuals Tank",
          "Total Tanks Cost"
        ),
        Cost = c(
          scales::dollar(tanks$backwash_tank_cost),
          scales::dollar(tanks$residuals_tank_cost),
          scales::dollar(tanks$total_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Piping Table
    output$piping_table <- renderDT({
      req(output_db())
      
      piping <- output_db()$piping
      
      df <- data.frame(
        Item = c(
          "Process Piping",
          "Valves",
          "Total Piping & Valves Cost"
        ),
        Cost = c(
          scales::dollar(piping$piping_cost),
          scales::dollar(piping$valve_cost),
          scales::dollar(piping$total_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Valves Table (separate from piping if needed)
    output$valves_table <- renderDT({
      req(output_db())
      
      piping <- output_db()$piping
      
      df <- data.frame(
        Item = c("Valves (included in Piping section)"),
        Cost = c(scales::dollar(piping$valve_cost)),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Flow Meters Table
    output$flowmeters_table <- renderDT({
      req(output_db())
      
      # Placeholder - typically part of controls
      df <- data.frame(
        Item = c("Flow Meters (included in Controls)"),
        Cost = c("See Instrumentation & Controls"),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Pumps Table
    output$pumps_table <- renderDT({
      req(output_db())
      
      pumps <- output_db()$pumps
      
      df <- data.frame(
        Item = c(
          "Service Pumps",
          "Backwash Pumps",
          "Residuals Pumps",
          "Total Pumps Cost"
        ),
        Quantity = c(
          pumps$service_pumps,
          pumps$backwash_pumps,
          pumps$residuals_pumps,
          ""
        ),
        Cost = c(
          scales::dollar(pumps$service_cost),
          scales::dollar(pumps$backwash_cost),
          scales::dollar(pumps$residuals_cost),
          scales::dollar(pumps$total_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # GAC Media Table
    output$gac_media_table <- renderDT({
      req(output_db())
      
      gac <- output_db()$gac
      
      df <- data.frame(
        Item = c(
          "Total GAC Mass (lbs)",
          "Initial Fill Cost",
          "Bed Life (months)",
          "Annual Throughput (cf/yr)",
          "Annual Replacement Cost"
        ),
        Value = c(
          scales::comma(round(gac$total_gac_mass_lb, 1)),
          scales::dollar(gac$initial_fill_cost),
          scales::comma(round(gac$bed_life_months, 1)),
          scales::comma(round(gac$GAC_yr_cf, 2)),
          scales::dollar(gac$annual_replacement_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Instrumentation & Controls Table
    output$controls_table <- renderDT({
      req(output_db())
      
      controls <- output_db()$controls
      
      df <- data.frame(
        Item = c(
          "Base Control System",
          "Per Contactor Controls",
          "Manual Override",
          "Total Controls Cost"
        ),
        Cost = c(
          scales::dollar(controls$base_cost),
          scales::dollar(controls$contactor_cost),
          scales::dollar(controls$override_cost),
          scales::dollar(controls$total_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Buildings Table
    output$buildings_table <- renderDT({
      req(output_db())
      
      site <- output_db()$site
      
      df <- data.frame(
        Item = c(
          "Process Buildings",
          "Land Acquisition",
          "Total Buildings Cost"
        ),
        Cost = c(
          scales::dollar(site$building_cost),
          scales::dollar(site$land_cost),
          scales::dollar(site$building_cost + site$land_cost)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Site Work Table
    output$sitework_table <- renderDT({
      req(output_db())
      
      site <- output_db()$site
      
      df <- data.frame(
        Item = c("Site Work"),
        Cost = c(scales::dollar(site$site_work_cost)),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        )
    })
    
    # Total Direct Capital Cost Table
    output$total_direct_table <- renderDT({
      req(output_db())
      
      costs <- output_db()$capital_costs
      
      df <- data.frame(
        Category = c(
          "Equipment",
          "Materials",
          "Controls",
          "Site",
          "TOTAL DIRECT CAPITAL COST"
        ),
        Cost = c(
          scales::dollar(costs$equipment_cost),
          scales::dollar(costs$materials_cost),
          scales::dollar(costs$controls_cost),
          scales::dollar(costs$site_cost),
          scales::dollar(costs$total_direct)
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 10,
            dom = 't',
            ordering = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "70%", targets = 0),   # Category/Item col
              list(width = "30%", targets = 1, className = "dt-right")  # Cost col
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) |> 
        formatStyle(
          'Category',
          target = 'row',
          fontWeight = styleEqual('TOTAL DIRECT CAPITAL COST', 'bold')
        )
    })
    
    # Indirect Costs Table — aligned to Excel OUTPUT rows 331-346
    output$indirect_costs_table <- renderDT({
      req(output_db())

      costs <- output_db()$capital_costs

      # Build rows matching workbook order. Percent column mirrors the D column
      # in the workbook (percentage of direct cost where applicable).
      direct <- costs$total_direct

      pct_fmt <- function(x) if (x == 0) "—" else scales::percent(x, accuracy = 0.1)

      rows <- list(
        list("Mobilization & Demobilization",        costs$mobilization,        pct_fmt(costs$mob_pct)),
        list("Architectural Fees",                   costs$architectural_fees,  pct_fmt(if (!is.null(costs$arch_pct)) costs$arch_pct else 0)),
        list("Installation, Transportation & O&P",  costs$installation_transp, pct_fmt(0)),
        list("Site Work",                            costs$sitework_indirect,   "—"),
        list("Yard Piping",                          costs$yard_piping,         "—"),
        list("Geotechnical",                         costs$geotechnical,        "—"),
        list("Standby Power",                        costs$standby_power,       "—"),
        list("Electrical (incl. yard wiring)",       costs$electrical,          pct_fmt(costs$elect_pct)),
        list("Instrumentation & Control",            costs$instrumentation,     pct_fmt(0)),
        list("Contingency",                          costs$contingency,         "—"),
        list("Process Engineering",                  costs$process_engineering, pct_fmt(costs$eng_pct)),
        list("Miscellaneous Allowance",              costs$misc_allowance,      pct_fmt(0.10)),
        list("Legal, Fiscal & Administrative",       costs$legal_fiscal,        pct_fmt(0.02)),
        list("Sales Tax",                            costs$sales_tax,           pct_fmt(0)),
        list("Financing during Construction",        costs$financing,           "—"),
        list("Construction Mgmt & GC Overhead",      costs$construction_mgmt,   "—")
      )

      df <- data.frame(
        Item    = sapply(rows, `[[`, 1),
        Cost    = sapply(rows, function(r) scales::dollar(as.numeric(r[[2]]))),
        Percent = sapply(rows, `[[`, 3),
        stringsAsFactors = FALSE
      )

      # Total row — workbook does not show a percent for the total line
      total_row <- data.frame(
        Item    = "TOTAL INDIRECT COSTS",
        Cost    = scales::dollar(costs$total_indirect),
        Percent = "—",
        stringsAsFactors = FALSE
      )
      df <- rbind(df, total_row)

      df |>
        datatable(
          options = list(
            pageLength = 20,
            dom = 't',
            ordering = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "55%", targets = 0),
              list(width = "25%", targets = 1, className = "dt-right"),
              list(width = "20%", targets = 2, className = "dt-right")
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) |>
        formatStyle(
          'Item',
          target = 'row',
          fontWeight = styleEqual('TOTAL INDIRECT COSTS', 'bold'),
          backgroundColor = styleEqual('TOTAL INDIRECT COSTS', '#e8f6f4')
        )
    })
    
    # Add-on Costs Table (workbook OUTPUT rows 324-326)
    output$addon_costs_table <- renderDT({
      req(output_db())

      costs <- output_db()$capital_costs

      # Guidance column mirrors workbook OUTPUT column D (row 326 for land)
      land_acres <- as.numeric(costs$land_req_acres %||% 0)
      land_guidance <- if (!is.na(land_acres) && land_acres > 0) {
        sprintf("For %.2f acres", land_acres)
      } else ""

      addon_rows <- list(
        list("Permits",     costs$permit_cost, ""),
        list("Pilot Study", costs$pilot_cost,  ""),
        list("Land Cost",   costs$land_cost,   land_guidance)
      )

      df <- data.frame(
        Item     = sapply(addon_rows, `[[`, 1),
        Cost     = sapply(addon_rows, function(r) {
          v <- as.numeric(r[[2]])
          if (is.na(v) || v == 0) "$0" else scales::dollar(v)
        }),
        Guidance = sapply(addon_rows, `[[`, 3),
        stringsAsFactors = FALSE
      )

      total_addon <- data.frame(
        Item     = "TOTAL ADD-ON COSTS",
        Cost     = scales::dollar(as.numeric(costs$addon_cost %||% 0)),
        Guidance = "",
        stringsAsFactors = FALSE
      )
      df <- rbind(df, total_addon)

      df |>
        datatable(
          options = list(
            pageLength = 10, 
            dom = 't', 
            ordering = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "55%", targets = 0),
              list(width = "25%", targets = 1, className = "dt-right"),
              list(width = "20%", targets = 2, className = "dt-right")
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) |>
        formatStyle(
          'Item',
          target = 'row',
          fontWeight = styleEqual('TOTAL ADD-ON COSTS', 'bold'),
          backgroundColor = styleEqual('TOTAL ADD-ON COSTS', '#e8f6f4')
        )
    })

    # Total Project Cost Table
    # Mirrors workbook OUTPUT structure: Direct + Indirect + Add-on = Total Project
    output$total_project_table <- renderDT({
      req(output_db())
      
      costs <- output_db()$capital_costs
      addon <- as.numeric(costs$addon_cost %||% 0)
      
      df <- data.frame(
        Item = c(
          "Total Direct Capital Cost",
          "Total Indirect Costs",
          "Total Add-on Costs",
          "TOTAL PROJECT COST"
        ),
        Cost = c(
          scales::dollar(as.numeric(costs$total_direct  %||% 0)),
          scales::dollar(as.numeric(costs$total_indirect %||% 0)),
          scales::dollar(addon),
          scales::dollar(as.numeric(costs$total_project  %||% 0))
        ),
        stringsAsFactors = FALSE
      )
      
      df |> 
        datatable(
          options = list(
            pageLength = 6,
            dom = 't',
            ordering = FALSE,
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "70%", targets = 0),   # Category/Item col
              list(width = "30%", targets = 1, className = "dt-right")  # Cost col
            )
          ),
          rownames = FALSE,
          class = 'cell-border stripe'
        ) |> 
        formatStyle(
          'Item',
          target = 'row',
          fontWeight = styleEqual('TOTAL PROJECT COST', 'bold'),
          fontSize  = styleEqual('TOTAL PROJECT COST', '16px'),
          backgroundColor = styleEqual('TOTAL PROJECT COST', '#e8f6f4')
        )
    })
    
    # Annual O&M Costs Table
    output$annual_om_table <- renderDT({
      req(output_db())

      om <- output_db()$om_costs

      # Helper: format dollar, show "--" for zero/NA
      fmt_d <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        if (is.null(x) || length(x) == 0 || is.na(x) || x == 0) "--"
        else scales::dollar(x, accuracy = 0.01)
      }
      # Helper: format qty with units
      fmt_qty <- function(qty, unit) {
        q <- suppressWarnings(as.numeric(qty))
        if (is.null(q) || length(q) == 0 || is.na(q) || q == 0) ""
        else paste0(formatC(q, format = "f", digits = 2), " ", unit)
      }
      # Helper: format unit cost
      fmt_uc <- function(uc, unit) {
        u <- suppressWarnings(as.numeric(uc))
        if (is.null(u) || length(u) == 0 || is.na(u) || u == 0) ""
        else paste0(scales::dollar(u, accuracy = 0.0001), unit)
      }

      # ── Build rows mirroring workbook OUTPUT rows 364-403 ──────────────────
      # Columns: Section/Item, Quantity, Unit Cost, Total ($/yr)
      rows <- list(
        # ── LABOR (rows 364-367) ───────────────────────────────────────────
        list("**Labor**",        "",    "",    ""),
        list("Manager",
             fmt_qty(om$Manager_LOE,  "hrs/yr"),
             fmt_uc(om$mgr_uc,        "/hr"),
             fmt_d(om$labor_manager)),
        list("Clerical",
             fmt_qty(om$Clerical_LOE, "hrs/yr"),
             fmt_uc(om$cler_uc,       "/hr"),
             fmt_d(om$labor_clerical)),
        list("Operator",
             fmt_qty(om$Operator_LOE, "hrs/yr"),
             fmt_uc(om$oper_uc,       "/hr"),
             fmt_d(om$labor_operator)),

        # ── MATERIALS (rows 368-373) ────────────────────────────────────────
        list("**Materials**",    "",    "",    ""),
        list("Materials for booster pumps",
             "% of capital", "", fmt_d(om$pump_mtl)),
        list("Materials for backwash pumps",
             "% of capital", "", fmt_d(om$back_pump_mtl)),
        list("Materials for residuals pumps",
             "% of capital", "", fmt_d(om$res_pump_mtl)),
        list("Materials for GAC contactors",
             "% of capital", "", fmt_d(om$filter_materials)),
        list("Materials for on-site regeneration",
             "% of capital", "", fmt_d(om$regen_materials)),
        list("Building and HVAC maintenance (materials and labor)",
             fmt_qty(om$total_fp %||% 0, "sf"),
             fmt_uc(6.479028535918001, "/sf/yr"),
             fmt_d(om$bldg_maint_cost)),

        # ── MEDIA & CHEMICALS (rows 375-379) ────────────────────────────────
        list("**Media and Chemicals**", "", "", ""),
        list("Makeup GAC",
             fmt_qty(om$GAC_makeup_lbs, "lbs/yr"),
             fmt_uc(om$replace_gac_uc,  "/lb"),
             fmt_d(om$makeup_gac_cost)),
        list("Off-site GAC regeneration",
             fmt_qty(om$regen_yr_lbs,   "lbs/yr"),
             fmt_uc(om$off_regen_uc,    "/lb"),
             fmt_d(om$off_regen_cost)),

        # ── ENERGY (rows 380-395) ────────────────────────────────────────────
        list("**Energy**",       "",    "",    ""),
        list("Energy for booster pumps",
             fmt_qty(om$pump_energy_mwh,       "MWh/yr"),
             fmt_uc(om$energy_cost_cl, "/kWh"), fmt_d(om$pump_energy_cost)),
        list("Energy for backwash pumps",
             fmt_qty(om$back_pump_energy_mwh,  "MWh/yr"),
             fmt_uc(om$energy_cost_cl, "/kWh"), fmt_d(om$back_pump_energy_cost)),
        list("Energy for residuals pumps",
             fmt_qty(om$res_pump_energy_mwh,   "MWh/yr"),
             fmt_uc(om$energy_cost_cl, "/kWh"), fmt_d(om$res_pump_energy_cost)),
        list("Energy for lighting",
             fmt_qty(om$lighting_mwh,          "MWh/yr"),
             fmt_uc(om$energy_cost_cl, "/kWh"), fmt_d(om$lighting_cost)),
        list("Energy for ventilation",
             fmt_qty(om$ventilation_mwh,       "MWh/yr"),
             fmt_uc(om$energy_cost_cl, "/kWh"), fmt_d(om$ventilation_cost)),

        # ── RESIDUALS (rows 398-402) ─────────────────────────────────────────
        list("**Residuals Discharge/Disposal**", "", "", ""),
        list("POTW discharge fees",
             fmt_qty(om$res_flow_annual_gal, "gal/yr"),
             fmt_uc(om$potw_rate_per_gal,   "/gal"),
             fmt_d(om$potw_fee)),

        # ── MISC (row 403) ───────────────────────────────────────────────────
        list("Miscellaneous Allowance (10%)",    "",    "",
             fmt_d(om$misc_allowance)),

        # ── TOTAL (row 404) ──────────────────────────────────────────────────
        list("**TOTAL ANNUAL O&M COST**",        "",    "",
             scales::dollar(om$total_annual, accuracy = 0.01))
      )

      df <- do.call(rbind, lapply(rows, function(r) {
        data.frame(
          Item      = as.character(r[[1]]),
          Quantity  = as.character(r[[2]]),
          `Unit Cost` = as.character(r[[3]]),
          `Total ($/yr)` = as.character(r[[4]]),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }))

      # Section header rows (bold, light-blue background)
      header_rows <- which(grepl("^\\*\\*", df$Item))
      # Strip ** markers for display
      df$Item <- gsub("^\\*\\*|\\*\\*$", "", df$Item)
      # Rows with all dashes → show nothing for zero lines
      all_dash <- which(df[["Total ($/yr)"]] == "--" &
                        df$Quantity == "" & df[["Unit Cost"]] == "")

      datatable(
        df,
        options = list(
          pageLength = 30,
          dom = "t",
          ordering = FALSE,
          autoWidth = FALSE,
          columnDefs = list(
            list(width = "45%", targets = 0),
            list(width = "20%", targets = 1, className = "dt-right"),
            list(width = "20%", targets = 2, className = "dt-right"),
            list(width = "15%", targets = 3, className = "dt-right")
          )
        ),
        rownames = FALSE,
        class = "cell-border stripe compact"
      ) |>
        formatStyle(
          "Item",
          target = "row",
          fontWeight = styleRow(header_rows, "bold"),
          backgroundColor = styleRow(header_rows, "#e8f6f4")
        ) |>
        formatStyle(
          "Total ($/yr)",
          target = "row",
          fontWeight = styleRow(nrow(df), "bold"),
          fontSize  = styleRow(nrow(df), "105%")
        )
    })

    # Cost Breakdown Chart
    output$cost_breakdown_chart <- renderPlot({
      req(output_db())
      
      costs <- output_db()$capital_costs
      
      if (!is.null(costs$breakdown)) {
        df <- costs$breakdown
        
        ggplot2::ggplot(df, ggplot2::aes(x = reorder(Category, Cost), y = Cost)) +
          ggplot2::geom_col(fill = "#0e8a7d") +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::labs(
            title = "Direct Capital Cost by Category",
            x = "",
            y = "Cost ($)"
          ) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
          )
      }
    })
    
    # Equipment Chart
    output$equipment_chart <- renderPlot({
      req(output_db())
      
      contactors <- output_db()$contactors$total_contactors
      pumps <- output_db()$pumps$service_pumps + 
               output_db()$pumps$backwash_pumps + 
               output_db()$pumps$residuals_pumps
      
      df <- data.frame(
        Equipment = c("Contactors", "Pumps", "Tanks"),
        Quantity = c(contactors, pumps, 2)
      )
      
      ggplot2::ggplot(df, ggplot2::aes(x = Equipment, y = Quantity, fill = Equipment)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("#0e8a7d", "#d4a017", "#1a5276")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          title = "Major Equipment Quantities",
          x = "",
          y = "Quantity"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        )
    })
    
    # Download Handlers
    output$download_full_excel <- downloadHandler(
      filename = function() {
        paste0("GAC_Output_Database_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(output_db())
        showNotification("Excel export feature coming soon", type = "info")
      }
    )
    
    output$download_full_csv <- downloadHandler(
      filename = function() {
        paste0("GAC_Output_Database_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(output_db())
        showNotification("CSV export feature coming soon", type = "info")
      }
    )

    # ── Collapsible section toggles (shinyjs) ────────────────────────────────
    toggle_section <- function(btn_id, body_id, icon_id) {
      observeEvent(input[[btn_id]], {
        shinyjs::toggle(id = body_id, anim = TRUE, animType = "slide", time = 0.2)
        # Flip the arrow icon
        current <- isolate(input[[btn_id]])  # click count — odd = open, even = closed
        is_open <- (current %% 2) == 1
        shinyjs::html(id = icon_id, html = if (is_open) "\u25BC" else "\u25B6")
      }, ignoreNULL = TRUE)
    }

    toggle_section("toggle_total_direct_table",   "body_total_direct_table",   "icon_total_direct_table")
    toggle_section("toggle_indirect_costs_table", "body_indirect_costs_table", "icon_indirect_costs_table")
    toggle_section("toggle_addon_costs_table",    "body_addon_costs_table",    "icon_addon_costs_table")
    toggle_section("toggle_total_project_table",  "body_total_project_table",  "icon_total_project_table")
    toggle_section("toggle_annual_om_table",      "body_annual_om_table",      "icon_annual_om_table")

  })
}