# Landing Page Module
# Welcome / overview page for the Water Treatment Cost Estimator

landingUI <- function(id) {
  ns <- NS(id)

  div(
    class = "landing-page",

    # ── HERO ──
    div(
      class = "lp-hero",
      div(
        class = "lp-hero-inner",
        div(
          class = "lp-hero-badge",
          tags$span("\u2666"),
          " Built on EPA Work Breakdown Structure Models"
        ),
        h1("Understand your water treatment costs \u2014 before you build."),
        p(
          "A modern, web-based interface for EPA\u2019s drinking water treatment cost models.",
          "Designed for community water systems, regulators, and technical assistance providers",
          "to quickly estimate capital and O&M costs for treatment technologies."
        ),
        div(
          class = "lp-hero-actions",
          actionButton(
            ns("launch_estimator"),
            label = tagList(icon("play"), "Estimate Costs Now"),
            class = "lp-btn lp-btn-primary"
          ),
          tags$a(
            href = "#",
            onclick = paste0(
              "document.getElementById('", ns("methodology_anchor"), "').scrollIntoView({behavior:'smooth'});return false;"
            ),
            class = "lp-btn lp-btn-outline",
            "Learn How It Works"
          )
        )
      )
    ),

    # ── PROVENANCE STRIP ──
    div(
      class = "lp-provenance",
      div(
        class = "lp-provenance-inner",

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-blue",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><path d="M10 2L3 6v8l7 4 7-4V6l-7-4z" stroke="currentColor" stroke-width="1.5" stroke-linejoin="round"/><path d="M10 10v8M10 10l7-4M10 10L3 6" stroke="currentColor" stroke-width="1.5"/></svg>')
          ),
          div(
            h3("EPA WBS Cost Models"),
            p("All cost calculations derive from EPA\u2019s peer-reviewed Work Breakdown Structure engineering models, the same models used in federal rulemaking.")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-teal",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><rect x="3" y="3" width="14" height="14" rx="2" stroke="currentColor" stroke-width="1.5"/><path d="M7 10l2 2 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>')
          ),
          div(
            h3("Validated & Transparent"),
            p("Every output traces to documented engineering equations, unit costs, and design assumptions. No black boxes.")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-amber",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><circle cx="10" cy="10" r="7" stroke="currentColor" stroke-width="1.5"/><path d="M10 6v4l3 2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>')
          ),
          div(
            h3("No Excel Required"),
            p("The original EPA models are complex Excel macros. This tool delivers the same analysis in seconds through a simple web interface.")
          )
        )
      )
    ),

    # ── HOW IT WORKS ──
    div(
      id = ns("how_section"),
      class = "lp-section",
      div(
        class = "lp-section-inner",
        div(class = "lp-section-label", "Getting Started"),
        div(class = "lp-section-title", "Three inputs. One cost estimate."),
        div(class = "lp-section-desc",
          "Select your contaminant, design type, and flow rate \u2014 the tool handles the rest using EPA\u2019s engineering equations."
        ),
        div(
          class = "lp-how-grid",

          div(class = "lp-step-card",
            div(class = "lp-step-num", "1"),
            h3("Select Contaminant"),
            p("Choose the target contaminant (e.g., TCE, PFAS, arsenic). The model selects appropriate design parameters like carbon life and EBCT.")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", "2"),
            h3("Choose Design Type"),
            p("Pressure or gravity configuration. The tool sizes contactors, vessels, and piping based on your selection.")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", "3"),
            h3("Enter Flow Rate"),
            p("Provide your system\u2019s design flow in MGD. The model determines system size category and scales all cost components accordingly.")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", HTML("&#10003;")),
            h3("Get Your Estimate"),
            p("Receive direct capital, indirect costs, total capital, and annual O&M \u2014 with a full breakdown of input parameters and assumptions.")
          )
        )
      )
    ),

    # ── TECHNOLOGIES ──
    div(
      id = ns("tech_section"),
      class = "lp-section lp-tech-section",
      div(
        class = "lp-section-inner",
        div(class = "lp-section-label", "Treatment Technologies"),
        div(class = "lp-section-title", "Available cost models"),
        div(class = "lp-section-desc",
          "Each model corresponds to an EPA WBS engineering workbook. We\u2019re adding new technologies as they\u2019re modernized."
        ),
        div(
          class = "lp-tech-grid",

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Granular Activated Carbon (GAC)"),
              tags$span(class = "lp-tech-status lp-status-live", "Live")
            ),
            div(class = "lp-tech-desc",
              "Porous adsorption media for removing organic contaminants. Supports pressure and gravity configurations for systems of all sizes."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "TCE"),
              tags$span(class = "lp-contaminant-tag", "PFAS"),
              tags$span(class = "lp-contaminant-tag", "Atrazine"),
              tags$span(class = "lp-contaminant-tag", "VOCs"),
              tags$span(class = "lp-contaminant-tag", "SOCs")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Anion Exchange"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Ion exchange resins for removing anionic contaminants from source water, including arsenic, nitrate, and perchlorate."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Arsenic"),
              tags$span(class = "lp-contaminant-tag", "Nitrate"),
              tags$span(class = "lp-contaminant-tag", "Perchlorate"),
              tags$span(class = "lp-contaminant-tag", "Uranium")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Reverse Osmosis / Nanofiltration"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Membrane-based treatment for a broad range of inorganic and organic contaminants. Applicable to both brackish and fresh source water."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "TDS"),
              tags$span(class = "lp-contaminant-tag", "Radium"),
              tags$span(class = "lp-contaminant-tag", "PFAS"),
              tags$span(class = "lp-contaminant-tag", "Hardness")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Aeration / Air Stripping"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Packed tower or diffused aeration for volatile contaminant removal. Effective for radon and volatile organic compounds."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Radon"),
              tags$span(class = "lp-contaminant-tag", "TCE"),
              tags$span(class = "lp-contaminant-tag", "PCE"),
              tags$span(class = "lp-contaminant-tag", "VOCs")
            )
          )
        )
      )
    ),

    # ── METHODOLOGY ──
    div(
      id = ns("methodology_anchor"),
      class = "lp-section",
      div(
        class = "lp-section-inner",
        div(class = "lp-section-label", "Methodology"),
        div(class = "lp-section-title", "How costs are calculated"),
        div(class = "lp-section-desc",
          "Every estimate traces back to EPA\u2019s Work Breakdown Structure approach \u2014 the same framework used to develop national cost estimates for federal drinking water regulations."
        ),
        div(
          class = "lp-method-content",

          div(
            class = "lp-method-text",
            h3("Work Breakdown Structure"),
            p(
              "EPA\u2019s WBS models use a bottom-up engineering approach. Each treatment technology is decomposed into individual components \u2014 vessels, piping, media, instrumentation, electrical \u2014 with quantities derived from engineering design equations rather than generic cost curves."
            ),
            h3("What\u2019s Included"),
            p(
              "Capital costs cover equipment, installation, and site-specific add-ons (permits, pilot studies, land).",
              "Indirect costs include contractor overhead, site work, and contingencies.",
              "Annual O&M covers labor, energy, chemical replacement, residuals disposal, and media regeneration or replacement."
            ),
            h3("Design Assumptions"),
            p(
              "Default parameters reflect standard engineering practice \u2014 redundancy, bed depth, empty bed contact time, carbon life.",
              "The original EPA Excel workbooks allow users to override these defaults; this web tool uses the standard assumptions for rapid screening-level estimates."
            )
          ),

          div(
            class = "lp-method-diagram",
            h4("Cost Estimation Flow"),

            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("User Inputs"),
                p("Contaminant type, design type, design flow rate")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Engineering Design"),
                p("Sizing equations determine equipment specs: vessels, contactors, piping, media volume")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Component Costing"),
                p("Unit costs applied to each WBS component using EPA cost references")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Add-on & Indirect Costs"),
                p("Permits, pilot studies, site work, contractor overhead, contingencies")
              )
            ),
            div(class = "lp-flow-step lp-flow-step-last",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Total Cost Output"),
                p("Direct capital + indirect = total capital, plus annual O&M estimate")
              )
            )
          )
        )
      )
    ),

    # ── CTA ──
    div(
      class = "lp-cta-section",
      h2("Ready to estimate treatment costs for your system?"),
      p("Select a technology above, or jump straight into the GAC estimator."),
      actionButton(
        ns("launch_estimator_bottom"),
        label = tagList(
          "Launch GAC Cost Estimator",
          HTML('<svg width="16" height="16" viewBox="0 0 16 16" fill="none"><path d="M3 8h10M9 4l4 4-4 4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>')
        ),
        class = "lp-btn lp-btn-primary lp-btn-lg"
      )
    ),

    # ── LANDING FOOTER ──
    div(
      class = "lp-footer",
      div(
        class = "lp-footer-links",
        tags$a(
          href   = "https://www.epa.gov/sdwa/drinking-water-treatment-technology-unit-cost-models",
          target = "_blank",
          rel    = "noopener noreferrer",
          "EPA WBS Cost Models"
        ),
        tags$a(
          href   = "https://www.epa.gov/ground-water-and-drinking-water/drinking-water-technologies",
          target = "_blank",
          rel    = "noopener noreferrer",
          "EPA Drinking Water Technologies"
        )
      ),
      p(
        "Source: Work Breakdown Structure-Based Cost Models \u00b7 US EPA Office of Water",
        tags$br(),
        "This tool is developed independently and is not an official EPA product."
      )
    )
  )
}


landingServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Both CTA buttons navigate to the System Design tab
    observeEvent(input$launch_estimator, {
      updateTabItems(parent_session, "sidebar", "inputs")
    })

    observeEvent(input$launch_estimator_bottom, {
      updateTabItems(parent_session, "sidebar", "inputs")
    })

  })
}
