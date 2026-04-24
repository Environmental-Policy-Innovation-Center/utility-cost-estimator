# Landing Page Module
# Welcome / overview page for the Water System Consolidation Cost Estimator

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
          "Built on EPA Sourced Data and [insert methods + citation]"
        ),
        h1("Identify water system consolidation candidates \u2014 before you plan."),
        p(
          "A web-based tool for identifying joining & receiving systems. Quickly estimate",
          "costs for physical consolidation. Designed for state agencies,",
          "technical assistance providers, and utilities screening for physical consolidation options."
        ),
        div(
          class = "lp-hero-actions",
          actionButton(
            ns("launch_tool"),
            label = tagList(icon("play"), "Estimate Consolidation Costs"),
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
            h3("EPA Data - powered by EPIC "),
            p("Utilizing US EPA's Safe Drinking Water Information System and Water System Service Area Boundaries data. Powered by EPIC's National Drinking Water Explorer")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-teal",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><rect x="3" y="3" width="14" height="14" rx="2" stroke="currentColor" stroke-width="1.5"/><path d="M7 10l2 2 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>')
          ),
          div(
            h3("Transparent Cost Model"),
            p("Derived from UCLA & Stanford Research, every estimate traces to documented engineering assumptions adjustable based on user inputs: per-mile pipeline costs, connection fees, service lines, and indirect cost multipliers.")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-amber",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><circle cx="10" cy="10" r="7" stroke="currentColor" stroke-width="1.5"/><path d="M10 6v4l3 2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>')
          ),
          div(
            h3("Pre-Computed GIS-Powered Routing"),
            p("Leverages euclidean and transit distance metrics for pipeline length estimates - across +44,000 water systems \u2014 pre-computed for rapid analysis")
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
        div(class = "lp-section-title", "Three steps. One cost estimate."),
        div(class = "lp-section-desc",
          "Select your state, define system filters, and set cost parameters \u2014 the tool identifies candidate pairs and estimates project costs."
        ),
        div(
          class = "lp-how-grid",

          div(class = "lp-step-card",
            div(class = "lp-step-num", "1"),
            h3("Select State"),
            p("Choose a state to load pre-processed water system boundaries and candidate consolidation pairs from our dataset.")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", "2"),
            h3("Define Systems"),
            p("Set criteria for consolidating systems (health violations, population, owner type) and receiving systems (capacity, distance cutoff).")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", "3"),
            h3("Set Cost Parameters"),
            p("Adjust per-mile pipeline cost, connection fees, service line fees, and indirect cost multipliers for your region.")
          ),
          div(class = "lp-step-card",
            div(class = "lp-step-num", HTML("&#10003;")),
            h3("Get Your Estimate"),
            p("View candidate pairs on a map with estimated capital costs, stacked cost charts, and a full project-cost breakdown.")
          )
        )
      )
    ),
    # ── USES ──
    div(
      id = ns("models_section"),
      class = "lp-section lp-tech-section",
      div(
        class = "lp-section-inner",
        div(class = "lp-section-label", "Who Uses It"),
        div(class = "lp-section-title", "Built for key water sector audiances"),
        div(class = "lp-section-desc",
          "From federal rulemaking to community advocacy \u2014 a faster, more targeted alternative to the Drinking Water Infrastructure Needs Assessment or a multimillion-dollar study."
        ),
        div(
          class = "lp-tech-grid",

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Federal & National Policy")
            ),
            div(class = "lp-tech-desc",
              "Federal agency staff and national policy organizations use the tool to get a national or state-level snapshot of CWS compliance and the cost of consolidation \u2014 without commissioning a major study."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "National Snapshot"),
              tags$span(class = "lp-contaminant-tag", "Rulemaking Support")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "State Regulators & SRF Programs")
            ),
            div(class = "lp-tech-desc",
              "State agency staff, funding program managers, and legislative staff use the tool to understand consolidation costs in their jurisdiction and model funding needs for a state-wide program to support physical consolidation"
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Primacy Agencies"),
              tags$span(class = "lp-contaminant-tag", "Funding Programs"),
              tags$span(class = "lp-contaminant-tag", "Legislative Staff")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Technical Assistance Providers")
            ),
            div(class = "lp-tech-desc",
              "TA providers pull comparative consolidation cost estimates regionally, or get a quick ballpark for a specific system they\u2019re already working with."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Regional Comparisons"),
              tags$span(class = "lp-contaminant-tag", "System-Level Estimates")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Engineers & Planners")
            ),
            div(class = "lp-tech-desc",
              "Engineers scoping a grant portfolio of 10\u201350 systems, or planners needing estimates for a large cohort of affected systems to bring to a funding board."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Multi-System Grants"),
              tags$span(class = "lp-contaminant-tag", "State Planning")
            )
          ),

          # div(class = "lp-tech-card",
          #   div(class = "lp-tech-header",
          #     div(class = "lp-tech-name", "Consulting Firms")
          #   ),
          #   div(class = "lp-tech-desc",
          #     "Engineering and management consultants assessing the business opportunity in a region identify candidate systems and get upfront cost estimates before proposing a detailed engagement."
          #   ),
          #   div(class = "lp-tech-contaminants",
          #     tags$span(class = "lp-contaminant-tag", "Market Sizing"),
          #     tags$span(class = "lp-contaminant-tag", "Business Development")
          #   )
          # ),

          # div(class = "lp-tech-card",
          #   div(class = "lp-tech-header",
          #     div(class = "lp-tech-name", "Larger Utilities")
          #   ),
          #   div(class = "lp-tech-desc",
          #     "Larger systems interested in regionalization, or facing pressure to assist neighboring underperforming systems, use the tool to get a rough upfront cost estimate of absorbing or interconnecting with adjacent utilities."
          #   ),
          #   div(class = "lp-tech-contaminants",
          #     tags$span(class = "lp-contaminant-tag", "Regionalization"),
          #     tags$span(class = "lp-contaminant-tag", "TMF Assistance")
          #   )
          # ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Environmental Justice & Advocacy")
            ),
            div(class = "lp-tech-desc",
              "Community-based organizations use the tool to understand consolidation options for struggling systems in a region and build a funding ask to bring to state or federal representatives."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Advocacy"),
              tags$span(class = "lp-contaminant-tag", "Funding Narratives")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Academics, Local Officials & Media")
            ),
            div(class = "lp-tech-desc",
              "Researchers studying water system vulnerability, local officials facing compliance pressure, and journalists covering drinking water use the tool to quickly scope the problem and its cost at any scale."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Research"),
              tags$span(class = "lp-contaminant-tag", "Local Officials"),
              tags$span(class = "lp-contaminant-tag", "Journalism")
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
          "Costs are built up from individual project components \u2014 pipeline, connections, permitting, and indirect multipliers \u2014 applied to each candidate pair."
        ),
        div(
          class = "lp-method-content",

          div(
            class = "lp-method-text",
            h3("Candidate Pair Identification"),
            p(
              "Candidate pairs are pre-computed using EPA SDWIS system boundaries and OSRM road-network routing. Pairs are filtered by health violations, ownership, population, and geographic proximity."
            ),
            h3("Direct Capital Costs"),
            p(
              "Direct costs include pipeline installation (per-mile), service connections (per connection), new source infrastructure (if the receiving system lacks existing capacity), and CEQA/permitting fees."
            ),
            h3("Indirect Cost Multipliers"),
            p(
              "Indirect costs are applied as percentage add-ons to direct capital: contingency, planning & construction management, engineering services, inflation, and a regional cost-of-construction multiplier."
            )
          ),

          div(
            class = "lp-method-diagram",
            h4("Cost Estimation Flow"),

            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("System Filters"),
                p("State, owner type, health violations, population, distance cutoff")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Candidate Pairs"),
                p("SDWIS boundaries + OSRM routing identify feasible system pairs")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Direct Capital Costs"),
                p("Pipeline, connections, service lines, new source, permitting")
              )
            ),
            div(class = "lp-flow-step",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Indirect Multipliers"),
                p("Contingency, engineering, inflation, regional adjustment")
              )
            ),
            div(class = "lp-flow-step lp-flow-step-last",
              div(class = "lp-flow-dot", div(class = "lp-flow-dot-inner")),
              div(class = "lp-flow-info",
                h5("Total Project Cost"),
                p("Direct capital + indirect markups = total estimated project cost")
              )
            )
          )
        )
      )
    ),

    # ── CTA ──
    div(
      class = "lp-cta-section",
      h2("Ready to identify consolidation candidates for your state?"),
      p("Select a state and define your system criteria to get started."),
      actionButton(
        ns("launch_tool_bottom"),
        label = tagList(
          "Launch Consolidation Estimator",
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
          href   = "https://www.epa.gov/ground-water-and-drinking-water/safe-drinking-water-information-system-sdwis-federal-reporting",
          target = "_blank", rel = "noopener noreferrer",
          "EPA SDWIS"
        ),
        tags$a(
          href   = "https://www.epa.gov/ground-water-and-drinking-water/public-water-system-service-areas",
          target = "_blank", rel = "noopener noreferrer",
          "EPA SABs"
        ),
        tags$a(
          href   = "https://www.policyinnovation.org/drinking-water-explorer-tool",
          target = "_blank", rel = "noopener noreferrer",
          "National Drinking Water Explorer"
        )
      ),
      p(
        "Built on EPA Safe Drinking Water Information System (SDWIS) & Service Area Boundaries (SABs) data \u00b7 Cost model developed by UCLA, Stanford, and EPIC.",
        tags$br(),
        "Powered by EPIC\u2019s National Drinking Water Explorer Dataset + Tool",
        tags$br(),
        "This tool is not an official EPA product. UCLA, Stanford and EPIC make no claims of accuracy. Please consult engineering services for more detailed costs."
      )
    )
  )
}


landingServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$launch_tool, {
      updateTabItems(parent_session, "sidebar", "tool")
    })

    observeEvent(input$launch_tool_bottom, {
      updateTabItems(parent_session, "sidebar", "tool")
    })

  })
}
