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
          " Built on SDWIS & EPIC Engineering Cost Models"
        ),
        h1("Map consolidation candidates \u2014 before you plan."),
        p(
          "A web-based tool for identifying candidate water system pairs and estimating",
          "capital costs for physical consolidation. Designed for state agencies,",
          "technical assistance providers, and utilities evaluating consolidation options."
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
            h3("SDWIS System Data"),
            p("Candidate pairs are built from EPA\u2019s Safe Drinking Water Information System \u2014 the same database used in federal compliance and enforcement.")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-teal",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><rect x="3" y="3" width="14" height="14" rx="2" stroke="currentColor" stroke-width="1.5"/><path d="M7 10l2 2 4-4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/></svg>')
          ),
          div(
            h3("Transparent Cost Model"),
            p("Every estimate traces to documented engineering assumptions: per-mile pipeline costs, connection fees, service lines, and indirect cost multipliers.")
          )
        ),

        div(
          class = "lp-prov-card",
          div(class = "lp-prov-icon lp-prov-amber",
            HTML('<svg width="20" height="20" viewBox="0 0 20 20" fill="none"><circle cx="10" cy="10" r="7" stroke="currentColor" stroke-width="1.5"/><path d="M10 6v4l3 2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/></svg>')
          ),
          div(
            h3("GIS-Powered Routing"),
            p("Candidate pairs are identified using spatial boundary overlap and road-network travel distance \u2014 so costs reflect real-world pipeline routing.")
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

    # ── CONSOLIDATION MODELS ──
    div(
      id = ns("models_section"),
      class = "lp-section lp-tech-section",
      div(
        class = "lp-section-inner",
        div(class = "lp-section-label", "Consolidation Types"),
        div(class = "lp-section-title", "Available cost models"),
        div(class = "lp-section-desc",
          "Each model targets a distinct consolidation pathway. We\u2019re expanding coverage as methodologies are validated."
        ),
        div(
          class = "lp-tech-grid",

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Physical Consolidation"),
              tags$span(class = "lp-tech-status lp-status-live", "Live")
            ),
            div(class = "lp-tech-desc",
              "Estimate capital costs for physically connecting a struggling system to a receiving system via new pipeline, service connections, and infrastructure."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Pipeline"),
              tags$span(class = "lp-contaminant-tag", "Connections"),
              tags$span(class = "lp-contaminant-tag", "New Source"),
              tags$span(class = "lp-contaminant-tag", "CEQA/Permits")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Managerial Consolidation"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Model costs for governance-level consolidation \u2014 shared management, joint operations, or administrative merger without physical connection."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Admin"),
              tags$span(class = "lp-contaminant-tag", "Legal"),
              tags$span(class = "lp-contaminant-tag", "Transition")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Emergency Interconnection"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Estimate costs for emergency tie-in connections \u2014 temporary or permanent infrastructure to address acute failures or drought vulnerability."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Tie-In"),
              tags$span(class = "lp-contaminant-tag", "Temporary"),
              tags$span(class = "lp-contaminant-tag", "Emergency")
            )
          ),

          div(class = "lp-tech-card",
            div(class = "lp-tech-header",
              div(class = "lp-tech-name", "Regionalization"),
              tags$span(class = "lp-tech-status lp-status-soon", "Coming Soon")
            ),
            div(class = "lp-tech-desc",
              "Model multi-system regional consolidation scenarios where multiple small systems connect to a central receiving utility."
            ),
            div(class = "lp-tech-contaminants",
              tags$span(class = "lp-contaminant-tag", "Multi-System"),
              tags$span(class = "lp-contaminant-tag", "Regional"),
              tags$span(class = "lp-contaminant-tag", "Shared Infra")
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
          target = "_blank",
          rel    = "noopener noreferrer",
          "EPA SDWIS"
        ),
        tags$a(
          href   = "https://epictech.org",
          target = "_blank",
          rel    = "noopener noreferrer",
          "EPIC-Tech"
        )
      ),
      p(
        "Data: EPA Safe Drinking Water Information System (SDWIS) \u00b7 EPIC Engineering Cost Model",
        tags$br(),
        "This tool is developed by EPIC-Tech and is not an official EPA product."
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
