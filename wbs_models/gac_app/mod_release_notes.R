# Release Notes Module

releaseNotesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .rn-wrapper {
          max-width: 950px;
          margin: 0 auto;
        }
        .rn-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          gap: 12px;
          margin-bottom: 20px;
          flex-wrap: wrap;
        }
        .rn-title {
          margin: 0;
          font-size: 1.6rem;
          font-weight: 700;
          color: #1a5276;
          text-transform: uppercase;
          letter-spacing: 0.06em;
        }
        .rn-version-card {
          border: 1px solid #dee2e6;
          border-top: 3px solid #1a5276;
          border-radius: 8px;
          padding: 18px 20px;
          margin-bottom: 16px;
          background: #fff;
          box-shadow: 0 1px 4px rgba(0,0,0,0.06);
        }
        .rn-version-header {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          gap: 12px;
          flex-wrap: wrap;
          margin-bottom: 12px;
          border-bottom: 1px solid #dee2e6;
          padding-bottom: 10px;
        }
        .rn-version-title {
          margin: 0;
          font-size: 2.2rem;
          font-weight: 700;
          color: #0a2540;
        }
        .rn-version-date {
          color: #6c757d;
          font-size: 1.6rem;
        }
        .rn-item {
          padding: 10px 0;
          border-bottom: 1px solid #f2f2f2;
        }
        .rn-item:last-child {
          border-bottom: none;
          padding-bottom: 0;
        }
        .rn-feature {
          font-weight: 600;
          margin-bottom: 4px;
          color: #1a5276;
        }
        .rn-description {
          color: #495057;
          line-height: 1.5;
        }
      "))
    ),
    div(
      class = "rn-wrapper",
      div(
        class = "rn-header",
        p(class = "rn-title", "What\u2019s New"),
        actionButton(ns("refresh"), "Refresh", class = "btn btn-primary btn-sm")
      ),
      uiOutput(ns("release_notes_ui"))
    )
  )
}

releaseNotesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    notes_data <- reactive({
      input$refresh

      raw <- read_sheet(
        ss = "1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0",
        sheet = "version_tracking",
        col_types = "cccc"
      )

      required_cols <- c("version_number", "feature", "description", "date")
      missing_cols <- setdiff(required_cols, names(raw))

      validate(
        need(length(missing_cols) == 0,
             paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      )

      raw |>
        transmute(
          version_number = trimws(as.character(version_number)),
          feature        = dplyr::coalesce(trimws(as.character(feature)), ""),
          description    = dplyr::coalesce(trimws(as.character(description)), ""),
          date           = suppressWarnings(as_date(date))
        ) |>
        filter(version_number != "") |>
        arrange(desc(date), desc(version_number))
    })

    output$release_notes_ui <- renderUI({
      notes <- notes_data()

      validate(
        need(nrow(notes) > 0, "No release notes found in the sheet.")
      )

      versions <- unique(notes$version_number)

      tagList(
        lapply(versions, function(v) {
          version_data <- notes |> filter(version_number == v)

          release_date <- version_data |>
            summarise(release_date = max(date, na.rm = TRUE)) |>
            pull(release_date)

          release_date_label <- if (length(release_date) == 0 ||
                                    is.infinite(release_date) ||
                                    is.na(release_date)) {
            "Release date unavailable"
          } else {
            paste("Released:", format(release_date, "%B %d, %Y"))
          }

          div(
            class = "rn-version-card",
            div(
              class = "rn-version-header",
              h3(class = "rn-version-title", paste("Version", v)),
              div(class = "rn-version-date", release_date_label)
            ),
            tagList(
              lapply(seq_len(nrow(version_data)), function(i) {
                div(
                  class = "rn-item",
                  div(class = "rn-feature",      version_data$feature[i]),
                  div(class = "rn-description",  version_data$description[i])
                )
              })
            )
          )
        })
      )
    })

    # Expose latest version number for the app footer
    list(
      latest_version = reactive({
        tryCatch({
          nd <- notes_data()
          if (nrow(nd) > 0) nd$version_number[1] else NA_character_
        }, error = function(e) NA_character_)
      })
    )

  })
}
