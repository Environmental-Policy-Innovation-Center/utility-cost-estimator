# =============================================================================
# utils.R — Shared utility functions for the GAC Cost Estimator
#
# Organised into five sections:
#   1. Sheet access       load/cache Google Sheets reference data
#   2. Standard inputs    fetch contaminant-specific design parameters
#   3. Flow & geometry    unit conversion and vessel sizing helpers
#   4. Type coercion      safe_as_* wrappers used throughout calculations
#   5. UI helpers         DT table formatter for the WBS output tab
# =============================================================================


# ── 1. Sheet access ───────────────────────────────────────────────────────────

#' Cache all reference sheets at startup.
#'
#' Reads contam_type, design_type, design_number, and standard_inputs from the
#' Google Sheet and stores them under the "gac.sheet_cache" option.  Called
#' once in app.R; subsequent calls to get_sheet_data() use the cache instead of
#' making live requests.
load_gac_sheet_cache <- function() {
  URL    <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  sheets <- c("contam_type", "design_type", "design_number", "standard_inputs")

  cache <- lapply(stats::setNames(sheets, sheets), function(s) {
    message("  caching sheet: ", s)
    data.frame(googlesheets4::read_sheet(URL, sheet = s)) |>
      janitor::clean_names()
  })

  # cost_data has no proper header row (row 1 is a title cell).
  # Read without col_names so every row is a data row and columns are accessed
  # positionally: col 1 = range_name label, col 4 = unit cost.
  message("  caching sheet: cost_data")
  cache[["cost_data"]] <- as.data.frame(
    googlesheets4::read_sheet(URL, sheet = "cost_data",
                              col_names = FALSE, col_types = "c"),
    stringsAsFactors = FALSE
  )

  options(gac.sheet_cache = cache)
  invisible(cache)
}

#' Fetch a single sheet from the cache (or live if cache is absent).
#'
#' @param sheet_name  Sheet tab name (must be one of the sheets cached by
#'   load_gac_sheet_cache).
#' @param return_type "vector" returns dplyr::pull(data, column);
#'   "table" returns the full data frame.
#' @param column      Column name to pull when return_type = "vector".
#'   Defaults to "name".
get_sheet_data <- function(sheet_name, return_type = "vector", column = "name") {
  URL   <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  cache <- getOption("gac.sheet_cache")

  data <- if (!is.null(cache) && !is.null(cache[[sheet_name]])) {
    cache[[sheet_name]]
  } else {
    data.frame(googlesheets4::read_sheet(URL, sheet = sheet_name)) |>
      janitor::clean_names()
  }

  if (return_type == "vector") {
    return(dplyr::pull(data, !!column))
  } else if (return_type == "table") {
    return(data)
  } else {
    stop("return_type must be 'vector' or 'table'")
  }
}

# Convenience wrappers for the three lookup sheets used in mod_inputs.R.
get_contam_type   <- function(return_type = "vector") get_sheet_data("contam_type",   return_type)
get_design_type   <- function(return_type = "vector") get_sheet_data("design_type",   return_type)
get_design_number <- function(return_type = "vector") get_sheet_data("design_number", return_type)

# ── cost_data lookups ─────────────────────────────────────────────────────────
#
# The cost_data sheet is a positional copy of the workbook Cost Data (CD) tab.
# Row 1 is a title cell, so the sheet is cached without column headers.
# Column layout (consistent across all named ranges in this sheet):
#   col 1  range_name label  (e.g. "backfill_cost_cl")
#   col 2  lower bound / item name
#   col 3  upper bound / unit
#   col 4  unit cost         ← the value most callers want
#   col 5  useful life (large system)
#   col 6  useful life (small system)

#' Look up a unit cost from cost_data by range_name (and optional item).
#'
#' @param range_name  Label in column 1 (e.g. "backfill_cost_cl",
#'   "metal_cost_cl", "conc_basin_cost_cl").
#' @param item        For multi-row ranges (e.g. "metal_cost_cl"), the item
#'   descriptor in column 2 (e.g. "Aluminum Railing").  NULL for single-row
#'   ranges.
#' @param default     Fallback value when the label is not found in the cache.
#' @return Numeric unit cost from column 4, or default on miss.
get_cost_data_uc <- function(range_name, item = NULL, default = NA_real_) {
  cd <- getOption("gac.sheet_cache")[["cost_data"]]
  if (is.null(cd)) {
    warning("get_cost_data_uc: cost_data not cached — returning default")
    return(default)
  }

  rows <- cd[!is.na(cd[[1]]) & cd[[1]] == range_name, , drop = FALSE]
  if (nrow(rows) == 0) {
    warning(sprintf("get_cost_data_uc: '%s' not found in cost_data", range_name))
    return(default)
  }

  if (!is.null(item)) {
    rows <- rows[!is.na(rows[[2]]) & rows[[2]] == item, , drop = FALSE]
    if (nrow(rows) == 0) {
      warning(sprintf("get_cost_data_uc: item '%s' not found in '%s'", item, range_name))
      return(default)
    }
  }

  uc <- suppressWarnings(as.numeric(rows[[4]][1]))
  if (is.na(uc)) default else uc
}

#' Build a lo/hi/uc step table from a multi-row cost_data range.
#'
#' Used for ranges like cont_top_cost_cl where each row represents a size band.
#' Rows whose unit cost column contains "contact vendor" (or is otherwise
#' non-numeric) are retained with uc = NA so callers can detect out-of-range.
#'
#' @param range_name  Label in column 1 (e.g. "cont_top_cost_cl").
#' @return Data frame with columns lo (numeric), hi (numeric), uc (numeric).
#'   Returns an empty data frame on cache miss.
get_cost_data_table <- function(range_name) {
  cd <- getOption("gac.sheet_cache")[["cost_data"]]
  if (is.null(cd)) {
    warning("get_cost_data_table: cost_data not cached")
    return(data.frame(lo = numeric(0), hi = numeric(0), uc = numeric(0)))
  }

  rows <- cd[!is.na(cd[[1]]) & cd[[1]] == range_name, , drop = FALSE]
  if (nrow(rows) == 0) {
    warning(sprintf("get_cost_data_table: '%s' not found in cost_data", range_name))
    return(data.frame(lo = numeric(0), hi = numeric(0), uc = numeric(0)))
  }

  data.frame(
    lo = suppressWarnings(as.numeric(rows[[2]])),
    hi = suppressWarnings(as.numeric(rows[[3]])),
    uc = suppressWarnings(as.numeric(rows[[4]]))
  )
}

#' Cache the Critical Design Assumptions sheet at startup.
#'
#' Stores the sheet under "critical.assumptions_cache" option.  Called once in
#' app.R; get_assumption() in calculations_headers.R reads from this cache.
load_critical_design_assumptions_sheet_cache <- function() {
  URL    <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  sheets <- c("critical_design_assumptions")

  cache <- lapply(stats::setNames(sheets, sheets), function(s) {
    message("  caching sheet: ", s)
    data.frame(googlesheets4::read_sheet(URL, sheet = s)) |>
      janitor::clean_names()
  })

  options(critical.assumptions_cache = cache)
  invisible(cache)
}


# ── 2. Standard inputs ────────────────────────────────────────────────────────

#' Fetch contaminant-specific design parameters from the standard_inputs sheet.
#'
#' Performs a three-key lookup (contaminant × design_type × design_number).
#' Falls back to a two-key match (contaminant × design_type) when no exact row
#' exists — this covers custom flow values not in the predefined list.
#'
#' @param contam_selection  Integer index into the contam_type sheet.
#' @param design_type_idx   Integer index into the design_type sheet.
#' @param design_number     Integer index into the design_number sheet (i.e.
#'   the row position of the selected flow, not the flow value itself).
#'
#' @return Named list of design parameters, or NULL if no row matched.
#'
#' @note Key names in the returned list must stay in sync with the field
#'   accesses in mod_inputs.R get_params().  A mismatch causes silent NULL
#'   values that fall through to hardcoded defaults instead of sheet values.
get_standard_inputs <- function(contam_selection, design_type_idx, design_number) {
  URL   <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  cache <- getOption("gac.sheet_cache")

  standard_inputs <- if (!is.null(cache) && !is.null(cache[["standard_inputs"]])) {
    cache[["standard_inputs"]]
  } else {
    data.frame(googlesheets4::read_sheet(URL, sheet = "standard_inputs")) |>
      janitor::clean_names()
  }

  message(sprintf(
    "[get_standard_inputs] contam=%s, design_type=%s, design_number=%s",
    paste(contam_selection, collapse = ","),
    paste(design_type_idx,  collapse = ","),
    paste(design_number,    collapse = ",")
  ))

  # Primary match: all three keys.
  matching_row <- if (length(design_number) > 0 &&
                      length(contam_selection) > 0 &&
                      length(design_type_idx) > 0) {
    standard_inputs |>
      dplyr::filter(
        contaminant_selection == contam_selection,
        design_type           == design_type_idx,
        design                == design_number
      )
  } else {
    data.frame()
  }

  # Fallback: contaminant + design_type only.
  if (nrow(matching_row) == 0 &&
      length(contam_selection) > 0 &&
      length(design_type_idx) > 0) {
    message("[get_standard_inputs] no exact match — falling back to contam+design_type only")
    matching_row <- standard_inputs |>
      dplyr::filter(
        contaminant_selection == contam_selection,
        design_type           == design_type_idx
      )
  }

  if (nrow(matching_row) == 0) {
    message("[get_standard_inputs] no matching row found — returning NULL")
    return(NULL)
  }

  if (nrow(matching_row) > 1) {
    warning(sprintf(
      "get_standard_inputs: %d rows matched — using the first one.", nrow(matching_row)
    ))
    matching_row <- matching_row[1, , drop = FALSE]
  }

  # Convert everything to character for consistent downstream handling.
  matching_row <- matching_row |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  list(
    # Flow
    design_flow        = matching_row$design_flow_i[1],
    design_flow_units  = matching_row$df_units[1],
    average_flow       = matching_row$average_flow_i[1],
    average_flow_units = matching_row$af_units[1],

    # Carbon / bed-life
    regen_type_I = matching_row$regen_type_i[1],
    freund_type  = matching_row$freund_type_i[1],
    freund_1     = matching_row$freund_1[1],
    freund_2     = matching_row$freund_2[1],
    C_0          = matching_row$c_0[1],
    C_b          = matching_row$c_b[1],

    # EBCT / removal
    ebct_input_type = matching_row$ebct_type_i[1],
    ebct            = matching_row$ebct_i[1],
    ebct_output     = matching_row$ebct_o[1],
    kss             = matching_row$kss[1],

    # Pressure vessel geometry
    Num_tanks_I        = matching_row$num_tanks_i[1],
    use_autosize       = matching_row$use_autosize[1],
    bed_depth          = matching_row$bed_depth[1],
    tank_geom_I        = matching_row$tank_geom_i[1],
    comm_height_length = matching_row$comm_height_length[1],
    comm_diam          = matching_row$comm_diam[1],

    # Gravity contactor geometry
    use_autosize_a = matching_row$use_autosize_a[1],
    basin_width    = matching_row$basin_width[1],
    basin_length   = matching_row$basin_length[1],
    basin_op_depth = matching_row$basin_op_depth[1],

    # Residuals
    back_interval_I  = matching_row$back_interval_i[1],
    res_s2_opt_I     = matching_row$res_s2_opt_i[1],
    res_s1_opt_I     = matching_row$res_s1_opt_i[1],
    transfer_method_I = matching_row$transfer_method_i[1],
    solids_haz_I     = matching_row$solids_haz_i[1],

    # Optional / system configuration
    NRD_I              = matching_row$nrd_i[1],
    lines_pump_I       = matching_row$lines_pump_i[1],
    no_backwash_I      = matching_row$no_backwash_i[1],
    no_back_tank_I     = matching_row$no_back_tank_i[1],
    manual_I           = matching_row$manual_i[1],
    component_level_I  = matching_row$component_level_i[1],
    include_buildings_I = matching_row$include_buildings_i[1],
    include_HVAC_I     = matching_row$include_hvac_i[1],
    include_land_I     = matching_row$include_land_i[1],
    addon              = matching_row$addon_i[1],

    # Retrofit
    retrofit_I              = matching_row$retrofit_i[1],
    retrofit_carbon_life_type = matching_row$r_freund_type_i[1],
    retrofit_carbon_life    = matching_row$r_freund_1[1],
    retrofit_freund_2       = matching_row$r_freund_2[1],
    retrofit_C_0            = matching_row$r_c_0[1],
    retrofit_C_b            = matching_row$r_c_b[1],

    # Full raw row (for any ad-hoc access)
    full_data = matching_row
  )
}


# ── 3. Flow & geometry helpers ────────────────────────────────────────────────

#' Convert a flow rate between MGD, gpm, and cfs.
#'
#' @param flow  Numeric flow value.
#' @param from  Input unit: "MGD", "gpm", or "cfs".
#' @param to    Output unit: "MGD", "gpm", or "cfs".
#' @return Numeric converted flow value.
convert_flow <- function(flow, from = "MGD", to = "MGD") {
  flow_mgd <- switch(from,
    "MGD" = flow,
    "gpm" = flow * 0.00144,
    "cfs" = flow * 0.646317,
    stop("convert_flow: invalid 'from' unit: ", from)
  )
  switch(to,
    "MGD" = flow_mgd,
    "gpm" = flow_mgd / 0.00144,
    "cfs" = flow_mgd / 0.646317,
    stop("convert_flow: invalid 'to' unit: ", to)
  )
}

#' Total GAC volume required for a given flow and EBCT (AutoSize row 18).
#'
#' volume (ft³) = flow_gpm × EBCT_min / 7.481 gal/ft³
#'
#' @param design_flow_mgd Design flow in MGD.
#' @param ebct_minutes    Empty bed contact time in minutes.
#' @return Required GAC volume in cubic feet.
calculate_required_volume <- function(design_flow_mgd, ebct_minutes) {
  design_flow_mgd <- as.numeric(design_flow_mgd)
  ebct_minutes    <- as.numeric(ebct_minutes)

  if (anyNA(c(design_flow_mgd, ebct_minutes))) {
    stop("calculate_required_volume: inputs must be numeric")
  }
  if (design_flow_mgd <= 0 || ebct_minutes <= 0) {
    stop("calculate_required_volume: inputs must be positive")
  }

  (design_flow_mgd * 1e6 / 1440) * ebct_minutes / 7.481
}

#' Target bed depth based on design flow (CDA C25/C26).
#'
#' @param design_flow_mgd  Design flow in MGD.
#' @param tank_geometry    "upright", "horizontal", or "basin".
#' @return Target bed depth in feet.
calculate_target_bed_depth <- function(design_flow_mgd, tank_geometry = "upright") {
  if (tank_geometry == "horizontal") return(8)
  if (design_flow_mgd <= 1)         return(4)
  return(7)
}

#' Bed depth for a pressure vessel contactor (AutoSize row 61).
#'
#' Mirrors workbook AutoSize cell C61:
#'   bed_depth = comp_vol / num_contactors_in_series / (num_trains × vessel_SA)
#'
#' When vessel_diameter is not supplied the function returns the CDA target bed
#' depth as a stand-in.  Result is clamped to [min_bed_depth, max_bed_depth]
#' and rounded up to one decimal place (ROUNDUP to 0.1 ft).
#'
#' @param design_flow_mgd          Design flow (MGD).
#' @param ebct_minutes             EBCT (minutes).
#' @param num_trains               Number of parallel treatment trains.
#' @param num_contactors_in_series Vessels in series per train.
#' @param tank_geometry            "upright", "horizontal", or "basin".
#' @param vessel_diameter          Vessel inner diameter (ft); NULL to use
#'   target bed depth fallback.
#' @param min_bed_depth            Minimum allowed bed depth (ft); default 2.
#' @param max_bed_depth            Maximum allowed bed depth (ft); default 8.5
#'   for upright/basin, 10 for horizontal.
#' @return Bed depth in feet, rounded up to nearest 0.1 ft.
calculate_autosize_bed_depth <- function(design_flow_mgd,
                                         ebct_minutes,
                                         num_trains               = 1,
                                         num_contactors_in_series = 1,
                                         tank_geometry            = "upright",
                                         vessel_diameter          = NULL,
                                         min_bed_depth            = 2,
                                         max_bed_depth            = NULL) {
  design_flow_mgd          <- as.numeric(design_flow_mgd)
  ebct_minutes             <- as.numeric(ebct_minutes)
  num_trains               <- as.numeric(num_trains)
  num_contactors_in_series <- as.numeric(num_contactors_in_series)

  if (anyNA(c(design_flow_mgd, ebct_minutes, num_trains))) {
    stop("calculate_autosize_bed_depth: numeric parameters cannot be NA")
  }

  if (is.null(max_bed_depth)) {
    max_bed_depth <- if (tank_geometry == "horizontal") 10 else 8.5
  }

  required_volume <- calculate_required_volume(design_flow_mgd, ebct_minutes)

  calculated_depth <- if (!is.null(vessel_diameter) &&
                          !is.na(vessel_diameter) &&
                          vessel_diameter > 0) {
    # AutoSize C61: comp_vol / num_series / (num_trains × vessel_SA)
    vessel_sa <- pi * (as.numeric(vessel_diameter) / 2)^2
    required_volume / num_contactors_in_series / (num_trains * vessel_sa)
  } else {
    calculate_target_bed_depth(design_flow_mgd, tank_geometry)
  }

  # Clamp then ROUNDUP to 0.1 ft (matches AutoSize E61).
  ceiling(max(min_bed_depth, min(calculated_depth, max_bed_depth)) * 10) / 10
}

#' Volume of a single pressure vessel or gravity basin (ft³).
#'
#' @param geometry       "upright", "horizontal", or "basin".
#' @param diameter       Vessel diameter (ft) — required for vessels.
#' @param height_length  Straight-side height (upright) or shell length
#'   (horizontal) in ft — required for vessels.
#' @param length         Basin length (ft) — required for basins.
#' @param width          Basin width (ft) — required for basins.
#' @param depth          Basin operating depth (ft) — required for basins.
#' @return Volume in cubic feet.
calculate_vessel_volume <- function(geometry,
                                    diameter      = NULL,
                                    height_length = NULL,
                                    length        = NULL,
                                    width         = NULL,
                                    depth         = NULL) {
  switch(geometry,
    "upright"    = pi * (diameter / 2)^2 * height_length,
    "horizontal" = pi * (diameter / 2)^2 * height_length,
    "basin"      = length * width * depth,
    stop("calculate_vessel_volume: invalid geometry '", geometry, "'")
  )
}

#' GAC mass for a given media volume (lbs).
#'
#' @param volume       GAC media volume (ft³).
#' @param bulk_density Bulk density (lb/ft³); workbook default 30.
#' @return Mass in pounds.
calculate_gac_mass <- function(volume, bulk_density = 30) {
  volume * bulk_density
}

#' Pipe diameter (inches) from a flow rate (gpm).
#'
#' Replicates the workbook VLOOKUP against pipe_size_table_cl
#' (Engineering Data rows 130–150): returns the diameter whose minimum-flow
#' breakpoint is the largest value that does not exceed flow_gpm.
#'
#' @param flow_gpm Flow rate in gpm.
#' @return Nominal pipe diameter in inches.
lookup_pipe_diameter <- function(flow_gpm) {
  pipe_size_table <- data.frame(
    min_flow = c(0,      2.1,   4.1,   7.1,   21.1,  41.1,  66.1,
                 116.1,  238.1, 697.1, 1435.1, 2608.1,
                 4132.1, 5299.1, 7528.1, 10265.1, 13643.1,
                 22174.1, 50561.1, 81777.1, 122025.1),
    diameter  = c(0.5,  0.75, 1,    1.5,  2,    2.5,  3,
                  4,    6,    8,    10,    12,
                  14,   16,   18,   20,    24,
                  30,   36,   42,   48)
  )
  flow_gpm <- as.numeric(flow_gpm)
  if (is.na(flow_gpm) || flow_gpm < 0) flow_gpm <- 0
  pipe_size_table$diameter[max(which(pipe_size_table$min_flow <= flow_gpm))]
}


# ── 4. Type coercion helpers ──────────────────────────────────────────────────
# These wrappers are used throughout calculations_headers.R to safely coerce
# parameters that may arrive as character strings, NULL, or NA.

#' Coerce to numeric, returning default on failure.
safe_as_numeric <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  result <- suppressWarnings(as.numeric(x))
  if (length(result) == 0 || is.na(result[1])) return(default)
  result[1]
}

#' Coerce to character, returning default on failure.
safe_as_char <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  result <- as.character(x)[1]
  if (is.na(result)) return(default)
  result
}

#' Coerce to logical, returning default on failure.
#'
#' Accepts TRUE/FALSE, 1/0, and common string representations
#' ("yes"/"no", "true"/"false", "t"/"f", "y"/"n", "1"/"0").
safe_as_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.logical(x)) return(x[1])
  if (is.numeric(x)) return(as.logical(x[1]))
  if (is.character(x)) {
    x_lower <- tolower(trimws(x[1]))
    if (x_lower %in% c("true",  "t", "yes", "y", "1"))  return(TRUE)
    if (x_lower %in% c("false", "f", "no",  "n", "0", "")) return(FALSE)
  }
  default
}


# ── 5. UI helpers ─────────────────────────────────────────────────────────────

#' Render the WBS line-item table as a DT datatable.
#'
#' Mirrors the column layout of the workbook OUTPUT sheet.  Row groups are
#' stamped with DOM ids ("wbs-sec-*") so the TOC links in mod_output_db.R can
#' scroll to them.
#'
#' @param wbs_data  Data frame produced by populate_wbs_table().
#' @return A DT::datatable object ready for renderDT().
format_wbs_table <- function(wbs_data) {
  wbs_data <- wbs_data |>
    dplyr::mutate(
      section   = table,
      is_header = !duplicated(table)
    ) |>
    dplyr::select(-full_line_item_name)

  # Column layout (mirrors workbook OUTPUT sheet):
  # WBS # | Item | Design Quantity | [Quantity Units] |
  # Design Size | [Design Size Units] | Size used in estimate |
  # [Design Size Units.1] | Unit Cost | Total Cost | Useful Life
  display_cols <- c("WBS", "Item",
                    "Design Quantity", "Quantity Units",
                    "Design Size", "Design Size Units",
                    "Size used in estimate", "Design Size Units.1",
                    "Unit Cost", "Total Cost", "Useful Life")

  missing_cols <- setdiff(display_cols, names(wbs_data))
  if (length(missing_cols) > 0) {
    warning("format_wbs_table: missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Hide internal helper columns; unit-label columns stay visible (headers
  # are blanked via colnames so cells still show their values).
  hide_cols <- which(names(wbs_data) %in% c(
    "table", "row_index", "section", "is_header"
  )) - 1

  # One entry per column in wbs_data (rownames = FALSE).
  # Unit columns (indices 3, 5, 7) get blank headers so no label appears.
  all_colnames <- c(
    "WBS #", "Item",
    "Design Quantity", "",
    "Design Size",     "",
    "Size used in estimate", "",
    "Unit Cost", "Total Cost", "Useful Life",
    "table", "row_index", "section", "is_header"
  )

  DT::datatable(
    wbs_data,
    options = list(
      order      = list(list(which(names(wbs_data) == "row_index") - 1, "asc")),
      pageLength = 50,
      paging     = FALSE,
      searching  = TRUE,
      ordering   = FALSE,
      rowGroup   = list(dataSrc = which(names(wbs_data) == "table") - 1),
      columnDefs = list(list(targets = as.list(hide_cols), visible = FALSE)),
      # Stamp each RowGroup <tr> with an id after every draw so TOC links work.
      # ID pattern: "wbs-sec-" + label lowercased, non-alphanumeric -> "-".
      # Must match the gsub() in mod_output_db.R renderUI exactly.
      initComplete = DT::JS("
        function(settings, json) {
          var api = this.api();
          function stampIds() {
            $(api.table().node()).find('tr.dtrg-group').each(function() {
              var label = $(this).find('th, td').first().text().trim();
              var id = 'wbs-sec-' + label.replace(/[^a-zA-Z0-9]+/g, '-').toLowerCase();
              $(this).attr('id', id);
            });
          }
          stampIds();
          api.on('draw', stampIds);
        }
      ")
    ),
    rownames   = FALSE,
    extensions = "RowGroup",
    colnames   = all_colnames,
    class      = "cell-border stripe"
  ) |>
    formatCurrency(c("Unit Cost", "Total Cost"), "$") |>
    formatRound(c("Design Quantity", "Design Size", "Size used in estimate"), digits = 2)
}
