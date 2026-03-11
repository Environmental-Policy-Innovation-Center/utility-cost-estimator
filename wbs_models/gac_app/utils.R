# Utility Functions
# Helper functions for the GAC Cost Estimator

# Format work breakdow structure model table
# format_wbs_table <- function(wbs_data) {
  
#   # Add a grouping column for section headers
#   wbs_data <- wbs_data |>
#     mutate(
#       # Create section header rows
#       section = table,
#       # Flag which rows are headers vs data
#       is_header = !duplicated(table)
#     ) 
#   # |>
#   #   arrange(table, WBS)
  
#   # Create the datatable
#   DT::datatable(
#     wbs_data,
    
#     # Options
#     options = list(
#       order = list(list(which(names(wbs_data) == "num_row") - 1, 'asc')),
#       pageLength = 50,
#       paging = FALSE,  # Show all rows
#       searching = TRUE,
#       ordering = FALSE,  # Disable sorting to maintain grouping
      
#       # Row grouping by 'table' column
#       rowGroup = list(
#         dataSrc = which(names(wbs_data) == "table") - 1  # 0-indexed for JS
#       ),
      
#       # Column definitions
#       columnDefs = list(
#         # Hide the 'table' column (used for grouping)
#         list(targets = which(names(wbs_data) == "table") - 1, visible = FALSE),
#         # Hide the section and is_header helper columns
#         list(targets = which(names(wbs_data) %in% c("section", "is_header", "num_row")) - 1, visible = FALSE)
#       )
#     ),
    
#     # Row names
#     rownames = FALSE,
    
#     # Extensions
#     extensions = 'RowGroup',
    
#     # Column names
#     colnames = c(
#       "WBS #" = "WBS",
#       "Item" = "Item",
#       "Design Quantity" = "Design Quantity",
#       "Design Size" = "Design Size",
#       "Size Used" = "Size Used_in_estimate",
#       "Unit Cost" = "Unit Cost",
#       "Total Cost" = "Total Cost",
#       "Useful Life" = "Useful Life"
#     )
#   ) |>
    
#     # Format currency columns
#     formatCurrency(c("Unit Cost", "Total Cost"), "$") |>
    
#     # Format numeric columns
#     formatRound(c("Design Quantity", "Design Size"), digits = 2)
# }

format_wbs_table <- function(wbs_data) {
  
  wbs_data <- wbs_data |>
    dplyr::mutate(
      section   = table,
      is_header = !duplicated(table)
    ) |>
    dplyr::select(-full_line_item_name)
  
  # Columns to display (must all exist in wbs_data)
  display_cols <- c("WBS", "Item", "Design Quantity", "Design Size",
                    "Size Used_in_estimate", "Unit Cost", "Total Cost", "Useful Life")
  
  # Verify they exist before proceeding
  missing_cols <- setdiff(display_cols, names(wbs_data))
  if (length(missing_cols) > 0) {
    warning("format_wbs_table: missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  DT::datatable(
    wbs_data,
    options = list(
      order      = list(list(which(names(wbs_data) == "row_index") - 1, "asc")),
      pageLength = 50,
      paging     = FALSE,
      searching  = TRUE,
      ordering   = FALSE,
      rowGroup   = list(
        dataSrc = which(names(wbs_data) == "table") - 1
      ),
      columnDefs = list(
        list(targets = which(names(wbs_data) == "table") - 1,                              visible = FALSE),
        list(targets = which(names(wbs_data) %in% c("section", "is_header", "row_index")) - 1, visible = FALSE)
      ),
      # Stamp each RowGroup header <tr> with an id after every draw.
      # The RowGroup header is a <th> inside the <tr class="dtrg-group">.
      # We put the id on the <tr> itself so scrollIntoView() works on the row.
      # ID pattern: "wbs-sec-" + label lowercased with non-alphanumeric -> "-"
      # This MUST match the gsub() in mod_output_db.R renderUI exactly.
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

          // Stamp on initial draw (initComplete fires after first draw)
          stampIds();

          // Re-stamp on any subsequent redraws
          api.on('draw', stampIds);
        }
      ")
    ),
    rownames   = FALSE,
    extensions = "RowGroup",
    colnames   = c("WBS #", "Item", "Design Quantity", "Design Size",
                   "Size Used", "Unit Cost", "Total Cost", "Useful Life"),
    class = 'cell-border stripe'
  ) |>
    formatCurrency(c("Unit Cost", "Total Cost"), "$") |>
    formatRound(c("Design Quantity", "Design Size"), digits = 2)
}


# Get standard inputs as a named list
get_standard_inputs <- function(contam_selection, design_type_idx, design_number) {
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  
  standard_inputs <- data.frame(googlesheets4::read_sheet(URL, sheet = "standard_inputs")) |>
    janitor::clean_names()
  
  # Filter to get the matching row (before converting to character)
  matching_row <- standard_inputs |>
    dplyr::filter(
      contaminant_selection == contam_selection,
      design_type == design_type_idx,
      design == design_number
    )

  if (nrow(matching_row) == 0) {
    return(NULL)
  }

  # Ensure only one row is returned
  if (nrow(matching_row) > 1) {
    warning(paste("Multiple rows matched:", nrow(matching_row), "rows found. Using the first one."))
    matching_row <- matching_row[1, , drop = FALSE]
  }
  
  # Now convert to character for consistent handling
  matching_row <- matching_row |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # Return as named list with all key parameters (extracting single values)
  list(
    # System size
    design_flow = matching_row$design_flow_i[1],
    design_flow_units = matching_row$df_units[1],
    average_flow = matching_row$average_flow_i[1],
    average_flow_units = matching_row$af_units[1],
    
    # Carbon inputs
    regen_method = matching_row$regen_type_i[1],
    carbon_life_input_type = matching_row$freund_type_i[1],
    carbon_life_or_kf = matching_row$freund_1[1],
    freund_2 = matching_row$freund_2[1],
    C_0 = matching_row$c_0[1],
    C_b = matching_row$c_b[1],
    
    # Contaminant removal
    ebct_input_type = matching_row$ebct_type_i[1],
    ebct = matching_row$ebct_i[1],
    ebct_output = matching_row$ebct_o[1],
    kss = matching_row$kss[1],
    
    # Pressure vessel design
    num_tanks = matching_row$num_tanks_i[1],
    use_autosize = matching_row$use_autosize[1],
    bed_depth = matching_row$bed_depth[1],
    tank_geometry = matching_row$tank_geom_i[1],
    vessel_height_length = matching_row$comm_height_length[1],
    vessel_diameter = matching_row$comm_diam[1],
    
    # Gravity contactor design
    use_autosize_gravity = matching_row$use_autosize_a[1],
    basin_width = matching_row$basin_width[1],
    basin_length = matching_row$basin_length[1],
    basin_depth = matching_row$basin_op_depth[1],
    
    # Residuals
    backwash_frequency = matching_row$back_interval_i[1],
    discharge_option = matching_row$res_s2_opt_i[1],
    holding_tank = matching_row$res_s1_opt_i[1],
    transfer_method = matching_row$transfer_method_i[1],
    solids_characteristics = matching_row$solids_haz_i[1],
    
    # Optional
    redundant_contactors = matching_row$nrd_i[1],
    num_booster_pumps = matching_row$lines_pump_i[1],
    backwash_pumping = matching_row$no_backwash_i[1],
    backwash_storage = matching_row$no_back_tank_i[1],
    system_automation = matching_row$manual_i[1],
    component_level = matching_row$component_level_i[1],
    include_buildings = matching_row$include_buildings_i[1],
    include_hvac = matching_row$include_hvac_i[1],
    include_land = matching_row$include_land_i[1],
    addon = matching_row$addon_i[1],
    
    # Retrofit
    retrofit = matching_row$retrofit_i[1],
    retrofit_carbon_life_type = matching_row$r_freund_type_i[1],
    retrofit_carbon_life = matching_row$r_freund_1[1],
    retrofit_freund_2 = matching_row$r_freund_2[1],
    retrofit_C_0 = matching_row$r_c_0[1],
    retrofit_C_b = matching_row$r_c_b[1],
    
    # Full row for any additional needs
    full_data = matching_row
  )
}

get_sheet_data <- function(sheet_name, return_type = "vector", column = "name") {
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"

  data <- data.frame(googlesheets4::read_sheet(URL, sheet = sheet_name))
  
  data <- data |>
    janitor::clean_names()
  
  if (return_type == "vector") {
    return(dplyr::pull(data, !!column))
  } else if (return_type == "table") {
    return(data)
  } else {
    stop("return_type must be either 'vector' or 'table'")
  }
}

# Wrapper functions for convenience (optional)
get_contam_type <- function(return_type = "vector") {
  get_sheet_data("contam_type", return_type)
}

get_design_type <- function(return_type = "vector") {
  get_sheet_data("design_type", return_type)
}

get_design_number <- function(return_type = "vector") {
  get_sheet_data("design_number", return_type)
}

#' Convert flow rates between units
#' @param flow Numeric flow rate value
#' @param from Character string of input units (MGD, gpm, cfs)
#' @param to Character string of output units (MGD, gpm, cfs)
#' @return Numeric converted flow rate
convert_flow <- function(flow, from = "MGD", to = "MGD") {
  
  # Convert to MGD first
  flow_mgd <- switch(
    from,
    "MGD" = flow,
    "gpm" = flow * 0.00144,
    "cfs" = flow * 0.646317,
    stop("Invalid input unit")
  )
  
  # Convert from MGD to target
  result <- switch(
    to,
    "MGD" = flow_mgd,
    "gpm" = flow_mgd / 0.00144,
    "cfs" = flow_mgd / 0.646317,
    stop("Invalid output unit")
  )
  
  return(result)
}

#' Calculate EBCT from Freundlich isotherm parameters
#' @param K Freundlich K parameter
#' @param n Freundlich 1/n parameter
#' @param C0 Influent concentration
#' @param Cb Effluent target concentration
#' @param bed_life Target bed life in bed volumes
#' @return EBCT in minutes
calculate_ebct_from_isotherm <- function(K, n, C0, Cb, bed_life = 50000) {
  
  # Simplified calculation - replace with actual isotherm model
  # This is a placeholder for the actual calculation
  
  q_e <- K * (Cb^n)  # Equilibrium loading
  
  # Calculate EBCT (placeholder formula)
  ebct <- (bed_life * 24 * 60) / (C0 - Cb) * 0.0001
  
  return(max(5, min(60, ebct)))  # Constrain to reasonable range
}

#' Calculate bed life from EBCT and isotherm parameters
#' @param ebct EBCT in minutes
#' @param K Freundlich K parameter
#' @param n Freundlich 1/n parameter
#' @param C0 Influent concentration
#' @param Cb Effluent target concentration
#' @return Bed life in bed volumes
calculate_bed_life <- function(ebct, K, n, C0, Cb) {
  
  # Simplified calculation - replace with actual isotherm model
  # This is a placeholder for the actual calculation
  
  q_e <- K * (Cb^n)
  
  # Calculate bed life (placeholder formula)
  bed_life <- ebct * (C0 - Cb) * 10 / 0.0001
  
  return(bed_life)
}

# ===== AutoSize Calculation Functions =====
# Replicates Excel AutoSize logic for bed depth and vessel dimensions

#' Calculate target bed depth based on design flow
#' @param design_flow_mgd Design flow in MGD
#' @param tank_geometry Tank geometry type ("upright", "horizontal", "basin")
#' @return Target bed depth in feet
calculate_target_bed_depth <- function(design_flow_mgd, tank_geometry = "upright") {
  
  # Default targets from Critical Design Assumptions sheet
  target_bed_depth_under <- 4   # for flows <= 0.1 MGD
  target_bed_depth_over <- 7    # for flows > 0.1 MGD
  target_bed_depth_horiz <- 8   # for horizontal vessels
  
  if (tank_geometry == "horizontal") {
    return(target_bed_depth_horiz)
  } else {
    if (design_flow_mgd <= 0.1) {
      return(target_bed_depth_under)
    } else {
      return(target_bed_depth_over)
    }
  }
}

#' Calculate required GAC volume
#' @param design_flow_mgd Design flow in MGD
#' @param ebct_minutes EBCT in minutes
#' @return Required GAC volume in cubic feet
calculate_required_volume <- function(design_flow_mgd, ebct_minutes) {
  
  # Ensure inputs are numeric
  design_flow_mgd <- as.numeric(design_flow_mgd)
  ebct_minutes <- as.numeric(ebct_minutes)
  
  # Validate inputs
  if (is.na(design_flow_mgd) || is.na(ebct_minutes)) {
    stop("calculate_required_volume: design_flow_mgd and ebct_minutes must be numeric")
  }
  
  if (design_flow_mgd <= 0 || ebct_minutes <= 0) {
    stop("calculate_required_volume: design_flow_mgd and ebct_minutes must be positive")
  }
  
  # Volume (cubic feet) = Flow (MGD) × EBCT (min) × 1,000,000 / (1440 min/day) / 7.481 gal/ft³
  volume <- (design_flow_mgd * ebct_minutes * 1000000 / 1440) / 7.481
  
  return(volume)
}

# #' Calculate bed depth using AutoSize logic
# #' @param design_flow_mgd Design flow in MGD
# #' @param ebct_minutes EBCT in minutes
# #' @param num_trains Number of treatment trains
# #' @param tank_geometry Tank geometry ("upright", "horizontal", "basin")
# #' @param vessel_diameter Vessel diameter in feet (optional, for checking)
# #' @param min_bed_depth Minimum allowed bed depth (default 2 ft)
# #' @param max_bed_depth Maximum allowed bed depth (default 8.5 ft for upright, 10 ft for horizontal)
# #' @return Calculated bed depth in feet
# calculate_autosize_bed_depth <- function(design_flow_mgd, 
#                                          ebct_minutes, 
#                                          num_trains = 1,
#                                          tank_geometry = "upright",
#                                          vessel_diameter = NULL,
#                                          min_bed_depth = 2,
#                                          max_bed_depth = NULL) {
  
#   # Ensure numeric inputs
#   design_flow_mgd <- as.numeric(design_flow_mgd)
#   ebct_minutes <- as.numeric(ebct_minutes)
#   num_trains <- as.numeric(num_trains)
  
#   # Validate
#   if (is.na(design_flow_mgd) || is.na(ebct_minutes) || is.na(num_trains)) {
#     stop("calculate_autosize_bed_depth: numeric parameters cannot be NA")
#   }
  
#   # Get target bed depth first (needed for max constraint)
#   target_depth <- calculate_target_bed_depth(design_flow_mgd, tank_geometry)
  
#   # Set max bed depth if not provided
#   # Use absolute max (8.5 for upright, 10 for horizontal)
#   if (is.null(max_bed_depth)) {
#     max_bed_depth <- if (tank_geometry == "horizontal") 10 else 8.5
#   }
  
#   # Calculate required volume
#   required_volume <- calculate_required_volume(design_flow_mgd, ebct_minutes)
  
#   # IMPORTANT: If num_trains is 1 (default), auto-calculate based on flow
#   # Excel's AutoSize calculates num_trains to keep vessel sizes reasonable
#   # Typical pattern: ~1 train per 0.06 MGD of flow
#   if (num_trains == 1 && design_flow_mgd > 0.06) {
#     # Calculate minimum trains needed to keep vessels at reasonable size
#     # Target: each vessel ~20-30 cf, with 2× multiplier (2 vessels in series)
#     # Volume per train = required_volume / num_trains
#     # Want: volume_per_train / 2 < 40 cf per vessel
    
#     min_trains_for_size <- ceiling(required_volume / 80)  # 80 cf = 2 vessels × 40 cf each
    
#     # Also consider flow-based rule (roughly 1 train per 0.06-0.08 MGD)
#     min_trains_for_flow <- ceiling(design_flow_mgd / 0.065)
    
#     # Use the larger of the two
#     num_trains <- max(min_trains_for_size, min_trains_for_flow, 1)
    
#     message(paste("  AutoSize calculated num_trains:", num_trains, 
#                   "(based on flow and vessel size constraints)"))
#   }
  
#   # Calculate surface area needed
#   total_surface_area <- required_volume / target_depth
  
#   # Surface area per train
#   sa_per_train <- total_surface_area / num_trains
  
#   # If vessel diameter is provided, calculate actual bed depth needed
#   if (!is.null(vessel_diameter) && !is.na(vessel_diameter) && vessel_diameter > 0) {
#     vessel_diameter <- as.numeric(vessel_diameter)
    
#     # Account for vessel thickness (0.02 ft typical)
#     vessel_thickness <- 0.02
#     effective_diameter <- vessel_diameter - 2 * vessel_thickness
    
#     # Calculate surface area per vessel
#     vessel_sa <- pi * (effective_diameter / 2)^2
    
#     # Total surface area for all trains
#     # Excel uses 2× vessel surface area (2 contactors in series per train)
#     # C60 = 6.28 = 2 × vessel_sa when num_trains = 1
#     sa_multiplier <- 2
#     total_sa <- num_trains * vessel_sa * sa_multiplier
    
#     # Calculate actual bed depth needed
#     # This matches Excel cell C61: =comp_vol_required/Num_tanks/surface_area
#     # Note: At this stage num_tanks = num_trains (before redundancy applied)
#     calculated_depth <- required_volume / total_sa
    
#   } else {
#     # No diameter provided - use target depth as estimate
#     calculated_depth <- target_depth
#   }
  
#   # Constrain to min/max limits and round up
#   final_depth <- ceiling(max(min_bed_depth, min(calculated_depth, max_bed_depth)) * 10) / 10
  
#   return(final_depth)
# }

calculate_autosize_bed_depth <- function(design_flow_mgd,
                                         ebct_minutes,
                                         num_trains = 1,
                                         num_contactors_in_series = 1,
                                         tank_geometry = "upright",
                                         vessel_diameter = NULL,
                                         min_bed_depth = 2,
                                         max_bed_depth = NULL) {

  design_flow_mgd <- as.numeric(design_flow_mgd)
  ebct_minutes    <- as.numeric(ebct_minutes)
  num_trains      <- as.numeric(num_trains)
  num_contactors_in_series <- as.numeric(num_contactors_in_series)

  if (is.na(design_flow_mgd) || is.na(ebct_minutes) || is.na(num_trains)) {
    stop("calculate_autosize_bed_depth: numeric parameters cannot be NA")
  }

  # Max bed depth defaults (from CDAs: 8.5 ft pressure, no horizontal change)
  if (is.null(max_bed_depth)) {
    max_bed_depth <- if (tank_geometry == "horizontal") 10 else 8.5
  }

  # comp_vol_required (row 18): design_flow_rate * EBCT / 7.481
  # design_flow_rate is in gpm = design_flow_mgd * 1e6 / 1440
  design_flow_gpm  <- design_flow_mgd * 1e6 / 1440
  required_volume  <- design_flow_gpm * ebct_minutes / 7.481   # ft³  ← matches C18

  if (!is.null(vessel_diameter) && !is.na(vessel_diameter) && vessel_diameter > 0) {
    vessel_diameter  <- as.numeric(vessel_diameter)
    vessel_thickness <- 0  # Vessel_thickness CDA = 0 (row 28)
    inner_diameter   <- vessel_diameter - 2 * vessel_thickness

    # Surface area of one vessel (row 59): π × (inner_diam/2)²
    vessel_sa_one <- pi * (inner_diameter / 2)^2

    # Surface area of ALL vessels (row 60): num_trains × vessel_sa_one
    total_sa <- num_trains * vessel_sa_one

    # Bed depth (row 61, cell C61):
    # comp_vol_required / Num_tanks / surface_area_all_vessels
    calculated_depth <- required_volume / num_contactors_in_series / total_sa

  } else {
    # No diameter — fall back to target bed depth from CDAs
    calculated_depth <- calculate_target_bed_depth(design_flow_mgd, tank_geometry)
  }

  # Apply min/max clamp then ROUNDUP to 1 decimal (matches E61)
  clamped_depth <- max(min_bed_depth, min(calculated_depth, max_bed_depth))
  final_depth   <- ceiling(clamped_depth * 10) / 10


  #===bed depth debug===#
  message(sprintf("[vessel_diameter debug] vessel_diameter    = %s", vessel_diameter))
  message(sprintf("[vessel_thickness debug] vessel_thickness    = %s", vessel_thickness))
  message(sprintf("[inner_diameter debug] inner_diameter    = %s", inner_diameter))
  
  message(sprintf("[bed_depth debug] calculated_depth    = %s", calculated_depth))
  message(sprintf("[bed_depth debug] final_depth    = %s", final_depth))
  
  #===bed depth debug end===#
  return(final_depth)
}

#' Calculate vessel dimensions using AutoSize logic
#' @param design_flow_mgd Design flow in MGD
#' @param ebct_minutes EBCT in minutes
#' @param num_trains Number of treatment trains
#' @param tank_geometry Tank geometry
#' @param bed_depth Bed depth in feet (if known)
#' @return List with vessel dimensions
calculate_autosize_vessel_dims <- function(design_flow_mgd,
                                           ebct_minutes,
                                           num_trains = 1,
                                           tank_geometry = "upright",
                                           bed_depth = NULL) {
  
  # Calculate required volume
  required_volume <- calculate_required_volume(design_flow_mgd, ebct_minutes)
  
  # Get bed depth if not provided
  if (is.null(bed_depth)) {
    bed_depth <- calculate_autosize_bed_depth(
      design_flow_mgd, ebct_minutes, num_trains, tank_geometry
    )
  }
  
  # Calculate surface area needed per train
  total_surface_area <- required_volume / bed_depth
  sa_per_train <- total_surface_area / num_trains
  
  if (tank_geometry == "upright") {
    # Calculate diameter from surface area
    # SA = π * r²
    # r = sqrt(SA / π)
    radius <- sqrt(sa_per_train / pi)
    diameter <- 2 * radius
    
    # Round to nearest 0.5 feet
    diameter <- round(diameter * 2) / 2
    
    # Calculate height (add freeboard and underdrain space)
    bed_expansion <- 0.5  # 50% expansion during backwash
    freeboard <- 2        # feet
    underdrain <- 0.5     # feet
    
    height <- bed_depth * (1 + bed_expansion) + freeboard + underdrain
    height <- ceiling(height)
    
    return(list(
      diameter = diameter,
      height = height,
      bed_depth = bed_depth,
      volume = pi * (diameter/2)^2 * height,
      gac_volume = pi * (diameter/2)^2 * bed_depth
    ))
    
  } else if (tank_geometry == "horizontal") {
    # For horizontal vessels, use standard diameter and calculate length
    # Common diameters: 8, 10, 12, 14 feet
    standard_diameters <- c(8, 10, 12, 14)
    
    # Choose diameter based on flow rate
    diameter <- if (design_flow_mgd < 0.5) {
      8
    } else if (design_flow_mgd < 1.0) {
      10
    } else if (design_flow_mgd < 2.0) {
      12
    } else {
      14
    }
    
    # Calculate length needed
    # Surface area = diameter × length (approximation for horizontal cylinder)
    length <- sa_per_train / diameter
    length <- ceiling(length)
    
    return(list(
      diameter = diameter,
      length = length,
      bed_depth = bed_depth,
      volume = pi * (diameter/2)^2 * length,
      gac_volume = pi * (diameter/2)^2 * bed_depth * length / diameter  # approximation
    ))
    
  } else {  # basin
    # For basins, use rectangular geometry
    # Typical aspect ratio 1:1 to 1.5:1 (width:length)
    aspect_ratio <- 1.25
    
    width <- sqrt(sa_per_train / aspect_ratio)
    length <- width * aspect_ratio
    
    # Round to nearest foot
    width <- ceiling(width)
    length <- ceiling(length)
    
    # Operating depth = bed depth + freeboard
    operating_depth <- bed_depth + 1  # 1 ft freeboard for basin
    
    return(list(
      width = width,
      length = length,
      bed_depth = bed_depth,
      operating_depth = operating_depth,
      volume = width * length * operating_depth,
      gac_volume = width * length * bed_depth
    ))
  }
}

#' Calculate vessel volume
#' @param geometry Character: "upright", "horizontal", or "basin"
#' @param diameter Diameter in feet (for vessels)
#' @param height_length Height (upright) or length (horizontal) in feet
#' @param length Basin length in feet
#' @param width Basin width in feet
#' @param depth Basin depth in feet
#' @return Volume in cubic feet
calculate_vessel_volume <- function(geometry, diameter = NULL, height_length = NULL,
                                   length = NULL, width = NULL, depth = NULL) {
  
  volume <- switch(
    geometry,
    "upright" = {
      pi * (diameter/2)^2 * height_length
    },
    "horizontal" = {
      pi * (diameter/2)^2 * height_length
    },
    "basin" = {
      length * width * depth
    },
    stop("Invalid geometry type")
  )
  
  return(volume)
}

#' Calculate GAC mass required
#' @param volume Volume in cubic feet
#' @param bulk_density GAC bulk density in lb/ft3 (default 30)
#' @return Mass in pounds
calculate_gac_mass <- function(volume, bulk_density = 30) {
  volume * bulk_density
}

#' Calculate number of contactors needed
#' @param num_trains Number of parallel trains
#' @param redundancy Redundancy value (N+)
#' @return Total number of contactors
calculate_total_contactors <- function(num_trains, redundancy) {
  num_trains * (1 + redundancy)
}

#' Format currency for display
#' @param value Numeric value
#' @param decimals Number of decimal places
#' @return Formatted string
format_currency <- function(value, decimals = 0) {
  scales::dollar(
    value, 
    largest_with_cents = 1000,
    accuracy = 10^(-decimals)
  ) |> 
    as.character()
}

#' Format large numbers with commas
#' @param value Numeric value
#' @return Formatted string
format_number <- function(value) {
  scales::comma(value, accuracy = 1) |> 
    as.character()
}

#' Safe conversion to numeric with default
#' @param x Value to convert
#' @param default Default value if conversion fails (default 0)
#' @return Numeric value or default
safe_as_numeric <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  result <- suppressWarnings(as.numeric(x))
  if (length(result) == 0 || is.na(result[1])) return(default)
  return(result[1])
}

#' Safe conversion to character with default
#' @param x Value to convert
#' @param default Default value if conversion fails (default "")
#' @return Character value or default
safe_as_char <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  result <- as.character(x)[1]
  if (is.na(result)) return(default)
  return(result)
}

#' Safe conversion to logical with default
#' @param x Value to convert
#' @param default Default value if conversion fails (default FALSE)
#' @return Logical value or default
safe_as_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.logical(x)) return(x[1])
  if (is.numeric(x)) return(as.logical(x[1]))
  if (is.character(x)) {
    x_lower <- tolower(trimws(x[1]))
    if (x_lower %in% c("true", "t", "yes", "y", "1")) return(TRUE)
    if (x_lower %in% c("false", "f", "no", "n", "0", "")) return(FALSE)
  }
  return(default)
}

#' Validate input parameters
#' @param params List of input parameters
#' @return List with valid flag and error messages
# validate_inputs <- function(params) {
  
#   errors <- character(0)
  
#   # Check flow rates
#   if (params$design_flow <= 0) {
#     errors <- c(errors, "Design flow rate must be positive")
#   }
  
#   if (params$average_flow <= 0) {
#     errors <- c(errors, "Average flow rate must be positive")
#   }
  
#   if (params$average_flow > params$design_flow) {
#     errors <- c(errors, "Average flow cannot exceed design flow")
#   }
  
#   # Check concentrations
#   if (params$influent_conc <= params$effluent_target) {
#     errors <- c(errors, "Influent concentration must exceed effluent target")
#   }
  
#   # Check EBCT
#   if (params$ebct_type == "EBCT" && (params$ebct < 1 || params$ebct > 60)) {
#     errors <- c(errors, "EBCT must be between 1 and 60 minutes")
#   }
  
#   # Check vessel dimensions
#   if (params$tank_geometry %in% c("upright", "horizontal")) {
#     if (is.null(params$vessel_diameter) || params$vessel_diameter <= 0) {
#       errors <- c(errors, "Vessel diameter must be positive")
#     }
#   }
  
#   # Check number of trains
#   if (params$num_trains < 1) {
#     errors <- c(errors, "Number of trains must be at least 1")
#   }
  
#   valid <- length(errors) == 0
  
#   list(
#     valid = valid,
#     errors = errors
#   )
# }

# validate_inputs <- function(params) {
  
#   errors <- character(0)
  
#   # Check for NA/NULL values in critical parameters
#   if (is.null(params$design_flow) || is.na(params$design_flow)) {
#     errors <- c(errors, "Design flow rate is required")
#   } else if (params$design_flow <= 0) {
#     errors <- c(errors, "Design flow rate must be positive")
#   }
  
#   if (is.null(params$average_flow) || is.na(params$average_flow)) {
#     errors <- c(errors, "Average flow rate is required")
#   } else if (params$average_flow <= 0) {
#     errors <- c(errors, "Average flow rate must be positive")
#   }
  
#   # Only check comparison if both values exist
#   if (!is.null(params$average_flow) && !is.na(params$average_flow) &&
#       !is.null(params$design_flow) && !is.na(params$design_flow)) {
#     if (params$average_flow > params$design_flow) {
#       errors <- c(errors, "Average flow cannot exceed design flow")
#     }
#   }
  
#   # Check concentrations
#   if (is.null(params$influent_conc) || is.na(params$influent_conc)) {
#     errors <- c(errors, "Influent concentration is required")
#   }
  
#   if (is.null(params$effluent_target) || is.na(params$effluent_target)) {
#     errors <- c(errors, "Effluent target is required")
#   }
  
#   # Only check comparison if both values exist
#   if (!is.null(params$influent_conc) && !is.na(params$influent_conc) &&
#       !is.null(params$effluent_target) && !is.na(params$effluent_target)) {
#     if (params$influent_conc <= params$effluent_target) {
#       errors <- c(errors, "Influent concentration must exceed effluent target")
#     }
#   }
  
#   # Check EBCT
#   if (params$ebct_type == "EBCT") {
#     if (is.null(params$ebct) || is.na(params$ebct)) {
#       errors <- c(errors, "EBCT is required when using EBCT design method")
#     } else if (params$ebct < 1 || params$ebct > 60) {
#       errors <- c(errors, "EBCT must be between 1 and 60 minutes")
#     }
#   }
  
#   # Check vessel dimensions
#   if (!is.null(params$tank_geometry) && params$tank_geometry %in% c("upright", "horizontal")) {
#     if (is.null(params$vessel_diameter) || is.na(params$vessel_diameter)) {
#       errors <- c(errors, "Vessel diameter is required for pressure vessels")
#     } else if (params$vessel_diameter <= 0) {
#       errors <- c(errors, "Vessel diameter must be positive")
#     }
#   }
  
#   # Check number of trains
#   if (is.null(params$num_trains) || is.na(params$num_trains)) {
#     errors <- c(errors, "Number of trains is required")
#   } else if (params$num_trains < 1) {
#     errors <- c(errors, "Number of trains must be at least 1")
#   }
  
#   valid <- length(errors) == 0
  
#   list(
#     valid = valid,
#     errors = errors
#   )
# }

#' Get default cost factors
#' @return List of cost factors
get_cost_factors <- function() {
  list(
    # Equipment multipliers
    vessel_cost_per_cf = 50,
    pump_cost_per_hp = 2000,
    piping_cost_per_lf = 100,
    valve_cost_base = 500,
    
    # Labor and indirect costs
    installation_factor = 0.40,
    electrical_factor = 0.15,
    instrumentation_factor = 0.20,
    site_work_factor = 0.10,
    
    # Engineering and contingency
    engineering_pct = 0.15,
    contingency_pct = 0.20,
    
    # GAC costs
    gac_cost_per_lb = 2.50,
    regeneration_cost_per_lb = 0.75,
    disposal_cost_per_lb = 1.50
  )
}
