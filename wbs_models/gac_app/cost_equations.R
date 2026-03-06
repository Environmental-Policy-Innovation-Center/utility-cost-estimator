# Equipment Cost Calculation Functions
# Implements Excel's power-law regression methodology

#' Load cost coefficient table from Google Sheets
#' @return Data frame with equipment cost coefficients
load_cost_coefficients <- function() {
  
  # Try to load from Google Sheets first (like standard_inputs does)
  tryCatch({
    
    # Google Sheet ID for cost coefficients
    sheet_id <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0"
    
    # Sheet name/tab (adjust if different)
    sheet_name <- "cost_coefficients"
    
    message("Loading cost coefficients from Google Sheets...")
    
    # Read from Google Sheets using googlesheets4
    coeffs <- googlesheets4::read_sheet(
      ss = sheet_id,
      sheet = sheet_name,
      col_types = "c"  # Read all as character first
    )
    
    # Convert to data frame
    coeffs <- as.data.frame(coeffs)
    
    names(coeffs) <- make.names(names(coeffs))
    
    # Convert numeric columns (C8, C9, C10, min_range, max_range)
    numeric_cols <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", 
                      "Pct.deviation.of.fit.equation", 
                      "Rsq.of.fit.equation", 
                      "max.size.used.to.gen.curve", 
                      "min.size.for.which.curve.fits.are.valid")
    
    for (col in numeric_cols) {
      if (col %in% names(coeffs)) {
        coeffs[[col]] <- as.numeric(coeffs[[col]])
      }
    }
    
    message(sprintf("Successfully loaded %d cost coefficient equations from Google Sheets", nrow(coeffs)))
    return(coeffs)
    
  }, error = function(e) {
    
    warning("Failed to load from Google Sheets: ", e$message)
    message("Falling back to local CSV file...")
    
    # Fallback to local CSV file
    possible_paths <- c(
      "/mnt/user-data/outputs/cost_coefficients.csv",
      "cost_coefficients.csv",
      "data/cost_coefficients.csv",
      "wbs_models/gac_app/cost_coefficients.csv",
      "../../cost_coefficients.csv",
      "../../../mnt/user-data/outputs/cost_coefficients.csv"
    )
    
    coeff_file <- NULL
    for (path in possible_paths) {
      if (file.exists(path)) {
        coeff_file <- path
        break
      }
    }
    
    if (is.null(coeff_file)) {
      stop("Cost coefficient file not found in Google Sheets or locally. Tried paths: ", 
           paste(possible_paths, collapse = ", "))
    }
    
    message(sprintf("Loading cost coefficients from local file: %s", coeff_file))
    coeffs <- read.csv(coeff_file, stringsAsFactors = FALSE)
    return(coeffs)
    
  })
}

#' Calculate equipment cost using Excel's power-law equation
#' @param Q Design parameter (volume, area, length, etc.)
#' @param coeffs Named list of coefficients (C1-C10)
#' @param min_range Minimum valid value for Q
#' @param max_range Maximum valid value for Q
#' @return Equipment cost in dollars
calculate_equipment_cost <- function(Q, coeffs, min_range = NULL, max_range = NULL) {
  
  # Validate Q
  if (is.null(Q) || is.na(Q) || Q <= 0) {
    return(0)
  }
  
  # Check if Q is within valid range
  if (!is.null(min_range) && !is.na(min_range) && Q < min_range) {
    warning(paste("Design parameter", Q, "below minimum", min_range))
    return(NA)  # Contact vendor
  }
  
  if (!is.null(max_range) && !is.na(max_range) && Q > max_range) {
    warning(paste("Design parameter", Q, "above maximum", max_range))
    return(NA)  # Contact vendor
  }
  
  # Extract coefficients (default to 0 if missing)
  C1 <- safe_as_numeric(coeffs$C1, 0)
  C2 <- safe_as_numeric(coeffs$C2, 0)
  C3 <- safe_as_numeric(coeffs$C3, 0)
  C4 <- safe_as_numeric(coeffs$C4, 0)
  C5 <- safe_as_numeric(coeffs$C5, 0)
  C6 <- safe_as_numeric(coeffs$C6, 0)
  C7 <- safe_as_numeric(coeffs$C7, 0)
  C8 <- safe_as_numeric(coeffs$C8, 0)
  C9 <- safe_as_numeric(coeffs$C9, 0)
  C10 <- safe_as_numeric(coeffs$C10, 0)
  
  # Calculate cost using combined equation:
  # Cost = Power term + Logarithmic term + Exponential term + Polynomial term
  
  cost <- 0
  
  # Power term: C1 * Q^C2
  if (C1 != 0) {
    cost <- cost + C1 * (Q ^ C2)
  }
  
  # Logarithmic term: C3 * ln(Q) + C4
  if (C3 != 0 || C4 != 0) {
    cost <- cost + C3 * log(Q) + C4
  }
  
  # Exponential term: C5 * e^(C6 * Q)
  if (C5 != 0) {
    cost <- cost + C5 * exp(C6 * Q)
  }
  
  # Polynomial term: C7*Q^3 + C8*Q^2 + C9*Q + C10
  if (C7 != 0 || C8 != 0 || C9 != 0 || C10 != 0) {
    cost <- cost + C7 * (Q^3) + C8 * (Q^2) + C9 * Q + C10
  }
  
  # Return cost (ensure non-negative)
  return(max(0, cost))
}

#' Get cost coefficients for a specific equipment type
#' @param equation_key Equipment equation key (e.g., "ss_pv_eq")
#' @param component_level Component quality level (1=low, 2=mid, 3=high)
#' @return List with coefficients and validity ranges
get_equipment_coefficients <- function(equation_key, component_level = 1) {
  
  # Load coefficient table
  coeff_table <- load_cost_coefficients()
  
  # Find the equipment row
  equipment_row <- coeff_table[coeff_table$Equation.Lookup.Name == equation_key, ]
  
  if (nrow(equipment_row) == 0) {
    warning(paste("Equipment equation not found:", equation_key))
    return(list(
      C1 = 0, C2 = 0, C3 = 0, C4 = 0, C5 = 0,
      C6 = 0, C7 = 0, C8 = 0, C9 = 0, C10 = 0,
      min_range = NA, max_range = NA
    ))
  }
  
  # Extract coefficients
  # Note: Column names from CSV will have dots instead of spaces
  list(
    C1 = equipment_row$C1[1],
    C2 = equipment_row$C2[1],
    C3 = equipment_row$C3[1],
    C4 = equipment_row$C4[1],
    C5 = equipment_row$C5[1],
    C6 = equipment_row$C6[1],
    C7 = equipment_row$C7[1],
    C8 = equipment_row$C8[1],
    C9 = equipment_row$C9[1],
    C10 = equipment_row$C10[1],
    min_range = equipment_row$min.size.for.which.curve.fits.are.valid[1],
    max_range = equipment_row$max.size.used.to.gen.curve[1]
  )
}

#' Calculate pressure vessel cost
#' @param volume_gal Vessel volume in gallons
#' @param material Material type ("SS", "CS", "CSP", "FG")
#' @param quantity Number of vessels
#' @param component_level Quality level (1-3)
#' @return Total cost for all vessels
calculate_pressure_vessel_cost <- function(volume_gal, material = "CS", 
                                          quantity = 1, component_level = 1) {
  
  message(sprintf("  [PV Cost Function] Input: %.2f gal, material=%s, qty=%d", 
                  volume_gal, material, quantity))
  
  # Determine equation key based on material
  equation_key <- switch(
    toupper(material),
    "SS" = "ss_pv_eq",
    "CS" = "cs_pv_eq",
    "CSP" = "csp_pv_eq",
    "FG" = "fg_pv_eq",
    "cs_pv_eq"  # default
  )
  
  message(sprintf("  [PV Cost Function] Using equation: %s", equation_key))
  
  # Get coefficients
  coeffs <- get_equipment_coefficients(equation_key, component_level)
  
  message(sprintf("  [PV Cost Function] C8=%.6f, C9=%.6f, C10=%.6f", 
                  coeffs$C8, coeffs$C9, coeffs$C10))
  
  # Calculate unit cost
  unit_cost <- calculate_equipment_cost(
    Q = volume_gal,
    coeffs = coeffs,
    min_range = coeffs$min_range,
    max_range = coeffs$max_range
  )
  
  message(sprintf("  [PV Cost Function] Unit cost: $%.2f, Total: $%.2f", 
                  unit_cost, unit_cost * quantity))
  
  # Return total cost
  if (is.na(unit_cost)) {
    return(list(
      unit_cost = NA,
      total_cost = NA,
      message = "Contact vendor - outside valid range"
    ))
  }
  
  return(list(
    unit_cost = unit_cost,
    total_cost = unit_cost * quantity,
    message = "OK"
  ))
}

#' Calculate tank cost (backwash, holding, etc.)
#' @param volume_gal Tank volume in gallons
#' @param tank_type Type of tank ("backwash_steel", "backwash_fg", "holding_steel", etc.)
#' @param quantity Number of tanks
#' @param component_level Quality level (1-3)
#' @return Total cost for all tanks
calculate_tank_cost <- function(volume_gal, tank_type = "backwash_steel",
                               quantity = 1, component_level = 1) {
  
  # Determine equation key based on tank type
  equation_key <- switch(
    tolower(tank_type),
    "backwash_steel" = "st_bwt_eq",
    "backwash_fg" = "fg_bwt_eq",
    "backwash_hdpe" = "hdpe_bwt_eq",
    "holding_steel" = "st_bwt_eq",
    "holding_fg" = "fg_bwt_eq",
    "holding_hdpe" = "hdpe_bwt_eq",
    "st_bwt_eq"  # default
  )
  
  # Get coefficients
  coeffs <- get_equipment_coefficients(equation_key, component_level)
  
  # Calculate unit cost
  unit_cost <- calculate_equipment_cost(
    Q = volume_gal,
    coeffs = coeffs,
    min_range = coeffs$min_range,
    max_range = coeffs$max_range
  )
  
  # Return total cost
  if (is.na(unit_cost)) {
    return(list(
      unit_cost = NA,
      total_cost = NA,
      message = "Contact vendor - outside valid range"
    ))
  }
  
  return(list(
    unit_cost = unit_cost,
    total_cost = unit_cost * quantity,
    message = "OK"
  ))
}

#' Calculate basin internals cost
#' @param area_sf Basin surface area in square feet
#' @param component_level Quality level (1-3)
#' @return Cost for basin internals
calculate_basin_internals_cost <- function(area_sf, component_level = 2) {
  
  # Underdrain system
  coeffs_underdrain <- get_equipment_coefficients("cont_bot_eq", component_level)
  cost_underdrain <- calculate_equipment_cost(
    Q = area_sf,
    coeffs = coeffs_underdrain,
    min_range = coeffs_underdrain$min_range,
    max_range = coeffs_underdrain$max_range
  )
  
  return(list(
    underdrain_cost = cost_underdrain,
    total_cost = cost_underdrain
  ))
}

#' Calculate piping cost per linear foot
#' @param diameter Pipe diameter in inches
#' @param material Pipe material ("DI", "CPVC", "PVC", "SS", "ST")
#' @param component_level Quality level (1-3)
#' @return Cost per linear foot
calculate_pipe_cost_per_lf <- function(diameter, material = "PVC", component_level = 2) {
  
  # Determine equation key based on material
  equation_key <- switch(
    toupper(material),
    "DI" = "di_pipe_eq",
    "CPVC" = "cpvc_pipe_eq",
    "PVC" = "pvc_pipe_eq",
    "SS" = "ss_pipe_eq",
    "ST" = "st_pipe_eq",
    "STEEL" = "st_pipe_eq",
    "pvc_pipe_eq"  # default
  )
  
  # Get coefficients
  coeffs <- get_equipment_coefficients(equation_key, component_level)
  
  # Calculate cost per linear foot
  cost_per_lf <- calculate_equipment_cost(
    Q = diameter,
    coeffs = coeffs,
    min_range = coeffs$min_range,
    max_range = coeffs$max_range
  )
  
  return(cost_per_lf)
}

#' Calculate piping cost for a system
#' @param diameter Pipe diameter in inches
#' @param length Total length in linear feet
#' @param material Pipe material
#' @param component_level Quality level (1-3)
#' @return Total piping cost
calculate_piping_cost <- function(diameter, length, material = "PVC", component_level = 2) {
  
  # Get cost per foot from Excel equations
  cost_per_lf <- calculate_pipe_cost_per_lf(diameter, material, component_level)
  
  # Calculate total - NO MULTIPLIER
  # Excel uses the equation cost directly
  total_cost <- cost_per_lf * length
  
  return(list(
    cost_per_lf = cost_per_lf,
    length_lf = length,
    total_cost = total_cost,
    diameter = diameter,
    material = material
  ))
}

#' Calculate valve cost
#' @param valve_size Valve size in inches
#' @param valve_type Type of valve ("MOV", "BFV", "CHV")
#' @param material Valve material ("PP", "SS", "CI")
#' @param component_level Quality level (1-3)
#' @return Valve unit cost
calculate_valve_cost <- function(valve_size, valve_type = "MOV", 
                                material = "CI", component_level = 2) {
  
  # Determine equation key based on type and material
  # For MOV (Motor Operated Valve)
  if (toupper(valve_type) == "MOV") {
    if (toupper(material) == "PP" || toupper(material) == "PVC") {
      # PP/PVC MOV - size dependent
      if (valve_size > 12) {
        equation_key <- "pp_mov_eq_large"
      } else {
        equation_key <- "pp_mov_eq_small"
      }
    } else if (toupper(material) == "SS") {
      equation_key <- "ss_mov_eq"
    } else {  # Cast Iron
      equation_key <- "ci_mov_eq"
    }
  } else if (toupper(valve_type) == "MANUAL") {
    # Manual Valve - typically cheaper than MOV
    # Use PP/PVC for small, CI for large
    if (toupper(material) == "PP" || toupper(material) == "PVC") {
      # Manual valves are typically 1/4 the cost of MOV
      # Use a simplified calculation
      equation_key <- "pp_mov_eq_small"  # Will divide by 4 later
    } else {
      equation_key <- "ci_mov_eq"
    }
  } else if (toupper(valve_type) == "BFV") {
    # Butterfly Valve (Cast Iron)
    if (valve_size > 12) {
      equation_key <- "ci_bfv_eq_large"
    } else {
      equation_key <- "ci_bfv_eq_small"
    }
  } else if (toupper(valve_type) == "CHV") {
    # Check Valve
    if (toupper(material) == "PP" || toupper(material) == "PVC") {
      equation_key <- "pp_chv_eq"  # PP check valve
    } else {
      equation_key <- "ci_chv_eq"  # Cast Iron check valve
    }
  } else {
    # Default to CI MOV
    equation_key <- "ci_mov_eq"
  }
  
  # Get coefficients
  coeffs <- get_equipment_coefficients(equation_key, component_level)
  
  # Calculate unit cost
  unit_cost <- calculate_equipment_cost(
    Q = valve_size,
    coeffs = coeffs,
    min_range = coeffs$min_range,
    max_range = coeffs$max_range
  )
  
  # Manual valves are typically 1/4 the cost of motorized
  if (toupper(valve_type) == "MANUAL") {
    unit_cost <- unit_cost * 0.25
  }
  
  return(list(
    unit_cost = unit_cost,
    valve_size = valve_size,
    valve_type = valve_type,
    material = material,
    equation = equation_key
  ))
}

#' Calculate total valve costs for a system
#' @param num_contactors Number of contactors
#' @param num_trains Number of treatment trains
#' @param valve_size Average valve size (inches)
#' @param component_level Quality level (1-3)
#' @return Total valve costs
calculate_system_valve_costs <- function(num_contactors, num_trains,
                                         valve_size        = 6,
                                         component_level   = 2,
                                         design_flow_mgd   = 1,
                                         no_backwash       = FALSE,
                                         no_back_tank      = TRUE,  # workbook default: no_back_tank=1 (existing storage)
                                         automation_level  = "fully_automated",
                                         proc_pipe_diam    = valve_size,
                                         back_pipe_diam    = valve_size,
                                         in_out_pipe_diam  = valve_size,
                                         backwash_pumps    = 1, #workbook default
                                         num_back_tanks    = 0,
                                         num_booster_pumps = 0,
                                         res_pumps = 0, 
                                         res_hold_tanks = 0, 
                                        #  res_pipe_diam = 2,
                                        #  res_pumps = 0, 
                                        #  res_hold_tanks = 0,
                                         res_pipe_diam = back_pipe_diam
                                        ) {

  # Mirrors Instrumentation and Control sheet + valve_table (CDA rows 267-290)
  # Workbook WBS 4.1.x = MOVs, 4.2.x = Manual valves, 4.3.x = Check valves
  #
  # automation_level maps to workbook `manual` variable:
  #   manual/semi_automated -> 0 MOVs, manual valve counts from valve_table col C/D
  #   fully_automated       -> MOV counts from valve_table col E, 0 manual valves

  # Ensure num_trains and num_contactors are numeric before any arithmetic
  num_trains     <- suppressWarnings(as.numeric(num_trains))
  num_contactors <- suppressWarnings(as.numeric(num_contactors))
  if (is.na(num_trains)     || num_trains <= 0)     num_trains     <- 1
  if (is.na(num_contactors) || num_contactors <= 0) num_contactors <- 1

  no_backwash <- safe_as_logical(no_backwash, FALSE)

  fully_auto <- tolower(automation_level) %in%
                  c("fully_automated", "fully automated", "automated", "3")

  # ── PP/PVC MOV cost equation (workbook pp_mov_eq_small) ──────────────────
  # f(d) = 599.000620282 * exp(0.18198897 * d)   valid for d <= 12"
  pp_mov_cost <- function(d) {
    if (is.null(d) || is.na(d) || d == 0) return(0)
    if (d <= 12) 599.000620282 * exp(0.18198897 * d)
    else         599.000620282 * exp(0.18198897 * 12)
  }

  # PP/PVC manual valve cost equation (workbook pp_man_eq_small)
  # f(d) = 12.117823524*d^2 + 45.057729*d + 112.997449909
  pp_man_cost <- function(d) {
    if (is.null(d) || is.na(d) || d == 0) return(0)
    12.117823524 * d^2 + 45.057729 * d + 112.997449909
  }

  # ── 4.1.1 Process MOVs ───────────────────────────────────────────────────
  # valve_table: proc_MOV_vessel_par = 2 (fully-auto, parallel)
  #              proc_MOV_vessel_ser = 3 (fully-auto, series)
  #              proc_MOV_pump       = 2 (fully-auto, per booster pump)
  #              all = 0 for manual/semi-auto
  vessels_in_series   <- (num_trains > 1) && ((num_contactors / max(1, num_trains)) > 1)
  proc_mov_per_vessel <- if (fully_auto) (if (vessels_in_series) 3L else 2L) else 0L
  proc_mov_per_pump   <- if (fully_auto) 2L else 0L

  tot_proc_MOVs  <- num_contactors * proc_mov_per_vessel +
                    num_booster_pumps * proc_mov_per_pump
  proc_mov_uc    <- pp_mov_cost(proc_pipe_diam)
  proc_mov_total <- tot_proc_MOVs * proc_mov_uc

  # ── 4.1.2 Backwash MOVs ──────────────────────────────────────────────────
  # valve_table: back_MOV_vessel  = 2/vessel  (fully-auto)
  #              back_MOV_pump    = 1/pump    (fully-auto)
  #              back_MOV_tank    = 1/tank    (fully-auto)
  #              back_MOV_no_back = 1         (fully-auto, when no_backwash=TRUE)
  #              all = 0 for manual/semi-auto
  back_mov_per_vessel <- if (fully_auto) 2L else 0L
  back_mov_per_pump   <- if (fully_auto) 1L else 0L
  back_mov_per_tank   <- if (fully_auto) 1L else 0L
  back_mov_no_back    <- if (fully_auto) 1L else 0L

  # Workbook: IF(no_back_tank=1, back_MOV_no_back, 0)
  # no_back_tank=1 means existing storage (no dedicated tank) — adds 1 extra valve
  # This is DIFFERENT from no_backwash (backwash disabled entirely)
  tot_back_MOVs  <- num_contactors * back_mov_per_vessel +
                    backwash_pumps  * back_mov_per_pump  +
                    num_back_tanks  * back_mov_per_tank  +
                    if (isTRUE(no_back_tank)) back_mov_no_back else 0L
  back_mov_uc    <- pp_mov_cost(back_pipe_diam)
  back_mov_total <- tot_back_MOVs * back_mov_uc

  total_mov_cost <- proc_mov_total + back_mov_total
  total_mov_qty  <- tot_proc_MOVs  + tot_back_MOVs

  # ── 4.1.3 Residuals MOVs ─────────────────────────────────────────────────
  # Workbook RM C140:
  #   total_res_MOVs = res_MOV_pump(=1) × res_pumps
  #                  + res_MOV_tank(=1) × (htanks or hbasins)
  #                  + res_MOV_ep(=1)   × ep_cells
  # All = 0 for manual/semi-auto
  res_mov_per_pump  <- if (fully_auto) 1L else 0L
  res_mov_per_tank  <- if (fully_auto) 1L else 0L

  tot_res_MOVs  <- res_pumps     * res_mov_per_pump +
                   res_hold_tanks * res_mov_per_tank
  res_mov_uc    <- pp_mov_cost(res_pipe_diam)
  res_mov_total <- tot_res_MOVs * res_mov_uc

  # ── 4.2.1 Influent/Treated Water Manual Valves ───────────────────────────
  # Workbook IC C24: tot_in_man = in_man = 2
  tot_in_man   <- 2L
  in_man_uc    <- pp_man_cost(in_out_pipe_diam)
  in_man_total <- tot_in_man * in_man_uc

  # ── 4.2.2 Process Manual Valves ──────────────────────────────────────────
  # Workbook IC C26:
  #   tot_proc_man = (proc_man_vessel_cont + proc_man_vessel_ftw) * num_contactors
  #                 + booster_pumps * proc_man_pump
  # valve_table: fully-auto  -> all = 0 (proc_man_vessel_par=0, pump=0)
  #              manual/semi -> proc_man_vessel_par=2 (or ser=3), pump=2
  # proc_man_vessel_ftw (filter-to-waste) = 1 always
  proc_man_per_vessel_cont <- if (fully_auto) 0L else (if (vessels_in_series) 3L else 2L)
  proc_man_per_vessel_ftw  <- 1L
  proc_man_per_pump        <- if (fully_auto) 0L else 2L

  tot_proc_man   <- (proc_man_per_vessel_cont + proc_man_per_vessel_ftw) * num_contactors +
                    num_booster_pumps * proc_man_per_pump
  proc_man_uc    <- pp_man_cost(proc_pipe_diam)
  proc_man_total <- tot_proc_man * proc_man_uc

  # ── 4.2.3 Backwash Manual Valves ─────────────────────────────────────────
  # Workbook IC C27:
  #   tot_back_man = num_contactors × back_man_vessel
  #               + backwash_pumps  × back_man_pump
  #               + num_back_tanks  × back_man_tank
  #               + IF(no_backwash, back_man_no_back, 0)
  # valve_table: fully-auto -> all = 0; manual/semi -> vessel=2, pump=1, tank=1, no_back=1
  back_man_per_vessel <- if (fully_auto) 0L else 2L
  back_man_per_pump   <- if (fully_auto) 0L else 1L
  back_man_per_tank   <- if (fully_auto) 0L else 1L
  back_man_no_back    <- if (fully_auto) 0L else 1L

  tot_back_man   <- num_contactors * back_man_per_vessel +
                    backwash_pumps  * back_man_per_pump  +
                    num_back_tanks  * back_man_per_tank  +
                    if (isTRUE(no_back_tank)) back_man_no_back else 0L
  back_man_uc    <- pp_man_cost(back_pipe_diam)
  back_man_total <- tot_back_man * back_man_uc

  # total_man_cost <- in_man_total + proc_man_total + back_man_total
  # total_man_qty  <- tot_in_man   + tot_proc_man   + tot_back_man

  

  # ── 4.2.4 Residuals Manual Valves ────────────────────────────────────────
  # Workbook RM C141:
  #   total_res_man = res_man_pump(=1) × res_pumps
  #                 + res_man_tank(=1) × (htanks or hbasins)
  #                 + res_man_ep(=1)   × ep_cells
  # All = 0 for fully-auto; manual/semi = 1/pump, 1/tank, 1/cell
  res_man_per_pump  <- if (fully_auto) 0L else 1L
  res_man_per_tank  <- if (fully_auto) 0L else 1L

  tot_res_man    <- res_pumps      * res_man_per_pump +
                    res_hold_tanks * res_man_per_tank
  res_man_uc     <- pp_man_cost(res_pipe_diam)
  res_man_total  <- tot_res_man * res_man_uc


  total_man_cost <- in_man_total + proc_man_total + back_man_total + res_man_total
  total_man_qty  <- tot_in_man   + tot_proc_man   + tot_back_man   + tot_res_man

  
  # ── 4.3 Check Valves ──────────────────────────────────────────────────────
  # PP/PVC CHV cost equation: 23.867181313*d^2 + 53.974127154*d + 43.16515077
  pp_chv_cost <- function(d) 23.867181313 * d^2 + 53.974127154 * d + 43.16515077

  # 4.3.1 Backwash CHV: IC C33
  #   = back_chv_pump(=1) × backwash_pumps + IF(no_backwash=1, back_chv_no_back(=1), 0)
  # Derived internally (independent of pump_results$backwash_pumps):
  #   no_backwash=TRUE  → 0 pumps + 1 existing-supply CHV = 1
  #   no_backwash=FALSE → 1 dedicated pump CHV + 0 = 1
  chv_back_pumps <- if (isTRUE(no_backwash)) 0L else 1L
  tot_back_chv   <- chv_back_pumps + if (isTRUE(no_backwash)) 1L else 0L
  back_chv_uc    <- pp_chv_cost(back_pipe_diam)
  back_chv_cost  <- tot_back_chv * back_chv_uc

  # 4.3.2 Residuals CHV: RM C142
  #   tot_res_chv = res_chv_pump(=1) × res_pumps
  #              + IF(res_pumps=0, res_chv_no_pump(=1), 0)
  tot_res_chv    <- res_pumps * 1L + if (res_pumps == 0) 1L else 0L
  res_chv_uc     <- pp_chv_cost(res_pipe_diam)
  res_chv_cost   <- tot_res_chv * res_chv_uc

  # 4.3.5 Influent CHV: IC C32 = in_chv = 1
  tot_in_chv     <- 1L
  in_chv_uc      <- pp_chv_cost(in_out_pipe_diam)
  in_chv_cost_v  <- tot_in_chv * in_chv_uc

  total_chv_cost <- back_chv_cost + res_chv_cost + in_chv_cost_v
  total_chv_qty  <- tot_back_chv  + tot_res_chv  + tot_in_chv



  # ── Grand total ───────────────────────────────────────────────────────────
  total_cost <- total_mov_cost + total_man_cost + total_chv_cost

  message("Valve costs (workbook IC sheet logic):")
  message(sprintf("  4.1.1 Process MOV:     %d x $%.0f = $%.0f",
                  tot_proc_MOVs, proc_mov_uc, proc_mov_total))
  message(sprintf("  4.1.2 Backwash MOV:    %d x $%.0f = $%.0f",
                  tot_back_MOVs, back_mov_uc, back_mov_total))
  message(sprintf("  4.2.1 In/Out Manual:   %d x $%.0f = $%.0f",
                  tot_in_man, in_man_uc, in_man_total))
  message(sprintf("  4.2.2 Process Manual:  %d x $%.0f = $%.0f",
                  tot_proc_man, proc_man_uc, proc_man_total))
  message(sprintf("  4.2.3 Backwash Manual: %d x $%.0f = $%.0f",
                  tot_back_man, back_man_uc, back_man_total))
  message(sprintf("  4.3.1 Backwash CHV:    %d x $%.0f = $%.0f", tot_back_chv, back_chv_uc, back_chv_cost))
  message(sprintf("  4.3.2 Residuals CHV:   %d x $%.0f = $%.0f", tot_res_chv,  res_chv_uc,  res_chv_cost))
  message(sprintf("  4.3.5 Influent CHV:    %d x $%.0f = $%.0f", tot_in_chv,   in_chv_uc,   in_chv_cost_v))
  message(sprintf("  Total valves: $%.0f", total_cost))

  list(
    # MOV subtotals
    mov_cost        = total_mov_cost,
    mov_quantity    = total_mov_qty,
    proc_mov_cost   = proc_mov_total,
    proc_mov_qty    = tot_proc_MOVs,
    back_mov_cost   = back_mov_total,
    back_mov_qty    = tot_back_MOVs,
    res_mov_cost    = res_mov_total,
    res_mov_qty     = tot_res_MOVs,
    # Manual subtotals
    manual_cost     = total_man_cost,
    manual_quantity = total_man_qty,
    in_man_cost     = in_man_total,
    in_man_qty      = tot_in_man,
    proc_man_cost   = proc_man_total,
    proc_man_qty    = tot_proc_man,
    back_man_cost   = back_man_total,
    back_man_qty    = tot_back_man,
    res_man_cost    = res_man_total,
    res_man_qty     = tot_res_man,
    
    # Check valve subtotals
    chv_cost        = total_chv_cost,
    chv_quantity    = total_chv_qty,
    in_chv_qty      = tot_in_chv,
    in_chv_cost     = in_chv_cost_v,
    back_chv_qty    = tot_back_chv,
    back_chv_cost   = back_chv_cost,
    res_chv_qty     = tot_res_chv,
    res_chv_cost    = res_chv_cost,


    # BFV placeholder
    bfv_cost        = 0,
    bfv_quantity    = 0,
    # Grand total
    total_cost      = total_cost
  )
}


#' Test function - calculate sample equipment costs
test_cost_calculations <- function() {
  
  cat("Testing Excel Cost Methodology Implementation\n")
  cat("=" %R>% rep(60) %R>% paste(collapse=""), "\n\n")
  
  # Test 1: Pressure Vessel
  cat("Test 1: Stainless Steel Pressure Vessel\n")
  cat("  Volume: 5000 gallons\n")
  pv_cost <- calculate_pressure_vessel_cost(5000, "SS", 4, 2)
  cat("  Unit Cost: $", format(pv_cost$unit_cost, big.mark=","), "\n", sep="")
  cat("  Total Cost (4 units): $", format(pv_cost$total_cost, big.mark=","), "\n\n", sep="")
  
  # Test 2: Carbon Steel Vessel
  cat("Test 2: Carbon Steel Pressure Vessel\n")
  cat("  Volume: 5000 gallons\n")
  pv_cost2 <- calculate_pressure_vessel_cost(5000, "CS", 4, 2)
  cat("  Unit Cost: $", format(pv_cost2$unit_cost, big.mark=","), "\n", sep="")
  cat("  Total Cost (4 units): $", format(pv_cost2$total_cost, big.mark=","), "\n\n", sep="")
  
  # Test 3: Backwash Tank
  cat("Test 3: Steel Backwash Tank\n")
  cat("  Volume: 10000 gallons\n")
  tank_cost <- calculate_tank_cost(10000, "backwash_steel", 2, 2)
  cat("  Unit Cost: $", format(tank_cost$unit_cost, big.mark=","), "\n", sep="")
  cat("  Total Cost (2 units): $", format(tank_cost$total_cost, big.mark=","), "\n\n", sep="")
  
  cat("✓ Tests complete\n")
}
