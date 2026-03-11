# Calculation Functions
# Core engineering calculations for GAC system design and costing ----

#' Main calculation function for GAC system
#' @param params List of input parameters
#' @return List containing all calculation results
# Section 8: Chemical Feed and Transfer
# Sources: RM sheet (mixer qty/size), PPS (transfer), Cost Equations rows 163-178
# Default: transfer_method=3, res_holding="none" -> all qty=0
calculate_chemical_feed <- function(
  transfer_method      = 3, #(1=slurry, 2=eductor, 3=none
  eductors             = 0,
  transfer_rate        = 0,
  eductor_size         = NA,
  res_holding          = "none", #("none"/"tanks"/"basins")
  res_transfer_method  = 3,
  res_slurry_pumps     = 0,
  res_eductors         = 0,
  res_transfer_rate    = 0,
  res_eductor_size     = NA,
  hmixers              = 0,
  hmix_size            = 0,
  coag_cmixers         = 0,
  coag_cmix_size       = 0,
  polymer_cmixers      = 0,
  polymer_cmix_size    = 0,
  curve                = 1 #flag (1=use equation, 0=VLOOKUP table)
) {
  transfer_method     <- safe_as_numeric(transfer_method,     3)
  eductors            <- safe_as_numeric(eductors,            0)
  transfer_rate       <- safe_as_numeric(transfer_rate,       0)
  res_transfer_method <- safe_as_numeric(res_transfer_method, 3)
  res_slurry_pumps    <- safe_as_numeric(res_slurry_pumps,    0)
  res_eductors        <- safe_as_numeric(res_eductors,        0)
  res_transfer_rate   <- safe_as_numeric(res_transfer_rate,   0)
  hmixers             <- safe_as_numeric(hmixers,             0)
  hmix_size           <- safe_as_numeric(hmix_size,           0)
  coag_cmixers        <- safe_as_numeric(coag_cmixers,        0)
  coag_cmix_size      <- safe_as_numeric(coag_cmix_size,      0)
  polymer_cmixers     <- safe_as_numeric(polymer_cmixers,     0)
  polymer_cmix_size   <- safe_as_numeric(polymer_cmix_size,   0)
  curve               <- safe_as_numeric(curve,               1)
  res_holding         <- safe_as_char(res_holding,            "none")

  # Step-wise VLOOKUP: table_data = list of c(lo, hi_or_NA, cost)
  vlookup_step <- function(val, tbl) {
    for (row in tbl) {
      lo  <- suppressWarnings(as.numeric(row[[1]]))
      hi  <- suppressWarnings(as.numeric(row[[2]]))
      cst <- suppressWarnings(as.numeric(row[[3]]))
      if (!is.na(lo) && val >= lo) {
        if (is.na(hi))  return(NA_real_)
        if (val <= hi)  return(if (is.na(cst)) NA_real_ else cst)
      }
    }
    NA_real_
  }

  # Cost equation: F*D^G + H*ln(D) + I + J*exp(K*D) + L*D^3 + M*D^2 + N*D + O
  cost_eq <- function(D, F,G,H,I,J,K,L,M,N,O, d_min, d_max) {
    if (is.na(D) || !is.numeric(D) || D == 0) return(0)
    if (D < d_min || D > d_max)               return(NA_real_)
    v <- 0
    if (!is.null(F) && !is.na(F) && F != 0) v <- v + F * D^G
    if (!is.null(H) && !is.na(H) && H != 0) v <- v + H * log(D)
    if (!is.null(I) && !is.na(I) && I != 0) v <- v + I
    if (!is.null(J) && !is.na(J) && J != 0) v <- v + J * exp(K * D)
    if (!is.null(L) && !is.na(L) && L != 0) v <- v + L * D^3
    if (!is.null(M) && !is.na(M) && M != 0) v <- v + M * D^2
    if (!is.null(N) && !is.na(N) && N != 0) v <- v + N * D
    if (!is.null(O) && !is.na(O) && O != 0) v <- v + O
    v
  }

  # Cost Data lookup tables (col B=lo, col C=hi, col D=cost)
  slurry_tbl <- list(
    c(0,50,2695.679),c(3370,8422,3397.740),c(8423,12513,4485.123),
    c(12514,21658,5623.150),c(21659,24064,5503.193),c(24065,29599,7395.644),
    c(29600,36096,8157.438),c(36097,41390,12412.345),c(41391,57032,15312.983),
    c(57033,66176,16434.285),c(66177,72193,16931.905),c(72194,108289,16394.998),
    c(108290,NA,NA)
  )
  # Fix: first row should start at 0
  slurry_tbl[[1]] <- c(0, 3369, 2695.679)

  seductor_tbl <- list(
    c(0,1.5,1592.011),c(1.6,2.0,2338.265),c(2.1,2.5,3611.052),
    c(2.6,3.0,5300.106),c(3.1,4.0,9224.983),c(4.1,6.0,11428.862),
    c(6.1,8.0,17349.222),c(8.1,NA,NA)
  )
  port_mix_tbl <- list(c(0,1.0,2673.582), c(1.1,NA,NA))
  mount_mix_tbl <- list(
    c(0,0.333,2576.057),c(0.34,0.50,3281.473),c(0.51,0.75,2703.243),
    c(0.76,1.00,3740.582),c(1.1,1.5,3059.363),c(1.6,2.0,3689.708),
    c(2.1,4.0,3958.349),c(4.1,NA,NA)
  )
  imp_mix_tbl <- list(
    c(0,1.0,5215.639),c(1.1,1.5,5974.826),c(1.6,2.0,6068.345),
    c(2.1,3.0,7378.363),c(3.1,5.0,8134.753),c(5.1,7.5,11316.308),
    c(7.6,10.0,14374.813),c(10.1,20.0,25944.469),c(20.1,30.0,39315.823),
    c(30.1,NA,NA)
  )

  # Helper: compute uc for transfer equipment
  slurry_uc <- function(rate) {
    if (curve == 1)
      cost_eq(rate, F=0,G=0,H=0,I=0,J=0,K=0,L=0,
              M=-5.31e-07, N=0.216379995, O=1726.136160226, d_min=0, d_max=108290)
    else vlookup_step(rate, slurry_tbl)
  }
  eductor_uc <- function(esz) {
    esz <- suppressWarnings(as.numeric(esz))
    if (is.na(esz)) return(NA_real_)
    if (curve == 1)
      cost_eq(esz, F=0,G=0,H=0,I=0,J=0,K=0,L=0,
              M=22.470786888, N=2229.891840031, O=-1911.930587201, d_min=1.5, d_max=8.1)
    else vlookup_step(esz, seductor_tbl)
  }
  mounted_uc <- function(hp) {
    if (curve == 1)
      cost_eq(hp, F=3220.98006349,G=0.148044622,H=0,I=0,J=0,K=0,L=0,M=0,N=0,O=0,
              d_min=0, d_max=4.1)
    else vlookup_step(hp, mount_mix_tbl)
  }
  impeller_uc <- function(hp) {
    if (curve == 1)
      cost_eq(hp, F=0,G=0,H=0,I=0,J=0,K=0,L=0,M=0,
              N=1053.024973895, O=4010.016173306, d_min=0, d_max=110)
    else vlookup_step(hp, imp_mix_tbl)
  }

  # ── 8.1.1  GAC Solids Transfer ─────────────────────────────────────────────
  # qty: CHOOSE(transfer_method, 1, eductors, 0)
  qty_8_1_1 <- switch(as.character(transfer_method),
    "1" = 1L, "2" = as.integer(eductors), 0L)
  uc_8_1_1  <- if (qty_8_1_1 == 0) NA_real_ else
    switch(as.character(transfer_method),
      "1" = slurry_uc(transfer_rate),
      "2" = eductor_uc(eductor_size),
      NA_real_)
  tc_8_1_1  <- if (is.na(uc_8_1_1) || qty_8_1_1 == 0) NA_real_ else qty_8_1_1 * uc_8_1_1

  # ── 8.1.2  Residuals Holding Tank Transfer ─────────────────────────────────
  # qty: IF(res_holding="none", 0, CHOOSE(res_transfer_method, res_slurry_pumps, res_eductors, 0))
  qty_8_1_2 <- if (res_holding == "none") 0L else
    switch(as.character(res_transfer_method),
      "1" = as.integer(res_slurry_pumps),
      "2" = as.integer(res_eductors),
      0L)
  uc_8_1_2  <- if (qty_8_1_2 == 0) NA_real_ else
    switch(as.character(res_transfer_method),
      "1" = slurry_uc(res_transfer_rate),
      "2" = eductor_uc(res_eductor_size),
      NA_real_)
  tc_8_1_2  <- if (is.na(uc_8_1_2) || qty_8_1_2 == 0) NA_real_ else qty_8_1_2 * uc_8_1_2

  # ── 8.2.1  Residuals Mixers (Portable / Mounted / Impeller) ────────────────
  qty_8_2_1          <- as.integer(hmixers)
  uc_8_2_1_portable  <- if (qty_8_2_1 == 0) NA_real_ else vlookup_step(hmix_size, port_mix_tbl)
  uc_8_2_1_mounted   <- if (qty_8_2_1 == 0) NA_real_ else mounted_uc(hmix_size)
  uc_8_2_1_impeller  <- if (qty_8_2_1 == 0) NA_real_ else impeller_uc(hmix_size)
  tc_8_2_1_portable  <- if (is.na(uc_8_2_1_portable))  NA_real_ else qty_8_2_1 * uc_8_2_1_portable
  tc_8_2_1_mounted   <- if (is.na(uc_8_2_1_mounted))   NA_real_ else qty_8_2_1 * uc_8_2_1_mounted
  tc_8_2_1_impeller  <- if (is.na(uc_8_2_1_impeller))  NA_real_ else qty_8_2_1 * uc_8_2_1_impeller

  # ── 8.4.1  Ferric Chloride Storage Tank Mixers ────────────────────────────
  qty_8_4_1          <- as.integer(coag_cmixers)
  uc_8_4_1_portable  <- if (qty_8_4_1 == 0) NA_real_ else vlookup_step(coag_cmix_size, port_mix_tbl)
  uc_8_4_1_mounted   <- if (qty_8_4_1 == 0) NA_real_ else mounted_uc(coag_cmix_size)
  uc_8_4_1_impeller  <- if (qty_8_4_1 == 0) NA_real_ else impeller_uc(coag_cmix_size)
  tc_8_4_1_portable  <- if (is.na(uc_8_4_1_portable))  NA_real_ else qty_8_4_1 * uc_8_4_1_portable
  tc_8_4_1_mounted   <- if (is.na(uc_8_4_1_mounted))   NA_real_ else qty_8_4_1 * uc_8_4_1_mounted
  tc_8_4_1_impeller  <- if (is.na(uc_8_4_1_impeller))  NA_real_ else qty_8_4_1 * uc_8_4_1_impeller

  # ── 8.5.1  Polymer Storage Tank Mixers ────────────────────────────────────
  qty_8_5_1          <- as.integer(polymer_cmixers)
  uc_8_5_1_portable  <- if (qty_8_5_1 == 0) NA_real_ else vlookup_step(polymer_cmix_size, port_mix_tbl)
  uc_8_5_1_mounted   <- if (qty_8_5_1 == 0) NA_real_ else mounted_uc(polymer_cmix_size)
  uc_8_5_1_impeller  <- if (qty_8_5_1 == 0) NA_real_ else impeller_uc(polymer_cmix_size)
  tc_8_5_1_portable  <- if (is.na(uc_8_5_1_portable))  NA_real_ else qty_8_5_1 * uc_8_5_1_portable
  tc_8_5_1_mounted   <- if (is.na(uc_8_5_1_mounted))   NA_real_ else qty_8_5_1 * uc_8_5_1_mounted
  tc_8_5_1_impeller  <- if (is.na(uc_8_5_1_impeller))  NA_real_ else qty_8_5_1 * uc_8_5_1_impeller

  list(
    qty_8_1_1=qty_8_1_1, uc_8_1_1=uc_8_1_1, tc_8_1_1=tc_8_1_1,
    qty_8_1_2=qty_8_1_2, uc_8_1_2=uc_8_1_2, tc_8_1_2=tc_8_1_2,
    qty_8_2_1=qty_8_2_1,
    uc_8_2_1_portable=uc_8_2_1_portable, tc_8_2_1_portable=tc_8_2_1_portable,
    uc_8_2_1_mounted=uc_8_2_1_mounted,   tc_8_2_1_mounted=tc_8_2_1_mounted,
    uc_8_2_1_impeller=uc_8_2_1_impeller, tc_8_2_1_impeller=tc_8_2_1_impeller,
    qty_8_4_1=qty_8_4_1,
    uc_8_4_1_portable=uc_8_4_1_portable, tc_8_4_1_portable=tc_8_4_1_portable,
    uc_8_4_1_mounted=uc_8_4_1_mounted,   tc_8_4_1_mounted=tc_8_4_1_mounted,
    uc_8_4_1_impeller=uc_8_4_1_impeller, tc_8_4_1_impeller=tc_8_4_1_impeller,
    qty_8_5_1=qty_8_5_1,
    uc_8_5_1_portable=uc_8_5_1_portable, tc_8_5_1_portable=tc_8_5_1_portable,
    uc_8_5_1_mounted=uc_8_5_1_mounted,   tc_8_5_1_mounted=tc_8_5_1_mounted,
    uc_8_5_1_impeller=uc_8_5_1_impeller, tc_8_5_1_impeller=tc_8_5_1_impeller
  )
}

calculate_gac_system <- function(params) {
  
  # Helper to get value or default - ensures numeric conversion where needed
  get_value <- function(value, default) {
    if (is.null(value) || length(value) == 0 || is.na(value) || 
        (is.character(value) && length(value) > 0 && nchar(trimws(value)) == 0)) {
      return(default)
    }
    # Try to convert to numeric if it's a character
    if (is.character(value)) {
      converted <- suppressWarnings(as.numeric(value))
      if (!is.na(converted)) {
        return(converted)
      }
    }
    return(value)
  }
  
  # Validate design_flow early — everything else depends on it
  if (is.null(params$design_flow) || length(params$design_flow) == 0 ||
      is.na(suppressWarnings(as.numeric(params$design_flow)))) {
    stop("calculate_gac_system: params$design_flow is missing or non-numeric")
  }
  params$design_flow       <- as.numeric(params$design_flow)
  params$design_flow_units <- if (is.null(params$design_flow_units) || params$design_flow_units == "")
                                "MGD" else params$design_flow_units

  # Apply defaults for optional parameters (all numeric)
  params$average_flow <- as.numeric(get_value(params$average_flow,
                           params$design_flow * 0.3))  # safe: design_flow validated above
  params$average_flow_units <- get_value(params$average_flow_units, params$design_flow_units)
  params$influent_conc <- as.numeric(get_value(params$influent_conc, 100))
  params$effluent_target <- as.numeric(get_value(params$effluent_target, 10))
  params$num_trains <- as.numeric(get_value(params$num_trains, NA))
  params$redundancy <- as.numeric(get_value(params$redundancy, 0))  # Default: no redundancy
  
  # EBCT must be set before AutoSize - ensure it's numeric
  ebct_minutes <- as.numeric(get_value(params$ebct, 7.5))
  
  # Validate that ebct_minutes is actually numeric
  if (is.na(ebct_minutes) || !is.numeric(ebct_minutes)) {
    ebct_minutes <- 7.5
    message("Warning: EBCT was not numeric, using default 7.5 minutes")
  }
  
  # Debug output
  message(paste("=== AutoSize Debug ==="))
  message(paste("Design Flow:", params$design_flow, params$design_flow_units))
  message(paste("EBCT:", ebct_minutes, "minutes (type:", class(ebct_minutes), ")"))
  message(paste("Num Trains:", params$num_trains, "(type:", class(params$num_trains), ")"))
  message(paste("====================="))
  
  # Convert all flows to MGD for consistency
  design_flow_mgd <- tryCatch(
    as.numeric(convert_flow(params$design_flow, from = params$design_flow_units, to = "MGD")),
    error = function(e) as.numeric(params$design_flow)  # fallback: assume already MGD
  )
  if (is.null(design_flow_mgd) || is.na(design_flow_mgd)) design_flow_mgd <- as.numeric(params$design_flow)

  average_flow_mgd <- tryCatch(
    as.numeric(convert_flow(params$average_flow, from = params$average_flow_units, to = "MGD")),
    error = function(e) as.numeric(params$average_flow)
  )
  if (is.null(average_flow_mgd) || is.na(average_flow_mgd)) average_flow_mgd <- design_flow_mgd * 0.3
  
  # ===== AutoSize Calculations =====
  # Calculate bed depth and vessel dimensions if not provided or if use_autosize is enabled
  
  # Map design_type to tank_geometry
  # design_type: "Pressure" (1) or "Basin"/"Gravity" (2)
  if (!is.null(params$design_type) && !is.na(params$design_type)) {
    if (is.numeric(params$design_type)) {
      # If numeric: 1 = Pressure, 2 = Basin/Gravity
      if (params$design_type == 2) {
        params$tank_geometry <- "basin"
        message("Design type 2 (Basin/Gravity) detected - setting geometry to 'basin'")
      }
    } else if (is.character(params$design_type)) {
      # If character: check for "basin", "gravity", "Basin", "Gravity"
      design_type_lower <- tolower(trimws(params$design_type))
      if (grepl("basin|gravity", design_type_lower)) {
        params$tank_geometry <- "basin"
        message(paste("Design type", params$design_type, "detected - setting geometry to 'basin'"))
      }
    }
  }
  
  # Helper function to safely check if a value is missing or empty
  is_missing <- function(x) {
    is.null(x) || length(x) == 0 || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
  }
  
  # Set defaults for vessel dimensions BEFORE AutoSize (needed for accurate calculation)
  if (is_missing(params$vessel_diameter)) {
    params$vessel_diameter <- 2  # Default diameter
  }
  if (is_missing(params$vessel_height_length)) {
    params$vessel_height_length <- 6  # Default height
  }
  if (is_missing(params$tank_geometry)) {
    params$tank_geometry <- "upright"
  }
  
  message(paste("Final tank_geometry:", params$tank_geometry))
  
  # ===== CALCULATE NUM_TRAINS IF NOT PROVIDED =====
  # Check if user provided num_trains or if it's NULL/NA
  # Use safe evaluation to avoid "missing value where TRUE/FALSE needed"
  need_to_calculate_trains <- TRUE
  
  if (!is.null(params$num_trains)) {
    if (!is.na(params$num_trains)) {
      if (params$num_trains > 0) {
        need_to_calculate_trains <- FALSE
      }
    }
  }
  
  if (need_to_calculate_trains) {
    
    # Estimate bed_depth for initial num_trains calculation
    estimated_bed_depth <- if (!is_missing(params$bed_depth)) {
      as.numeric(params$bed_depth)
    # } else {
    #   # Typical values based on geometry
    #   if (params$tank_geometry == "basin") 5.0 else 3.5
    # }
    } else if (params$tank_geometry == "basin") {
      5.0
    } else {
      if (design_flow_mgd <= 0.1) 4.0 else 7.0
    }  
    
    message("=== AUTO-CALCULATING NUM_TRAINS ===")
    
    # Ensure design_flow_mgd is numeric for sprintf
    flow_for_display <- tryCatch(as.numeric(design_flow_mgd), error = function(e) 0)
    ebct_for_display <- tryCatch(as.numeric(ebct_minutes), error = function(e) 0)
    
    message(sprintf("  Design flow: %.3f MGD", flow_for_display))
    message(sprintf("  EBCT: %.2f minutes", ebct_for_display))
    message(sprintf("  Using estimated bed_depth: %.2f ft", estimated_bed_depth))
    
    # Ensure numeric conversion for calculation
    design_flow_numeric <- as.numeric(design_flow_mgd)
    ebct_numeric <- as.numeric(ebct_minutes)
    
    params$num_trains <- calculate_num_trains(
      design_flow = design_flow_numeric,
      ebct = ebct_numeric,
      bed_depth = estimated_bed_depth,
      vessel_diameter = params$vessel_diameter,
      vessel_height_length = params$vessel_height_length,
      tank_geometry = params$tank_geometry,
      design_type = if (params$tank_geometry == "basin") 2 else 1,
      num_contactors_in_series = 1,
      vessel_thickness = 0
    )
    
    # Handle basin case (returns NA)
    if (is.na(params$num_trains)) {
      params$num_trains <- 1
    }
    
    message(sprintf("  Calculated num_trains: %d", params$num_trains))
    
    } else {
    params$num_trains <- as.numeric(params$num_trains)
    message(sprintf("Using user-specified num_trains: %d", params$num_trains))
  }
  
  # Ensure at least 1 train
  params$num_trains <- max(1, as.integer(params$num_trains))
  
  # Determine if we should use AutoSize
  use_autosize <- FALSE
  if (!is.null(params$standard_inputs) && !is.null(params$standard_inputs$use_autosize)) {
    use_autosize <- tolower(params$standard_inputs$use_autosize) == "yes"
  }
  
  # For basin (gravity) designs, basin dimensions must be resolved BEFORE the
  # bed_depth check — bed_depth for a basin IS basin_depth (the GAC bed layer).
  # Apply basin defaults here so they are available everywhere below.
  if (isTRUE(tolower(params$tank_geometry) == "basin")) {
    # ── Gravity Basin AutoSize ──────────────────────────────────────────────────
    # Mirrors workbook AutoSize sheet gravity logic.
    # Runs when basin dims are missing (sheet stores NA — they're always computed).
    # CDAs from workbook: min_length_g=6, max_length_g=30, min_width=6, max_width=30,
    #                     min_depth=3, max_depth=10, load_max=10, load_min=0.5
    #                     target_bed_depth_under_g=6 (design_flow<=1 MGD, non-UVAOP)
    use_autosize_basin <- is_missing(params$basin_width) ||
                          is_missing(params$basin_length) ||
                          is_missing(params$basin_depth)  ||
                          isTRUE(tolower(params$standard_inputs$use_autosize_a) == "yes")

    if (use_autosize_basin) {
      df_gpm <- design_flow_mgd * 1e6 / 1440  # design flow in gpm

      # CDAs
      min_length_g <- 6; max_length_g <- 30
      min_width_g  <- 6; max_width_g  <- 30
      min_depth_g  <- 3; max_depth_g  <- 10
      # Target bed depth: 6 ft for <=1 MGD, 8 ft for >1 MGD (non-UVAOP)
      target_bd_g <- if (design_flow_mgd <= 1) 6 else 8

      # Step 1: volume required (comp_vol_required_a)
      comp_vol <- df_gpm * ebct_minutes / 7.481

      # Step 2: surface area needed at target bed depth
      comp_sa  <- comp_vol / target_bd_g  # ft²

      # Step 3: raw square side = sqrt(comp_sa) — workbook uses 1 line in autosize
      raw_side <- sqrt(comp_sa)

      # Step 4: clamp to [min_length_g, max_length_g], then round up to nearest 0.5
      raw_side <- max(min_length_g, min(max_length_g, raw_side))
      basin_lw <- ceiling(2 * raw_side) / 2  # ROUNDUP(2*x, 0)/2

      # Step 5: raw bed depth given chosen side length
      basin_sa  <- basin_lw^2
      raw_bd    <- comp_vol / basin_sa           # Num_tanks=1 for gravity
      # Clamp and round up to 1 decimal (ROUNDUP(x, 1))
      raw_bd    <- max(min_depth_g, min(max_depth_g, raw_bd))
      basin_op_depth <- ceiling(raw_bd * 10) / 10   # = basin_op_depth (operating/media depth)

      params$basin_width  <- basin_lw
      params$basin_length <- basin_lw
      params$basin_depth  <- basin_op_depth   # media/operating depth = basin_op_depth

      message(sprintf("Gravity AutoSize: %.0fx%.0f ft basin, bed_depth=%.1f ft",
                      basin_lw, basin_lw, basin_op_depth))
    } else {
      params$basin_width  <- as.numeric(get_value(params$basin_width,  6))
      params$basin_length <- as.numeric(get_value(params$basin_length, 6))
      params$basin_depth  <- as.numeric(get_value(params$basin_depth,  3))
    }

    # For gravity basins, bed_depth = basin_op_depth (the GAC media operating depth).
    # Always override — params$bed_depth from standard_inputs may hold a pressure-vessel
    # bed depth loaded from Google Sheets, which must not bleed into basin calculations.
    params$bed_depth <- params$basin_depth
    message(paste("Basin design: bed_depth set to basin_depth =", params$bed_depth, "ft"))
  # } else if (is_missing(params$bed_depth) || use_autosize) {
  #   # Pressure vessel AutoSize — derive bed_depth from vessel geometry
  #   message("Running AutoSize to calculate bed depth...")
  #   message(paste("  Inputs: flow =", design_flow_mgd, "MGD, ebct =", ebct_minutes,
  #                 "min, num_trains =", params$num_trains, ", diameter =", params$vessel_diameter, "ft"))
    # AutoSize: bed_depth = volume_per_vessel_cf / cross_section_area
    # volume_per_vessel_cf = design_flow_gpm * ebct / 7.48 / num_trains
    # cross_section_area (upright) = pi * (diameter/2)^2
    # cross_section_area (horizontal) = diameter * height_length
    # {
    #   design_flow_gpm_as <- design_flow_mgd * 1e6 / 1440
    #   vol_per_vessel_cf  <- design_flow_gpm_as * ebct_minutes / 7.48 /
    #                           max(1, as.numeric(params$num_trains))
    #   geom <- tolower(params$tank_geometry)
    #   diam <- as.numeric(params$vessel_diameter)
    #   if (geom == "horizontal") {
    #     ht   <- as.numeric(params$vessel_height_length)
    #     xsa  <- diam * ht
    #   } else {
    #     xsa  <- pi * (diam / 2)^2
    #   }
    #   params$bed_depth <- if (xsa > 0) max(2, vol_per_vessel_cf / xsa) else 3.5
    # }

    } else if (is_missing(params$bed_depth) || use_autosize) {
  message("Running AutoSize to calculate bed depth...")
  message(paste("  Inputs: flow =", design_flow_mgd, "MGD, ebct =", ebct_minutes,
                "min, num_trains =", params$num_trains, ", diameter =", params$vessel_diameter, "ft"))

  params$bed_depth <- calculate_autosize_bed_depth(
    design_flow_mgd          = design_flow_mgd,
    ebct_minutes             = ebct_minutes,
    num_trains               = as.numeric(params$num_trains),
    num_contactors_in_series = as.numeric(get_value(params$num_tanks, 1)),
    tank_geometry            = tolower(params$tank_geometry),
    vessel_diameter          = as.numeric(params$vessel_diameter)
  )
  message(paste("AutoSize calculated bed depth:", round(params$bed_depth, 2), "feet"))
  } else {
    params$bed_depth <- as.numeric(params$bed_depth)
  }
  
  # Apply final defaults for vessel dimensions if still missing
  params$vessel_diameter <- as.numeric(get_value(params$vessel_diameter, 2))
  params$vessel_height_length <- as.numeric(get_value(params$vessel_height_length, 6))
  params$tank_geometry <- get_value(params$tank_geometry, "upright")
  params$basin_length <- as.numeric(get_value(params$basin_length, 10))
  params$basin_width <- as.numeric(get_value(params$basin_width, 10))
  params$basin_depth <- as.numeric(get_value(params$basin_depth, 5))
  
  # Add defaults for pump and tank parameters
  params$service_pumps <- as.numeric(get_value(params$service_pumps, 0))
  params$backwash_pumps <- as.numeric(get_value(params$backwash_pumps, 0))
  params$residuals_pumps <- as.numeric(get_value(params$residuals_pumps, 0))
  params$no_backwash <- as.numeric(get_value(params$no_backwash, 0))
  params$no_backwash_tank <- get_value(params$no_backwash_tank, FALSE)
  params$backwash_interval <- as.numeric(get_value(params$backwash_interval, 168))
  params$residuals_disposal <- get_value(params$residuals_disposal, "POTW")
  params$residuals_tank <- get_value(params$residuals_tank, "no holding tank")
  params$automation_level <- get_value(params$automation_level, "fully automated")
  params$manual_override <- get_value(params$manual_override, FALSE)
  # Workbook INPUT row 101/103: "Leave blank to include buildings/land" → default TRUE
  # Sheet16: include_buildings/land = IF(..._I="no",0,1) — blank means include
  params$include_buildings <- get_value(params$include_buildings, TRUE)
  params$include_hvac      <- get_value(params$include_hvac,      FALSE)
  params$include_land      <- get_value(params$include_land,      TRUE)
  # Workbook Sheet7 (Indirect Assumptions): include_permits = 1, include_pilot = 1 by default
  params$include_permits <- get_value(params$include_permits, TRUE)
  params$include_pilot   <- get_value(params$include_pilot,   TRUE)
  params$retrofit <- get_value(params$retrofit, FALSE)
  params$regen_type <- get_value(params$regen_type, "regeneration off-site (non-hazardous)")
  params$backwash_frequency <- as.numeric(get_value(params$backwash_frequency, 52))
  
  # Calculate contactor sizing
  contactor_results <- calculate_contactors(
    design_flow = design_flow_mgd,
    ebct = ebct_minutes,
    geometry = params$tank_geometry,
    num_trains = params$num_trains,
    num_contactors_in_series = as.numeric(get_value(params$num_contactors_in_series, 1)),
    redundancy = params$redundancy,
    bed_depth = params$bed_depth,
    diameter = params$vessel_diameter,
    height_length = params$vessel_height_length,
    basin_length = params$basin_length,
    basin_width = params$basin_width,
    basin_depth = params$basin_depth
  )
  
  # ── Gravity basin counts (workbook: op_num_basins, total_num_basins) ────────
  # op_num_basins  = ROUNDUP(min_basin_vol / (basin_width * basin_length * basin_op_depth), 0)
  # min_basin_vol  = design_flow_rate_gpm * EBCT_minutes / 7.48
  # total_num_basins = op_num_basins + NRD_g
  # NRD_g: if op_num_basins==1 → 1; small system → 0; medium → 1; large → 2
  # Use tank_geometry as the reliable signal — it's set by the mapping block above
  is_gravity_design <- isTRUE(tolower(params$tank_geometry %||% "") == "basin") ||
                       isTRUE(as.character(params$design_type %||% "1") %in% c("2","gravity","basin","open channel"))
  if (is_gravity_design) {
    design_flow_gpm_calc <- design_flow_mgd * 1e6 / 1440
    ebct_min_calc        <- as.numeric(params$ebct %||% 7.5)
    basin_w              <- as.numeric(params$basin_width  %||% 10)
    basin_l              <- as.numeric(params$basin_length %||% 10)
    basin_d              <- as.numeric(params$basin_depth  %||% 5)   # basin_op_depth
    min_basin_vol_calc   <- design_flow_gpm_calc * ebct_min_calc / 7.48
    op_num_basins_calc   <- ceiling(min_basin_vol_calc / (basin_w * basin_l * basin_d))
    # NRD_g: workbook CHOOSE(ss_cat2, 0, 1, 2); ss_cat2=1 if flow<1MGD, 2 if <10, 3 otherwise
    ss_cat2_calc <- if (design_flow_mgd < 1) 1L else if (design_flow_mgd < 10) 2L else 3L
    # NRD_g: respect user-provided NRD_I first, else apply workbook defaults
    nrd_i_val <- suppressWarnings(as.numeric(params$redundancy))
    NRD_g_calc <- if (!is.na(nrd_i_val) && !is.null(params$redundancy) && 
                       !is.na(params$redundancy) && params$redundancy != "") {
      as.integer(nrd_i_val)
    } else if (op_num_basins_calc == 1) {
      1L  # NRD_small_1 = 1 (workbook default: always 1 redundant when 1 operating)
    } else {
      c(0L, 1L, 2L)[ss_cat2_calc]  # NRD_small=0, NRD_basins_medium=1, NRD_basins_large=2
    }
    total_num_basins_calc <- op_num_basins_calc + NRD_g_calc
  } else {
    op_num_basins_calc    <- NULL
    total_num_basins_calc <- NULL
  }

  # Calculate GAC requirements
  gac_results <- calculate_gac_requirements(
    total_volume    = contactor_results$total_gac_volume,
    influent_conc   = params$influent_conc,
    effluent_target = params$effluent_target,
    average_flow    = average_flow_mgd,
    regen_type      = params$regen_type,
    # Bed-life parameters — workbook Pump-Pipe-Structure / CDA sheet defaults
    freund_type     = params$freund_type      %||% 4,
    freund_1        = params$freund_1         %||% 66600,
    freund_2        = params$freund_2         %||% NULL,
    design_type     = if (isTRUE(tolower(params$tank_geometry) == "basin")) 2L else 1L,
    media_volume    = contactor_results$gac_volume_per_contactor,
    num_treat_lines = params$num_trains       %||% 2,
    BV_definition   = params$BV_definition    %||% "EBCT per vessel",
    Num_tanks       = params$num_contactors_in_series %||% 1,
    op_num_basins   = op_num_basins_calc,   # computed above for gravity
    GAC_each        = contactor_results$gac_volume_per_contactor,
    GAC_density     = params$GAC_density      %||% 30,
    makeup_rate     = params$makeup_rate      %||% 0.10,
    makeup_rate_off = params$makeup_rate_off  %||% 0.30,
    # needed by options 2 and 3 to derive D, v, M
    bed_depth       = contactor_results$bed_depth,    
    basin_op_depth  = contactor_results$basin_depth 
  )
  
  # Calculate pump requirements
  pump_results <- calculate_pumps(
    design_flow     = design_flow_mgd,
    num_trains      = params$num_trains,
    service_pumps   = params$service_pumps,
    backwash_pumps  = params$backwash_pumps,
    residuals_pumps = params$residuals_pumps,
    tank_geometry   = params$tank_geometry,
    no_backwash     = params$no_backwash,
    ss_cat2         = params$ss_cat2 %||% 1
  )
  
  # Calculate tank requirements
  tank_results <- calculate_tanks(
    design_flow = design_flow_mgd,
    no_backwash = params$no_backwash,
    no_backwash_tank = params$no_backwash_tank,
    backwash_interval = params$backwash_interval,
    residuals_disposal = params$residuals_disposal,
    residuals_tank = params$residuals_tank,
    num_contactors = contactor_results$total_contactors,
    component_level = 1  # Default to low-cost
  )
  
  # Calculate piping and valves
  # piping_results <- calculate_piping_valves(
  #   num_contactors = contactor_results$total_contactors,
  #   num_trains = params$num_trains,
  #   automation_level = params$automation_level,
  #   design_flow_mgd = design_flow_mgd,
  #   component_level = 1, # Default to low-cost
  #   no_backwash = params$no_backwash
  # )

  piping_results <- calculate_piping_valves(
    num_contactors  = contactor_results$total_contactors,
    num_trains      = params$num_trains,
    automation_level = params$automation_level,
    design_flow_mgd = design_flow_mgd,
    component_level = 1,
    no_backwash     = params$no_backwash,
    facil_length    = NULL,
    vessel_diameter = params$vessel_diameter,     
    vessel_length   = params$vessel_height_length, 
    tank_geometry   = params$tank_geometry,
    params          = params,        
    tanks           = tank_results,
    backwash_pumps    = pump_results$backwash_pumps    %||% 0,
    num_back_tanks    = tank_results$num_back_tanks    %||% 0,  
    num_booster_pumps = pump_results$booster_pumps     %||% 0      
  )
  
  # Calculate instrumentation and controls
  controls_results <- calculate_controls(
    automation_level    = params$automation_level,
    num_contactors      = contactor_results$total_contactors,
    num_trains          = params$num_trains,
    manual_override     = params$manual_override,
    ss_cat2             = params$ss_cat2            %||% 1,
    design_type         = params$design_type        %||% 1,
    add_on              = params$add_on             %||% 0,
    num_back_tanks      = tank_results$num_back_tanks        %||% 0,
    res_holding         = params$res_holding        %||% "none",
    num_res_tanks       = tank_results$num_residuals_tanks   %||% 0,
    num_res_basins      = tank_results$num_residuals_basins  %||% 0,
    bp_pct              = params$bp_pct             %||% 0,
    in_out_pipe_diam    = piping_results$in_out_pipe_diam    %||% 1.5,
    proc_pipe_diam      = piping_results$proc_pipe_diam      %||% 1.5,
    back_pipe_diam      = piping_results$back_pipe_diam      %||% 2.0,
    res_pipe_diam       = piping_results$res_pipe_diam       %||% 2.0,
    tot_MOVs            = piping_results$mov_quantity         %||% 0,
    fm_lkp_io           = params$fm_lkp_io          %||% "flow_prop",
    fm_lkp_proc         = params$fm_lkp_proc        %||% "flow_mag",
    fm_lkp_back         = params$fm_lkp_back        %||% "flow_prop",
    fm_lkp_res          = params$fm_lkp_res         %||% "flow_prop",
    Operator_LOE        = params$Operator_LOE        %||% 40.607,
    booster_pumps       = pump_results$booster_pumps         %||% 0,
    backwash_pumps      = pump_results$backwash_pumps        %||% 0,
    res_pumps           = pump_results$residuals_pumps       %||% 0,
    transfer_method     = params$transfer_method    %||% 3,
    res_transfer_method = params$res_transfer_method %||% 3,
    eductors            = params$eductors           %||% 0,
    res_slurry_pumps    = params$res_slurry_pumps   %||% 0,
    res_eductors        = params$res_eductors       %||% 0,
    mixers              = params$mixers             %||% 0
  )
  
  # Calculate chemical feed and transfer (Section 8)
  chem_feed_results <- calculate_chemical_feed(
    transfer_method     = params$transfer_method     %||% 3,
    eductors            = params$eductors            %||% 0,
    transfer_rate       = params$transfer_rate       %||% 0,
    eductor_size        = params$eductor_size        %||% NA,
    res_holding         = params$res_holding         %||% "none",
    res_transfer_method = params$res_transfer_method %||% 3,
    res_slurry_pumps    = params$res_slurry_pumps    %||% 0,
    res_eductors        = params$res_eductors        %||% 0,
    res_transfer_rate   = params$res_transfer_rate   %||% 0,
    res_eductor_size    = params$res_eductor_size    %||% NA,
    hmixers             = params$hmixers             %||% 0,
    hmix_size           = params$hmix_size           %||% 0,
    coag_cmixers        = params$coag_cmixers        %||% 0,
    coag_cmix_size      = params$coag_cmix_size      %||% 0,
    polymer_cmixers     = params$polymer_cmixers     %||% 0,
    polymer_cmix_size   = params$polymer_cmix_size   %||% 0,
    curve               = params$curve               %||% 1
  )

  # Calculate buildings and site work
  site_results <- calculate_site_buildings(
    include_buildings = params$include_buildings,
    include_hvac = params$include_hvac,
    include_land = params$include_land,
    retrofit = params$retrofit,
    total_contactors = contactor_results$total_contactors,
    design_flow = design_flow_mgd,
    tank_geometry = params$tank_geometry,
    piping_length_lf = piping_results$piping_length_lf
  )
  
  # Compile capital costs
  capital_costs <- compile_capital_costs(
    contactors = contactor_results,
    gac = gac_results,
    pumps = pump_results,
    tanks = tank_results,
    piping = piping_results,
    controls = controls_results,
    site = site_results,
    include_land = params$include_land,
    include_permits = params$include_permits,
    include_pilot = params$include_pilot,
    retrofit = params$retrofit,
    design_flow_mgd = design_flow_mgd,
    residuals_disposal = params$residuals_disposal %||% "potw"
  )
  
  # Calculate O&M costs
  om_costs <- calculate_om_costs(
    design_flow_mgd    = design_flow_mgd,
    average_flow_mgd   = average_flow_mgd,
    gac_results        = gac_results,
    pump_results       = pump_results,
    contactor_results  = contactor_results,
    tank_results       = tank_results,
    site_results       = site_results,
    regen_type         = params$regen_type   %||% "regeneration off-site (non-hazardous)",
    design_type        = if (tolower(params$tank_geometry %||% "") == "basin") 2L else 1L,
    automation_level   = params$automation_level,
    residuals_disposal = params$residuals_disposal %||% "potw",
    retrofit           = params$retrofit,
    backwash_interval  = params$backwash_interval,
    num_trains         = as.numeric(params$num_trains %||% 2),
    total_num_basins   = if (!is.null(total_num_basins_calc)) as.numeric(total_num_basins_calc) else NULL
  )
  
  # Return comprehensive results
  list(
    success = TRUE,
    params = params,
    design_flow_mgd = design_flow_mgd,
    average_flow_mgd = average_flow_mgd,
    ebct_minutes = ebct_minutes,
    # Gravity basin counts (workbook: op_num_basins, total_num_basins)
    op_num_basins    = op_num_basins_calc,
    total_num_basins = total_num_basins_calc,
    contactors = contactor_results,
    gac = gac_results,
    pumps = pump_results,
    tanks = tank_results,
    piping = piping_results,
    controls = controls_results,
    chem_feed = chem_feed_results,
    site = site_results,
    capital_costs = capital_costs,
    om_costs = om_costs,
    total_capital   = capital_costs$total_direct,
    indirect_cost   = capital_costs$total_indirect,
    addon_cost      = capital_costs$addon_cost,
    total_project   = capital_costs$total_project,
    annualized_cost = om_costs$total_annual
  )
}


# Calculate contactor specifications and costs -----
calculate_contactors <- function(design_flow, ebct, geometry, num_trains, num_contactors_in_series = 1, redundancy,
                                bed_depth, diameter = NULL, height_length = NULL,
                                basin_length = NULL, basin_width = NULL, basin_depth = NULL) {
  
  # Ensure all numeric parameters are actually numeric
  design_flow <- as.numeric(design_flow)
  ebct <- as.numeric(ebct)
  num_trains <- as.numeric(num_trains)
  redundancy <- as.numeric(redundancy)
  bed_depth <- as.numeric(bed_depth)
  
  if (!is.null(diameter)) diameter <- as.numeric(diameter)
  if (!is.null(height_length)) height_length <- as.numeric(height_length)
  if (!is.null(basin_length)) basin_length <- as.numeric(basin_length)
  if (!is.null(basin_width)) basin_width <- as.numeric(basin_width)
  if (!is.null(basin_depth)) basin_depth <- as.numeric(basin_depth)
  
  # Validate critical parameters
  if (is.na(design_flow) || is.na(ebct) || is.na(num_trains) || is.na(bed_depth)) {
    stop("calculate_contactors: Critical parameters cannot be NA")
  }
  
  # Determine if this is a Basin (gravity) or Pressure design
  is_basin <- (geometry == "basin")
  
  message(sprintf("=== CONTACTOR COSTING DEBUG ==="))
  message(sprintf("Design type: %s", if (is_basin) "Basin (Gravity)" else "Pressure"))
  message(sprintf("Design flow: %.3f MGD", design_flow))
  
  # Calculate number of contactors based on FLOW CAPACITY (Excel's method)
  # NOT based on num_trains × (1 + redundancy)
  
  # Calculate flow capacity per vessel (gpm)
  gac_volume_per_vessel_gal <- if (geometry %in% c("upright", "horizontal")) {
    # For pressure vessels
    if (is.null(diameter)) {
      diameter <- sqrt(volume_per_contactor_cf / (pi * bed_depth))
    }
    pi * (diameter/2)^2 * bed_depth * 7.48052  # GAC volume in gallons
  } else {
    # For basins
    if (!is.null(basin_length) && !is.null(basin_width)) {
      basin_length * basin_width * bed_depth * 7.48052
    } else {
      # Fallback: derive from required EBCT volume
      (design_flow * 1e6 / 1440) * bed_depth * 7.48052 / ebct
    }
  }
  
  flow_per_vessel_gpm <- gac_volume_per_vessel_gal / ebct
  
  # Calculate total flow (gpm)
  design_flow_gpm <- design_flow * 1000000 / 1440
  
  # Calculate vessels needed to handle the flow
  vessels_needed <- ceiling(design_flow_gpm / flow_per_vessel_gpm)
  
  # Total contactors — workbook logic differs by design type
  if (is_basin) {
    # ── Gravity basins: workbook op_num_basins + NRD_g ────────────────────────
    # op_num_basins = ROUNDUP(min_basin_vol / (basin_w × basin_l × basin_op_depth), 0)
    # NRD_g: IF(NRD_I<>"", NRD_I, IF(op_num_basins==1, 1, CHOOSE(ss_cat2, 0,1,2)))
    # NRD_I is passed as `redundancy`; NULL/NA means use workbook default
    basin_op_d       <- if (!is.null(basin_depth) && !is.na(basin_depth)) basin_depth else bed_depth
    min_basin_vol_cf <- design_flow_gpm * ebct / 7.48
    basin_vol_cf     <- basin_length * basin_width * basin_op_d
    op_num_basins    <- ceiling(min_basin_vol_cf / basin_vol_cf)
    nrd_i_num        <- suppressWarnings(as.numeric(redundancy))
    NRD_g <- if (!is.na(nrd_i_num) && nrd_i_num > 0) {
      as.integer(nrd_i_num)
    } else if (op_num_basins == 1) {
      1L   # NRD_small_1 = 1 (workbook: single operating basin always gets 1 redundant)
    } else {
      ss_cat2 <- if (design_flow < 1) 1L else if (design_flow < 10) 2L else 3L
      c(0L, 1L, 2L)[ss_cat2]   # NRD_small=0, medium=1, large=2
    }
    total_contactors <- op_num_basins + NRD_g
    message(sprintf("  Basin calc: op_basins=%d, NRD_g=%d, total=%d",
                    op_num_basins, NRD_g, total_contactors))
  } else {
    # ── Pressure vessels: num_trains × contactors_in_series + NRD ─────────────
    total_contactors <- (num_trains * num_contactors_in_series) + as.integer(redundancy)
  }
  
  message(sprintf("  Flow-based calculation:"))
  message(sprintf("    Design flow: %.2f gpm", design_flow_gpm))
  message(sprintf("    Flow per vessel: %.2f gpm", flow_per_vessel_gpm))
  message(sprintf("    Vessels needed: %d", vessels_needed))
  message(sprintf("    Redundancy: %d", as.integer(redundancy)))
  message(sprintf("    Total contactors: %d", total_contactors))
  
  # Calculate required volume per contactor
  # Volume (gal) = Flow (MGD) × EBCT (min) × 1,000,000 / (1440 min/day)
  total_volume_gal <- design_flow * ebct * 1000000 / 1440
  volume_per_contactor_gal <- total_volume_gal / num_trains
  volume_per_contactor_cf <- volume_per_contactor_gal / 7.48052  # Convert to ft³
  
  # Calculate vessel dimensions if not provided
  if (geometry %in% c("upright", "horizontal")) {
    if (is.null(diameter)) {
      # Auto-calculate diameter based on volume
      diameter <- sqrt(volume_per_contactor_cf / (pi * bed_depth))
    }
    actual_volume_cf <- calculate_vessel_volume(
      geometry = geometry,
      diameter = diameter,
      height_length = height_length
    )
  } else {
    # Basin — compute directly (length x width x total depth)
    # calculate_vessel_volume may not support "basin" in all sourced versions
    actual_volume_cf <- if (!is.null(basin_length) && !is.null(basin_width) && !is.null(basin_depth) &&
                             !is.na(basin_length) && !is.na(basin_width) && !is.na(basin_depth)) {
      as.numeric(basin_length) * as.numeric(basin_width) * as.numeric(basin_depth)
    } else {
      (design_flow * 1e6 / 1440) * ebct / 7.48
    }
  }
    # === TEMP DEBUG: geometry inputs at gac_volume_per_contactor ===
  message(sprintf("[contactor debug] geometry    = %s", geometry))
  message(sprintf("[contactor debug] diameter    = %.10f ft  (= %.6f inches)", diameter, diameter * 12))
  message(sprintf("[contactor debug] bed_depth   = %.10f ft", bed_depth))
  message(sprintf("[contactor debug] gac_vol_exp = %.10f cf", pi * (diameter/2)^2 * bed_depth))
  # === END TEMP DEBUG ===

  # Calculate GAC volume (based on bed depth / basin operating depth)
  # Workbook: media_volume = comm_SA*bed_depth (pressure) or basin_w*basin_l*basin_op_depth (basin)
  # basin_op_depth is the GAC media depth inside the basin = basin_depth parameter
  gac_volume_per_contactor <- if (geometry %in% c("upright", "horizontal")) {
    pi * (diameter/2)^2 * bed_depth
  } else {
    # Use basin_depth (operating depth) per workbook media_volume formula
    op_depth <- if (!is.null(basin_depth) && !is.na(basin_depth)) basin_depth else bed_depth
    basin_length * basin_width * op_depth
  }
  
  total_gac_volume <- gac_volume_per_contactor * total_contactors
  
  # Calculate costs using Excel methodology
  component_level <- 1  # Default to Low cost 
  
  # Convert component_level number to name
  component_level_name <- switch(
    as.character(component_level),
    "1" = "Low",
    "2" = "Mid",
    "3" = "High",
    "Low"  # Default to Low
  )
  
  # Select vessel material using Excel's priority system
  # Load priority selection functions
  if (file.exists("wbs_models/gac_app/priority_selection.R")) {
    source("wbs_models/gac_app/priority_selection.R")
  } else if (file.exists("priority_selection.R")) {
    source("priority_selection.R")
  }
  
  vessel_material <- tryCatch({
    select_vessel_material(design_flow, component_level_name)
  }, error = function(e) {
    # Fallback to simplified approach if priority system fails
    warning("Priority selection failed, using fallback: ", e$message)
    if (design_flow < 0.5) "FG" else if (design_flow < 2.0) "CS" else "SS"
  })
  
  message(sprintf("=== CONTACTOR COSTING DEBUG ==="))
  message(sprintf("Design flow: %.3f MGD", design_flow))
  message(sprintf("Component level: %s", component_level_name))
  message(sprintf("Num trains: %d, Redundancy: %.0f%%", num_trains, redundancy * 100))
  message(sprintf("Total contactors: %d (trains × (1 + redundancy))", total_contactors))
  message(sprintf("Selected vessel material: %s (priority-based)", vessel_material))
  message(sprintf("Actual volume per vessel: %.2f cf (%.2f gal)", 
                  actual_volume_cf, actual_volume_cf * 7.48052))
  
  # actual_volume_gal needed in both branches and the return list
  actual_volume_gal <- actual_volume_cf * 7.48052

  # Calculate costs based on design type
  if (!is_basin) {
    # ===== PRESSURE VESSEL DESIGN =====
    # Use WBS 1.1.1 - Pressure Vessels
    
    message(sprintf("  Calling PRESSURE VESSEL cost function:"))
    message(sprintf("    Volume: %.2f cf = %.2f gal", actual_volume_cf, actual_volume_gal))
    message(sprintf("    Material: %s", vessel_material))
    message(sprintf("    Quantity: %d", total_contactors))
    
    # Use Excel cost equations for pressure vessels
    vessel_cost_result <- tryCatch({
      calculate_pressure_vessel_cost(
        volume_gal = actual_volume_gal,
        material = vessel_material,
        quantity = total_contactors,
        component_level = component_level
      )
    }, error = function(e) {
      # Fallback to simple cost if Excel equations fail
      warning("Using fallback cost calculation: ", e$message)
      list(
        unit_cost = actual_volume_cf * 50 * 1.2,
        total_cost = actual_volume_cf * 50 * 1.2 * total_contactors,
        message = "Fallback calculation"
      )
    })
    
    unit_cost <- vessel_cost_result$unit_cost
    total_cost <- vessel_cost_result$total_cost
    
  } else {
    # ===== BASIN (GRAVITY) DESIGN =====
    # Use WBS 1.2 - GAC Contact Basins (Concrete + Internals + Excavation + etc.)
    
    basin_area_sf <- basin_length * basin_width
    total_basin_area <- basin_area_sf * total_contactors
    total_basin_volume_cf <- actual_volume_cf * total_contactors
    
    message(sprintf("  Calling BASIN cost functions:"))
    message(sprintf("    Basin area: %.2f sf per basin", basin_area_sf))
    message(sprintf("    Total area: %.2f sf (%d basins)", total_basin_area, total_contactors))
    message(sprintf("    Basin volume: %.2f cf per basin", actual_volume_cf))
    
    # 1.2.1 - Concrete basin structure
    # Simplified: Use volume-based cost (would need full equation from Excel)
    concrete_cost <- tryCatch({
      # Placeholder - would call calculate_concrete_basin_cost()
      total_basin_volume_cf * 45  # $/cf for concrete basin
    }, error = function(e) {
      total_basin_volume_cf * 45
    })
    
    # 1.2.2 - Internals (Underdrain/Backwash System)
    # Based on basin surface area
    internals_cost <- tryCatch({
      # Typical cost: $20-30 per square foot
      total_basin_area * 25
    }, error = function(e) {
      total_basin_area * 25
    })
    
    # 1.2.3 - Aluminum Railing
    # Based on perimeter
    basin_perimeter <- 2 * (basin_length + basin_width) * total_contactors
    railing_cost <- basin_perimeter * 50  # $/LF
    
    # 1.2.4 - Aluminum Stairs
    # Fixed cost per basin
    stairs_cost <- total_contactors * 2500  # per basin
    
    # 1.2.5 - Excavation
    # Based on volume excavated (basin volume + extra)
    excavation_volume_cy <- (total_basin_volume_cf * 1.3) / 27  # cy, with 30% extra
    excavation_cost <- excavation_volume_cy * 30  # $/cy
    
    # 1.2.6 - Backfill and Compaction
    backfill_cost <- excavation_volume_cy * 18  # $/cy
    
    # Total basin cost
    total_cost <- concrete_cost + internals_cost + railing_cost + 
                  stairs_cost + excavation_cost + backfill_cost
    unit_cost <- total_cost / total_contactors
    
    message(sprintf("    Concrete: $%.0f", concrete_cost))
    message(sprintf("    Internals: $%.0f", internals_cost))
    message(sprintf("    Railing: $%.0f", railing_cost))
    message(sprintf("    Stairs: $%.0f", stairs_cost))
    message(sprintf("    Excavation: $%.0f", excavation_cost))
    message(sprintf("    Backfill: $%.0f", backfill_cost))
  }
  
  message(sprintf("Unit cost: $%.0f", unit_cost))
  message(sprintf("Total cost: $%.0f", total_cost))
  message(sprintf("================================"))
  
  list(
    component_level_name = component_level_name,
    total_contactors = total_contactors,
    volume_per_contactor_gal = actual_volume_gal,
    volume_per_contactor_cf = actual_volume_cf,
    gac_volume_per_contactor = gac_volume_per_contactor,
    total_gac_volume = total_gac_volume,
    diameter = diameter,
    height_length = height_length,
    bed_depth = bed_depth,
    # Basin dimensions — used by display layer for gravity designs
    basin_length = basin_length,
    basin_width  = basin_width,
    basin_depth  = basin_depth,
    unit_cost = unit_cost,
    total_cost = total_cost
  )
}

# Calculate GAC requirements and costs -----
calculate_gac_requirements <- function(
  total_volume, influent_conc, effluent_target, average_flow, regen_type,
  # Bed-life / breakthrough parameters (workbook: Pump-Pipe-Structure sheet)
  freund_type     = 4,                   # 1=direct months, 2=Freundlich, 3=BDST, 4=BV/EBCT
  freund_1        = 66600,               # BV (gal/cf) when freund_type=4; months when =1
  freund_2        = NULL,                # Freundlich 1/n exponent (types 2,3 only)
  design_type     = 1,                   # 1=pressure vessels, 2=basins
  media_volume    = NULL,                # cf per vessel/basin
  num_treat_lines = NULL,                # number of treatment lines (vessels, type=1)
  BV_definition   = "EBCT per vessel",   # "EBCT per vessel" or "EBCT per train"
  Num_tanks       = 1,                   # tanks in series per train
  op_num_basins   = NULL,                # operating basins (type=2)
  GAC_each        = NULL,                # cf of GAC per vessel/basin (= media_volume * num per line)
  GAC_density     = 30,                  # lb/cf
  # Freundlich/BDST inputs (types 2,3 only)
  M = NULL, C_0 = NULL, C_b = NULL, QL = NULL,
  N_0_e = NULL, v = NULL, D = NULL,
  bed_depth      = NULL,    # ft — pressure vessel bed depth 
  basin_op_depth = NULL,    # ft — gravity basin GAC depth 
  # Regeneration parameters
  makeup_rate     = 0.10,                # fraction of annual throughput as new GAC (on-site regen)
  makeup_rate_off = 0.30,                # fraction as new GAC (off-site regen)
  regen_runtime   = 0.85,               # furnace utilisation fraction
  regen_redund    = 1                    # regeneration redundancy factor
) {
  # ── Coerce inputs ────────────────────────────────────────────────────────────
  total_volume    <- suppressWarnings(as.numeric(total_volume))
  average_flow    <- suppressWarnings(as.numeric(average_flow))   # MGD
  freund_type     <- suppressWarnings(as.numeric(freund_type))
  freund_1        <- suppressWarnings(as.numeric(freund_1))
  design_type     <- suppressWarnings(as.numeric(design_type))
  GAC_density     <- suppressWarnings(as.numeric(GAC_density))
  makeup_rate     <- suppressWarnings(as.numeric(makeup_rate))
  makeup_rate_off <- suppressWarnings(as.numeric(makeup_rate_off))
  # Replace any NAs from bad coercions with workbook defaults
  if (is.na(freund_type))     freund_type     <- 4
  if (is.na(freund_1))        freund_1        <- 66600
  if (is.na(design_type))     design_type     <- 1
  if (is.na(GAC_density))     GAC_density     <- 30
  if (is.na(makeup_rate))     makeup_rate     <- 0.10
  if (is.na(makeup_rate_off)) makeup_rate_off <- 0.30

  # Normalise regen_type string
  if (is.null(regen_type) || length(regen_type) == 0 || is.na(regen_type) ||
      nchar(trimws(as.character(regen_type))) == 0) {
    regen_type <- "regeneration off-site (non-hazardous)"
  } else {
    regen_type <- trimws(as.character(regen_type)[1])
  }
  # Map to workbook integer code (for CHOOSE logic)
  regen_code <- if (grepl("on-site",            regen_type, ignore.case=TRUE)) 1L else
                if (grepl("off-site.*non-haz",   regen_type, ignore.case=TRUE)) 2L else
                if (grepl("throwaway.*non-haz",  regen_type, ignore.case=TRUE)) 3L else
                if (grepl("off-site.*haz",        regen_type, ignore.case=TRUE)) 4L else
                if (grepl("throwaway.*haz",       regen_type, ignore.case=TRUE)) 5L else
                if (grepl("throwaway.*radio",     regen_type, ignore.case=TRUE)) 6L else 7L

  # ── GAC mass ─────────────────────────────────────────────────────────────────
  total_gac_mass_lb <- tryCatch(
    as.numeric(calculate_gac_mass(total_volume)),
    error = function(e) as.numeric(total_volume) * 30  # fallback: volume_cf * 30 lb/cf
  )
  if (is.null(total_gac_mass_lb) || is.na(total_gac_mass_lb))
    total_gac_mass_lb <- as.numeric(total_volume) * 30

  # ── 9.1 Initial fill cost ─────────────────────────────────────────────────────
  # Workbook OUTPUT J248: IF(curve=1, gac_eq, VLOOKUP(GAC, gac_cost_cl, 3))
  # gac_eq (CE row 180): J*exp(K*D), D = MIN(GAC_lb, 40000)
  # J = 2.101628083, K = -5.637e-06
  gac_eq_D          <- min(total_gac_mass_lb, 40000)
  gac_uc            <- if (gac_eq_D == 0) 0 else 2.101628083 * exp(-5.637e-06 * gac_eq_D)
  initial_fill_cost <- total_gac_mass_lb * gac_uc

  # ── Bed life (months) via calculate_gac_bed_life ─────────────────────────────
  # Workbook: bed_life = CHOOSE(freund_type, freund_1,
  #   Freundlich throughput, BDST, BV/EBCT method)
  # average_flow_rate in workbook = average_flow_MGD * 1e6 / (24*60)  [GPM]
  average_flow_rate_gpm <- average_flow * 1e6 / (24 * 60)


  # === TEMP DEBUG: bed_life option 4 inputs — remove after diagnosis ===
  message(sprintf("[bed_life] freund_type        = %s", freund_type))
  message(sprintf("[bed_life] freund_1           = %s", freund_1))
  message(sprintf("[bed_life] media_volume       = %s", media_volume))
  message(sprintf("[bed_life] num_treat_lines    = %s", num_treat_lines))
  message(sprintf("[bed_life] Num_tanks          = %s", Num_tanks))
  message(sprintf("[bed_life] BV_definition      = %s", BV_definition))
  message(sprintf("[bed_life] average_flow_rate_gpm = %s", average_flow_rate_gpm))
  message(sprintf("[bed_life] design_type        = %s", design_type))
  message(sprintf("[bed_life] op_num_basins      = %s", op_num_basins))
  if (!is.null(media_volume) && !is.null(num_treat_lines)) {
    .mv   <- suppressWarnings(as.numeric(media_volume))
    .ntl  <- suppressWarnings(as.numeric(num_treat_lines))
    .nt   <- suppressWarnings(as.numeric(Num_tanks %||% 1))
    .mult <- if (!is.null(BV_definition) && BV_definition == "EBCT per vessel") 1 else .nt
    .exp  <- freund_1 * .mv * .ntl * .mult * 7.48 / average_flow_rate_gpm / 60 / 24 / 30
    message(sprintf("[bed_life] EXPECTED result   = %.6f months", .exp))
  }
  # === END TEMP DEBUG ===

  # ensure no silent NA is returned for other feund type selections (resulting in 4 as fallback)
  if (is.character(freund_type) && !suppressWarnings(!is.na(as.numeric(freund_type)))) {
  freund_type <- dplyr::case_when(
    grepl("months",     freund_type, ignore.case = TRUE) ~ 1L,
    grepl("Freundlich", freund_type, ignore.case = TRUE) ~ 2L,
    grepl("BDST",       freund_type, ignore.case = TRUE) ~ 3L,
    TRUE ~ 4L
  )
} else {
  freund_type <- as.integer(freund_type)
}
  
  # ── Derive Freundlich / BDST intermediate variables (options 2 and 3 only) ────
  # Workbook computes M, QL, N_0_e, D, v as named ranges from design parameters.
  # R must derive them here since calculate_gac_system does not pre-compute them.
  if (freund_type %in% c(2L, 3L)) {
    # C_0 / C_b: use influent_conc and effluent_target already in scope
    if (is.null(C_0) || is.na(suppressWarnings(as.numeric(C_0))))
      C_0 <- suppressWarnings(as.numeric(influent_conc))
    if (is.null(C_b) || is.na(suppressWarnings(as.numeric(C_b))))
      C_b <- suppressWarnings(as.numeric(effluent_target))

    # QL: workbook = average_flow_rate * 3.785 (gpm → L/min)
    if (is.null(QL) || is.na(suppressWarnings(as.numeric(QL))))
      QL <- average_flow_rate_gpm * 3.785

    # M: workbook = IF(design_type=1, GAC_each * num_treat_lines, GAC) * 453.59  (lbs → grams)
    if (is.null(M) || is.na(suppressWarnings(as.numeric(M)))) {
      gac_lbs <- if (design_type == 2) {
        suppressWarnings(as.numeric(media_volume)) *
          suppressWarnings(as.numeric(op_num_basins)) *
          GAC_density
      } else {
        suppressWarnings(as.numeric(media_volume)) *
          suppressWarnings(as.numeric(num_treat_lines)) *
          GAC_density
      }
      M <- gac_lbs * 453.59
    }

    # D: workbook = IF(design_type=1, bed_depth, basin_op_depth) * 0.3048  (ft → m)
    if (is.null(D) || is.na(suppressWarnings(as.numeric(D)))) {
      depth_ft <- if (design_type == 2)
        suppressWarnings(as.numeric(basin_op_depth))
      else
        suppressWarnings(as.numeric(bed_depth))
      D <- depth_ft * 0.3048
    }

    # v: workbook = QL*0.001 / (contact_area_m2 * num_lines)
    # contact_area = media_volume / bed_depth  (workbook: media_volume = comm_SA * bed_depth)
    if (is.null(v) || is.na(suppressWarnings(as.numeric(v)))) {
      depth_ft_v <- if (design_type == 2)
        suppressWarnings(as.numeric(basin_op_depth))
      else
        suppressWarnings(as.numeric(bed_depth))
      contact_area_ft2 <- suppressWarnings(as.numeric(media_volume)) / depth_ft_v
      contact_area_m2  <- contact_area_ft2 * 0.0929
      n_lines <- if (design_type == 2)
        suppressWarnings(as.numeric(op_num_basins))
      else
        suppressWarnings(as.numeric(num_treat_lines))
      v <- (QL * 0.001) / (contact_area_m2 * n_lines)
    }

    # N_0_e: workbook = freund_1 * GAC_density * 453.59 / 0.02832
    if (is.null(N_0_e) || is.na(suppressWarnings(as.numeric(N_0_e))))
      N_0_e <- freund_1 * GAC_density * 453.59 / 0.02832
  }

  bed_life_months <- calculate_gac_bed_life(
    freund_type     = freund_type,
    freund_1        = freund_1,
    freund_2        = freund_2,
    M               = M,
    C_0             = C_0,
    C_b             = C_b,
    QL              = QL,
    N_0_e           = N_0_e,
    v               = v,
    D               = D,
    design_type     = design_type,
    media_volume    = media_volume,
    num_treat_lines = num_treat_lines,
    BV_definition   = BV_definition,
    Num_tanks       = Num_tanks,
    op_num_basins   = op_num_basins,
    average_flow_rate = average_flow_rate_gpm
  )

  bed_life_months <- as.numeric(bed_life_months)
  if (is.na(bed_life_months) || bed_life_months <= 0) bed_life_months <- 12  # fallback

  # ── Annual throughput (cf/yr) ─────────────────────────────────────────────────
  # Workbook: regen_day = GAC_each * num_treat_lines / (bed_life/12) / 365
  #           GAC_yr    = regen_day * 365  →  simplifies to GAC_each * num_treat_lines / (bed_life/12)
  # media_volume = cf per vessel; GAC_each = media_volume * GAC_density in lb,
  # but for volume calculations GAC_each in workbook is lb, and regen_day is in cf.
  # Workbook actually uses: regen_day [cf/day] = GAC_each [lb] * treat_lines / (bed_life/12) / 365 / GAC_density
  # Verify: GAC_each = media_volume * GAC_density (lb);  regen_day in cf = GAC_each / GAC_density * treat_lines ...
  # Actually checking the formula: regen_day = GAC_each * num_treat_lines / (bed_life/12) / 365
  # Units: lb * dimensionless / months*(12/1) / days → consistent if GAC_yr kept in lb then /density for cf
  # Let's be explicit: GAC_yr_lb = total_gac_mass_lb / (bed_life_months / 12)
  GAC_yr_lb <- total_gac_mass_lb / (bed_life_months / 12)

  # ── Makeup and regen quantities (cf/yr for cost equations) ───────────────────
  # Workbook uses cf; convert: GAC_yr_cf = GAC_yr_lb / GAC_density
  GAC_yr_cf <- GAC_yr_lb / GAC_density

  # Workbook CHOOSE(regen_type):
  #   1 (on-site):        GAC_makeup_cf = makeup_rate * GAC_yr_cf
  #   2 (off-site non-haz): GAC_makeup_cf = makeup_rate_off * GAC_yr_cf
  #   3,5,6,7 (throwaway):  GAC_makeup_cf = GAC_yr_cf  (full replacement)
  #   4 (off-site haz):   GAC_makeup_cf = makeup_rate_off * GAC_yr_cf
  GAC_makeup_cf <- switch(as.character(regen_code),
    "1" = makeup_rate     * GAC_yr_cf,
    "2" = makeup_rate_off * GAC_yr_cf,
    "3" = GAC_yr_cf,
    "4" = makeup_rate_off * GAC_yr_cf,
    GAC_yr_cf   # 5,6,7 = throwaway
  )

  # regen_yr_cf: volume sent for regeneration (types 2,4 only)
  regen_yr_cf <- switch(as.character(regen_code),
    "1" = (1 - makeup_rate)     * GAC_yr_cf,
    "2" = (1 - makeup_rate_off) * GAC_yr_cf,
    "4" = (1 - makeup_rate_off) * GAC_yr_cf,
    0
  )

  # ── Annual replacement cost ───────────────────────────────────────────────────
  # Workbook OUTPUT E376: replace_gac_eq  (CE row 196)
  #   J*exp(K*D),  D=MIN(GAC_makeup_cf, 40000),  J=2.101628083, K=-5.637e-06  ($/cf)
  # Workbook OUTPUT E377: off_regen_eq    (CE row 197)
  #   J*exp(K*D),  D=MIN(regen_yr_cf, 40000),    J=1.991910116, K=-1.3224e-05 ($/cf)
  # annual cost = GAC_makeup_cf * replace_gac_uc + regen_yr_cf * off_regen_uc

  replace_gac_D  <- min(GAC_makeup_cf, 40000)
  replace_gac_uc <- if (replace_gac_D == 0) 0 else
    2.101628083 * exp(-5.637e-06 * replace_gac_D)

  off_regen_D    <- min(regen_yr_cf, 40000)
  off_regen_uc   <- if (off_regen_D == 0 || regen_code %in% c(1L, 3L, 5L, 6L, 7L)) 0 else
    1.991910116 * exp(-1.3224e-05 * off_regen_D)

  annual_replacement_cost <- GAC_makeup_cf * replace_gac_uc + regen_yr_cf * off_regen_uc

  # Convert cf → lbs for O&M display (workbook OUTPUT col C uses lbs)
  GAC_density_lb_cf <- GAC_density  # lbs/cf passed in
  GAC_makeup_lbs    <- GAC_makeup_cf * GAC_density_lb_cf
  regen_yr_lbs      <- regen_yr_cf  * GAC_density_lb_cf

  list(
    total_gac_mass_lb       = total_gac_mass_lb,
    gac_unit_cost           = gac_uc,
    initial_fill_cost       = initial_fill_cost,
    bed_life_months         = bed_life_months,
    GAC_yr_cf               = GAC_yr_cf,
    GAC_makeup_cf           = GAC_makeup_cf,
    regen_yr_cf             = regen_yr_cf,
    GAC_makeup_lbs          = GAC_makeup_lbs,
    regen_yr_lbs            = regen_yr_lbs,
    replace_gac_uc          = replace_gac_uc,   # $/lb — for O&M display
    off_regen_uc            = off_regen_uc,     # $/lb — for O&M display
    regen_capacity          = 0,  # on-site regeneration capacity (furnace) — not computed here
    annual_replacement_cost = annual_replacement_cost
  )
}

# Calculate number of treatment trains ----
calculate_num_trains <- function(design_flow, ebct, bed_depth, vessel_diameter, 
                                 vessel_height_length, tank_geometry = "upright",
                                 design_type = 1, num_contactors_in_series = 1,
                                 vessel_thickness = 0) {
  
  # Convert inputs
  design_flow <- as.numeric(design_flow)
  ebct <- as.numeric(ebct)
  bed_depth <- as.numeric(bed_depth)
  vessel_diameter <- as.numeric(vessel_diameter)
  
  # Basin designs return NA
  if (design_type == 2 || tolower(tank_geometry) == "basin") {
    return(1)  # Default for basins
  }
  
  # Calculate surface area
  inner_diameter <- vessel_diameter - (2 * vessel_thickness)
  max_sa <- if (tolower(tank_geometry) == "upright") {
    pi * inner_diameter^2 / 4
  } else {
    inner_diameter * vessel_height_length
  }
  
  # Calculate EBCT per vessel
  EBCT_tank <- ebct / num_contactors_in_series
  
  # Calculate flow capacity
  flow_per_vessel <- (max_sa * bed_depth * 7.48) / EBCT_tank
  
  # Calculate trains needed
  design_flow_gpm <- design_flow * 1e6 / (24 * 60)
  num_trains <- ceiling(design_flow_gpm / flow_per_vessel)
  
  return(max(1, as.integer(num_trains)))
}



# Calculate GAC bed life based on Freundlich isotherm ----
#' @param freund_type Type of calculation (1-4)
#' @param freund_1 Freundlich K coefficient
#' @param freund_2 Freundlich 1/n coefficient
#' @param M GAC mass (lbs)
#' @param C_0 Influent concentration (ug/L)
#' @param C_b Effluent target concentration (ug/L)
#' @param QL Loading rate
#' @param N_0_e N0/e parameter
#' @param v Velocity parameter
#' @param D Depth parameter
#' @param design_type Design type (1 = pressure, 2 = basin)
#' @param media_volume Media volume per vessel/basin (cf)
#' @param num_treat_lines Number of treatment lines
#' @param BV_definition Bed volume definition
#' @param Num_tanks Number of tanks
#' @param op_num_basins Operating number of basins
#' @param average_flow_rate Average flow rate (MGD)
#' @return Bed life in months
calculate_gac_bed_life <- function(freund_type, freund_1, freund_2, 
                                   M = NULL, C_0, C_b, QL = NULL,
                                   N_0_e = NULL, v = NULL, D = NULL,
                                   design_type = 1, media_volume = NULL,
                                   num_treat_lines = NULL, BV_definition = NULL,
                                   Num_tanks = NULL, op_num_basins = NULL,
                                   average_flow_rate = NULL) {
  
  # Ensure numeric inputs — guard NULLs for optional params
  freund_type <- as.numeric(freund_type)
  freund_1    <- as.numeric(freund_1)
  freund_2    <- if (!is.null(freund_2) && length(freund_2) > 0) as.numeric(freund_2) else NA_real_
  C_0         <- if (!is.null(C_0)      && length(C_0)      > 0) as.numeric(C_0)      else NA_real_
  C_b         <- if (!is.null(C_b)      && length(C_b)      > 0) as.numeric(C_b)      else NA_real_
  
  # CHOOSE logic - equivalent to Excel's CHOOSE(freund_type, option1, option2, ...)
  bed_life_months <- switch(
    as.character(freund_type),
    
    # Option 1: freund_1 (direct value)
    "1" = {
      freund_1
    },
    
    # Option 2: Freundlich throughput equation
    "2" = {
      M <- as.numeric(M)
      QL <- as.numeric(QL)
      
      numerator <- M * (freund_1 / 1000) * (C_0 * 1000)^freund_2
      denominator <- QL * (C_0 - C_b)
      
      # Convert to months (30 days * 24 hours * 60 minutes)
      (numerator / denominator) / (30 * 24 * 60)
    },
    
    # Option 3: Bed depth service time (BDST) equation
    "3" = {
      N_0_e <- as.numeric(N_0_e)
      v <- as.numeric(v)
      D <- as.numeric(D)
      
      term1 <- N_0_e / (1000 * v * C_0) * D
      term2 <- log10(C_0 / C_b - 1) / (freund_2 * C_0)
      
      # Convert to months
      (term1 - term2) / (30 * 24 * 60)
    },
    
    # Option 4: Empty bed contact time (EBCT) based calculation
    "4" = {
      design_type       <- as.numeric(design_type)
      media_volume      <- suppressWarnings(as.numeric(media_volume))
      average_flow_rate <- suppressWarnings(as.numeric(average_flow_rate))
      
      # Calculate total volume based on design type
      if (!is.na(design_type) && design_type == 1) {
        # Pressure vessels
        num_treat_lines <- suppressWarnings(as.numeric(num_treat_lines))
        Num_tanks       <- suppressWarnings(as.numeric(Num_tanks))
        
        # Check BV_definition
        multiplier <- if (BV_definition == "EBCT per vessel") {
          1
        } else {
          Num_tanks
        }
        
        total_volume_cf <- media_volume * num_treat_lines * multiplier
        
      } else {
        # Basins (design_type == 2)
        op_num_basins <- suppressWarnings(as.numeric(op_num_basins))
        if (is.null(op_num_basins) || is.na(op_num_basins) || op_num_basins <= 0) {
          op_num_basins <- max(1, suppressWarnings(as.numeric(num_treat_lines)), na.rm = TRUE)
        }
        total_volume_cf <- media_volume * op_num_basins
      }
      
      # Convert to gallons and calculate EBCT-based bed life
      total_volume_gal <- total_volume_cf * 7.48
      flow_gpm <- average_flow_rate  # already in GPM (converted before call)
      
      # Bed life in months
      freund_1 * total_volume_gal / flow_gpm / 60 / 24 / 30
    },
    
    # Default (shouldn't happen)
    {
      warning("Invalid freund_type: ", freund_type)
      NA
    }
  )
  
  return(bed_life_months)
}

#' Calculate pump requirements and costs
calculate_pumps <- function(design_flow, num_trains, service_pumps, backwash_pumps,
                            residuals_pumps, tank_geometry, no_backwash,
                            # Workbook parameters with faithful defaults
                            ss_cat2            = 1,       # 1 = small system (default)
                            design_type        = 1,       # 1 = pressure vessels (default)
                            water_flush_gpm    = 0,       # backwash flow (gpm), from B&R C13
                            res_flow_gpm       = 0,       # residuals flow (gpm), from RM
                            res_holding        = "none",  # "none" = no residuals holding
                            NRD_pumps          = 0,       # redundant booster pumps, CDA C52
                            NRD_back_pumps     = 1,       # redundant backwash pumps, CDA C53
                            NRD_res_pump       = 0,       # redundant residuals pumps
                            pump_safety_factor = 0.25,    # CDA C51
                            max_pump_size      = 10000) { # gpm, PPS C8

  # Workbook cost equation (Cost Equations rows 130-132, all three pump types identical):
  # pump_cost(Q) = -0.00067003*Q^2 + 14.80901498*Q + 4093.494684836  (Q in gpm)
  # Returns 0 when Q = 0 (workbook IF(D=0,0,...))
  pump_cost_eq <- function(Q) {
    Q <- safe_as_numeric(Q, 0)
    if (Q <= 0) return(0)
    -0.00067003 * Q^2 + 14.80901498 * Q + 4093.494684836
  }

  design_flow        <- safe_as_numeric(design_flow,        0)
  num_trains         <- safe_as_numeric(num_trains,         1)
  ss_cat2            <- safe_as_numeric(ss_cat2,            1)
  design_type        <- safe_as_numeric(design_type,        1)
  water_flush_gpm    <- safe_as_numeric(water_flush_gpm,    0)
  res_flow_gpm       <- safe_as_numeric(res_flow_gpm,       0)
  NRD_pumps          <- safe_as_numeric(NRD_pumps,          0)
  NRD_back_pumps     <- safe_as_numeric(NRD_back_pumps,     1)
  NRD_res_pump       <- safe_as_numeric(NRD_res_pump,       0)
  pump_safety_factor <- safe_as_numeric(pump_safety_factor, 0.25)
  max_pump_size      <- safe_as_numeric(max_pump_size,      10000)

  # B&R C9: no_backwash = IF(no_backwash_I="existing pumps", 1,
  #                          IF(no_backwash_I="new pumps",    "",
  #                          IF(ss_cat2=1,                   1, "")))
  # Default: ss_cat2=1 → no_backwash=1
  no_backwash_val <- safe_as_numeric(no_backwash, 0)
  if (no_backwash_val == 0 && ss_cat2 == 1) no_backwash_val <- 1

  design_flow_gpm <- design_flow * 1e6 / (24 * 60)  # MGD -> gpm

  # ── 5.1 Booster pumps (PPS C7, C9, C10) ──────────────────────────────────
  # pump_rating = 0 when ss_cat2=1 AND design_type=1 (small skid, no booster)
  # total_booster_pumps = IF(booster_pumps=0, 0, booster_pumps + NRD_pumps)
  if (ss_cat2 == 1 && design_type == 1) {
    pump_rating         <- 0
    total_booster_pumps <- 0L
  } else {
    pump_rating         <- min(
      max(design_flow_gpm, design_flow_gpm / 2) * (1 + pump_safety_factor),
      max_pump_size
    )
    n_booster           <- if (pump_rating == 0) 0L else
      ceiling(design_flow_gpm * (1 + pump_safety_factor) / pump_rating)
    total_booster_pumps <- if (n_booster == 0) 0L else n_booster + NRD_pumps
  }
  booster_uc   <- pump_cost_eq(pump_rating)
  booster_cost <- total_booster_pumps * booster_uc

  # ── 5.2 Backwash pumps (PPS C12, C13, C14) ────────────────────────────────
  # backwash_pumps = IF(no_backwash=1, 0, ROUNDUP(back_pump_flow_total/max_pump_size,0) + NRD_back_pumps)
  # back_pump_rating = IF(no_backwash=1, 0, back_pump_flow_total / (backwash_pumps - NRD_back_pumps))
  back_pump_flow_total <- water_flush_gpm * (1 + pump_safety_factor)

  if (no_backwash_val == 1) {
    n_back_pumps     <- 0L
    back_pump_rating <- 0
  } else {
    n_back_pumps     <- ceiling(back_pump_flow_total / max_pump_size) + NRD_back_pumps
    operating_back   <- n_back_pumps - NRD_back_pumps
    back_pump_rating <- if (operating_back <= 0) 0 else
      back_pump_flow_total / operating_back
  }
  back_uc   <- pump_cost_eq(back_pump_rating)
  back_cost <- n_back_pumps * back_uc

  # ── 5.3 Residuals pumps (RM C105, C106) ───────────────────────────────────
  # res_pumps = IF(res_holding<>"none", ROUNDUP(res_flow*(1+sf)/max_size,0) + NRD_res_pump, 0)
  # res_pump_rating = IF(res_holding<>"none", res_flow*(1+sf)/(res_pumps-NRD_res_pump), 0)
  if (res_holding == "none") {
    n_res_pumps     <- 0L
    res_pump_rating <- 0
  } else {
    res_flow_total  <- res_flow_gpm * (1 + pump_safety_factor)
    n_res_pumps     <- ceiling(res_flow_total / max_pump_size) + NRD_res_pump
    operating_res   <- n_res_pumps - NRD_res_pump
    res_pump_rating <- if (operating_res <= 0) 0 else res_flow_total / operating_res
  }
  res_uc   <- pump_cost_eq(res_pump_rating)
  res_cost <- n_res_pumps * res_uc

  total_cost <- booster_cost + back_cost + res_cost

  message(sprintf("5.1 Booster:   %d x %.0f gpm = $%.0f ea -> $%.0f",
                  total_booster_pumps, pump_rating, booster_uc, booster_cost))
  message(sprintf("5.2 Backwash:  %d x %.0f gpm = $%.0f ea -> $%.0f",
                  n_back_pumps, back_pump_rating, back_uc, back_cost))
  message(sprintf("5.3 Residuals: %d x %.0f gpm = $%.0f ea -> $%.0f",
                  n_res_pumps, res_pump_rating, res_uc, res_cost))

  list(
    service_pumps     = total_booster_pumps,
    backwash_pumps    = n_back_pumps,
    residuals_pumps   = n_res_pumps,
    booster_pumps     = total_booster_pumps,
    pump_rating       = pump_rating,
    back_pump_rating  = back_pump_rating,
    res_pump_rating   = res_pump_rating,
    service_cost      = booster_cost,
    backwash_cost     = back_cost,
    residuals_cost    = res_cost,
    total_cost        = total_cost
  )
}

#' Calculate tank requirements
calculate_tanks <- function(design_flow, no_backwash, no_backwash_tank,
                           backwash_interval, residuals_disposal, residuals_tank,
                           num_contactors = 1, component_level = 1) {
  
  # Ensure numeric parameters
  design_flow <- safe_as_numeric(design_flow, 0)
  num_contactors <- safe_as_numeric(num_contactors, 1)
  backwash_interval <- safe_as_numeric(backwash_interval, 168)  # hours
  
  # Handle logical/boolean parameters
  no_backwash <- safe_as_logical(no_backwash, FALSE)
  no_backwash_tank <- safe_as_logical(no_backwash_tank, FALSE)
  residuals_tank <- safe_as_char(residuals_tank, "no holding tank")
  
  # IMPORTANT: For small systems (< 0.5 MGD), Excel defaults to no backwash
  # This matches Excel's logic: IF(ss_cat2=1, 1, "") where ss_cat2=1 means small
  if (design_flow < 0.5 && !isTRUE(no_backwash)) {
    # Auto-set no_backwash for small systems unless explicitly enabled
    # Check if backwash was explicitly requested
    no_backwash <- TRUE
    no_backwash_tank <- TRUE
    message("Small system (<0.5 MGD): Auto-disabling backwash tanks per Excel logic")
  }
  
  # For medium systems (0.5-10 MGD), backwash tank still not needed by default
  # Excel: IF(ss_cat2=2, "", 1) means medium systems also default to no tank
  if (design_flow >= 0.5 && design_flow <= 10 && !isTRUE(no_backwash_tank)) {
    no_backwash_tank <- TRUE
    message("Medium system (0.5-10 MGD): Auto-disabling backwash storage per Excel logic")
  }
  
  # ===== BACKWASH TANK SIZING =====
  
  backwash_tank_cost <- 0
  backwash_tank_volume <- 0
  num_backwash_tanks <- 0
  
  if (!no_backwash && !no_backwash_tank) {
    
    # Calculate backwash volume based on GAC BED VOLUME, not daily flow
    # Backwash expands the bed by ~30% and needs to be stored
    
    # Get GAC bed volume from contactors (passed as parameter)
    # If not available, estimate from design flow
    gac_bed_volume_cf <- if (!is.null(num_contactors) && num_contactors > 0) {
      # Estimate bed volume per contactor (simplified)
      # For 0.03 MGD with 2 contactors: ~10 cf each
      bed_volume_per_contactor <- (design_flow * 1000000 / 1440) * 7.5 / 7.48052 / num_contactors
      bed_volume_per_contactor * num_contactors
    } else {
      # Fallback estimate
      (design_flow * 1000000 / 1440) * 7.5 / 7.48052
    }
    
    # Backwash volume = bed volume × expansion factor (1.3) × number of contactors
    # Plus safety factor for multiple cycles
    backwash_volume_per_cycle_gal <- gac_bed_volume_cf * 7.48052 * 1.3  # 30% expansion
    
    # Storage for multiple cycles between maintenance
    # Typically 2-3 cycles worth of storage
    num_cycles_stored <- 2
    
    total_backwash_gal <- backwash_volume_per_cycle_gal * num_cycles_stored
    
    # Backwash storage multiplier (safety factor)
    back_multiplier <- 1.2  # Reduced from 1.5
    
    # Max tank size (10,000 gallons for small systems)
    max_tank_size <- if (design_flow < 1) 10000 else 15000
    
    # Calculate number of tanks needed
    num_backwash_tanks <- ceiling(total_backwash_gal * back_multiplier / max_tank_size)
    num_backwash_tanks <- max(1, num_backwash_tanks)
    
    # Volume per tank
    backwash_tank_volume <- (total_backwash_gal * back_multiplier) / num_backwash_tanks
    
    message(sprintf("Backwash tank sizing:"))
    message(sprintf("  GAC bed volume: %.1f cf", gac_bed_volume_cf))
    message(sprintf("  Backwash per cycle: %.0f gal", backwash_volume_per_cycle_gal))
    message(sprintf("  Total storage: %.0f gal", total_backwash_gal * back_multiplier))
    message(sprintf("  Tank volume: %.0f gal", backwash_tank_volume))
    
    # Calculate cost using Excel methodology
    # Default to steel tanks
    backwash_cost_result <- tryCatch({
      calculate_tank_cost(
        volume_gal = backwash_tank_volume,
        tank_type = "backwash_steel",
        quantity = num_backwash_tanks,
        component_level = component_level
      )
    }, error = function(e) {
      warning("Using fallback backwash tank cost: ", e$message)
      # Fallback: $5 per gallon
      unit_cost <- backwash_tank_volume * 5
      list(
        unit_cost = unit_cost,
        total_cost = unit_cost * num_backwash_tanks,
        message = "Fallback calculation"
      )
    })
    
    backwash_tank_cost <- backwash_cost_result$total_cost
  }
  
  # ===== RESIDUALS/HOLDING TANK SIZING =====
  
  residuals_tank_cost <- 0
  residuals_tank_volume <- 0
  num_residuals_tanks <- 0
  
  # Check if holding tank is needed
  # Excel uses res_s1_opt: 1 = holding tank, 2 = no holding tank
  # Our residuals_tank parameter can be:
  #   - "holding tank" (explicit request)
  #   - "no holding tank" (explicit no)
  #   - anything else defaults to no holding tank
  
  needs_holding_tank <- FALSE
  if (!is.null(residuals_tank) && !is.na(residuals_tank)) {
    residuals_tank_str <- tolower(trimws(as.character(residuals_tank)))
    # Only set to TRUE if explicitly contains "holding tank" and NOT "no"
    needs_holding_tank <- grepl("holding tank", residuals_tank_str) && 
                         !grepl("^no", residuals_tank_str)
  }
  
  # For small systems, Excel defaults to no holding tank (res_s1_opt = 2)
  if (design_flow < 0.5 && !needs_holding_tank) {
    needs_holding_tank <- FALSE
    message("Small system: No residuals holding tank (Excel default)")
  }
  
  if (needs_holding_tank) {
    
    # Calculate residuals volume
    # Simplified: volume generated per backwash cycle
    # Assume 2% of daily flow as residuals
    residuals_per_cycle <- design_flow * 1000000 * 0.02
    
    # Storage capacity factor (days of storage)
    storage_days <- 1  # Typical: 1 day storage
    
    # Total residuals volume
    res_cap_factor <- 1.2  # Safety factor
    total_res_vol <- residuals_per_cycle * storage_days * res_cap_factor
    
    # Max holding tank size
    max_htank_size <- if (design_flow < 1) 8000 else 12000
    
    # Calculate number of holding tanks
    num_residuals_tanks <- ceiling(total_res_vol / max_htank_size)
    num_residuals_tanks <- max(1, num_residuals_tanks)
    
    # Volume per tank
    residuals_tank_volume <- total_res_vol / num_residuals_tanks
    
    # Calculate cost using Excel methodology
    # Check disposal type for material selection
    tank_material <- if (grepl("hazardous", tolower(residuals_disposal))) {
      "holding_steel"  # Use steel for hazardous
    } else {
      "holding_hdpe"   # Use HDPE for non-hazardous
    }
    
    residuals_cost_result <- tryCatch({
      calculate_tank_cost(
        volume_gal = residuals_tank_volume,
        tank_type = tank_material,
        quantity = num_residuals_tanks,
        component_level = component_level
      )
    }, error = function(e) {
      warning("Using fallback residuals tank cost: ", e$message)
      # Fallback: $3 per gallon
      unit_cost <- residuals_tank_volume * 3
      list(
        unit_cost = unit_cost,
        total_cost = unit_cost * num_residuals_tanks,
        message = "Fallback calculation"
      )
    })
    
    residuals_tank_cost <- residuals_cost_result$total_cost
  }
  
  # ===== TOTAL COST =====
  
  total_cost <- backwash_tank_cost + residuals_tank_cost
  
  # Return detailed results
  list(
    # Backwash tanks
    backwash_tank_cost = backwash_tank_cost,
    backwash_tank_volume = backwash_tank_volume,
    num_backwash_tanks = num_backwash_tanks,
    
    # Residuals tanks
    residuals_tank_cost = residuals_tank_cost,
    residuals_tank_volume = residuals_tank_volume,
    num_residuals_tanks = num_residuals_tanks,
    
    # Total
    total_cost = total_cost,
    
    # Breakdown for output_db
    breakdown = data.frame(
      Item = c("Backwash Tanks", "Residuals Tanks"),
      Quantity = c(num_backwash_tanks, num_residuals_tanks),
      Volume_gal = c(backwash_tank_volume, residuals_tank_volume),
      Cost = c(backwash_tank_cost, residuals_tank_cost)
    )
  )
}

# ============================================================
# Pipe diameter lookup - mirrors Excel VLOOKUP on
# pipe_size_table_cl (Engineering Data rows 130-150)
# ============================================================
lookup_pipe_diameter <- function(flow_gpm) {
  # pipe_size_table_cl: flow breakpoints -> diameter (inches)
  # Row format: flows >= breakpoint get that diameter
  pipe_size_table <- data.frame(
    min_flow = c(0,    2.1,  4.1,  7.1,  21.1, 41.1, 66.1,
                 116.1, 238.1, 697.1, 1435.1, 2608.1,
                 4132.1, 5299.1, 7528.1, 10265.1, 13643.1,
                 22174.1, 50561.1, 81777.1, 122025.1),
    diameter  = c(0.5,  0.75, 1,    1.5,  2,    2.5,  3,
                  4,    6,    8,    10,    12,
                  14,   16,   18,   20,    24,
                  30,   36,   42,   48)
  )
  # VLOOKUP approximate match: largest breakpoint <= flow_gpm
  flow_gpm <- as.numeric(flow_gpm)
  if (is.na(flow_gpm) || flow_gpm < 0) flow_gpm <- 0
  idx <- max(which(pipe_size_table$min_flow <= flow_gpm))
  pipe_size_table$diameter[idx]
}

#' Calculate piping and valve costs
# calculate_piping_valves <- function(num_contactors, num_trains, automation_level, design_flow_mgd = 1, component_level = 1, no_backwash = FALSE) {
  
#   # Ensure numeric parameters
#   num_contactors <- safe_as_numeric(num_contactors, 1)
#   num_trains <- safe_as_numeric(num_trains, 1)
#   design_flow_mgd <- safe_as_numeric(design_flow_mgd, 1)
  
#   # ===== PIPE SIZING AND COSTING =====
  
#   # Estimate facility length (typical spacing between equipment)
#   facility_length <- if (design_flow_mgd < 0.5) {
#     10  # Small systems: 10 ft 
#   } else if (design_flow_mgd < 2) {
#     20  # Medium systems: 20 ft
#   } else {
#     30  # Large systems: 30 ft
#   }
  
#   # Calculate pipe diameter based on flow
#   # Simplified: use 2 ft/sec velocity
#   # Q (gpm) = 694.4 × D² × V
#   # D = sqrt(Q / (694.4 × 2))
#   flow_gpm <- design_flow_mgd * 694.4  # Convert MGD to GPM
#   pipe_diameter <- sqrt(flow_gpm / (694.4 * 2))  # inches
#   pipe_diameter <- max(2, min(24, pipe_diameter))  # Constrain to 2-24 inches
  
#   # Round to standard pipe sizes
#   standard_sizes <- c(2, 3, 4, 6, 8, 10, 12, 14, 16, 18, 20, 24)
#   pipe_diameter <- standard_sizes[which.min(abs(standard_sizes - pipe_diameter))]
  
#   # Calculate piping lengths for different sections
#   # Excel multipliers (from Piping sheet):
#   # proc_pipe_mult = 2
#   # back_pipe_mult = 2.5
#   # in_out_pipe_mult = 2
#   # res_pipe_mult = 1
  
#   # Process piping: multiplier × facility length
#   proc_pipe_mult <- 2.0  # Excel value
#   proc_pipe_length <- proc_pipe_mult * facility_length
  
#   # Backwash piping: multiplier × facility length
#   back_pipe_mult <- 2.5  # Excel value
#   back_pipe_length <- back_pipe_mult * facility_length
  
#   # Influent/Effluent piping: multiplier × facility length
#   in_out_pipe_mult <- 2.0  # Excel value
#   in_out_pipe_length <- in_out_pipe_mult * facility_length
  
#   # Residuals piping: base + add-on
#   res_pipe_mult <- 1.0
#   res_pipe_base <- res_pipe_mult * facility_length
#   # Add-on for residuals line (scales with system size)
#   res_pipe_addon <- if (design_flow_mgd < 0.5) {
#     40  # Small systems
#   } else if (design_flow_mgd < 2) {
#     60
#   } else {
#     80
#   }
#   res_pipe_length <- res_pipe_base + res_pipe_addon
  
#   # Total piping length
#   total_pipe_length <- proc_pipe_length + back_pipe_length + in_out_pipe_length + res_pipe_length
  
#   message(sprintf("Piping lengths: Process=%d, Backwash=%d, In/Out=%d, Residuals=%d, Total=%d LF",
#                   proc_pipe_length, back_pipe_length, in_out_pipe_length, res_pipe_length, total_pipe_length))
  
#   # Calculate piping costs using Excel equations
#   # Material selection based on system size (matching Excel logic)
#   # Small systems: All PVC (cheapest)
#   # Large systems: DI for mains, PVC for process
  
#   use_all_pvc <- design_flow_mgd < 1.0  # Small systems use all PVC
  
#   piping_cost_result <- tryCatch({
    
#     # Process piping - always PVC for small/medium systems
#     proc_material <- if (design_flow_mgd < 2.0) "PVC" else "CPVC"
#     proc_cost <- calculate_piping_cost(
#       diameter = pipe_diameter,
#       length = proc_pipe_length,
#       material = proc_material,
#       component_level = component_level
#     )
    
#     # Backwash piping - always PVC
#     back_cost <- calculate_piping_cost(
#       diameter = pipe_diameter,
#       length = back_pipe_length,
#       material = "PVC",
#       component_level = component_level
#     )
    
#     # Influent/Effluent piping - PVC for small, DI for large
#     in_out_material <- if (use_all_pvc) "PVC" else "DI"
#     in_out_cost <- calculate_piping_cost(
#       diameter = pipe_diameter,
#       length = in_out_pipe_length,
#       material = in_out_material,
#       component_level = component_level
#     )
    
#     list(
#       proc_cost = proc_cost$total_cost,
#       back_cost = back_cost$total_cost,
#       in_out_cost = in_out_cost$total_cost,
#       total_cost = proc_cost$total_cost + back_cost$total_cost + in_out_cost$total_cost,
#       message = "OK"
#     )
    
#   }, error = function(e) {
#     warning("Using fallback piping cost: ", e$message)
#     # Fallback: $100/LF average
#     list(
#       proc_cost = proc_pipe_length * 100,
#       back_cost = back_pipe_length * 100,
#       in_out_cost = in_out_pipe_length * 100,
#       total_cost = total_pipe_length * 100,
#       message = "Fallback calculation"
#     )
#   })
  
#   piping_cost <- piping_cost_result$total_cost
  
#   # ===== PIPING INSTALLATION COSTS (WBS 3.4) =====
#   # Excel includes excavation, bedding, backfill, and thrust blocks with piping
  
#   # 3.4.2 - Excavation (cy)
#   # Trench volume: length × width × depth / 27
#   # Typical trench: 2 ft wide × 3 ft deep
#   excavation_cy <- (total_pipe_length * 2 * 3) / 27
#   excavation_cost <- excavation_cy * 31  # $31/cy from Excel
  
#   # 3.4.3 - Bedding (cy)
#   # Sand/gravel bed under pipe: ~6 inches deep
#   bedding_cy <- (total_pipe_length * 2 * 0.5) / 27
#   bedding_cost <- bedding_cy * 45  # $45/cy from Excel
  
#   # 3.4.5 - Backfill and Compaction (cy)
#   # Same volume as excavation
#   backfill_cy <- excavation_cy
#   backfill_cost <- backfill_cy * 19  # $19/cy from Excel
  
#   # 3.4.6 - Thrust Blocks (cy)
#   # Concrete thrust blocks at bends/tees
#   # Estimate: 1 per 100 LF of piping
#   num_thrust_blocks <- total_pipe_length / 100
#   thrust_block_cy <- num_thrust_blocks * 0.5  # 0.5 cy each
#   thrust_block_cost <- thrust_block_cy * 740  # $740/cy from Excel
  
#   # Total piping installation costs
#   piping_installation_cost <- excavation_cost + bedding_cost + backfill_cost + thrust_block_cost
  
#   message(sprintf("Piping installation costs:"))
#   message(sprintf("  Excavation: %.1f cy @ $31 = $%.0f", excavation_cy, excavation_cost))
#   message(sprintf("  Bedding: %.1f cy @ $45 = $%.0f", bedding_cy, bedding_cost))
#   message(sprintf("  Backfill: %.1f cy @ $19 = $%.0f", backfill_cy, backfill_cost))
#   message(sprintf("  Thrust blocks: %.2f cy @ $740 = $%.0f", thrust_block_cy, thrust_block_cost))
#   message(sprintf("  Total installation: $%.0f", piping_installation_cost))
  
#   # Total piping costs = material + installation
#   total_piping_cost <- piping_cost + piping_installation_cost
  
#   # ===== VALVE SIZING AND COSTING =====
  
#   # Valve size typically matches pipe diameter
#   valve_size <- pipe_diameter
  
#   # Calculate valve costs using Excel equations
#   valve_cost_result <- tryCatch({
#     calculate_system_valve_costs(
#       num_contactors = num_contactors,
#       num_trains = num_trains,
#       valve_size = valve_size,
#       component_level = component_level,
#       design_flow_mgd = design_flow_mgd,
#       no_backwash = no_backwash
#     )
#   }, error = function(e) {
#     warning("Using fallback valve cost: ", e$message)
#     # Fallback: $2000 per valve
#     num_valves <- num_contactors * 4
#     list(
#       total_cost = num_valves * 2000,
#       mov_cost = num_valves * 2000 * 0.6,
#       mov_quantity = num_valves,
#       chv_cost = num_valves * 2000 * 0.2,
#       chv_quantity = num_trains,
#       bfv_cost = num_valves * 2000 * 0.2,
#       bfv_quantity = num_trains * 2,
#       message = "Fallback calculation"
#     )
#   })
  
#   valve_cost <- valve_cost_result$total_cost
  
#   # ===== TOTAL COST =====
  
#   total_cost <- total_piping_cost + valve_cost
  
#   # Return detailed results
#   list(
#     # Piping material
#     piping_material_cost = piping_cost,
#     piping_length_lf = total_pipe_length,
#     pipe_diameter = pipe_diameter,
#     proc_pipe_length = proc_pipe_length,
#     back_pipe_length = back_pipe_length,
#     in_out_pipe_length = in_out_pipe_length,
    
#     # Piping installation
#     excavation_cost = excavation_cost,
#     bedding_cost = bedding_cost,
#     backfill_cost = backfill_cost,
#     thrust_block_cost = thrust_block_cost,
#     piping_installation_cost = piping_installation_cost,
    
#     # Total piping
#     piping_cost = total_piping_cost,
    
#     # Valves
#     valve_cost = valve_cost,
#     num_valves = valve_cost_result$mov_quantity + 
#                  valve_cost_result$chv_quantity + 
#                  valve_cost_result$bfv_quantity,
#     mov_cost = valve_cost_result$mov_cost,
#     mov_quantity = valve_cost_result$mov_quantity,
#     chv_cost = valve_cost_result$chv_cost,
#     chv_quantity = valve_cost_result$chv_quantity,
#     bfv_cost = valve_cost_result$bfv_cost,
#     bfv_quantity = valve_cost_result$bfv_quantity,
    
#     # Total
#     total_cost = total_cost,
    
#     # Breakdown for output_db
#     breakdown = data.frame(
#       Item = c("Process Piping", "Backwash Piping", "In/Out Piping", 
#                "Motor Operated Valves", "Check Valves", "Butterfly Valves"),
#       Quantity = c(proc_pipe_length, back_pipe_length, in_out_pipe_length,
#                    valve_cost_result$mov_quantity, valve_cost_result$chv_quantity, 
#                    valve_cost_result$bfv_quantity),
#       Unit = c("LF", "LF", "LF", "EA", "EA", "EA"),
#       Cost = c(piping_cost_result$proc_cost, piping_cost_result$back_cost, 
#                piping_cost_result$in_out_cost, valve_cost_result$mov_cost,
#                valve_cost_result$chv_cost, valve_cost_result$bfv_cost)
#     )
#   )
# }

#' Calculate piping and valve costs
#' Mirrors workbook "Pumps Pipe Structure" sheet logic:
#'   - proc_pipe_flow  = flow_per_vessel = design_flow_gpm / num_trains
#'   - in_out_pipe_flow = total design_flow_gpm
#'   - back_pipe_flow   = water_flush (backwash flow per train * num_trains)
#'   - Diameters derived independently via VLOOKUP on pipe_size_table_cl
#'   - Pipe lengths = multiplier * facil_length
#'   - facil_length = ROUNDUP(sqrt(total_fp), -1)  [passed in or estimated]
calculate_piping_valves <- function(num_contactors, num_trains, automation_level, design_flow_mgd = 1, component_level = 1, no_backwash = FALSE, facil_length = NULL,  backwash_rate_gpm = NULL, vessel_diameter, vessel_length, tank_geometry, params = list(), tanks = list(),
                                    backwash_pumps    = 0,  
                                    num_back_tanks    = 0,   
                                    num_booster_pumps = 0) { 

  # Ensure numeric parameters
  num_contactors  <- safe_as_numeric(num_contactors,  1)
  num_trains      <- safe_as_numeric(num_trains,      1)
  design_flow_mgd <- safe_as_numeric(design_flow_mgd, 1)

  # ── 1. Facility length (facil_length) ─────────────────────────────────────
  # Workbook: facil_length = ROUNDUP(SQRT(total_fp), -1) = 10 ft for small systems
  # If not provided, estimate from flow (matches workbook defaults)
  if (is.null(facil_length) || is.na(facil_length)) {
    facil_length <- if (design_flow_mgd < 0.5) {
      10   # ROUNDUP(sqrt(26 sf), -1) = 10 ft  (workbook default)
    } else if (design_flow_mgd < 2) {
      20
    } else {
      30
    }
  }

 # ── 2. Per-section flows ───────────────────────────────────────────────────
# Workbook:
#   in_out_pipe_flow = design_flow_rate  (total flow, gpm)
#   proc_pipe_flow   = flow_per_vessel   (flow per single train/vessel)
#   back_pipe_flow   = water_flush       = ROUND(h2oflush_backwash_rate * comm_SA, 0)
#                      where comm_SA = pi*(vessel_diameter/2)^2  (upright)
#                                    = vessel_diameter * vessel_length  (horizontal)

  design_flow_gpm <- design_flow_mgd * 1e6 / 1440

  in_out_flow_gpm <- design_flow_gpm                       # total flow
  proc_flow_gpm   <- design_flow_gpm / max(1, num_trains)  # per vessel

  # Backwash flow: h2oflush_backwash_rate (12 gpm/ft²) × surface area of ONE vessel
  h2oflush_backwash_rate <- 12  # gpm/ft² — constant from Critical Design Assumptions C61

  comm_SA <- if (!is.null(vessel_diameter) && !is.na(vessel_diameter)) {
    if (tolower(tank_geometry) == "horizontal") {
      vessel_diameter * vessel_length   # horizontal: diameter × length
    } else {
      pi * (vessel_diameter / 2)^2      # upright: π r²
    }
  } else {
    # fallback: back-calculate from proc_flow at typical HLR
    proc_flow_gpm / 5   # ~5 gpm/ft² typical surface loading
  }

  back_flow_gpm <- round(h2oflush_backwash_rate * comm_SA, 0)

  # ── 3. Per-section diameters via VLOOKUP ──────────────────────────────────
  in_out_pipe_diam <- lookup_pipe_diameter(in_out_flow_gpm)
  proc_pipe_diam   <- lookup_pipe_diameter(proc_flow_gpm)
  back_pipe_diam   <- lookup_pipe_diameter(back_flow_gpm)

  message(sprintf("Pipe diameters: in_out=%s\", proc=%s\", back=%s\"",
                  in_out_pipe_diam, proc_pipe_diam, back_pipe_diam))

  # ── 4. Pipe lengths (multiplier × facil_length) ───────────────────────────
  # Workbook multipliers (Critical Design Assumptions rows 86-89):
  #   in_out_pipe_mult = 2    back_pipe_mult = 2.5    proc_pipe_mult = 2
  in_out_pipe_mult <- 2.0
  proc_pipe_mult   <- 2.0
  back_pipe_mult   <- 2.5

  in_out_pipe_length <- in_out_pipe_mult * facil_length
  proc_pipe_length   <- proc_pipe_mult   * facil_length
  back_pipe_length   <- back_pipe_mult   * facil_length

  total_pipe_length <- in_out_pipe_length + proc_pipe_length + back_pipe_length

  message(sprintf("Pipe lengths (facil_length=%d ft): in_out=%d, proc=%d, back=%d, total=%d LF",
                  facil_length, in_out_pipe_length, proc_pipe_length,
                  back_pipe_length, total_pipe_length))

  # ── 5. Material selection (per-section) ───────────────────────────────────
  # Small/medium systems: PVC for process and in/out, PVC for backwash
  # Large systems: DI or CPVC for process and in/out
  use_all_pvc   <- design_flow_mgd < 1.0
  proc_material   <- if (design_flow_mgd < 2.0) "PVC" else "CPVC"
  in_out_material <- if (use_all_pvc) "PVC" else "DI"
  back_material   <- "PVC"

  # ── 6. Piping material costs ───────────────────────────────────────────────
  piping_cost_result <- tryCatch({
    proc_cost   <- calculate_piping_cost(proc_pipe_diam,   proc_pipe_length,   proc_material,   component_level)
    back_cost   <- calculate_piping_cost(back_pipe_diam,   back_pipe_length,   back_material,   component_level)
    in_out_cost <- calculate_piping_cost(in_out_pipe_diam, in_out_pipe_length, in_out_material, component_level)

    list(
      proc_cost   = proc_cost$total_cost,
      back_cost   = back_cost$total_cost,
      in_out_cost = in_out_cost$total_cost,
      total_cost  = proc_cost$total_cost + back_cost$total_cost + in_out_cost$total_cost,
      message     = "OK"
    )
  }, error = function(e) {
    warning("Using fallback piping cost: ", e$message)
    list(
      proc_cost   = proc_pipe_length   * 100,
      back_cost   = back_pipe_length   * 100,
      in_out_cost = in_out_pipe_length * 100,
      total_cost  = total_pipe_length  * 100,
      message     = "Fallback"
    )
  })

  piping_cost <- piping_cost_result$total_cost

  # ── 6b. Residuals Piping (Residuals Management sheet logic) ───────────────
  # Constants (all from workbook — CDA and Indirect Assumptions sheets)
    res_pipe_mult          <- 1      # CDA C203: res_pipe_mult
    add_res_pipe_length    <- 40     # CDA C204: distance to discharge point (ft)
    min_res_pipe_size      <- 1      # CDA C202: minimum residuals pipe diameter (in)
    backwash_time_min      <- 10     # CDA C62:  backwash duration (minutes)
    back_interval_hrs      <- safe_as_numeric(params$backwash_interval %||% 168, 168)
    res_cap_factor         <- 2      # CDA C193: holding tank capacity safety factor
    frost_in               <- 38     # Indirect C32 (inches)
    min_trench_depth_in    <- 36     # Indirect C33 (inches)
    bedding_depth_in       <- 6      # Indirect C37 (inches)
    bedding_pct            <- 0.25   # Indirect C38 (fraction of pipe diameter)
    trench_angle_rad       <- pi / 4 # Indirect C39: RADIANS(45)
    soil_dens              <- 97     # Indirect C34 (lb/ft³)
    Coeff_Kp               <- 3      # Indirect C35
    Reduction              <- 0.467  # Indirect C36

    # res_vol = total_backwash = water_flush (back_flow_gpm) × backwash_time
    res_vol_gal <- back_flow_gpm * backwash_time_min

    # res_stagger_time = back_interval × 60 / num_contactors (minutes)
    # Workbook RM C11: back_interval*60 / final_num_tanks
    res_stagger_time_min <- back_interval_hrs * 60 / max(1, num_contactors)

    # res_flow: branches on whether a holding tank is present
    # Workbook RM C12-C13:
    #   no holding tank → res_flow = res_vol / backwash_time  (= back_flow_gpm exactly)
    #   holding tank    → res_flow = res_cap_factor × res_vol / res_stagger_time
    has_holding_tank <- isTRUE(tanks$num_residuals_tanks > 0)
    res_flow_gpm <- if (has_holding_tank) {
      res_cap_factor * res_vol_gal / res_stagger_time_min
    } else {
      res_vol_gal / backwash_time_min  # = back_flow_gpm
    }

    # Pipe diameter: MAX(min_res_pipe_size, VLOOKUP(res_flow, pipe_size_table_cl))
    # Workbook RM C122
    res_pipe_diam <- max(min_res_pipe_size, lookup_pipe_diameter(res_flow_gpm))

    # Pipe length: res_pipe_mult × facil_length + add_res_pipe_length
    # Workbook RM C121
    res_pipe_length    <- res_pipe_mult * facil_length + add_res_pipe_length  # e.g. 1×10 + 40 = 50 ft

    # Buried pipe length: only the add-on portion goes underground
    # Workbook RM C123: = add_res_pipe_length (40 ft by default)
    buried_res_pipe_length <- add_res_pipe_length

    # Residuals pipe material cost
    res_pipe_cost_result <- tryCatch({
      calculate_piping_cost(res_pipe_diam, res_pipe_length, "PVC", component_level)
    }, error = function(e) {
      list(total_cost = res_pipe_length * 10, cost_per_lf = 10)
    })
    res_pipe_material_cost <- res_pipe_cost_result$total_cost

    # ── Trench geometry (Residuals Management C132-C136) ──────────────────────
    res_trn_depth_in <- max(frost_in, min_trench_depth_in)  # = 38" for defaults
    d_ft    <- res_pipe_diam     / 12
    bd_ft   <- bedding_depth_in  / 12
    td_ft   <- res_trn_depth_in  / 12

    # Trench cross-section area (ft²) — RM C132
    res_trench_area <- (d_ft + 2) * (bd_ft + d_ft + td_ft) +
                      (bd_ft + d_ft + td_ft)^2 * tan(trench_angle_rad)

    # Trench volume (cy) — RM C133-C134
    res_trench_vol_cy <- if (buried_res_pipe_length == 0 || res_pipe_diam == 0) 0 else
      res_trench_area * buried_res_pipe_length / 27

    # Bedding cross-section (ft²) — RM C135
    bdg_d_ft <- bedding_pct * d_ft
    res_bedding_area <- (d_ft + 2) * (bd_ft + bdg_d_ft) +
                        (bd_ft + bdg_d_ft)^2 * tan(trench_angle_rad) -
                        ((d_ft/2)^2 * acos(pmax(-1, pmin(1, (d_ft/2 - bdg_d_ft) / (d_ft/2)))) -
                        (d_ft/2 - bdg_d_ft) * sqrt(pmax(0, 2*(d_ft/2)*bdg_d_ft - bdg_d_ft^2)))

    # Bedding volume (cy) — RM C136: divided by 2 per workbook formula
    res_bedding_vol_cy <- if (buried_res_pipe_length == 0 || res_pipe_diam == 0) 0 else
      res_bedding_area * buried_res_pipe_length / 2 / 27

    # ── Thrust block (Residuals Management C125-C131) ─────────────────────────
    # Thrust force: VLOOKUP(res_pipe_diam, thrust_block_table_cl, 2) × 2.2
    thrust_block_table <- data.frame(
      min_diam = c(0,    2.1,   3.1,   4.1,    6.1,    8.1,    10.1,   12.1,
                  14.1, 16.1,  18.1,  20.1,   24.1,   28.1,   30.1,   40.1,   42.1),
      force_kg  = c(347.6, 681.9, 1260.0, 2835.0, 4990.7, 7813.4, 11192.3, 15314.3,
                    20002.3, 25315.5, 31130.7, 45064.3, 61050.6, 70505.4, 124768.6,
                    137828.6, 180257.3)
    )
    thrust_idx       <- max(which(thrust_block_table$min_diam <= res_pipe_diam))
    res_thrust_force <- thrust_block_table$force_kg[thrust_idx] * 2.2  # kg → lbs

    # Bearing area (ft²) — RM C126
    res_bearing_area <- 1.5 * res_thrust_force /
                        (soil_dens * (td_ft + d_ft) * Coeff_Kp * Reduction)

    # Block dimensions (RM C127-C130)
    H_tb  <- sqrt(res_bearing_area / 2)
    L_tb  <- res_bearing_area / H_tb
    F_tb  <- max(0.5, d_ft)
    T_tb  <- L_tb / 2

    # Thrust block volume (cy) — RM C131
    res_block_vol_cy <- if (buried_res_pipe_length == 0 || res_pipe_diam == 0) 0 else
      (T_tb + (buried_res_pipe_length / 12) / 2) * (L_tb + F_tb) / 2 * H_tb / 27
  
  # ── 7. Piping installation costs ──────────────────────────────────────────
  excavation_cy     <- (total_pipe_length * 2 * 3) / 27
  bedding_cy        <- (total_pipe_length * 2 * 0.5) / 27
  backfill_cy       <- excavation_cy
  thrust_block_cy   <- (total_pipe_length / 100) * 0.5

  excavation_cost   <- excavation_cy   * 31
  bedding_cost      <- bedding_cy      * 45
  backfill_cost     <- backfill_cy     * 19
  thrust_block_cost <- thrust_block_cy * 740

  piping_installation_cost <- excavation_cost + bedding_cost + backfill_cost + thrust_block_cost

  total_piping_cost <- piping_cost + piping_installation_cost

  # ── 8. Valve sizing and costing ───────────────────────────────────────────
  # Valve size matches the process pipe diameter (workbook convention)
  valve_size <- proc_pipe_diam

  valve_cost_result <- tryCatch({
    calculate_system_valve_costs(
      num_contactors  = num_contactors,
      num_trains      = num_trains,
      valve_size      = valve_size,
      component_level = component_level,
      design_flow_mgd = design_flow_mgd,
      no_backwash     = no_backwash,
      no_back_tank    = no_backwash || (num_back_tanks == 0),  # workbook: no_back_tank=1 when using existing storage
      automation_level  = automation_level,
      proc_pipe_diam    = proc_pipe_diam,
      back_pipe_diam    = back_pipe_diam,
      in_out_pipe_diam  = in_out_pipe_diam,
      backwash_pumps    = backwash_pumps,
      num_back_tanks    = num_back_tanks,
      num_booster_pumps = num_booster_pumps,
      res_pumps      = tanks$residuals_pumps  %||% 0,
      res_hold_tanks = (tanks$num_residuals_tanks %||% 0) +
                       (tanks$num_residuals_basins %||% 0),
      res_pipe_diam  = res_pipe_diam
    )
  }, error = function(e) {
    warning("Using fallback valve cost: ", e$message)
    num_valves <- num_contactors * 4
    list(
      total_cost   = num_valves * 2000,
      mov_cost     = num_valves * 2000 * 0.6,
      mov_quantity = num_valves,
      chv_cost     = num_valves * 2000 * 0.2,
      chv_quantity = num_trains,
      bfv_cost     = num_valves * 2000 * 0.2,
      bfv_quantity = num_trains * 2,
      message      = "Fallback"
    )
  })

  valve_cost <- valve_cost_result$total_cost
  total_cost <- total_piping_cost + valve_cost

  # ── 9. Return ──────────────────────────────────────────────────────────────
  list(
    # Per-section flows (mirrors workbook named ranges)
    in_out_pipe_flow   = in_out_flow_gpm,
    proc_pipe_flow     = proc_flow_gpm,
    back_pipe_flow     = back_flow_gpm,

    # Per-section diameters (mirrors workbook named ranges)
    in_out_pipe_diam   = in_out_pipe_diam,
    proc_pipe_diam     = proc_pipe_diam,    # was incorrectly = back_pipe_diam (2") before
    back_pipe_diam     = back_pipe_diam,
    pipe_diameter      = proc_pipe_diam,    # kept for backward compat; = proc_pipe_diam

    # Facility length
    facil_length       = facil_length,

    # Piping material
    piping_material_cost = piping_cost,
    piping_length_lf   = total_pipe_length,
    proc_pipe_length   = proc_pipe_length,
    back_pipe_length   = back_pipe_length,
    in_out_pipe_length = in_out_pipe_length,

    # Piping installation
    excavation_cost    = excavation_cost,
    bedding_cost       = bedding_cost,
    backfill_cost      = backfill_cost,
    thrust_block_cost  = thrust_block_cost,
    piping_installation_cost = piping_installation_cost,

    piping_cost        = total_piping_cost,

    # Valves
    valve_cost         = valve_cost,
    num_valves         = valve_cost_result$mov_quantity +
                         valve_cost_result$chv_quantity +
                         valve_cost_result$bfv_quantity,
    mov_cost           = valve_cost_result$mov_cost,
    mov_quantity       = valve_cost_result$mov_quantity,
    proc_mov_cost      = valve_cost_result$proc_mov_cost,
    proc_mov_qty       = valve_cost_result$proc_mov_qty,
    back_mov_cost      = valve_cost_result$back_mov_cost,
    back_mov_qty       = valve_cost_result$back_mov_qty,
    res_mov_cost    = valve_cost_result$res_mov_cost,
    res_mov_qty     = valve_cost_result$res_mov_qty,

    res_man_cost    = valve_cost_result$res_man_cost,
    res_man_qty     = valve_cost_result$res_man_qty,

    chv_cost           = valve_cost_result$chv_cost,
    chv_quantity       = valve_cost_result$chv_quantity,
    in_chv_cost        = valve_cost_result$in_chv_cost,
    in_chv_qty         = valve_cost_result$in_chv_qty,
    back_chv_cost      = valve_cost_result$back_chv_cost,
    back_chv_qty       = valve_cost_result$back_chv_qty,
    res_chv_cost       = valve_cost_result$res_chv_cost,
    res_chv_qty        = valve_cost_result$res_chv_qty,
    bfv_cost           = valve_cost_result$bfv_cost,
    bfv_quantity       = valve_cost_result$bfv_quantity,


    in_man_cost        = valve_cost_result$in_man_cost,
    in_man_qty         = valve_cost_result$in_man_qty,
    proc_man_cost      = valve_cost_result$proc_man_cost,
    proc_man_qty       = valve_cost_result$proc_man_qty,
    back_man_cost      = valve_cost_result$back_man_cost,
    back_man_qty       = valve_cost_result$back_man_qty,

    proc_pipe_cost   = piping_cost_result$proc_cost,
    back_pipe_cost   = piping_cost_result$back_cost,
    in_out_pipe_cost = piping_cost_result$in_out_cost,
    
    total_cost         = total_cost,

    # Residuals piping (Residuals Management sheet)
    res_pipe_diam          = res_pipe_diam,
    res_pipe_length        = res_pipe_length,
    buried_res_pipe_length = buried_res_pipe_length,
    res_pipe_material_cost = res_pipe_material_cost,
    res_trench_vol_cy      = res_trench_vol_cy,
    res_bedding_vol_cy     = res_bedding_vol_cy,
    res_block_vol_cy       = res_block_vol_cy,
    res_trench_area        = res_trench_area,
    res_flow_gpm           = res_flow_gpm,


    breakdown = data.frame(
      Item     = c("Process Piping", "Backwash Piping", "In/Out Piping",
                   "Motor Operated Valves", "Check Valves", "Butterfly Valves"),
      Quantity = c(proc_pipe_length, back_pipe_length, in_out_pipe_length,
                   valve_cost_result$mov_quantity, valve_cost_result$chv_quantity,
                   valve_cost_result$bfv_quantity),
      Unit     = c("LF", "LF", "LF", "EA", "EA", "EA"),
      Cost     = c(piping_cost_result$proc_cost,  piping_cost_result$back_cost,
                   piping_cost_result$in_out_cost, valve_cost_result$mov_cost,
                   valve_cost_result$chv_cost,     valve_cost_result$bfv_cost)
    )
  )
}

#' Calculate controls and instrumentation costs
calculate_controls <- function(automation_level, num_contactors, num_trains, manual_override,
                               # Workbook parameters (with workbook defaults)
                               ss_cat2          = 1,
                               design_type      = 1,
                               add_on           = 0,
                               num_back_tanks   = 0,
                               res_holding      = "none",
                               num_res_tanks    = 0,
                               num_res_basins   = 0,
                               bp_pct           = 0,
                               in_out_pipe_diam = 1.5,
                               proc_pipe_diam   = 1.5,
                               back_pipe_diam   = 2.0,
                               res_pipe_diam    = 2.0,
                               # Section 7 inputs
                               tot_MOVs         = 0,    # total MOVs from valve calc (IC C22)
                               fm_lkp_io        = "flow_prop",   # selected flow meter type codes
                               fm_lkp_proc      = "flow_mag",
                               fm_lkp_back      = "flow_prop",
                               fm_lkp_res       = "flow_prop",
                               Operator_LOE     = 40.607,  # O&M C39
                               booster_pumps    = 0,
                               backwash_pumps   = 0,
                               res_pumps        = 0,
                               transfer_method  = 3,      # 1=slurry, 2=eductor, 3=none (workbook default: 3=none)
                               res_transfer_method = 3,
                               eductors         = 0,
                               res_slurry_pumps = 0,
                               res_eductors     = 0,
                               mixers           = 0) {

  # ── Sources: IC rows 37-75, CDA rows 105-155, RM row 143-144
  # ── Section 6: flow meter polynomial costs + fixed sensor costs
  # ── Section 7: I/O port table -> module counts -> fixed unit costs

  num_contactors   <- safe_as_numeric(num_contactors,   1)
  num_trains       <- safe_as_numeric(num_trains,       1)
  ss_cat2          <- safe_as_numeric(ss_cat2,          1)
  design_type      <- safe_as_numeric(design_type,      1)
  add_on           <- safe_as_numeric(add_on,           0)
  num_back_tanks   <- safe_as_numeric(num_back_tanks,   0)
  num_res_tanks    <- safe_as_numeric(num_res_tanks,    0)
  num_res_basins   <- safe_as_numeric(num_res_basins,   0)
  bp_pct           <- safe_as_numeric(bp_pct,           0)
  in_out_pipe_diam <- safe_as_numeric(in_out_pipe_diam, 1.5)
  proc_pipe_diam   <- safe_as_numeric(proc_pipe_diam,   1.5)
  back_pipe_diam   <- safe_as_numeric(back_pipe_diam,   2.0)
  res_pipe_diam    <- safe_as_numeric(res_pipe_diam,    2.0)
  tot_MOVs         <- safe_as_numeric(tot_MOVs,         0)
  Operator_LOE     <- safe_as_numeric(Operator_LOE,     40.607)
  booster_pumps    <- safe_as_numeric(booster_pumps,    0)
  backwash_pumps   <- safe_as_numeric(backwash_pumps,   0)
  res_pumps        <- safe_as_numeric(res_pumps,        0)
  eductors         <- safe_as_numeric(eductors,         0)
  res_slurry_pumps <- safe_as_numeric(res_slurry_pumps, 0)
  res_eductors     <- safe_as_numeric(res_eductors,     0)
  mixers           <- safe_as_numeric(mixers,           0)

  # ss_cat: "small" when ss_cat2=1, "large" otherwise (OUTPUT C4)
  ss_cat <- if (ss_cat2 == 1) "small" else "large"

  # manual: IC C6 = 1=manual, 2=semi-automated, 3=fully automated
  automation_level <- tolower(safe_as_char(automation_level, "fully automated"))
  manual <- switch(automation_level,
    "manual"          = 1L,
    "semi-automated"  = 2L,
    "semi automated"  = 2L,
    "fully automated" = 3L,
    3L)

  # ── Flow meter cost equations: M*d^2 + N*d + O (Cost Equations rows 143-161) ──
  op_fm_cost   <- function(d) -20.141018838*d^2 + 567.622805504*d + 674.161169679
  prop_fm_cost <- function(d) -19.177375645*d^2 + 593.880351607*d + 1500.545795131
  ven_fm_cost  <- function(d)   7.381419291*d^2 + 135.408778870*d + 2010.462068774
  mag_fm_cost  <- function(d)   3.727367106*d^2 + 488.194092080*d + 3103.652214921

  # ── Section 6 fixed unit costs (Cost Data col D) ──────────────────────────
  alarm_uc      <- 600.1870766967089
  headloss_uc   <- 2388.936473342225
  pH_meter_uc   <- 3047.115547753862
  temp_meter_uc <- 794.7585292375769
  turb_meter_uc <- 6183.704593712282
  elec_encl_uc  <- 1400
  sampling_uc   <- 50

  # ── 6.1 tot_fm_in (IC C41) ─────────────────────────────────────────────────
  tot_fm_in <- if (add_on == 1) 0L else 1L
  in_meter_size <- in_out_pipe_diam

  # ── 6.2 tot_fm_proc (IC C40) ───────────────────────────────────────────────
  tot_fm_proc <- 0L
  proc_meter_size <- proc_pipe_diam

  # ── 6.3 tot_fm_back (CDA C108 = 1) ────────────────────────────────────────
  tot_fm_back <- 1L
  back_meter_size <- back_pipe_diam

  # ── 6.4 tot_fm_res (CDA C210 = 1) ─────────────────────────────────────────
  tot_fm_res <- 1L
  res_meter_size <- res_pipe_diam

  # ── 6.5 tot_level_switch (IC C43) ──────────────────────────────────────────
  tot_level_switch <- if (design_type == 1) {
    num_contactors * if (ss_cat2 == 1) 0L else 0L
  } else {
    num_trains * if (ss_cat2 == 1) 0L else 1L
  }

  # ── 6.6 tot_back_alarm (IC C44) ────────────────────────────────────────────
  tot_back_alarm <- num_back_tanks * 1L

  # ── 6.7 tot_res_alarm (RM C143) ────────────────────────────────────────────
  tot_res_alarm <- switch(res_holding,
    "tanks"  = num_res_tanks  * 1L,
    "basins" = num_res_basins * 1L,
    0L)

  # ── 6.8 pH_controls (IC C45) ───────────────────────────────────────────────
  pH_controls <- if (design_type == 2) {
    num_trains * if (ss_cat2 == 1) 0L else 1L
  } else {
    if (ss_cat2 == 1) 0L else 2L
  }

  # ── 6.9 tot_temp_meters (IC C46) ───────────────────────────────────────────
  tot_temp_meters <- if (ss_cat2 == 1) 0L else 2L

  # ── 6.10 tot_turb_meters (IC C47) ──────────────────────────────────────────
  tot_turb_meters <- (0L * num_contactors) +
    (if (ss_cat2 == 1) 0L else 2L) +
    (if (design_type == 1) (if (ss_cat2 == 1) 0L else 1L) else 0L)

  # ── 6.11 tot_head_sens (IC C48) ────────────────────────────────────────────
  tot_head_sens <- (if (ss_cat2 == 1) 0L else 1L) * num_contactors

  # ── 6.12 ports (IC C49) ────────────────────────────────────────────────────
  ports <- 1L * num_contactors + 3L + if (bp_pct > 0) 1L else 0L

  # ── 6.13 elec_encl (CDA C131 = 0) ─────────────────────────────────────────
  elec_encl <- 0L

  # ── Per-type flow meter unit costs ─────────────────────────────────────────
  fm_in_op_uc    <- op_fm_cost(in_meter_size);    fm_in_op_tc    <- tot_fm_in   * fm_in_op_uc
  fm_in_prop_uc  <- prop_fm_cost(in_meter_size);  fm_in_prop_tc  <- tot_fm_in   * fm_in_prop_uc
  fm_in_ven_uc   <- ven_fm_cost(in_meter_size);   fm_in_ven_tc   <- tot_fm_in   * fm_in_ven_uc
  fm_in_mag_uc   <- mag_fm_cost(in_meter_size);   fm_in_mag_tc   <- tot_fm_in   * fm_in_mag_uc

  fm_proc_op_uc   <- op_fm_cost(proc_meter_size);   fm_proc_op_tc   <- tot_fm_proc * fm_proc_op_uc
  fm_proc_prop_uc <- prop_fm_cost(proc_meter_size); fm_proc_prop_tc <- tot_fm_proc * fm_proc_prop_uc
  fm_proc_ven_uc  <- ven_fm_cost(proc_meter_size);  fm_proc_ven_tc  <- tot_fm_proc * fm_proc_ven_uc
  fm_proc_mag_uc  <- mag_fm_cost(proc_meter_size);  fm_proc_mag_tc  <- tot_fm_proc * fm_proc_mag_uc

  fm_back_op_uc   <- op_fm_cost(back_meter_size);   fm_back_op_tc   <- tot_fm_back * fm_back_op_uc
  fm_back_prop_uc <- prop_fm_cost(back_meter_size); fm_back_prop_tc <- tot_fm_back * fm_back_prop_uc
  fm_back_ven_uc  <- ven_fm_cost(back_meter_size);  fm_back_ven_tc  <- tot_fm_back * fm_back_ven_uc
  fm_back_mag_uc  <- mag_fm_cost(back_meter_size);  fm_back_mag_tc  <- tot_fm_back * fm_back_mag_uc

  fm_res_op_uc    <- op_fm_cost(res_meter_size);    fm_res_op_tc    <- tot_fm_res  * fm_res_op_uc
  fm_res_prop_uc  <- prop_fm_cost(res_meter_size);  fm_res_prop_tc  <- tot_fm_res  * fm_res_prop_uc
  fm_res_ven_uc   <- ven_fm_cost(res_meter_size);   fm_res_ven_tc   <- tot_fm_res  * fm_res_ven_uc
  fm_res_mag_uc   <- mag_fm_cost(res_meter_size);   fm_res_mag_tc   <- tot_fm_res  * fm_res_mag_uc

  level_switch_cost  <- tot_level_switch  * alarm_uc
  back_alarm_cost    <- tot_back_alarm    * alarm_uc
  res_alarm_cost     <- tot_res_alarm     * alarm_uc
  pH_cost            <- pH_controls       * pH_meter_uc
  temp_cost          <- tot_temp_meters   * temp_meter_uc
  turb_cost          <- tot_turb_meters   * turb_meter_uc
  headloss_cost_val  <- tot_head_sens     * headloss_uc
  ports_ss_cost      <- ports             * sampling_uc
  ports_cs_cost      <- ports             * sampling_uc
  elec_encl_cost     <- elec_encl         * elec_encl_uc

  # ── Section 7: I/O port table (CDA rows 296-307) ──────────────────────────
  # Columns: [discrete_in, discrete_out, analog_in, analog_out]
  port_di <- function(key) switch(key,
    "alarm"=1,"flow_mag"=0,"flow_op"=0,"flow_prop"=0,"flow_ven"=0,
    "head"=1,"ph"=0,"port"=1,"switch"=3,"temp"=0,"turb"=0,"valve"=0, 0)
  port_do <- function(key) switch(key,
    "alarm"=1,"flow_mag"=0,"flow_op"=0,"flow_prop"=0,"flow_ven"=0,
    "head"=0,"ph"=0,"port"=0,"switch"=1,"temp"=0,"turb"=0,"valve"=0, 0)
  port_ai <- function(key) switch(key,
    "alarm"=0,"flow_mag"=1,"flow_op"=2,"flow_prop"=1,"flow_ven"=2,
    "head"=0,"ph"=1,"port"=0,"switch"=0,"temp"=1,"turb"=1,"valve"=1, 0)
  port_ao <- function(key) switch(key,
    "alarm"=0,"flow_mag"=0,"flow_op"=1,"flow_prop"=0,"flow_ven"=1,
    "head"=0,"ph"=0,"port"=0,"switch"=0,"temp"=0,"turb"=0,"valve"=1, 0)

  alarms <- tot_level_switch + tot_back_alarm + tot_res_alarm

  # ── 7.2.1 Drive controllers (IC C50) ───────────────────────────────────────
  # IF(manual=2, 0, S_booster*booster + S_back*backwash + S_res*res + ...)
  # S_ coefficients all = 1 (CDA rows 137-140)
  tot_switches <- if (manual == 2) 0L else {
    transfer_drives <- switch(transfer_method,
      `1` = 1L,   # slurry: 1 drive (S_slurry=1 per slurry pump system)
      `2` = eductors * 1L,
      0L)
    res_transfer_drives <- switch(res_transfer_method,
      `1` = res_slurry_pumps * 1L,
      `2` = res_eductors * 1L,
      0L)
    as.integer(
      1 * booster_pumps + 1 * backwash_pumps + 1 * res_pumps +
      transfer_drives + res_transfer_drives + 1 * mixers
    )
  }

  # ── IC C57-C60: I/O totals ─────────────────────────────────────────────────
  discrete_inputs <- (
    port_di(fm_lkp_io)*tot_fm_in + port_di(fm_lkp_proc)*tot_fm_proc +
    port_di(fm_lkp_back)*tot_fm_back + port_di(fm_lkp_res)*tot_fm_res +
    port_di("alarm")*alarms + port_di("ph")*pH_controls +
    port_di("temp")*tot_temp_meters + port_di("turb")*tot_turb_meters +
    port_di("head")*tot_head_sens + port_di("port")*ports +
    port_di("switch")*tot_switches + port_di("valve")*tot_MOVs
  )
  discrete_outputs <- if (manual == 2) 0L else (
    port_do(fm_lkp_io)*tot_fm_in + port_do(fm_lkp_proc)*tot_fm_proc +
    port_do(fm_lkp_back)*tot_fm_back + port_do(fm_lkp_res)*tot_fm_res +
    port_do("alarm")*alarms + port_do("ph")*pH_controls +
    port_do("temp")*tot_temp_meters + port_do("turb")*tot_turb_meters +
    port_do("head")*tot_head_sens + port_do("port")*ports +
    port_do("switch")*tot_switches + port_do("valve")*tot_MOVs
  )
  analog_inputs <- (
    port_ai(fm_lkp_io)*tot_fm_in + port_ai(fm_lkp_proc)*tot_fm_proc +
    port_ai(fm_lkp_back)*tot_fm_back + port_ai(fm_lkp_res)*tot_fm_res +
    port_ai("alarm")*alarms + port_ai("ph")*pH_controls +
    port_ai("temp")*tot_temp_meters + port_ai("turb")*tot_turb_meters +
    port_ai("head")*tot_head_sens + port_ai("port")*ports +
    port_ai("switch")*tot_switches + port_ai("valve")*tot_MOVs
  )
  analog_outputs <- if (manual == 2) 0L else (
    port_ao(fm_lkp_io)*tot_fm_in + port_ao(fm_lkp_proc)*tot_fm_proc +
    port_ao(fm_lkp_back)*tot_fm_back + port_ao(fm_lkp_res)*tot_fm_res +
    port_ao("alarm")*alarms + port_ao("ph")*pH_controls +
    port_ao("temp")*tot_temp_meters + port_ao("turb")*tot_turb_meters +
    port_ao("head")*tot_head_sens + port_ao("port")*ports +
    port_ao("switch")*tot_switches + port_ao("valve")*tot_MOVs
  )

  # ── PLC module counts (IC C62-C70) ─────────────────────────────────────────
  plc_discrete_output            <- ceiling(discrete_outputs / 32)
  plc_discrete_input             <- ceiling(discrete_inputs  / 32)
  plc_combination_analog         <- ceiling((analog_inputs + analog_outputs) / 12)
  plc_cpu                        <- if (add_on == 1) 0L else 2L   # CDA C143/C144
  plc_ethernet                   <- if (add_on == 1) 0L else 2L   # CDA C145/C146
  plc_slots                      <- plc_cpu + plc_ethernet + plc_discrete_input +
                                    plc_discrete_output + plc_combination_analog
  plc_expansions                 <- if (plc_slots <= 9) 0L else ceiling((plc_slots - 9) / 8)
  plc_rack                       <- if (add_on == 1 && 0 == 0) 0L else 1L + plc_expansions  # plc_rack_addon=0
  plc_base_expansion             <- plc_expansions
  plc_base_expansion_controller  <- plc_expansions
  plc_interface                  <- if (add_on == 1) 0L else 2L   # CDA C147/C148
  ups                            <- if (add_on == 1) 0L else 1L   # CDA C149/C150

  # ── 7.2.3/7.2.4 Workstations and printers ──────────────────────────────────
  # IC C74: ROUNDUP(Operator_LOE/2080/shifts_day/workstation_ratio, 0)  (shifts_day=3, ratio=1)
  # IC C75: ROUNDUP(workstations/printer_ratio, 0)  (printer_ratio=4)
  workstations <- if (add_on == 1 && 0 == 0) 0L else ceiling(Operator_LOE / 2080 / 3 / 1)
  printers     <- if (add_on == 1 && 0 == 0) 0L else ceiling(workstations / 4)

  # Section 7 quantity rules:
  # 7.1.x: IF(manual=1, 0, plc_xxx)
  # 7.2.1: IF(manual=1, 0, tot_switches)
  # 7.2.2: IF(manual=1, 0, plc_interface)
  # 7.2.3/7.2.4: IF(OR(manual=1, ss_cat="small"), 0, workstations/printers)
  # 7.3.x: IF(OR(manual=1, ss_cat="small"), 0, plc_interface/workstations)
  is_manual   <- manual == 1L
  is_small    <- ss_cat == "small"

  qty_7_1_1 <- if (is_manual) 0L else plc_rack
  qty_7_1_2 <- if (is_manual) 0L else plc_cpu
  qty_7_1_3 <- if (is_manual) 0L else plc_discrete_input
  qty_7_1_4 <- if (is_manual) 0L else plc_discrete_output
  qty_7_1_5 <- if (is_manual) 0L else plc_combination_analog
  qty_7_1_6 <- if (is_manual) 0L else plc_ethernet
  qty_7_1_7 <- if (is_manual) 0L else plc_base_expansion
  qty_7_1_8 <- if (is_manual) 0L else plc_base_expansion_controller
  qty_7_1_9 <- if (is_manual) 0L else ups
  qty_7_2_1 <- if (is_manual) 0L else tot_switches
  qty_7_2_2 <- if (is_manual) 0L else plc_interface
  qty_7_2_3 <- if (is_manual || is_small) 0L else workstations
  qty_7_2_4 <- if (is_manual || is_small) 0L else printers
  qty_7_3_1 <- if (is_manual || is_small) 0L else plc_interface
  qty_7_3_2 <- if (is_manual || is_small) 0L else workstations
  qty_7_3_3 <- if (is_manual || is_small) 0L else workstations
  qty_7_3_4 <- if (is_manual || is_small) 0L else workstations

  # ── Section 7 unit costs (Cost Data col D = VLOOKUP(0, cost_cl, 3)) ────────
  uc_plc_rack                  <- 355.56
  uc_plc_cpu                   <- 378.8
  uc_plc_discrete_input        <- 382.68
  uc_plc_discrete_output       <- 340.425
  uc_plc_combination_analog    <- 557.335
  uc_plc_ethernet              <- 1342.235
  uc_plc_base_expansion        <- 142.67
  uc_plc_base_expansion_ctrl   <- 107.74
  uc_ups                       <- 877.495
  uc_switch                    <- 1300.897872615849
  uc_plc_op_interface          <- if (ss_cat == "small") 1828.625 else 616.5   # VLOOKUP(ss_cat, 2-row table)
  uc_pc_workstation            <- 1325.79
  uc_laser_printer             <- 373.995
  uc_op_interface_software     <- 40.5
  uc_plc_software              <- 553.5
  uc_plc_data_software         <- 597.235
  uc_plant_intel_software      <- 20680.74

  message(sprintf("7.1.1 PLC rack:     %d x $%.2f = $%.0f", qty_7_1_1, uc_plc_rack, qty_7_1_1*uc_plc_rack))
  message(sprintf("7.1.2 CPUs:         %d x $%.2f = $%.0f", qty_7_1_2, uc_plc_cpu, qty_7_1_2*uc_plc_cpu))
  message(sprintf("7.1.5 Comb analog:  %d x $%.2f = $%.0f", qty_7_1_5, uc_plc_combination_analog, qty_7_1_5*uc_plc_combination_analog))
  message(sprintf("7.2.2 Op interface: %d x $%.2f = $%.0f", qty_7_2_2, uc_plc_op_interface, qty_7_2_2*uc_plc_op_interface))

  list(
    # ── system metadata ───────────────────────────────────────────────────────
    system_scale = if (num_contactors <= 2) "small" else if (num_contactors <= 6) "medium" else "large", #design flow
    ss_cat       = ss_cat,
    manual       = manual,
    # ── 6.1 Flow Meters – Influent ────────────────────────────────────────────
    tot_fm_in      = tot_fm_in,     in_meter_size   = in_meter_size,
    fm_in_op_uc    = fm_in_op_uc,   fm_in_op_tc     = fm_in_op_tc,
    fm_in_prop_uc  = fm_in_prop_uc, fm_in_prop_tc   = fm_in_prop_tc,
    fm_in_ven_uc   = fm_in_ven_uc,  fm_in_ven_tc    = fm_in_ven_tc,
    fm_in_mag_uc   = fm_in_mag_uc,  fm_in_mag_tc    = fm_in_mag_tc,
    # ── 6.2 Flow Meters – Process ─────────────────────────────────────────────
    tot_fm_proc      = tot_fm_proc,     proc_meter_size   = proc_meter_size,
    fm_proc_op_uc    = fm_proc_op_uc,   fm_proc_op_tc     = fm_proc_op_tc,
    fm_proc_prop_uc  = fm_proc_prop_uc, fm_proc_prop_tc   = fm_proc_prop_tc,
    fm_proc_ven_uc   = fm_proc_ven_uc,  fm_proc_ven_tc    = fm_proc_ven_tc,
    fm_proc_mag_uc   = fm_proc_mag_uc,  fm_proc_mag_tc    = fm_proc_mag_tc,
    # ── 6.3 Flow Meters – Backwash ────────────────────────────────────────────
    tot_fm_back      = tot_fm_back,     back_meter_size   = back_meter_size,
    fm_back_op_uc    = fm_back_op_uc,   fm_back_op_tc     = fm_back_op_tc,
    fm_back_prop_uc  = fm_back_prop_uc, fm_back_prop_tc   = fm_back_prop_tc,
    fm_back_ven_uc   = fm_back_ven_uc,  fm_back_ven_tc    = fm_back_ven_tc,
    fm_back_mag_uc   = fm_back_mag_uc,  fm_back_mag_tc    = fm_back_mag_tc,
    # ── 6.4 Flow Meters – Residuals ───────────────────────────────────────────
    tot_fm_res       = tot_fm_res,      res_meter_size    = res_meter_size,
    fm_res_op_uc     = fm_res_op_uc,    fm_res_op_tc      = fm_res_op_tc,
    fm_res_prop_uc   = fm_res_prop_uc,  fm_res_prop_tc    = fm_res_prop_tc,
    fm_res_ven_uc    = fm_res_ven_uc,   fm_res_ven_tc     = fm_res_ven_tc,
    fm_res_mag_uc    = fm_res_mag_uc,   fm_res_mag_tc     = fm_res_mag_tc,
    # ── 6.5–6.11 Sensors / Alarms ─────────────────────────────────────────────
    tot_level_switch = tot_level_switch, level_switch_uc = alarm_uc,      level_switch_cost = level_switch_cost,
    tot_back_alarm   = tot_back_alarm,   back_alarm_uc   = alarm_uc,      back_alarm_cost   = back_alarm_cost,
    tot_res_alarm    = tot_res_alarm,    res_alarm_uc    = alarm_uc,      res_alarm_cost    = res_alarm_cost,
    pH_controls      = pH_controls,      pH_meter_uc     = pH_meter_uc,   pH_cost           = pH_cost,
    tot_temp_meters  = tot_temp_meters,  temp_meter_uc   = temp_meter_uc, temp_cost         = temp_cost,
    tot_turb_meters  = tot_turb_meters,  turb_meter_uc   = turb_meter_uc, turb_cost         = turb_cost,
    tot_head_sens    = tot_head_sens,    headloss_uc     = headloss_uc,   headloss_cost     = headloss_cost_val,
    # ── 6.12 Sampling Ports ───────────────────────────────────────────────────
    ports            = ports,            sampling_uc     = sampling_uc,
    ports_ss_cost    = ports_ss_cost,    ports_cs_cost   = ports_cs_cost,
    # ── 6.13 Electrical enclosure ─────────────────────────────────────────────
    elec_encl        = elec_encl,        elec_encl_uc    = elec_encl_uc,  elec_encl_cost    = elec_encl_cost,
    # ── 7 I/O counts (for diagnostics) ────────────────────────────────────────
    discrete_inputs  = discrete_inputs,  discrete_outputs = discrete_outputs,
    analog_inputs    = analog_inputs,    analog_outputs   = analog_outputs,
    # ── 7.1 PLC Units ─────────────────────────────────────────────────────────
    qty_7_1_1 = qty_7_1_1, uc_plc_rack               = uc_plc_rack,
    qty_7_1_2 = qty_7_1_2, uc_plc_cpu                = uc_plc_cpu,
    qty_7_1_3 = qty_7_1_3, uc_plc_discrete_input     = uc_plc_discrete_input,
    qty_7_1_4 = qty_7_1_4, uc_plc_discrete_output    = uc_plc_discrete_output,
    qty_7_1_5 = qty_7_1_5, uc_plc_combination_analog = uc_plc_combination_analog,
    qty_7_1_6 = qty_7_1_6, uc_plc_ethernet           = uc_plc_ethernet,
    qty_7_1_7 = qty_7_1_7, uc_plc_base_expansion     = uc_plc_base_expansion,
    qty_7_1_8 = qty_7_1_8, uc_plc_base_expansion_ctrl= uc_plc_base_expansion_ctrl,
    qty_7_1_9 = qty_7_1_9, uc_ups                    = uc_ups,
    # ── 7.2 Operator Equipment ────────────────────────────────────────────────
    tot_switches  = tot_switches,
    qty_7_2_1 = qty_7_2_1, uc_switch             = uc_switch,
    qty_7_2_2 = qty_7_2_2, uc_plc_op_interface   = uc_plc_op_interface,
    qty_7_2_3 = qty_7_2_3, uc_pc_workstation     = uc_pc_workstation,
    qty_7_2_4 = qty_7_2_4, uc_laser_printer      = uc_laser_printer,
    # ── 7.3 Controls Software ─────────────────────────────────────────────────
    qty_7_3_1 = qty_7_3_1, uc_op_interface_software  = uc_op_interface_software,
    qty_7_3_2 = qty_7_3_2, uc_plc_software           = uc_plc_software,
    qty_7_3_3 = qty_7_3_3, uc_plc_data_software      = uc_plc_data_software,
    qty_7_3_4 = qty_7_3_4, uc_plant_intel_software   = uc_plant_intel_software,
    # ── legacy total ──────────────────────────────────────────────────────────
    total_cost = fm_in_prop_tc + fm_proc_prop_tc + fm_back_prop_tc + fm_res_prop_tc +
                 level_switch_cost + back_alarm_cost + res_alarm_cost +
                 pH_cost + temp_cost + turb_cost + headloss_cost_val +
                 ports_ss_cost + elec_encl_cost +
                 qty_7_1_1*uc_plc_rack + qty_7_1_2*uc_plc_cpu +
                 qty_7_1_3*uc_plc_discrete_input + qty_7_1_4*uc_plc_discrete_output +
                 qty_7_1_5*uc_plc_combination_analog + qty_7_1_6*uc_plc_ethernet +
                 qty_7_1_7*uc_plc_base_expansion + qty_7_1_8*uc_plc_base_expansion_ctrl +
                 qty_7_1_9*uc_ups + qty_7_2_1*uc_switch + qty_7_2_2*uc_plc_op_interface +
                 qty_7_2_3*uc_pc_workstation + qty_7_2_4*uc_laser_printer +
                 qty_7_3_1*uc_op_interface_software + qty_7_3_2*uc_plc_software +
                 qty_7_3_3*uc_plc_data_software + qty_7_3_4*uc_plant_intel_software
  )
}

#' Calculate site and building costs
calculate_site_buildings <- function(include_buildings, include_hvac, include_land,
                                    retrofit, total_contactors, design_flow = 0.03,
                                    tank_geometry = "upright", piping_length_lf = 0) {
  
  # Ensure numeric parameters
  total_contactors <- safe_as_numeric(total_contactors, 1)
  design_flow <- safe_as_numeric(design_flow, 0.03)
  piping_length_lf <- safe_as_numeric(piping_length_lf, 0)
  
  # Handle logical parameters
  include_buildings <- safe_as_logical(include_buildings, FALSE)
  include_hvac <- safe_as_logical(include_hvac, FALSE)
  include_land <- safe_as_logical(include_land, FALSE)
  retrofit <- safe_as_logical(retrofit, FALSE)
  
  # Building cost — workbook OUTPUT rows 277-314
  # 14.1.1 Small Low Cost Shed: qty=1 if build1_fp>0 && build1_fp<500
  #   J277 = build1_fp * VLOOKUP(build1_fp, bldg_shed_cost_cl, 3)
  #   bldg_shed_cost_cl: fp∈[0,500) → uc=$50.7423/sf
  # 14.5 Concrete Pad: always 1 unit × $492.75/unit for non-retrofit
  #   (include_pad=1 by default per Sheet7 row 109)
  # Note: building_footprint_sf is assigned below after this block

  # For small systems: shed footprint = 30 sf (workbook default for 0.03 MGD)
  # For larger systems: rough proxy from flow (workbook would compute from vessel dimensions)
  build1_fp_est <- if (design_flow < 0.5 && !retrofit) {
    30
  } else {
    max(30, round(design_flow * 2000))
  }

  # bldg_shed_cost_cl: uc = $50.7423/sf for fp < 500 sf
  shed_uc <- 50.742289444355997
  concrete_pad_uc <- 492.74999999999994

  building_cost <- if (!retrofit) {
    if (build1_fp_est < 500) {
      # Small shed + concrete pad (workbook rows 277 + 314)
      build1_fp_est * shed_uc + concrete_pad_uc
    } else {
      # Larger building: low/mid/high quality cost equations (not yet implemented)
      # Placeholder: shed formula scaled — will be replaced with polynomial equations
      build1_fp_est * shed_uc + concrete_pad_uc
    }
  } else 0
  
  
  # Concrete pad: workbook row 314: qty=1 unit × $492.75 for non-retrofit small systems
  # (include_pad=1 default per Sheet7 row 109; already included in building_cost above)
  concrete_pad_qty <- if (design_flow < 0.5 && !retrofit) 1 else 0

  # Workbook: tech_fp = total_fp + sept_fp + ep_fp + solids_pad_area
  # For GAC app: sept_fp = ep_fp = solids_pad_area = 0; total_fp = build1_fp + build2_fp
  # build1_fp drives building footprint so we use building_footprint_sf as proxy
  # Estimate building footprint from design flow (consistent with shed/building sizing above)
  # building_footprint_sf: use same estimate as used for building cost above
  building_footprint_sf <- build1_fp_est

  # Land cost: land_required × VLOOKUP(design_flow, land_cost_cl, 3)
  # land_required: ROUNDUP(((fp^0.5 + 2*10) × (fp^0.5 + 10 + 40)) / 43560, 2) acres
  # non_fire_buffer = 10 ft, fire_buffer = 40 ft (CDA Sheet5 defaults)
  # land_cost_cl ($/acre, MGD breakpoints from Sheet23):
  {
    bps <- c(0, 0.031, 0.1241, 0.3051, 0.741, 2.1521, 7.3651, 22.6141)
    rts <- c(21165.71, 20660.02, 16458.50, 19327.22, 31219.85, 68950.85, 129311.24, 262816.68)
    idx <- max(1L, min(findInterval(design_flow, bps), length(rts)))
    land_cost_per_acre_site <- rts[idx]
  }
  # Workbook Sheet11 row83: ROUNDUP(raw_sqft, -2) / 43560
  land_req_site <- ceiling(
    ((building_footprint_sf^0.5 + 20) * (building_footprint_sf^0.5 + 50)) / 100
  ) * 100 / 43560
  land_cost <- if (include_land) land_req_site * land_cost_per_acre_site else 0

  # sitework_cost per workbook: tech_fp * sitework_sf_cl ($15.19/sf)
  sitework_sf_cl   <- 15.191208791208791
  sitework_cost_wb <- building_footprint_sf * sitework_sf_cl

  list(
    building_cost         = building_cost,
    land_cost             = land_cost,
    site_work_cost        = sitework_cost_wb,   # ← workbook formula: tech_fp × $15.19/sf
    total_cost            = building_cost + land_cost + sitework_cost_wb,
    building_footprint_sf = building_footprint_sf,
    sitework_sf_cl        = sitework_sf_cl,
    concrete_pad_qty      = concrete_pad_qty,
    concrete_pad_uc       = concrete_pad_uc,
    concrete_pad_tc       = concrete_pad_qty * concrete_pad_uc
  )
}

#' Compile all capital costs
compile_capital_costs <- function(contactors, gac, pumps, tanks, piping, controls, site,
                                  include_land = FALSE, include_permits = FALSE, include_pilot = FALSE,
                                  include_pilot_addon = TRUE,  # workbook Sheet7 default = 1
                                  retrofit = FALSE, design_flow_mgd = 1,
                                  design_type = 1,        # 1 = pressure, 2 = gravity basin
                                  component_level = NULL, # NULL = low, 1 = mid, 2 = high
                                  residuals_disposal = "potw",  # for permit calc (NPDES vs POTW)
                                  add_on = FALSE) {

  # Safe coercions
  include_land      <- safe_as_logical(include_land,    FALSE)
  include_permits   <- safe_as_logical(include_permits, FALSE)
  include_pilot     <- safe_as_logical(include_pilot,   FALSE)
  retrofit          <- safe_as_logical(retrofit,        FALSE)
  add_on            <- safe_as_logical(add_on,          FALSE)
  design_flow_mgd   <- safe_as_numeric(design_flow_mgd, 1)
  design_type       <- safe_as_numeric(design_type,     1)

  # System size category: 1 = small (<1 MGD), 2 = medium (1-10 MGD), 3 = large (>10 MGD)
  ss_cat2 <- if (design_flow_mgd < 1) 1L else if (design_flow_mgd <= 10) 2L else 3L

  # ── DIRECT COSTS ────────────────────────────────────────────────────────────
  equipment_cost <- contactors$total_cost + pumps$total_cost + tanks$total_cost

  valve_cost <- if (!is.null(piping$valve_cost)) piping$valve_cost else 0
  equipment_cost <- equipment_cost + valve_cost

  piping_material_cost  <- if (!is.null(piping$piping_material_cost))  piping$piping_material_cost  else piping$piping_cost
  piping_install_cost   <- if (!is.null(piping$piping_installation_cost)) piping$piping_installation_cost else 0
  materials_cost        <- gac$initial_fill_cost + piping_material_cost
  controls_cost         <- controls$total_cost

  site_work_cost <- if (!is.null(site$site_work_cost)) site$site_work_cost
                    else site$total_cost - (if (!is.null(site$building_cost)) site$building_cost else 0)
  building_cost  <- if (!is.null(site$building_cost)) site$building_cost else 0

  # Excel: process_cost = equipment + materials + piping_install + controls + site_work
  process_cost <- equipment_cost + materials_cost + piping_install_cost + controls_cost + site_work_cost
  # Excel: direct_cost  = process_cost + building_cost
  total_direct <- process_cost + building_cost

  message(sprintf("Process cost components:"))
  message(sprintf("  Equipment:      $%.0f (contactors=$%.0f, pumps=$%.0f, tanks=$%.0f)",
                  equipment_cost, contactors$total_cost, pumps$total_cost, tanks$total_cost))
  message(sprintf("  Materials:      $%.0f (GAC=$%.0f, piping_material=$%.0f)",
                  materials_cost, gac$initial_fill_cost, piping_material_cost))
  message(sprintf("  Piping install: $%.0f", piping_install_cost))
  message(sprintf("  Controls:       $%.0f", controls_cost))
  message(sprintf("  Site work:      $%.0f", site_work_cost))
  message(sprintf("  Valves:         $%.0f", valve_cost))
  message(sprintf("  Building:       $%.0f", building_cost))
  message(sprintf("  Total direct:   $%.0f", total_direct))

  # ── INDIRECT COSTS — aligned to Excel OUTPUT rows 331-346 ───────────────────

  # Helper: step-function VLOOKUP (first column lookup, nth column return)
  vlookup_step_local <- function(val, breakpoints, returns) {
    idx <- findInterval(val, breakpoints, rightmost.closed = FALSE)
    idx <- max(1L, min(idx, length(returns)))
    returns[idx]
  }

  # ── 331: Mobilization & Demobilization ──────────────────────────────────────
  # Excel: mob_pct_pre=0; mob_pct_small=5%; mob_pct_medium=4%; mob_pct_large=2%
  # Excluded for pre-engineered packages (design_type==1 & small system)
  mob_pct <- if (design_type == 1 && ss_cat2 == 1) {
    0        # pre-engineered: mob_pct_pre = 0
  } else {
    c(0.05, 0.04, 0.02)[ss_cat2]
  }
  mobilization <- total_direct * mob_pct

  # ── 332: Architectural Fees ──────────────────────────────────────────────────
  # Excel: VLOOKUP(building_cost, architectural_table_cl, 3) × building_cost
  # Excluded by default for small systems (include_arch_small = 0)
  include_arch <- if (ss_cat2 == 1) FALSE else TRUE
  arch_pct <- vlookup_step_local(
    building_cost,
    c(0, 250000, 500000, 1000000, 5000000, 10000000, 50000000),
    c(0.09, 0.08, 0.07, 0.062, 0.053, 0.049, 0.045)
  )
  architectural_fees <- if (include_arch) building_cost * arch_pct else 0

  # ── 333: Installation, Transportation, O&P ───────────────────────────────────
  # Excel: inst_pct = 0% (included in line item unit costs)
  installation_transp <- total_direct * 0   # inst_pct default = 0

  # ── 334: Site Work ───────────────────────────────────────────────────────────
  # Excel: sitework_cost = tech_fp × sitework_sf_cl (from Indirect sheet row 47)
  # We receive it already calculated in site$site_work_cost
  sitework_cost_indirect <- if (!retrofit) site_work_cost else 0

  # ── 335: Yard Piping ─────────────────────────────────────────────────────────
  # Excel: yard_cost = bedding + excavation + backfill + thrust block + pipe cost
  # Already calculated in site results if available, otherwise estimate by system size
  # For add-on: excluded unless include_yardpipe_addon = 1 (default 0)
  # For retrofit: excluded
  include_yardpipe_addon <- FALSE  # workbook default
  yard_piping_cost <- if (retrofit || (add_on && !include_yardpipe_addon)) {
    0
  } else {
    # Yard piping estimated from design flow: workbook small system ≈ $1,227
    # Scales with sqrt of flow (pipe length and trench scale with area)
    base_flow <- 0.03  # MGD for reference $1,227 value
    if (design_flow_mgd <= 0) 0
    else round(1227 * sqrt(design_flow_mgd / base_flow))
  }

  # ── 336: Geotechnical ────────────────────────────────────────────────────────
  # Excel: always included for gravity basins (excavation required)
  # For pressure: included only for high-cost systems (include_geo_high = 1)
  # include_geo_low = 0, include_geo_medium = 0, include_geo_high = 1
  # Excluded for add-on pressure systems and retrofit
  include_geo <- if (retrofit) FALSE
                 else if (add_on && design_type == 1) FALSE
                 else if (design_type == 2) TRUE          # gravity: always
                 else isTRUE(component_level >= 2)         # pressure: high cost only
  # Geotech cost: workbook small system ≈ $3,625; scales with footprint/flow
  geotech_cost_val <- if (include_geo) {
    if (design_flow_mgd < 0.5)       3625
    else if (design_flow_mgd < 2)    8000
    else if (design_flow_mgd < 10)   20000
    else                              50000
  } else 0

  # ── 337: Standby Power ───────────────────────────────────────────────────────
  # Excel: excluded for small systems (include_standby_small = 0)
  # excluded for add-on (include_standby_addon = 0)
  include_standby <- if (ss_cat2 == 1) FALSE
                     else if (add_on)   FALSE
                     else               TRUE
  # std_power_cost = VLOOKUP(std_power, std_gen_cost_cl, 3) × std_power
  # Workbook small system value ≈ $990 (1.5 kW generator)
  # For larger systems, scales roughly with flow
  standby_power_cost <- if (include_standby) {
    if (design_flow_mgd < 2)        2500
    else if (design_flow_mgd < 10)  8000
    else                            25000
  } else 0

  # ── 338: Electrical (including yard wiring) ──────────────────────────────────
  # Excel: elect_pct = 10%, applied to PROCESS cost (not total direct)
  electrical_cost <- process_cost * 0.10

  # ── 339: Instrumentation & Control ───────────────────────────────────────────
  # Excel: i_c_pct = 0% (included as direct cost line item)
  instrumentation_cost <- total_direct * 0

  # ── 340: Contingency ─────────────────────────────────────────────────────────
  # Excel: included only for high-cost systems by default
  # VLOOKUP(direct_cost, contingency_table_cl, 3) × complexity × direct_cost
  # include_conting_low=0, include_conting_medium=0, include_conting_high=1
  # complexity factor = 1.0 for GAC
  complexity <- 1.0
  include_conting <- if (is.null(component_level) || isTRUE(component_level < 2)) FALSE
                     else TRUE   # high cost (component_level >= 2) only
  conting_base_pct <- vlookup_step_local(
    total_direct,
    c(0, 500000, 3000000, 15000000, 50000000, 100000000),
    c(0.067, 0.058, 0.049, 0.041, 0.032, 0.058)
  )
  contingency_cost <- if (include_conting) total_direct * conting_base_pct * complexity else 0

  # ── 341: Process Engineering ─────────────────────────────────────────────────
  # Excel: eng_pct_small=20%, eng_pct_medium=12%, eng_pct_large=8% × direct_cost
  eng_pct <- c(0.20, 0.12, 0.08)[ss_cat2]
  process_engineering_cost <- total_direct * eng_pct

  # ── 342: Miscellaneous Allowance ─────────────────────────────────────────────
  # Excel: misc_pct = 10% × direct_cost
  misc_allowance_cost <- total_direct * 0.10

  # ── 343: Legal, Fiscal, and Administrative ────────────────────────────────────
  # Excel: legal_pct = 2% × direct_cost
  legal_fiscal_cost <- total_direct * 0.02

  # ── 344: Sales Tax ───────────────────────────────────────────────────────────
  # Excel: tax_pct = 0% (may be exempt if public funds)
  sales_tax_cost <- total_direct * 0

  # ── 345: Financing during Construction ───────────────────────────────────────
  # Excel: finance_pct_small = 0% (< 1 yr construction), finance_pct = 5%
  finance_pct <- if (ss_cat2 == 1) 0 else 0.05
  financing_cost <- total_direct * finance_pct

  # ── 346: Construction Management & GC Overhead ───────────────────────────────
  # Excel: cm_cost = builders_risk_ins + performance_bond + cm_fee
  # Builder's risk: br_ins_pct = 0.34% × direct_cost
  builders_risk <- total_direct * 0.0034

  # Performance bond: VLOOKUP(direct_cost, performance_bond_table_cl, 3 + 4*(direct-col5))
  # Table from Engineering Data rows 119-124
  pb_breaks   <- c(0,       100000, 500000,  2500000, 5000000, 7500000)
  pb_base     <- c(0,       2500,   8500,    28500,   47250,   64750)
  pb_rate     <- c(0.025,   0.015,  0.010,   0.0075,  0.007,   0.006)
  pb_over     <- c(0,       100000, 500000,  2500000, 5000000, 7500000)
  pb_idx           <- max(1L, findInterval(total_direct, pb_breaks))
  include_bond_flag <- 1  # include_bond default = 1 (TRUE)
  performance_bond <- include_bond_flag * (pb_base[pb_idx] + pb_rate[pb_idx] * (total_direct - pb_over[pb_idx]))

  # Construction management fee: VLOOKUP(direct_cost, construction_mgt_table_cl, 3) × direct_cost
  # Table from Engineering Data rows 65-70
  cm_pct <- vlookup_step_local(
    total_direct,
    c(0, 100000, 250000, 1000000, 5000000, 10000000),
    c(0.10, 0.09, 0.06, 0.05, 0.04, 0.032)
  )
  # include_cm = 1 by default; excluded for pre-engineered small systems (include_cm_pre = 0)
  include_cm_flag <- if (design_type == 1 && ss_cat2 == 1) 0 else 1
  cm_fee <- include_cm_flag * cm_pct * total_direct

  construction_mgmt_cost <- builders_risk + performance_bond + cm_fee

  # ── ADD-ON COSTS (OUTPUT rows 324-326) ───────────────────────────────────────
  # Workbook: include_permits * permit_cost  (Sheet16 rows 80-86)

  # Helper: BPCost_UBC97(valuation) — UBC 1997 building permit fee schedule
  # Source: Abt Associates 2020. Permitting Requirement Costs for Drinking Water Treatment Cost Models.
  # UBC 1997 building permit fee schedule — step function with ceiling on fractions
  # Source: Abt Associates 2020. Verified against workbook: BPCost_UBC97(1522.27)=57.05, ×2.6=148.33
  bpcost_ubc97 <- function(val) {
    val <- max(0, as.numeric(val))
    if (val <= 500)    return(23.50)
    if (val <= 2000)   return(23.50 + 3.05  * ceiling((val - 500)    / 100))
    if (val <= 25000)  return(69.25 + 14.00 * ceiling((val - 2000)   / 1000))
    if (val <= 50000)  return(391.25 + 10.10 * ceiling((val - 25000) / 1000))
    if (val <= 100000) return(643.75 +  7.00 * ceiling((val - 50000) / 1000))
    if (val <= 500000) return(993.75 +  5.60 * ceiling((val - 100000)/ 1000))
    return(3233.75 + 4.75 * ceiling((val - 500000) / 1000))
  }

  # UBC_mult_table_cl: VLOOKUP(design_flow, UBC_mult_table_cl, 3)
  ubc_mult <- vlookup_step_local(
    design_flow_mgd,
    c(0, 0.031, 0.1241, 0.3051, 0.741, 2.1521, 7.3651, 22.6141),
    c(2.6, 2.3, 2.2, 1.8, 1.6, 1.5, 1.5, 1.4)
  )

  # Building #1 value for permit calc = building_cost from site object
  bldg1_val <- as.numeric(site$building_cost %||% 0)

  # Row 80: Building permit = multiplier × BPCost_UBC97(building_value)
  bldg_permit <- ubc_mult * bpcost_ubc97(bldg1_val)

  # Row 81: NPDES permit — only if surface water discharge (res_s2_opt = 1)
  # res_s2_opt from params (1=surface water, 2=POTW, 3=recycle, 4=septic, 5=evap)
  res_s2_opt_num <- {
    d <- tolower(trimws(as.character(residuals_disposal %||% "potw")))
    if (grepl("surface water", d)) 1L
    else if (grepl("potw|pretreat", d)) 2L
    else if (grepl("recycle", d)) 3L
    else if (grepl("septic", d)) 4L
    else if (grepl("evap", d)) 5L
    else 2L  # default POTW
  }
  # res_flow_annual (gal/yr): approximated as backwash volume × contactors × cycles/yr
  # Use site calc or fall back to tanks$backwash_volume_gal estimate
  res_flow_annual_gal <- as.numeric(tanks$res_flow_annual_gal %||%
                           (as.numeric(tanks$backwash_volume_gal %||% 0) *
                            as.numeric(tanks$num_backwash_tanks %||% 1) * 365))
  npdes_permit <- if (res_s2_opt_num == 1L) {
    # NPDES_permit_cost_cl: avg daily discharge (gpd) → cost
    avg_daily_gpd <- res_flow_annual_gal / 365
    vlookup_step_local(avg_daily_gpd,
      c(0, 100000.1, 1000000.1),
      c(2408.05, 3036.24, 5548.98))
  } else 0

  # Row 82: POTW pretreatment permit ($2722, fixed)
  potw_permit <- if (res_s2_opt_num == 2L) 2722.14 else 0

  # Row 83: Stormwater permit — VLOOKUP(land_required, SW_permit_cost_cl, 2)
  # land_required: ROUNDUP(((fp^0.5 + 2*10) × (fp^0.5 + 10 + 40)) / 43560, 2) acres
  # non_fire_buffer = 10 ft, fire_buffer = 40 ft (CDA defaults)
  build1_fp  <- as.numeric(site$building_footprint_sf %||% 30)
  non_fire_b <- 10; fire_b <- 40
  # Workbook Sheet11 row83: ROUNDUP(raw_sqft, -2) / 43560
  # ROUNDUP(x, -2) = ceil(x/100)*100  i.e. round raw sq ft UP to nearest 100, then convert to acres
  land_req_raw_sqft  <- (build1_fp^0.5 + 2*non_fire_b) * (build1_fp^0.5 + non_fire_b + fire_b)
  land_req_acres     <- ceiling(land_req_raw_sqft / 100) * 100 / 43560
  sw_permit <- vlookup_step_local(land_req_acres,
    c(0, 1, 5),
    c(0, 1779.86, 2722.14))

  # Row 84: Risk management plan = 0 (not applicable for GAC)
  # Row 85: NEPA — default (include_NEPA=1): only if surface water discharge
  nepa_cost <- if (res_s2_opt_num == 1L) 30467.07 else 0

  permit_cost_val <- if (isTRUE(include_permits)) {
    bldg_permit + npdes_permit + potw_permit + sw_permit + nepa_cost
  } else 0

  # PILOT STUDY (OUTPUT C325)
  # Workbook: IF(add_on=1, include_pilot_addon, include_pilot) *
  #           VLOOKUP("GAC", pilot_cost_[size]_cl, 2)
  # Exact GAC values from Sheet23 pilot_cost_small/medium/large_cl:
  #   small  (ss_cat2=1): $15,793.54
  #   medium (ss_cat2=2): $70,415.24
  #   large  (ss_cat2=3): $70,415.24
  pilot_include_flag <- if (add_on) isTRUE(include_pilot_addon) else isTRUE(include_pilot)
  pilot_lookup <- c(15793.54, 70415.24, 70415.24)[min(ss_cat2, 3L)]
  pilot_cost_val <- if (pilot_include_flag) pilot_lookup else 0

  # LAND COST (OUTPUT C326)
  # Workbook: include_land * land_required * land_cost
  # land_cost = VLOOKUP(design_flow, land_cost_cl, 3) [$/acre]
  # land_cost_cl ($/acre, MGD breakpoints from Sheet23):
  land_cost_per_acre <- vlookup_step_local(
    design_flow_mgd,
    c(0, 0.031, 0.1241, 0.3051, 0.741, 2.1521, 7.3651, 22.6141),
    c(21165.71, 20660.02, 16458.50, 19327.22, 31219.85, 68950.85, 129311.24, 262816.68)
  )
  land_cost_val <- if (isTRUE(include_land) && !isTRUE(retrofit)) {
    land_req_acres * land_cost_per_acre
  } else 0

  addon_cost_total <- permit_cost_val + pilot_cost_val + land_cost_val

  # ── TOTALS ───────────────────────────────────────────────────────────────────
  total_indirect <- mobilization + architectural_fees + installation_transp +
                    sitework_cost_indirect + yard_piping_cost + geotech_cost_val +
                    standby_power_cost + electrical_cost + instrumentation_cost +
                    contingency_cost + process_engineering_cost + misc_allowance_cost +
                    legal_fiscal_cost + sales_tax_cost + financing_cost +
                    construction_mgmt_cost

  total_project <- total_direct + addon_cost_total + total_indirect

  message(sprintf("Indirect cost breakdown:"))
  message(sprintf("  Mobilization:          $%.0f (%.1f%% of direct)", mobilization, mob_pct*100))
  message(sprintf("  Architectural fees:    $%.0f", architectural_fees))
  message(sprintf("  Installation/T&O&P:    $%.0f", installation_transp))
  message(sprintf("  Site work:             $%.0f", sitework_cost_indirect))
  message(sprintf("  Yard piping:           $%.0f", yard_piping_cost))
  message(sprintf("  Geotechnical:          $%.0f", geotech_cost_val))
  message(sprintf("  Standby power:         $%.0f", standby_power_cost))
  message(sprintf("  Electrical:            $%.0f (10%% of process)", electrical_cost))
  message(sprintf("  I&C:                   $%.0f", instrumentation_cost))
  message(sprintf("  Contingency:           $%.0f", contingency_cost))
  message(sprintf("  Process engineering:   $%.0f (%.0f%% of direct)", process_engineering_cost, eng_pct*100))
  message(sprintf("  Misc allowance:        $%.0f (10%% of direct)", misc_allowance_cost))
  message(sprintf("  Legal/fiscal/admin:    $%.0f (2%% of direct)", legal_fiscal_cost))
  message(sprintf("  Sales tax:             $%.0f", sales_tax_cost))
  message(sprintf("  Financing:             $%.0f (%.0f%% of direct)", financing_cost, finance_pct*100))
  message(sprintf("  Constr mgmt/GC:        $%.0f (risk=$%.0f bond=$%.0f cm=$%.0f)",
                  construction_mgmt_cost, builders_risk, performance_bond, cm_fee))
  message(sprintf("  TOTAL INDIRECT:        $%.0f (%.0f%% of direct)", total_indirect, total_indirect/total_direct*100))
  message(sprintf("Add-on costs: $%.0f (permits=$%.0f pilot=$%.0f land=$%.0f)",
                  addon_cost_total, permit_cost_val, pilot_cost_val, land_cost_val))
  message(sprintf("TOTAL PROJECT:           $%.0f", total_project))

  list(
    # Direct
    equipment_cost          = equipment_cost,
    materials_cost          = materials_cost,
    controls_cost           = controls_cost,
    site_cost               = site_work_cost + building_cost,
    building_cost           = building_cost,
    process_cost            = process_cost,
    total_direct            = total_direct,

    # Indirect — individual line items (mirroring OUTPUT rows 331-346)
    mobilization            = mobilization,
    architectural_fees      = architectural_fees,
    installation_transp     = installation_transp,
    sitework_indirect       = sitework_cost_indirect,
    yard_piping             = yard_piping_cost,
    geotechnical            = geotech_cost_val,
    standby_power           = standby_power_cost,
    electrical              = electrical_cost,
    instrumentation         = instrumentation_cost,
    contingency             = contingency_cost,
    process_engineering     = process_engineering_cost,
    misc_allowance          = misc_allowance_cost,
    legal_fiscal            = legal_fiscal_cost,
    sales_tax               = sales_tax_cost,
    financing               = financing_cost,
    construction_mgmt       = construction_mgmt_cost,
    # Sub-components of construction_mgmt
    builders_risk           = builders_risk,
    performance_bond        = performance_bond,
    cm_fee                  = cm_fee,

    total_indirect          = total_indirect,

    # Add-on costs (separate from indirect in workbook)
    permit_cost             = permit_cost_val,
    pilot_cost              = pilot_cost_val,
    land_cost               = land_cost_val,
    land_req_acres          = land_req_acres,   # for "For X.XX acres" guidance label
    addon_cost              = addon_cost_total,

    total_project           = total_project,

    # Percent contributions (for display)
    mob_pct                 = mob_pct,
    arch_pct                = if (include_arch) arch_pct else 0,
    eng_pct                 = eng_pct,
    elect_pct               = 0.10,
    cm_pct                  = cm_pct,

    # Legacy breakdown data.frame (kept for backward compat)
    breakdown = data.frame(
      Category = c("GAC Contactors", "Tanks", "Piping & Valves", "Pumps",
                   "GAC Media", "Controls", "Site Work"),
      Cost = c(contactors$total_cost, tanks$total_cost,
               if (!is.null(piping$total_cost)) piping$total_cost else piping$piping_cost,
               pumps$total_cost, gac$initial_fill_cost, controls$total_cost,
               site_work_cost + building_cost)
    )
  )
}


#' Calculate annual O&M costs
calculate_om_costs <- function(
    design_flow_mgd,       # design flow (MGD)  — for labor_table_cl lookup
    average_flow_mgd,      # average flow (MGD)  — for POTW fees
    gac_results,           # from calculate_gac_requirements
    pump_results,          # from calculate_pumps
    contactor_results,     # from calculate_contactors — for filter_materials
    tank_results,          # from calculate_tanks — for res_flow_annual_gal
    site_results,          # from calculate_site_buildings — for building_footprint_sf
    regen_type  = "regeneration off-site (non-hazardous)",
    design_type = 1,       # 1=pressure, 2=gravity
    automation_level = "fully automated",
    residuals_disposal = "potw",
    retrofit    = FALSE,
    backwash_interval = 168,  # hrs between backwashes
    num_trains  = NULL,    # passed from calculate_gac_system (pressure design)
    total_num_basins = NULL  # passed from calculate_gac_system (gravity design)
  ) {

  # ── Helpers ──────────────────────────────────────────────────────────────────
  safe_n <- function(x, default = 0) {
    v <- suppressWarnings(as.numeric(x))
    if (is.null(v) || length(v) == 0 || is.na(v)) default else v
  }
  vlookup_step <- function(val, bps, returns) {
    idx <- max(1L, min(findInterval(val, bps), length(returns)))
    returns[idx]
  }

  # Normalise inputs
  design_flow_mgd   <- safe_n(design_flow_mgd, 0.03)
  average_flow_mgd  <- safe_n(average_flow_mgd, design_flow_mgd)
  retrofit          <- isTRUE(retrofit)
  backwash_interval <- safe_n(backwash_interval, 168)

  regen_code <- {
    r <- tolower(trimws(as.character(regen_type %||% "")))
    if (grepl("on.?site|onsite", r)) 1L
    else if (grepl("off.?site.*non|non.*haz", r)) 2L
    else if (grepl("throw|replac|virgin", r)) 3L
    else if (grepl("off.?site.*haz|haz.*off", r)) 4L
    else 2L  # default off-site non-haz
  }

  # system size category
  ss_cat2 <- if (design_flow_mgd < 1) 1L else if (design_flow_mgd <= 10) 2L else 3L

  # manual = 0 (fully automated), 1 (manual), 2 (semi-automated)
  manual <- {
    al <- tolower(trimws(as.character(automation_level %||% "")))
    if (grepl("manual", al)) 1L
    else if (grepl("semi", al)) 2L
    else 0L
  }

  # ── Derived counts (workbook O&M sheet rows 11-20) ───────────────────────────
  # instruments, pumps, valves — pulled from sub-results where available
  tot_fm_in   <- safe_n(contactor_results$tot_fm_in,   1)
  tot_fm_proc <- safe_n(contactor_results$tot_fm_proc, 0)
  tot_fm_bw   <- safe_n(contactor_results$tot_fm_bw,   0)
  tot_level_switch <- safe_n(contactor_results$tot_level_switch, 0)
  tot_turb    <- safe_n(contactor_results$tot_turb_meters, 0)
  tot_head    <- safe_n(contactor_results$tot_head_sens, 0)
  pH_ctrl     <- safe_n(contactor_results$pH_controls, 0)
  tot_temp    <- safe_n(contactor_results$tot_temp_sensors, 0)
  tot_sampling <- safe_n(contactor_results$tot_sampling_ports, 0)
  # O&M row 12: tot_inst = instruments for daily observation
  tot_inst      <- max(1, tot_fm_in + tot_fm_proc + tot_level_switch + tot_turb +
                          tot_head + pH_ctrl + tot_temp + tot_sampling)
  # O&M row 13: tot_inst_maint = all instruments for PM (adds backwash FM)
  tot_inst_maint <- tot_inst + tot_fm_bw

  booster_pumps  <- safe_n(pump_results$num_booster_pumps,  0)
  backwash_pumps <- safe_n(pump_results$num_backwash_pumps, 0)
  res_pumps      <- safe_n(pump_results$num_residuals_pumps, 0)
  tot_pumps      <- booster_pumps
  # PM pumps: large, frequently operated only (backwash if interval <= 1 week)
  tot_pumps_maint <- booster_pumps + res_pumps +
                     if (backwash_interval <= 24*7) backwash_pumps else 0

  # MOV + manual process valves from piping — fallback to default
  tot_proc_MOVs <- safe_n(contactor_results$tot_proc_MOVs, 2)
  tot_bp_MOVs   <- safe_n(contactor_results$tot_bp_MOVs,   0)
  tot_proc_man  <- safe_n(contactor_results$tot_proc_man,  2)
  tot_bp_man    <- safe_n(contactor_results$tot_bp_man,    2)
  tot_valves    <- tot_proc_MOVs + tot_bp_MOVs + tot_proc_man + tot_bp_man

  # Treatment line / basin counts — use values passed directly from calculate_gac_system
  # to avoid depending on fields that sub-results don't expose
  num_treat_lines <- if (!is.null(num_trains) && !is.na(suppressWarnings(as.numeric(num_trains)))) {
    as.numeric(num_trains)
  } else {
    safe_n(contactor_results$total_contactors, 2)
  }

  total_num_basins_val <- if (!is.null(total_num_basins) && !is.na(suppressWarnings(as.numeric(total_num_basins)))) {
    as.numeric(total_num_basins)
  } else if (design_type == 2) {
    2  # fallback for gravity when not passed
  } else {
    safe_n(contactor_results$total_contactors, 2)
  }

  final_num_tanks <- if (design_type == 2) total_num_basins_val else
                       safe_n(contactor_results$total_contactors, 2)

  # O&M row 17: backwashes per year
  backwash_yr <- final_num_tanks * (365 * 24 / backwash_interval)

  # chemical supply tanks (coagulant/polymer)
  tot_chem <- 0  # default: no chemical feed for basic GAC

  # O&M rows 19-20: media change-outs
  disp_freq   <- safe_n(gac_results$disposal_freq, 0)
  media_changes    <- num_treat_lines * disp_freq   # approx
  GAC_makeup_cf    <- safe_n(gac_results$GAC_makeup_cf, 0)
  GAC_yr_cf        <- safe_n(gac_results$GAC_yr_cf, GAC_makeup_cf)
  spent_media_yr_cf <- GAC_yr_cf - GAC_makeup_cf
  # throwaway: both removal + refill
  throwaway <- regen_code %in% c(3L, 5L, 6L, 7L)
  media_changes_cf <- if (throwaway) GAC_makeup_cf + spent_media_yr_cf * 1
                      else if (regen_code %in% c(2L, 4L)) GAC_makeup_cf + spent_media_yr_cf
                      else GAC_makeup_cf

  # ── O&M assumption constants (from O&M Assumptions sheet) ──────────────────
  uloe_inst       <- 5    # min/instrument/day (manual)
  uloe_inst_auto  <- 5    # min/day (automated)
  uloe_inst_maint <- 10   # min/instrument/month (PM)
  uloe_pump       <- 5    # min/pump/day
  uloe_pump_maint <- 30.25 # hrs/pump/year
  uloe_valve      <- 5    # min/valve/week
  uloe_valve_maint <- 5   # min/valve/year
  uloe_fp         <- 1    # min/100sf/day
  uloe_autoback   <- 1    # min/event (automated)
  uloe_throwaway  <- 8/(20*27)  # hrs/cf
  backwash_time   <- 30   # minutes per backwash event (default)
  Manager_percent  <- 0.10
  Clerical_percent <- 0.10
  Weekly_labor    <- 1    # hr/week/basin (gravity)

  # ── General Operator Labor (O&M rows 22-32) ─────────────────────────────────
  # Row 22: Record parameters
  op_record <- if (manual == 1) tot_inst * uloe_inst * 365/60 else uloe_inst_auto * 365/60
  # Row 23: PM instruments
  op_pm_inst <- tot_inst_maint * uloe_inst_maint * 12/60
  # Row 24: Verify/adjust pumps
  op_pumps <- if (manual %in% c(1L, 2L)) tot_pumps * uloe_pump * 365/60 else 0
  # Row 25: PM pumps
  op_pm_pumps <- tot_pumps_maint * uloe_pump_maint
  # Row 26: Verify/adjust valves
  op_valves <- if (manual %in% c(1L, 2L)) tot_valves * uloe_valve * 52/60 else 0
  # Row 27: PM valves
  op_pm_valves <- tot_valves * uloe_valve_maint / 60
  # Row 28: Visual inspection
  total_fp <- safe_n(site_results$building_footprint_sf, 30)
  op_inspect <- total_fp / 100 * uloe_fp * 365/60
  # Row 29: Chemical supply
  op_chem <- tot_chem * 120 * 12/60
  # Row 30: Managing backwash
  op_backwash <- if (manual %in% c(1L, 2L)) backwash_yr * 2 * backwash_time/60
                 else backwash_yr * uloe_autoback/60
  # Row 31: Managing media changeouts
  transfer_method <- 2  # default: mechanical
  transfer_time   <- 2  # hrs/changeout for mechanical
  op_media <- if (transfer_method == 3) media_changes_cf * uloe_throwaway
              else media_changes * transfer_time
  # Row 32: Managing residual solids (simplified)
  op_res_solids <- 0  # default: no solid handling labour (POTW discharge)

  # Row 33: General_Oper_Labor
  General_Oper_Labor <- op_record + op_pm_inst + op_pumps + op_pm_pumps +
                        op_valves + op_pm_valves + op_inspect + op_chem +
                        op_backwash + op_media + op_res_solids

  # Row 36: filter_labor (gravity basin maintenance, 1 hr/week/basin)
  filter_labor <- if (design_type == 2) Weekly_labor * 52 * total_num_basins_val else 0

  # Row 37: regen_labor (on-site only)
  regen_capacity <- safe_n(gac_results$regen_capacity, 0)
  regen_labor <- if (regen_code == 1L && regen_capacity > 0)
                   2.2515 * regen_capacity^0.7554 else 0

  # Row 39: Operator_LOE
  Operator_LOE <- General_Oper_Labor + regen_labor + filter_labor
  # Row 40-41: Manager/Clerical LOE
  Manager_LOE  <- Operator_LOE * Manager_percent
  Clerical_LOE <- Operator_LOE * Clerical_percent

  # ── Labor unit costs (labor_table_cl) ───────────────────────────────────────
  # Columns: [breakpoint, design_flow_needed, oper_rate, mgr_rate, cler_rate]
  # From Cost Data row 1313 (single row for small systems; same for 0.03 MGD)
  labor_bps <- c(0, 0.031, 0.1241, 0.3051, 0.741, 2.1521, 7.3651, 22.6141)
  oper_rates <- c(34.161577540106947, 34.161577540106947, 34.161577540106947,
                  34.161577540106947, 36.431310160427806, 38.459358288770055,
                  40.151149732620318, 46.928823529411758)
  mgr_rates  <- c(50.648663101604278, 50.648663101604278, 50.648663101604278,
                  50.648663101604278, 57.930721925133689, 64.529759358288771,
                  75.300481283422457, 80.449411764705886)
  cler_rates <- c(32.90061497326203, 32.90061497326203, 32.90061497326203,
                  32.90061497326203, 32.90061497326203, 42.483930481283423,
                  42.483930481283423, 42.483930481283423)
  oper_uc  <- vlookup_step(design_flow_mgd, labor_bps, oper_rates)
  mgr_uc   <- vlookup_step(design_flow_mgd, labor_bps, mgr_rates)
  cler_uc  <- vlookup_step(design_flow_mgd, labor_bps, cler_rates)

  # O&M OUTPUT rows 365-367
  labor_manager  <- if (!retrofit) Manager_LOE  * mgr_uc  else 0
  labor_clerical <- if (!retrofit) Clerical_LOE * cler_uc else 0
  labor_operator <- if (Operator_LOE > 0) Operator_LOE * oper_uc else 0

  # ── Materials (OUTPUT rows 369-373) ──────────────────────────────────────────
  booster_pump_maint_pct <- 0.01
  # Row 369: booster pump materials
  booster_pump_tc <- safe_n(pump_results$booster_pump_tc, 0)
  pump_mtl <- booster_pump_tc * booster_pump_maint_pct

  # Row 370: backwash pump materials (only if high frequency: ≤1 week)
  backwash_pump_tc <- safe_n(pump_results$backwash_pump_tc, 0)
  back_pump_mtl <- if (backwash_interval <= 24*7 && backwash_pump_tc > 0)
                     backwash_pump_tc * booster_pump_maint_pct else 0

  # Row 371: residuals pump materials
  res_pump_tc <- safe_n(pump_results$res_pump_tc, 0)
  res_pump_mtl <- res_pump_tc * booster_pump_maint_pct

  # Row 372: filter_materials = Materials_pct × SUMPRODUCT(contactor J costs)
  # Materials_pct = 1% of contactor capital
  Materials_pct <- 0.01
  contactor_capital <- safe_n(contactor_results$total_cost, 0)
  filter_materials <- Materials_pct * contactor_capital

  # Row 373: on-site regeneration materials
  regen_materials <- if (regen_code == 1L && regen_capacity > 0)
                       8451.4 * regen_capacity^0.2017 else 0

  # ── Building & HVAC maintenance (OUTPUT row 374) ─────────────────────────────
  # O&M row 44: IF(include_buildings=0, 0, building_matl_rate_cl × total_fp)
  building_matl_rate_cl <- 6.4790285359180011  # $/sf/yr (Cost Data row 1268)
  include_buildings <- !is.null(site_results$building_cost) && site_results$building_cost > 0
  bldg_maint_cost <- if (include_buildings) building_matl_rate_cl * total_fp else 0

  # ── GAC media costs (OUTPUT rows 376-377) ─────────────────────────────────
  # These come directly from gac_results (already computed via workbook CE equations)
  GAC_makeup_lbs    <- safe_n(gac_results$GAC_makeup_lbs, GAC_makeup_cf * 30)
  replace_gac_uc    <- safe_n(gac_results$replace_gac_uc, 0)
  makeup_gac_cost   <- GAC_makeup_lbs * replace_gac_uc

  regen_yr_lbs      <- safe_n(gac_results$regen_yr_lbs, 0)
  off_regen_uc      <- safe_n(gac_results$off_regen_uc, 0)
  off_regen_cost    <- regen_yr_lbs * off_regen_uc

  # ── Energy (OUTPUT rows 381-387) ─────────────────────────────────────────────
  energy_cost_cl <- 0.11001750924784216  # $/kWh (Cost Data row 1277)
  gas_cost_cl    <- 0.57856140350877194  # $/therm (Cost Data row 1280)

  # Row 381: booster pump energy
  pump_energy_mwh <- safe_n(pump_results$pump_energy_mwh, 0)
  pump_energy_cost <- pump_energy_mwh * energy_cost_cl * 1000  # MWh→kWh

  # Row 382: backwash pump energy
  back_pump_hp   <- safe_n(pump_results$back_pump_hp, 0)
  back_hrs_yr    <- final_num_tanks * backwash_time/60 * (365*24/backwash_interval)
  back_pump_energy_mwh <- back_pump_hp * 0.7457/1000 * back_hrs_yr
  back_pump_energy_cost <- if (back_pump_energy_mwh > 0) back_pump_energy_mwh * energy_cost_cl * 1000 else 0

  # Row 383: residuals pump energy
  res_pump_energy_mwh <- safe_n(pump_results$res_pump_energy_mwh, 0)
  res_pump_energy_cost <- if (res_pump_energy_mwh > 0) res_pump_energy_mwh * energy_cost_cl * 1000 else 0

  # Row 386: Lighting
  # lighting = IF(include_buildings=0, 0, MIN(1, Operator_LOE/(24*365)) * build1_fp * 2.628 / 1000) MWh/yr
  lighting_power_low <- 2.6279999999999997  # kWh/sf/yr for small buildings
  lighting_mwh <- if (include_buildings) {
    min(1, Operator_LOE / (24*365)) * total_fp * lighting_power_low / 1000
  } else 0
  lighting_cost <- lighting_mwh * energy_cost_cl * 1000

  # Row 387: Ventilation
  # ventilation: from HVAC sheet, simplified here
  # For small systems: shed with no mechanical ventilation or minimal
  # Workbook: ventilation = MIN(1, Operator_LOE/(24*365)) * (build1_fp * air_changes * ceiling_ht / 60 / 1000) * fan_days
  air_changes_contactor <- 3   # air changes/hr
  ceiling_ht_ft <- 10          # assumed
  fan_days <- if (ss_cat2 == 1) 90 else if (ss_cat2 == 2) 120 else 185
  # Fan power: air_cfm * fan_eff * 0.7457/1000 kW, simplified to volume-based
  ventilation_mwh <- if (include_buildings) {
    cfm <- total_fp * ceiling_ht_ft * air_changes_contactor / 60
    # Fan power at 0.5" WG: ~0.0001 kW/cfm (rough)
    fan_kw <- cfm * 0.0001
    fan_kw * fan_days * 24 / 1000  # MWh/yr
  } else 0
  # Use workbook value as better estimate: lighting+ventilation combo
  # Workbook for 0.03 MGD: lighting=0.000365 MWh, ventilation=0.01143 MWh
  # Fallback to the workbook Operator_LOE fraction method
  ventilation_mwh <- if (include_buildings) {
    min(1, Operator_LOE / (24*365)) * total_fp * air_changes_contactor *
      ceiling_ht_ft / 60 * fan_days * 24 / 1e6
  } else 0
  ventilation_cost <- ventilation_mwh * energy_cost_cl * 1000

  total_energy_cost <- pump_energy_cost + back_pump_energy_cost + res_pump_energy_cost +
                       lighting_cost + ventilation_cost

  # ── Residuals disposal (OUTPUT rows 399-402) ──────────────────────────────────
  res_s2_opt <- {
    d <- tolower(trimws(as.character(residuals_disposal %||% "potw")))
    if (grepl("surface water|npdes", d)) 1L
    else if (grepl("potw|pretreat", d)) 2L
    else if (grepl("recycle", d)) 3L
    else if (grepl("septic", d)) 4L
    else if (grepl("evap", d)) 5L
    else 2L
  }
  res_flow_annual_gal <- safe_n(tank_results$res_flow_annual_gal, 0)
  # Row 399: POTW discharge fee = POTW_fee
  # POTW_fee = base_cost_avg/month × 12 + vol_cost_avg/1000gal × res_flow_annual_gal/1000
  # Workbook uses "average" rates (Cost Data rows 1548, 1560)
  potw_base_monthly <- 17.200849956393093  # $/month (average)
  potw_vol_rate     <- 5.5573636935648949  # $/1000 gal (average)
  potw_fee <- if (res_s2_opt == 2L && res_flow_annual_gal > 0) {
    (potw_base_monthly * 12) + (res_flow_annual_gal / 1000 * potw_vol_rate)
  } else 0

  # ── Misc allowance (OUTPUT row 403) ─────────────────────────────────────────
  # misc_pct = 10% applied to sum of all included O&M items
  misc_pct <- 0.10
  # Workbook: SUMPRODUCT(G365:G402, H365:H402) — sum each item × its Use? flag
  subtotal_before_misc <-
    (if (!retrofit)             labor_manager         else 0) +
    (if (!retrofit)             labor_clerical        else 0) +
    (if (labor_operator > 0)    labor_operator        else 0) +
    (if (pump_mtl > 0)          pump_mtl              else 0) +
    (if (back_pump_mtl > 0)     back_pump_mtl         else 0) +
    (if (res_pump_mtl > 0)      res_pump_mtl          else 0) +
    (if (filter_materials > 0)  filter_materials      else 0) +
    (if (regen_code == 1L)      regen_materials       else 0) +
    (if (bldg_maint_cost > 0)   bldg_maint_cost       else 0) +
    makeup_gac_cost +
    (if (regen_code %in% c(2L, 4L)) off_regen_cost    else 0) +
    (if (pump_energy_cost > 0)       pump_energy_cost  else 0) +
    (if (back_pump_energy_cost > 0)  back_pump_energy_cost else 0) +
    (if (res_pump_energy_cost > 0)   res_pump_energy_cost  else 0) +
    (if (lighting_cost > 0)     lighting_cost         else 0) +
    (if (ventilation_cost > 0)  ventilation_cost      else 0) +
    (if (res_s2_opt == 2L && !retrofit) potw_fee      else 0)
  misc_allowance <- misc_pct * subtotal_before_misc

  # ── Total (OUTPUT row 404) ────────────────────────────────────────────────────
  total_annual <- (subtotal_before_misc + misc_allowance)

  message("O&M cost breakdown (workbook-aligned):")
  message(sprintf("  Labor — Manager:   $%.2f (%.2f hrs × $%.2f/hr)", labor_manager, Manager_LOE, mgr_uc))
  message(sprintf("  Labor — Clerical:  $%.2f (%.2f hrs × $%.2f/hr)", labor_clerical, Clerical_LOE, cler_uc))
  message(sprintf("  Labor — Operator:  $%.2f (%.2f hrs × $%.2f/hr)", labor_operator, Operator_LOE, oper_uc))
  message(sprintf("  Materials — booster pumps: $%.2f", pump_mtl))
  message(sprintf("  Materials — backwash pumps: $%.2f", back_pump_mtl))
  message(sprintf("  Materials — residuals pumps: $%.2f", res_pump_mtl))
  message(sprintf("  Materials — GAC contactors (filter): $%.2f", filter_materials))
  message(sprintf("  Materials — on-site regen: $%.2f", regen_materials))
  message(sprintf("  Building/HVAC maintenance: $%.2f (%g sf × $%.4f/sf/yr)", bldg_maint_cost, total_fp, building_matl_rate_cl))
  message(sprintf("  Makeup GAC:        $%.2f (%.2f lbs × $%.4f/lb)", makeup_gac_cost, GAC_makeup_lbs, replace_gac_uc))
  message(sprintf("  Off-site regen:    $%.2f (%.2f lbs × $%.4f/lb)", off_regen_cost, regen_yr_lbs, off_regen_uc))
  message(sprintf("  Energy — booster pumps: $%.2f", pump_energy_cost))
  message(sprintf("  Energy — backwash pumps: $%.2f", back_pump_energy_cost))
  message(sprintf("  Energy — residuals pumps: $%.2f", res_pump_energy_cost))
  message(sprintf("  Energy — lighting: $%.2f", lighting_cost))
  message(sprintf("  Energy — ventilation: $%.2f", ventilation_cost))
  message(sprintf("  POTW discharge fee: $%.2f", potw_fee))
  message(sprintf("  Misc allowance (10%%): $%.2f", misc_allowance))
  message(sprintf("  TOTAL ANNUAL O&M:  $%.2f", total_annual))

  list(
    # ── Labour ──────────────────────────────────────────────────────────────
    labor_manager    = labor_manager,
    labor_clerical   = labor_clerical,
    labor_operator   = labor_operator,
    labor_total      = labor_manager + labor_clerical + labor_operator,
    Manager_LOE      = Manager_LOE,
    Clerical_LOE     = Clerical_LOE,
    Operator_LOE     = Operator_LOE,
    oper_uc          = oper_uc,
    mgr_uc           = mgr_uc,
    cler_uc          = cler_uc,

    # ── Materials ────────────────────────────────────────────────────────────
    pump_mtl         = pump_mtl,
    back_pump_mtl    = back_pump_mtl,
    res_pump_mtl     = res_pump_mtl,
    filter_materials = filter_materials,
    regen_materials  = regen_materials,
    materials_total  = pump_mtl + back_pump_mtl + res_pump_mtl + filter_materials + regen_materials,

    # ── Building maintenance ─────────────────────────────────────────────────
    bldg_maint_cost  = bldg_maint_cost,

    # ── GAC media ────────────────────────────────────────────────────────────
    makeup_gac_cost  = makeup_gac_cost,
    off_regen_cost   = off_regen_cost,
    gac_media_total  = makeup_gac_cost + off_regen_cost,
    GAC_makeup_lbs   = GAC_makeup_lbs,
    replace_gac_uc   = replace_gac_uc,
    regen_yr_lbs     = regen_yr_lbs,
    off_regen_uc     = off_regen_uc,

    # ── Energy ───────────────────────────────────────────────────────────────
    pump_energy_cost      = pump_energy_cost,
    back_pump_energy_cost = back_pump_energy_cost,
    res_pump_energy_cost  = res_pump_energy_cost,
    lighting_cost         = lighting_cost,
    ventilation_cost      = ventilation_cost,
    energy_total          = total_energy_cost,
    energy_cost_cl        = energy_cost_cl,

    # ── Residuals ────────────────────────────────────────────────────────────
    potw_fee         = potw_fee,
    res_flow_annual_gal = res_flow_annual_gal,

    # ── Misc & total ─────────────────────────────────────────────────────────
    misc_allowance   = misc_allowance,
    subtotal         = subtotal_before_misc,
    total_annual     = total_annual,

    # Display helper
    total_fp           = total_fp,

    # Legacy aliases for backward compat
    energy_annual      = total_energy_cost,
    labor_annual       = labor_manager + labor_clerical + labor_operator,
    maintenance_annual = pump_mtl + back_pump_mtl + res_pump_mtl + filter_materials + regen_materials + bldg_maint_cost,
    chemical_annual    = 0,
    monitoring_annual  = 0,
    gac_replacement_annual = makeup_gac_cost + off_regen_cost
  )
}

#' Helper function to convert bed life to EBCT
bed_life_to_ebct <- function(bed_life, flow_mgd) {
  # Simplified conversion - replace with actual model
  ebct <- bed_life / (flow_mgd * 1000)
  max(5, min(60, ebct))
}
