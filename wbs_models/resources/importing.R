### Drive Data Access Functions ----

get_freundlich <- function() {
  
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  
  freundlich <- data.frame(googlesheets4::read_sheet(URL, sheet="freundlich_reference"))
  
  freundlich <- freundlich |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        !c(contaminant, cas_number),
        as.numeric
    )
  )
  
  return(freundlich)
}

get_atrazine_and_cyanazine_data <- function() {
  
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"

  atrazine_and_cyanazine_data <- data.frame(googlesheets4::read_sheet(URL, sheet="atrazine_and_cyanazine_data", skip = 1))
  
  atrazine_and_cyanazine_data <- atrazine_and_cyanazine_data |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
    )
  )
  
  return(atrazine_and_cyanazine_data)
}

get_critical_design_assumptions_data <- function() {
  
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"

  critical_design_assumptions <- data.frame(googlesheets4::read_sheet(URL, sheet="critical_design_assumptions"))
  
  critical_design_assumptions <- critical_design_assumptions |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
    )
  )
  
  return(critical_design_assumptions)
}

get_direct_capital_costs_baseline <- function() {
  
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"

  direct_capital_costs_baseline <- data.frame(googlesheets4::read_sheet(URL, sheet="direct_capital_costs_baseline"))
  
  direct_capital_costs_baseline <- direct_capital_costs_baseline |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
    )
  )
  
  return(direct_capital_costs_baseline)
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

# Get standard inputs as a named list
get_standard_inputs <- function(contam_selection, design_type, design_number) {
  URL <- "https://docs.google.com/spreadsheets/d/1usWl2SuplV5IAXYgnzUvs4KmaLImTeZdTFDE4OXHpH0/"
  
  standard_inputs <- data.frame(googlesheets4::read_sheet(URL, sheet = "standard_inputs")) |>
    janitor::clean_names()
  
  # Filter to get the matching row (before converting to character)
  matching_row <- standard_inputs |>
    dplyr::filter(
      contaminant_selection == contam_selection,
      design_type == design_type,
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
