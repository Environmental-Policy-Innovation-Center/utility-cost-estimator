# WBS Table Population Logic

resolve_priority_col <- function(size_selection, cost_selection) {
  
  size_key <- dplyr::case_when(
    grepl("small",  size_selection, ignore.case = TRUE) ~ "small",
    grepl("medium", size_selection, ignore.case = TRUE) ~ "medium",
    grepl("large",  size_selection, ignore.case = TRUE) ~ "large",
    .default = "small"
  )
  
  # cost_selection is already lowered ("low" / "mid" / "high")
  paste0(size_key, "_", cost_selection)          # e.g. "small_low"
}

# ---------------------------------------------------------------------------
# Helper: derive applicability flags from calculation results
# ---------------------------------------------------------------------------
# Returns a named logical list — TRUE means the WBS section IS applicable.
#
# Mirrors the applicability rules from the WBS mapping document and from
# the OUTPUT sheet formula analysis (design_type, regen_type, res_s2_opt, etc.)
# ---------------------------------------------------------------------------
derive_applicability <- function(params, contactors, tanks, piping, pumps, gac, controls, chem_feed, site) {
  
  is_pressure <- !isTRUE(params$tank_geometry == "basin")
  is_basin    <- isTRUE(params$tank_geometry == "basin")
  
  # Coagulation active? (ferric chloride / polymer sections)
  # No coag_res field yet in calculations.R — derive from residuals_disposal or default FALSE
  coag_active <- isTRUE(params$coag_res == 1) ||
                 isTRUE(grepl("coag", tolower(params$residuals_disposal %||% "")))
  
  # Bypass active?
  bp_active <- isTRUE(as.numeric(params$bp_pct %||% 0) > 0)
  
  # Backwash tanks present?
  back_tanks_present <- isTRUE(tanks$num_backwash_tanks > 0)
  
  # Residuals holding tanks present?
  res_holding_present <- isTRUE(tanks$num_residuals_tanks > 0)
  
  # Residuals disposal method
  res_s2_opt <- tolower(params$residuals_disposal %||% "potw")
  res_septic  <- grepl("septic",    res_s2_opt)
  res_evap    <- grepl("evaporation|evap pond", res_s2_opt)
  
  # On-site regeneration?
  regen_onsite <- grepl("on-site|onsite", tolower(params$regen_type %||% "off-site"))
  
  # Retrofit?
  is_retrofit <- isTRUE(params$retrofit)
  
  # System automation (manual = no PLC)
  is_manual <- grepl("manual", tolower(params$automation_level %||% "fully automated"))
  
  # System size category (mirrors ss_cat in workbook)
  design_flow_mgd <- as.numeric(params$design_flow %||% 0)
  is_small  <- design_flow_mgd < 1
  is_medium <- design_flow_mgd >= 1  && design_flow_mgd <= 10
  is_large  <- design_flow_mgd > 10
  
  # Pumps present?
  booster_present  <- isTRUE(pumps$service_pumps   > 0)
  backwash_pump_present <- isTRUE(pumps$backwash_pumps > 0)
  res_pump_present      <- isTRUE(pumps$residuals_pumps > 0)

  # building_footprint_sf is set by calculate_site_buildings (default 30 sf for small system)
  build1_fp <- as.numeric(site$building_footprint_sf %||% 30)
  cost_lv   <- tolower(trimws(as.character(contactors$component_level_name %||% "low")))
  is_shed_fp <- build1_fp > 0 && build1_fp < 500   # < 500 sf → shed
  
  list(
    # 1. GAC Contactors
    wbs_1_1_pressure_vessels = is_pressure,   # Pressure vessel rows
    wbs_1_2_gac_basins       = is_basin,      # Basin rows (concrete, internals, etc.)
    
    # 2. Tanks
    wbs_2_1_backwash_tanks   = back_tanks_present,
    wbs_2_2_residuals_tanks  = res_holding_present,
    wbs_2_3_ferric_tanks     = coag_active,
    wbs_2_4_polymer_tanks    = coag_active,
    
    # 3. Piping — process/backwash/influent/residuals always present; others conditional
    wbs_3_1_process_piping   = TRUE,
    wbs_3_2_backwash_piping  = TRUE,
    wbs_3_3_influent_piping  = TRUE,
    wbs_3_4_residuals_piping = TRUE,
    wbs_3_5_ferric_piping    = coag_active,
    wbs_3_6_polymer_piping   = coag_active,
    wbs_3_7_bypass_piping    = bp_active,
    
    # 4. Valves — process/backwash/influent always present; others conditional
    wbs_4_1_1_process_mov    = TRUE,
    wbs_4_1_2_backwash_mov   = TRUE,
    wbs_4_1_3_residuals_mov  = res_pump_present || res_holding_present,
    wbs_4_1_4_ferric_mov     = coag_active,
    wbs_4_1_5_polymer_mov    = coag_active,
    wbs_4_1_6_bypass_mov     = bp_active,
    wbs_4_2_1_influent_man   = TRUE,
    wbs_4_2_2_process_man    = TRUE,
    wbs_4_2_3_backwash_man   = TRUE,
    wbs_4_2_4_residuals_man  = res_pump_present || res_holding_present,
    wbs_4_2_5_ferric_man     = coag_active,
    wbs_4_2_6_polymer_man    = coag_active,
    wbs_4_2_7_bypass_man     = bp_active,
    wbs_4_3_1_backwash_chv   = TRUE,
    # Workbook: tot_res_chv = res_chv_pump*res_pumps + IF(res_pumps=0, res_chv_no_pump(=1), 0)
    # Always >= 1 — residuals line always has a check valve even without holding tank/pumps.
    wbs_4_3_2_residuals_chv  = TRUE,
    wbs_4_3_3_ferric_chv     = coag_active,
    wbs_4_3_4_polymer_chv    = coag_active,
    wbs_4_3_5_influent_chv   = TRUE,
    wbs_4_3_6_bypass_chv     = bp_active,
    
    # 5. Pumps
    wbs_5_1_booster          = booster_present,
    wbs_5_2_backwash         = backwash_pump_present,
    wbs_5_3_residuals        = res_pump_present,
    wbs_5_4_ferric_pumps     = coag_active,
    wbs_5_5_polymer_pumps    = coag_active,
    
    # 6. Instrumentation — always present (qty may be 0 for small systems)
    wbs_6_all                = TRUE,
    
    # 7. System Controls
    wbs_7_plc                = !is_manual,
    wbs_7_2_1_drive          = !is_manual,
    wbs_7_2_2_interface      = !is_manual,
    wbs_7_2_3_workstations   = !is_manual && !is_small,
    wbs_7_2_4_printers       = !is_manual && !is_small,
    wbs_7_3_software         = !is_manual && !is_small,
    
    # 8. Chemical Feed
    wbs_8_1_gac_transfer     = TRUE,       # always present (manual transfer = qty 0)
    wbs_8_2_residuals_mixers = coag_active && res_holding_present,
    wbs_8_4_ferric_mixers    = coag_active,
    wbs_8_5_polymer_mixers   = coag_active,
    
    # 9. Media
    wbs_9_1_gac_charge       = TRUE,
    
    # 10. On-site Regeneration
    wbs_10_regen             = regen_onsite,
    
    # 11-13. Residuals disposal structures (mutually exclusive)
    wbs_11_septic            = res_septic,
    wbs_12_drying_pad        = !res_evap && !res_septic,
    wbs_13_evap_pond         = res_evap,
    
    # 14. Buildings
    # Sub-type flags mirror workbook OUTPUT column L (Use?) for 14.1.1 rows:
    #   Shed row  (row 277): C = IF(build1_fp>0 AND build1_fp<500, 1, 0)
    #   Low/Mid/High (rows 278-280): C = IF(build1_fp>=500, 1, 0)

    wbs_14_buildings         = TRUE,
    wbs_14_1_shed            = is_shed_fp,
    wbs_14_1_low             = (!is_shed_fp) && grepl("low",  cost_lv),
    wbs_14_1_mid             = (!is_shed_fp) && grepl("mid",  cost_lv),
    wbs_14_1_high            = (!is_shed_fp) && grepl("high", cost_lv),
    wbs_14_3_retrofit_bldg1  = is_retrofit,
    wbs_14_4_retrofit_bldg2  = is_retrofit
  )
}


# ---------------------------------------------------------------------------
# Helper: map a WBS # (string) to its applicability flag name
# ---------------------------------------------------------------------------
# Returns TRUE if the item is applicable, FALSE if it should be hidden.
# Items with NULL WBS (section headers) are kept for display purposes.
# ---------------------------------------------------------------------------
is_wbs_applicable <- function(wbs, full_item_name, app) {
  
  wbs <- as.character(wbs %||% "")
  item_lower <- tolower(full_item_name %||% "")
  
  # Section headers (e.g. "1.", "2.") — always keep for display
  if (grepl("^\\d+\\.$", trimws(wbs))) return(TRUE)
  
  # Sub-section headers (no item variants) — keep
  wbs_numeric_depth <- length(strsplit(trimws(wbs), "\\.")[[1]])
  if (wbs_numeric_depth <= 1) return(TRUE)
  
  # ------ 1. GAC Contactors ------
  if (grepl("^1\\.1", wbs)) return(app$wbs_1_1_pressure_vessels)
  if (grepl("^1\\.2", wbs)) return(app$wbs_1_2_gac_basins)
  
  # ------ 2. Tanks ------
  if (grepl("^2\\.1", wbs)) return(app$wbs_2_1_backwash_tanks)
  if (grepl("^2\\.2", wbs)) return(app$wbs_2_2_residuals_tanks)
  if (grepl("^2\\.3", wbs)) return(app$wbs_2_3_ferric_tanks)
  if (grepl("^2\\.4", wbs)) return(app$wbs_2_4_polymer_tanks)
  
  # ------ 3. Piping ------
  if (grepl("^3\\.1", wbs)) return(app$wbs_3_1_process_piping)
  if (grepl("^3\\.2", wbs)) return(app$wbs_3_2_backwash_piping)
  if (grepl("^3\\.3", wbs)) return(app$wbs_3_3_influent_piping)
  if (grepl("^3\\.4", wbs)) return(app$wbs_3_4_residuals_piping)
  if (grepl("^3\\.5", wbs)) return(app$wbs_3_5_ferric_piping)
  if (grepl("^3\\.6", wbs)) return(app$wbs_3_6_polymer_piping)
  if (grepl("^3\\.7", wbs)) return(app$wbs_3_7_bypass_piping)
  
  # ------ 4. Valves ------
  if (grepl("^4\\.1\\.1", wbs)) return(app$wbs_4_1_1_process_mov)
  if (grepl("^4\\.1\\.2", wbs)) return(app$wbs_4_1_2_backwash_mov)
  if (grepl("^4\\.1\\.3", wbs)) return(app$wbs_4_1_3_residuals_mov)
  if (grepl("^4\\.1\\.4", wbs)) return(app$wbs_4_1_4_ferric_mov)
  if (grepl("^4\\.1\\.5", wbs)) return(app$wbs_4_1_5_polymer_mov)
  if (grepl("^4\\.1\\.6", wbs)) return(app$wbs_4_1_6_bypass_mov)
  if (grepl("^4\\.1",     wbs)) return(TRUE)   # sub-header row
  if (grepl("^4\\.2\\.1", wbs)) return(app$wbs_4_2_1_influent_man)
  if (grepl("^4\\.2\\.2", wbs)) return(app$wbs_4_2_2_process_man)
  if (grepl("^4\\.2\\.3", wbs)) return(app$wbs_4_2_3_backwash_man)
  if (grepl("^4\\.2\\.4", wbs)) return(app$wbs_4_2_4_residuals_man)
  if (grepl("^4\\.2\\.5", wbs)) return(app$wbs_4_2_5_ferric_man)
  if (grepl("^4\\.2\\.6", wbs)) return(app$wbs_4_2_6_polymer_man)
  if (grepl("^4\\.2\\.7", wbs)) return(app$wbs_4_2_7_bypass_man)
  if (grepl("^4\\.2",     wbs)) return(TRUE)
  if (grepl("^4\\.3\\.1", wbs)) return(app$wbs_4_3_1_backwash_chv)
  if (grepl("^4\\.3\\.2", wbs)) return(app$wbs_4_3_2_residuals_chv)
  if (grepl("^4\\.3\\.3", wbs)) return(app$wbs_4_3_3_ferric_chv)
  if (grepl("^4\\.3\\.4", wbs)) return(app$wbs_4_3_4_polymer_chv)
  if (grepl("^4\\.3\\.5", wbs)) return(app$wbs_4_3_5_influent_chv)
  if (grepl("^4\\.3\\.6", wbs)) return(app$wbs_4_3_6_bypass_chv)
  if (grepl("^4\\.3",     wbs)) return(TRUE)
  
  # ------ 5. Pumps ------
  if (grepl("^5\\.1", wbs)) return(app$wbs_5_1_booster)
  if (grepl("^5\\.2", wbs)) return(app$wbs_5_2_backwash)
  if (grepl("^5\\.3", wbs)) return(app$wbs_5_3_residuals)
  if (grepl("^5\\.4", wbs)) return(app$wbs_5_4_ferric_pumps)
  if (grepl("^5\\.5", wbs)) return(app$wbs_5_5_polymer_pumps)
  
  # ------ 6. Instrumentation ------
  if (grepl("^6\\.", wbs)) return(TRUE)
  
  # ------ 7. System Controls ------
  if (grepl("^7\\.", wbs)) return(TRUE)
  
  # ------ 8. Chemical Feed ------
  if (grepl("^8\\.1", wbs)) return(app$wbs_8_1_gac_transfer)
  if (grepl("^8\\.2", wbs)) return(app$wbs_8_2_residuals_mixers)
  if (grepl("^8\\.4", wbs)) return(app$wbs_8_4_ferric_mixers)
  if (grepl("^8\\.5", wbs)) return(app$wbs_8_5_polymer_mixers)
  
  # ------ 9. Media ------
  if (grepl("^9\\.", wbs)) return(app$wbs_9_1_gac_charge)
  
  # ------ 10. On-site Regen ------
  if (grepl("^10\\.", wbs)) return(app$wbs_10_regen)
  
  # ------ 11–13. Residuals structures ------
  if (grepl("^11\\.", wbs)) return(app$wbs_11_septic)
  if (grepl("^12\\.", wbs)) return(app$wbs_12_drying_pad)
  if (grepl("^13\\.", wbs)) return(app$wbs_13_evap_pond)
  
  # ------ 14. Buildings ------
  if (grepl("^14\\.3", wbs)) return(app$wbs_14_3_retrofit_bldg1)
  if (grepl("^14\\.4", wbs)) return(app$wbs_14_4_retrofit_bldg2)
  # 14.1.1 / 14.2.1: pick the correct sub-type by matching item name keywords.
  # Workbook selects exactly one of: Small Low Cost Shed, Low Quality, Medium Quality, High Quality.
  if (grepl("^14\\.[12]\\.1", wbs)) {
    item_lc <- tolower(trimws(as.character(full_item_name %||% "")))
    if (grepl("small low cost shed|shed", item_lc))   return(isTRUE(app$wbs_14_1_shed))
    if (grepl("low quality",             item_lc))   return(isTRUE(app$wbs_14_1_low))
    if (grepl("medium quality",          item_lc))   return(isTRUE(app$wbs_14_1_mid))
    if (grepl("high quality",            item_lc))   return(isTRUE(app$wbs_14_1_high))
    return(isTRUE(app$wbs_14_buildings))             # fallback for other 14.x.1 rows
  }
  if (grepl("^14\\.",  wbs)) return(app$wbs_14_buildings)
  
  # Default: keep
  TRUE
}


# ---------------------------------------------------------------------------
# Helper: populate Design Quantity, Unit Cost, Total Cost from results
# ---------------------------------------------------------------------------
# Returns a named list with `quantity`, `unit_cost`, `total_cost` for a given
# WBS row, or NULLs when the field is not mapped.
# ---------------------------------------------------------------------------
populate_wbs_values <- function(wbs, item_lower, contactors, tanks, piping, pumps, gac, controls, chem_feed, site, capital_costs) {
  
  qty  <- NA_real_
  ds <- NA_real_
  uc   <- NA_real_
  tc   <- NA_real_
  
  wbs <- trimws(as.character(wbs %||% ""))
  
  # --- 1. GAC Contactors (pressure vessels) ---
  if (grepl("^1\\.1\\.1$", wbs)) {
    qty <- contactors$total_contactors
    ds <- contactors$volume_per_contactor_gal 
    uc  <- contactors$unit_cost
    tc  <- contactors$total_cost
  }
  
  # --- 2.1 Backwash Tanks ---
  if (grepl("^2\\.1\\.1$", wbs)) {
    qty <- tanks$num_backwash_tanks
    uc  <- if (qty > 0) tanks$backwash_tank_cost / qty else 0
    tc  <- tanks$backwash_tank_cost
  }
  
  # --- 2.2 Residuals Tanks ---
  if (grepl("^2\\.2\\.1$", wbs)) {
    qty <- tanks$num_residuals_tanks
    uc  <- if (qty > 0) tanks$residuals_tank_cost / qty else 0
    tc  <- tanks$residuals_tank_cost
  }
  
  # # --- 3.1 Process Piping ---
  # if (grepl("^3\\.1\\.1$", wbs)) {
  #   qty <- piping$proc_pipe_length
  #   uc  <- if (qty > 0) piping$piping_material_cost / piping$piping_length_lf else 0
  #   tc  <- piping$piping_material_cost
  # }
  
  # # --- 3.2 Backwash Piping ---
  # if (grepl("^3\\.2\\.1$", wbs)) {
  #   qty <- piping$back_pipe_length
  #   uc  <- if (qty > 0) piping$piping_material_cost / piping$piping_length_lf else 0
  #   tc  <- NA_real_   # included in piping total
  # }
  
  # # --- 3.3 Influent/Treated Water Piping ---
  # if (grepl("^3\\.3\\.1$", wbs)) {
  #   qty <- piping$in_out_pipe_length
  #   uc  <- NA_real_
  #   tc  <- NA_real_
  # }
  
  # --- 3.1 Process Piping ---
  if (grepl("^3\\.1\\.1$", wbs)) {
    qty <- piping$proc_pipe_length
    uc  <- if (!is.null(piping$proc_pipe_cost) && qty > 0)
            piping$proc_pipe_cost / qty
          else 0
    tc  <- piping$proc_pipe_cost %||% NA_real_
  }

# --- 3.2 Backwash Piping ---
  if (grepl("^3\\.2\\.1$", wbs)) {
    qty <- piping$back_pipe_length
    uc  <- if (!is.null(piping$back_pipe_cost) && qty > 0)
            piping$back_pipe_cost / qty
          else 0
    tc  <- piping$back_pipe_cost %||% NA_real_
  }

# --- 3.3 Influent/Treated Water Piping ---
  if (grepl("^3\\.3\\.1$", wbs)) {
    qty <- piping$in_out_pipe_length
    uc  <- if (!is.null(piping$in_out_pipe_cost) && qty > 0)
            piping$in_out_pipe_cost / qty
          else NA_real_
    tc  <- piping$in_out_pipe_cost %||% NA_real_
  }

  # --- 3.4 Residuals Piping ---
  if (grepl("^3\\.4\\.1$", wbs)) {
    qty <- piping$res_pipe_length
    ds <- piping$res_pipe_diam
    uc  <- if (!is.null(piping$res_pipe_material_cost) && qty > 0)
           piping$res_pipe_material_cost / qty else NA_real_
    tc  <- piping$res_pipe_material_cost %||% NA_real_
  }

  if (grepl("^3\\.4\\.2$", wbs)) {
    qty <- piping$res_trench_vol_cy
    uc  <- 31
    tc  <- piping$res_trench_vol_cy * 31
  }

  if (grepl("^3\\.4\\.3$", wbs)) {
    qty <- piping$res_bedding_vol_cy
    uc  <- 45
    tc  <- piping$res_bedding_vol_cy * 45
  }
  if (grepl("^3\\.4\\.5$", wbs)) {
    # Backfill = trench vol - bedding vol (workbook uses res_trench_vol - res_pipe_bedding_vol)
  qty <- piping$res_trench_vol_cy - piping$res_bedding_vol_cy
  uc  <- 19
  tc  <- qty * 19
  }
  if (grepl("^3\\.4\\.6$", wbs)) {
    qty <- piping$res_block_vol_cy
    uc  <- 740
    tc  <- piping$res_block_vol_cy * 740
  }
  
  # --- 4. Valves (MOVs) ---
  if (grepl("^4\\.1\\.1$", wbs)) {
    qty <- piping$proc_mov_qty  %||% 0
    uc  <- if (qty > 0) (piping$proc_mov_cost %||% 0) / qty else 0
    tc  <- piping$proc_mov_cost %||% 0
  }
  if (grepl("^4\\.1\\.2$", wbs)) {
    qty <- piping$back_mov_qty  %||% 0
    uc  <- if (qty > 0) (piping$back_mov_cost %||% 0) / qty else 0
    tc  <- piping$back_mov_cost %||% 0
  }

  if (grepl("^4\\.1\\.3$", wbs)) {
    qty <- piping$res_mov_qty  %||% 0
    uc  <- if (qty > 0) (piping$res_mov_cost %||% 0) / qty else 0
    tc  <- piping$res_mov_cost %||% 0
  }

  if (grepl("^4\\.2\\.1$", wbs)) {
    qty <- piping$in_man_qty   %||% 0
    uc  <- if (qty > 0) (piping$in_man_cost   %||% 0) / qty else 0
    tc  <- piping$in_man_cost  %||% 0
  }
  if (grepl("^4\\.2\\.2$", wbs)) {
    qty <- piping$proc_man_qty %||% 0
    uc  <- if (qty > 0) (piping$proc_man_cost  %||% 0) / qty else 0
    tc  <- piping$proc_man_cost %||% 0
  }
  if (grepl("^4\\.2\\.3$", wbs)) {
    qty <- piping$back_man_qty %||% 0
    uc  <- if (qty > 0) (piping$back_man_cost  %||% 0) / qty else 0
    tc  <- piping$back_man_cost %||% 0
  }
  if (grepl("^4\\.2\\.4$", wbs)) {
    qty <- piping$res_man_qty  %||% 0
    uc  <- if (qty > 0) (piping$res_man_cost   %||% 0) / qty else 0
    tc  <- piping$res_man_cost %||% 0
  }
  
  if (grepl("^4\\.3\\.1$", wbs)) {
    qty <- piping$back_chv_qty  %||% 0
    uc  <- if (qty > 0) (piping$back_chv_cost %||% 0) / qty else 0
    tc  <- piping$back_chv_cost %||% 0
  }
  if (grepl("^4\\.3\\.2$", wbs)) {
    qty <- piping$res_chv_qty   %||% 0
    uc  <- if (qty > 0) (piping$res_chv_cost  %||% 0) / qty else 0
    tc  <- piping$res_chv_cost  %||% 0
  }
  if (grepl("^4\\.3\\.5$", wbs)) {
    qty <- piping$in_chv_qty    %||% 0
    uc  <- if (qty > 0) (piping$in_chv_cost   %||% 0) / qty else 0
    tc  <- piping$in_chv_cost   %||% 0
  }
  
  # --- 5. Pumps ---
  # Workbook cost equation: pump_cost(Q) = -0.00067003*Q^2 + 14.80901498*Q + 4093.494684836
  # Q = pump rating in gpm; returns 0 when Q = 0 (qty=0 row shows "--")
  pump_cost_eq <- function(Q) {
    Q <- Q %||% 0
    if (Q <= 0) return(0)
    -0.00067003 * Q^2 + 14.80901498 * Q + 4093.494684836
  }
  if (grepl("^5\\.1$", wbs)) {
    qty <- pumps$service_pumps   %||% 0
    uc  <- pump_cost_eq(pumps$pump_rating      %||% 0)
    tc  <- qty * uc
  }
  if (grepl("^5\\.2$", wbs)) {
    qty <- pumps$backwash_pumps  %||% 0
    uc  <- pump_cost_eq(pumps$back_pump_rating %||% 0)
    tc  <- qty * uc
  }
  if (grepl("^5\\.3$", wbs)) {
    qty <- pumps$residuals_pumps %||% 0
    uc  <- pump_cost_eq(pumps$res_pump_rating  %||% 0)
    tc  <- qty * uc
  }
  
  # --- 6. Instrumentation ---
  # Flow meter costs: polynomial M*d^2 + N*d + O (Cost Equations rows 143-161)
  # Fixed item costs from Cost Data lookup tables
  # All four meter subtypes are populated; the selected one has col12=1 in workbook

  # 6.1.x  Flow Meters – Influent and Treated Water
  if (grepl("^6\\.1\\.1$", wbs)) {
    if (grepl("orifice", item_lower)) {
      qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_op_uc   %||% 0; tc <- qty * uc
    } else if (grepl("propeller", item_lower)) {
      qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_prop_uc %||% 0; tc <- qty * uc
    } else if (grepl("venturi", item_lower)) {
      qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_ven_uc  %||% 0; tc <- qty * uc
    } else if (grepl("magnetic", item_lower)) {
      qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_mag_uc  %||% 0; tc <- qty * uc
    }
  }

  # 6.2.x  Flow Meters – Process
  if (grepl("^6\\.2\\.1$", wbs)) {
    if (grepl("orifice", item_lower)) {
      qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_op_uc   %||% 0; tc <- qty * uc
    } else if (grepl("propeller", item_lower)) {
      qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_prop_uc %||% 0; tc <- qty * uc
    } else if (grepl("venturi", item_lower)) {
      qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_ven_uc  %||% 0; tc <- qty * uc
    } else if (grepl("magnetic", item_lower)) {
      qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_mag_uc  %||% 0; tc <- qty * uc
    }
  }

  # 6.3.x  Flow Meters – Backwash
  if (grepl("^6\\.3\\.1$", wbs)) {
    if (grepl("orifice", item_lower)) {
      qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_op_uc   %||% 0; tc <- qty * uc
    } else if (grepl("propeller", item_lower)) {
      qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_prop_uc %||% 0; tc <- qty * uc
    } else if (grepl("venturi", item_lower)) {
      qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_ven_uc  %||% 0; tc <- qty * uc
    } else if (grepl("magnetic", item_lower)) {
      qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_mag_uc  %||% 0; tc <- qty * uc
    }
  }

  # 6.4.x  Flow Meters – Residuals
  if (grepl("^6\\.4\\.1$", wbs)) {
    if (grepl("orifice", item_lower)) {
      qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_op_uc   %||% 0; tc <- qty * uc
    } else if (grepl("propeller", item_lower)) {
      qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_prop_uc %||% 0; tc <- qty * uc
    } else if (grepl("venturi", item_lower)) {
      qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_ven_uc  %||% 0; tc <- qty * uc
    } else if (grepl("magnetic", item_lower)) {
      qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_mag_uc  %||% 0; tc <- qty * uc
    }
  }

  # 6.5  Level Switches/Alarms (for vessels)
  if (grepl("^6\\.5$", wbs)) {
    qty <- controls$tot_level_switch  %||% 0
    uc  <- controls$level_switch_uc   %||% 0
    tc  <- controls$level_switch_cost %||% 0
  }

  # 6.6  High/Low Alarms (for backwash tanks)
  if (grepl("^6\\.6$", wbs)) {
    qty <- controls$tot_back_alarm  %||% 0
    uc  <- controls$back_alarm_uc   %||% 0
    tc  <- controls$back_alarm_cost %||% 0
  }

  # 6.7  High/Low Alarm (for holding tanks)
  if (grepl("^6\\.7$", wbs)) {
    qty <- controls$tot_res_alarm  %||% 0
    uc  <- controls$res_alarm_uc   %||% 0
    tc  <- controls$res_alarm_cost %||% 0
  }

  # 6.8  pH Meters
  if (grepl("^6\\.8$", wbs)) {
    qty <- controls$pH_controls %||% 0
    uc  <- controls$pH_meter_uc %||% 0
    tc  <- controls$pH_cost     %||% 0
  }

  # 6.9  Temperature meters
  if (grepl("^6\\.9$", wbs)) {
    qty <- controls$tot_temp_meters %||% 0
    uc  <- controls$temp_meter_uc   %||% 0
    tc  <- controls$temp_cost       %||% 0
  }

  # 6.10  Turbidity meters
  if (grepl("^6\\.10$", wbs)) {
    qty <- controls$tot_turb_meters %||% 0
    uc  <- controls$turb_meter_uc   %||% 0
    tc  <- controls$turb_cost       %||% 0
  }

  # 6.11  Head loss sensors
  if (grepl("^6\\.11$", wbs)) {
    qty <- controls$tot_head_sens  %||% 0
    uc  <- controls$headloss_uc    %||% 0
    tc  <- controls$headloss_cost  %||% 0
  }

  # 6.12.1  Sampling Ports
  if (grepl("^6\\.12\\.1$", wbs)) {
    qty <- controls$ports       %||% 0
    uc  <- controls$sampling_uc %||% 0
    tc  <- qty * uc
  }

  # 6.13  Electrical enclosure
  if (grepl("^6\\.13$", wbs)) {
    qty <- controls$elec_encl      %||% 0
    uc  <- controls$elec_encl_uc   %||% 0
    tc  <- controls$elec_encl_cost %||% 0
  }

    # --- 7. System Controls ---
  # 7.1.1  PLC racks/power supplies
  if (grepl("^7\\.1\\.1$", wbs)) {
    qty <- controls$qty_7_1_1 %||% 0
    uc  <- controls$uc_plc_rack %||% 0
    tc  <- qty * uc
  }

  # 7.1.2  CPUs
  if (grepl("^7\\.1\\.2$", wbs)) {
    qty <- controls$qty_7_1_2 %||% 0
    uc  <- controls$uc_plc_cpu %||% 0
    tc  <- qty * uc
  }

  # 7.1.3  I/O discrete input modules
  if (grepl("^7\\.1\\.3$", wbs)) {
    qty <- controls$qty_7_1_3 %||% 0
    uc  <- controls$uc_plc_discrete_input %||% 0
    tc  <- qty * uc
  }

  # 7.1.4  I/O discrete output modules
  if (grepl("^7\\.1\\.4$", wbs)) {
    qty <- controls$qty_7_1_4 %||% 0
    uc  <- controls$uc_plc_discrete_output %||% 0
    tc  <- qty * uc
  }

  # 7.1.5  I/O combination analog modules
  if (grepl("^7\\.1\\.5$", wbs)) {
    qty <- controls$qty_7_1_5 %||% 0
    uc  <- controls$uc_plc_combination_analog %||% 0
    tc  <- qty * uc
  }

  # 7.1.6  Ethernet modules
  if (grepl("^7\\.1\\.6$", wbs)) {
    qty <- controls$qty_7_1_6 %||% 0
    uc  <- controls$uc_plc_ethernet %||% 0
    tc  <- qty * uc
  }

  # 7.1.7  Base expansion modules
  if (grepl("^7\\.1\\.7$", wbs)) {
    qty <- controls$qty_7_1_7 %||% 0
    uc  <- controls$uc_plc_base_expansion %||% 0
    tc  <- qty * uc
  }

  # 7.1.8  Base expansion controller modules
  if (grepl("^7\\.1\\.8$", wbs)) {
    qty <- controls$qty_7_1_8 %||% 0
    uc  <- controls$uc_plc_base_expansion_ctrl %||% 0
    tc  <- qty * uc
  }

  # 7.1.9  UPSs
  if (grepl("^7\\.1\\.9$", wbs)) {
    qty <- controls$qty_7_1_9 %||% 0
    uc  <- controls$uc_ups %||% 0
    tc  <- qty * uc
  }

  # 7.2.1  Drive controllers
  if (grepl("^7\\.2\\.1$", wbs)) {
    qty <- controls$qty_7_2_1 %||% 0
    uc  <- controls$uc_switch %||% 0
    tc  <- qty * uc
  }

  # 7.2.2  Operator interface units
  if (grepl("^7\\.2\\.2$", wbs)) {
    qty <- controls$qty_7_2_2 %||% 0
    uc  <- controls$uc_plc_op_interface %||% 0
    tc  <- qty * uc
  }

  # 7.2.3  PC Workstations
  if (grepl("^7\\.2\\.3$", wbs)) {
    qty <- controls$qty_7_2_3 %||% 0
    uc  <- controls$uc_pc_workstation %||% 0
    tc  <- qty * uc
  }

  # 7.2.4  Printers - laser jet
  if (grepl("^7\\.2\\.4$", wbs)) {
    qty <- controls$qty_7_2_4 %||% 0
    uc  <- controls$uc_laser_printer %||% 0
    tc  <- qty * uc
  }

  # 7.3.1  Operator interface software
  if (grepl("^7\\.3\\.1$", wbs)) {
    qty <- controls$qty_7_3_1 %||% 0
    uc  <- controls$uc_op_interface_software %||% 0
    tc  <- qty * uc
  }

  # 7.3.2  PLC programming software
  if (grepl("^7\\.3\\.2$", wbs)) {
    qty <- controls$qty_7_3_2 %||% 0
    uc  <- controls$uc_plc_software %||% 0
    tc  <- qty * uc
  }

  # 7.3.3  PLC data collection software
  if (grepl("^7\\.3\\.3$", wbs)) {
    qty <- controls$qty_7_3_3 %||% 0
    uc  <- controls$uc_plc_data_software %||% 0
    tc  <- qty * uc
  }

  # 7.3.4  Plant intelligence software
  if (grepl("^7\\.3\\.4$", wbs)) {
    qty <- controls$qty_7_3_4 %||% 0
    uc  <- controls$uc_plant_intel_software %||% 0
    tc  <- qty * uc
  }


  # --- 8.1.1  GAC Solids Transfer ---
  if (grepl("^8\\.1\\.1$", wbs)) {
    qty <- chem_feed$qty_8_1_1 %||% 0
    uc  <- chem_feed$uc_8_1_1
    tc  <- chem_feed$tc_8_1_1
  }

  # --- 8.1.2  Residuals Holding Tank Transfer ---
  if (grepl("^8\\.1\\.2$", wbs)) {
    qty <- chem_feed$qty_8_1_2 %||% 0
    uc  <- chem_feed$uc_8_1_2
    tc  <- chem_feed$tc_8_1_2
  }

  # --- 8.2.1  Residuals Mixers ---
  if (grepl("^8\\.2\\.1$", wbs) && grepl("portable", item_lower)) {
    qty <- chem_feed$qty_8_2_1 %||% 0
    uc  <- chem_feed$uc_8_2_1_portable
    tc  <- chem_feed$tc_8_2_1_portable
  }
  if (grepl("^8\\.2\\.1$", wbs) && grepl("mounted", item_lower)) {
    qty <- chem_feed$qty_8_2_1 %||% 0
    uc  <- chem_feed$uc_8_2_1_mounted
    tc  <- chem_feed$tc_8_2_1_mounted
  }
  if (grepl("^8\\.2\\.1$", wbs) && grepl("impeller", item_lower)) {
    qty <- chem_feed$qty_8_2_1 %||% 0
    uc  <- chem_feed$uc_8_2_1_impeller
    tc  <- chem_feed$tc_8_2_1_impeller
  }

  # --- 8.4.1  Ferric Chloride Mixers ---
  if (grepl("^8\\.4\\.1$", wbs) && grepl("portable", item_lower)) {
    qty <- chem_feed$qty_8_4_1 %||% 0
    uc  <- chem_feed$uc_8_4_1_portable
    tc  <- chem_feed$tc_8_4_1_portable
  }
  if (grepl("^8\\.4\\.1$", wbs) && grepl("mounted", item_lower)) {
    qty <- chem_feed$qty_8_4_1 %||% 0
    uc  <- chem_feed$uc_8_4_1_mounted
    tc  <- chem_feed$tc_8_4_1_mounted
  }
  if (grepl("^8\\.4\\.1$", wbs) && grepl("impeller", item_lower)) {
    qty <- chem_feed$qty_8_4_1 %||% 0
    uc  <- chem_feed$uc_8_4_1_impeller
    tc  <- chem_feed$tc_8_4_1_impeller
  }

  # --- 8.5.1  Polymer Mixers ---
  if (grepl("^8\\.5\\.1$", wbs) && grepl("portable", item_lower)) {
    qty <- chem_feed$qty_8_5_1 %||% 0
    uc  <- chem_feed$uc_8_5_1_portable
    tc  <- chem_feed$tc_8_5_1_portable
  }
  if (grepl("^8\\.5\\.1$", wbs) && grepl("mounted", item_lower)) {
    qty <- chem_feed$qty_8_5_1 %||% 0
    uc  <- chem_feed$uc_8_5_1_mounted
    tc  <- chem_feed$tc_8_5_1_mounted
  }
  if (grepl("^8\\.5\\.1$", wbs) && grepl("impeller", item_lower)) {
    qty <- chem_feed$qty_8_5_1 %||% 0
    uc  <- chem_feed$uc_8_5_1_impeller
    tc  <- chem_feed$tc_8_5_1_impeller
  }

    # --- 9.1 Initial GAC Charge ---
  if (grepl("^9\\.1$", wbs)) {
    qty <- gac$total_gac_mass_lb
    uc  <- gac$gac_unit_cost
    tc  <- gac$initial_fill_cost
  }
  
  # --- 14.5 Concrete Pad ---
  # qty = concrete cy; uc = VLOOKUP(0, conc_pad_cost_cl, 3) = $492.75 fixed
  if (grepl("^14\\.5$", wbs)) {
    qty <- site$concrete_pad_qty %||% 0
    uc  <- site$concrete_pad_uc  %||% NA_real_
    tc  <- site$concrete_pad_tc  %||% NA_real_
  }
  
  # Workbook rule: IF(qty=0, uc="NA", tc="--") — mirror by setting uc/tc to NA when qty=0
  if (!is.na(qty) && isTRUE(qty == 0)) {
    uc <- NA_real_
    tc <- NA_real_
  }

  list(quantity = qty, design_size = ds, unit_cost = uc, total_cost = tc)
}


# ---------------------------------------------------------------------------
# `%||%` null-coalescing operator (define if not already in utils.R)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b


# ===========================================================================
# MAIN: build_wbs_table()
#
# Call this inside output$test_table renderDT() in place of the current block.
#
# Arguments:
#   data   — results$data (the list returned by calculate_gac_system)
#
# Returns a DT datatable ready for renderDT.
# ===========================================================================
build_wbs_table <- function(data) {
  
  # Unpack results
  params        <- data$params
  contactors    <- data$contactors
  tanks         <- data$tanks
  piping        <- data$piping
  pumps         <- data$pumps
  gac           <- data$gac
  controls      <- data$controls
  chem_feed     <- data$chem_feed
  site          <- data$site
  capital_costs <- data$capital_costs
  
  # ── Stage 0: Resolve priority column ──────────────────────────────────────
  size_selection <- controls$system_scale
  cost_selection <- stringr::str_to_lower(contactors$component_level_name)
  priority_col   <- resolve_priority_col(size_selection, cost_selection)
  
  # ── Stage 1: Load Sheet23 and filter by num_row (priority selection) ──────
  baseline <- get_sheet_data("baseline_priority_selection", return_type = "table")
  
  # The selected num_row for this size × cost combination
  selected_num_row <- baseline |>
    dplyr::filter(grepl(size_selection, size, ignore.case = TRUE)) |>
    dplyr::mutate(row_index = dplyr::row_number()) |>
    dplyr::filter(!!rlang::sym(cost_selection)==1) |>
    dplyr::pull(row_index)
  
  df <- get_sheet_data("Sheet23", return_type = "table") |>
    janitor::clean_names() |>
    dplyr::mutate(row_index = dplyr::row_number()) |>
    dplyr::filter(
      row_index %in% selected_num_row  # keep items matching priority selection
    )
  
  # ── Stage 2: Applicability filter ─────────────────────────────────────────
  app <- derive_applicability(params, contactors, tanks, piping, pumps, gac, controls, chem_feed, site)
  
  df <- df |>
    dplyr::filter(
      purrr::map2_lgl(
        wbs,
        full_line_item_name,
        ~ is_wbs_applicable(.x, .y, app)
      )
    )
  
  # ── Stage 3: Populate values from calculation results ─────────────────────
  values <- purrr::map2(
    df$wbs,
    tolower(df$full_line_item_name %||% ""),
    ~ populate_wbs_values(
        .x, .y,
        contactors, tanks, piping, pumps, gac, controls, chem_feed, site, capital_costs
      )
  )
  
  df <- df |>
    dplyr::mutate(
      design_quantity = purrr::map_dbl(values, ~ .x$quantity %||% NA_real_),
      design_size     = purrr::map_dbl(values, ~ .x$design_size %||% NA_real_),
      unit_cost       = purrr::map_dbl(values, ~ .x$unit_cost %||% NA_real_),
      total_cost      = purrr::map_dbl(values, ~ .x$total_cost %||% NA_real_)
    )
  
  df <- df |>
    dplyr::rename(
    WBS                = wbs,
    Item               = item,
    `Design Quantity`  = design_quantity,
    `Design Size`      = design_size,
    `Size Used_in_estimate` = size_used_in_estimate,
    `Unit Cost`        = unit_cost,
    `Total Cost`       = total_cost,
    `Useful Life`      = useful_life,
    table              = table
  )
  
  # ── Stage 4: Format and render ────────────────────────────────────────────
  # Preserve ordered unique section names (the table column = RowGroup labels)
  # for the sticky nav bar in mod_output_db.R
  section_names <- unique(df$table[!is.na(df$table)])

  list(
    dt       = format_wbs_table(df),
    sections = section_names
  )
}