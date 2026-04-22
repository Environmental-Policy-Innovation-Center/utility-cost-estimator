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

  qty <- NA_real_
  ds  <- NA_real_
  uc  <- NA_real_
  tc  <- NA_real_
  ul  <- NA_real_

  # ── Useful life helper ────────────────────────────────────────────────────
  # Mirrors workbook OUTPUT col K: VLOOKUP(size, cost_cl, IF(ss_cat="large",4,5))
  # ss_cat = "small" when design_flow < 1 MGD, else "large".
  # controls$system_scale maps to: "small" | "medium" | "large"
  # "medium" and "large" both use the large-column values (ss_cat2 = 2 or 3).
  {
    scale_lc    <- tolower(trimws(as.character(controls$system_scale %||% "small")))
    is_lg       <- (scale_lc != "small")        # large col when medium or large
    item_lc_ul  <- tolower(trimws(item_lower))  # already lowercased in caller

    ul <- {
      # ── 1.1.1 Pressure vessels ──
      if (grepl("^1\\.1\\.1$", wbs)) {
        if (grepl("fiberglass", item_lc_ul)) if (is_lg) 25L else 20L
        else if (is_lg) 35L else 30L          # SS, CS, CSP

      # ── 1.2.x GAC Basins (concrete, internals, railing, stairs, excav, backfill)
      } else if (grepl("^1\\.2\\.", wbs)) {
        if (is_lg) 40L else 37L               # conc_basin_cost_cl

      # ── 2.1.1 / 2.2.1 Backwash / Residuals Tanks ──
      } else if (grepl("^2\\.[12]\\.1$", wbs)) {
        if (grepl("concrete", item_lc_ul))                          if (is_lg) 40L else 37L
        else if (grepl("steel", item_lc_ul))                        if (is_lg) 35L else 30L
        else if (grepl("fiberglass|hdpe|plastic", item_lc_ul))      if (is_lg) 25L else 20L
        else if (is_lg) 25L else 20L

      # ── 2.3.1 Ferric Chloride Tanks (plastic/FG) ──
      } else if (grepl("^2\\.3\\.1$", wbs)) {
        if (is_lg) 10L else 7L

      # ── 2.3.2 Secondary Containment – Concrete Curbing ──
      } else if (grepl("^2\\.3\\.2$", wbs)) {
        if (is_lg) 40L else 37L

      # ── 2.3.3 Secondary Containment – Chemical Resistant Coating ──
      } else if (grepl("^2\\.3\\.3$", wbs)) {
        10L                                   # same for both sizes

      # ── 2.4.1 Polymer Tanks ──
      } else if (grepl("^2\\.4\\.1$", wbs)) {
        if (grepl("stainless", item_lc_ul))   if (is_lg) 35L else 30L
        else if (is_lg) 10L else 7L           # plastic/XLPE, FG

      # ── 3.x.1 Piping (process / backwash / influent / residuals / ferric / polymer / bypass)
      } else if (grepl("^3\\.[1-7]\\.1$", wbs)) {
        if (grepl("ductile iron", item_lc_ul))           if (is_lg) 40L else 35L
        else if (grepl("cpvc",            item_lc_ul))   if (is_lg) 22L else 17L
        else if (grepl("pvc",             item_lc_ul))   if (is_lg) 22L else 17L
        else if (grepl("stainless steel", item_lc_ul))   if (is_lg) 45L else 40L
        else if (grepl("steel",           item_lc_ul))   if (is_lg) 35L else 33L
        else NA_real_

      # 3.4.2 Excavation / 3.4.3 Bedding / 3.4.5 Backfill / 3.4.6 Thrust Blocks
      # → inherit from 3.4.1 (residuals piping); filled by post-processing step in build_wbs_table
      } else if (grepl("^3\\.4\\.[2356]$", wbs)) {
        NA_real_

      # ── 4.x Valves (all materials: PP/PVC, SS, CI = 25/20) ──
      } else if (grepl("^4\\.", wbs)) {
        if (is_lg) 25L else 20L

      # ── 5.x Pumps ──
      } else if (grepl("^5\\.[123]$", wbs)) {
        if (is_lg) 20L else 17L               # booster / backwash / residuals
      } else if (grepl("^5\\.[45]\\.1$", wbs)) {
        if (grepl("electric", item_lc_ul))    if (is_lg) 20L else 15L
        else if (is_lg) 20L else 17L          # motor driven

      # ── 6.x Instrumentation ──
      } else if (grepl("^6\\.[1-4]\\.1$", wbs)) {
        if (is_lg) 15L else 14L               # all flow meter types
      } else if (grepl("^6\\.[5-9]$|^6\\.1[01]$", wbs)) {
        if (is_lg) 15L else 14L               # alarms, pH, temp, turbidity, headloss
      } else if (grepl("^6\\.12\\.1$", wbs)) {
        if (grepl("stainless|ss", item_lc_ul)) if (is_lg) 35L else 30L
        else if (is_lg) 25L else 22L          # carbon steel
      } else if (grepl("^6\\.13$", wbs)) {
        if (is_lg) 22L else 17L               # electrical enclosure

      # ── 7.x System Controls ──
      } else if (grepl("^7\\.2\\.1$", wbs)) {
        if (is_lg) 15L else 14L               # drive controllers (switch_cost_cl)
      } else if (grepl("^7\\.", wbs)) {
        if (is_lg) 10L else 8L                # all other PLC / software / operator equip

      # ── 8.x Chemical Feed and Transfer ──
      } else if (grepl("^8\\.[12]\\.1$", wbs)) {
        if (grepl("eductor", item_lc_ul))     if (is_lg) 45L else 40L
        else if (grepl("manual", item_lc_ul)) NA_real_
        else if (is_lg) 20L else 15L          # slurry pump system
      } else if (grepl("^8\\.[245]\\.1$", wbs)) {
        if (grepl("mounted", item_lc_ul))     if (is_lg) 25L else 22L
        else if (is_lg) 25L else 20L          # portable / impeller

      # ── 9.1 GAC Media – N/A per workbook ──
      } else if (grepl("^9\\.1$", wbs)) {
        NA_real_

      # ── 10.x On-site Regeneration ──
      } else if (grepl("^10\\.[12]$", wbs)) {
        20L                                   # same for both sizes
      } else if (grepl("^10\\.[345]\\.1$", wbs)) {
        if (grepl("concrete", item_lc_ul))    if (is_lg) 40L else 37L
        else if (is_lg) 35L else 30L          # steel tanks

      # ── 11.x Septic System ──
      } else if (grepl("^11\\.[123]$", wbs)) {
        if (is_lg) 40L else 37L
      } else if (grepl("^11\\.[456]$", wbs)) {
        if (is_lg) 45L else 40L

      # ── 12.1 Solids Drying Pad ──
      } else if (grepl("^12\\.1$", wbs)) {
        if (is_lg) 40L else 37L               # conc_pad_cost_cl

      # ── 13.x Evaporation Pond ──
      } else if (grepl("^13\\.", wbs)) {
        if (is_lg) 10L else 10L               # ep_liner_cost_cl

      # ── 14.x Buildings and HVAC ──
      } else if (grepl("^14\\.[1-4]\\.1$", wbs)) {
        if (grepl("shed|small low", item_lc_ul))  if (is_lg) 25L else 20L
        else if (is_lg) 40L else 37L              # low / mid / high quality
      } else if (grepl("^14\\.[12]\\.[23]\\.1$", wbs)) {
        if (is_lg) 25L else 20L                   # HVAC systems
      } else if (grepl("^14\\.5$", wbs)) {
        if (is_lg) 40L else 37L                   # concrete pad

      } else {
        NA_real_
      }
    }
  }

  wbs <- trimws(as.character(wbs %||% ""))

  # ── 1.1.1  GAC Pressure Vessels ──────────────────────────────────────────────
  if (grepl("^1\\.1\\.1$", wbs)) {
    qty <- contactors$total_contactors
    ds  <- contactors$volume_per_contactor_gal       # gal per vessel
    uc  <- contactors$unit_cost
    tc  <- contactors$total_cost
  }

  # ── 1.2.1  GAC Basins – Concrete ─────────────────────────────────────────────
  if (grepl("^1\\.2\\.1$", wbs)) {
    qty <- contactors$num_basins    %||% 0
    ds  <- contactors$basin_vol_gal %||% NA_real_   # gal per basin (workbook: basin_vol)
    uc  <- contactors$basin_concrete_uc      %||% NA_real_
    tc  <- contactors$basin_concrete_cost    %||% NA_real_
  }

  # ── 1.2.2  GAC Basins – Internals (Underdrain/Backwash) ──────────────────────
  if (grepl("^1\\.2\\.2$", wbs)) {
    qty <- contactors$num_basins     %||% 0
    ds  <- contactors$basin_area_sf  %||% NA_real_  # sf per basin (workbook: basin_area = L×W)
    uc  <- contactors$basin_internals_uc_per %||% NA_real_
    tc  <- contactors$basin_internals_cost   %||% NA_real_
  }

  # ── 1.2.3  GAC Basins – Aluminum Railing ─────────────────────────────────────
  if (grepl("^1\\.2\\.3$", wbs)) {
    qty <- contactors$basin_railing_lf       %||% NA_real_   # qty unit = lf
    uc  <- contactors$basin_railing_uc       %||% NA_real_
    tc  <- contactors$basin_railing_cost     %||% NA_real_
  }

  # ── 1.2.4  GAC Basins – Aluminum Stairs ──────────────────────────────────────
  if (grepl("^1\\.2\\.4$", wbs)) {
    qty <- contactors$basin_stairs_risers    %||% NA_real_   # qty unit = risers
    uc  <- contactors$basin_stairs_uc        %||% NA_real_
    tc  <- contactors$basin_stairs_cost      %||% NA_real_
  }

  # ── 1.2.5  GAC Basins – Excavation ───────────────────────────────────────────
  if (grepl("^1\\.2\\.5$", wbs)) {
    qty <- contactors$basin_excav_cy         %||% NA_real_   # qty unit = cy
    uc  <- contactors$basin_excav_uc         %||% NA_real_
    tc  <- contactors$basin_excav_cost       %||% NA_real_
  }

  # ── 1.2.6  GAC Basins – Backfill and Compaction ──────────────────────────────
  if (grepl("^1\\.2\\.6$", wbs)) {
    qty <- contactors$basin_backfill_cy      %||% NA_real_   # qty unit = cy
    uc  <- contactors$basin_backfill_uc      %||% NA_real_
    tc  <- contactors$basin_backfill_cost    %||% NA_real_
  }

  # ── 2.1.1  Backwash Tanks ─────────────────────────────────────────────────────
  if (grepl("^2\\.1\\.1$", wbs)) {
    qty <- tanks$num_backwash_tanks  %||% 0
    ds  <- tanks$backwash_tank_volume %||% NA_real_   # gal per tank
    uc  <- if (!is.na(qty) && qty > 0) (tanks$backwash_tank_cost %||% 0) / qty else NA_real_
    tc  <- tanks$backwash_tank_cost  %||% NA_real_
  }

  # ── 2.2.1  Residuals Holding Tanks ───────────────────────────────────────────
  if (grepl("^2\\.2\\.1$", wbs)) {
    qty <- tanks$num_residuals_tanks  %||% 0
    ds  <- tanks$residuals_tank_volume %||% NA_real_  # gal per tank
    uc  <- if (!is.na(qty) && qty > 0) (tanks$residuals_tank_cost %||% 0) / qty else NA_real_
    tc  <- tanks$residuals_tank_cost  %||% NA_real_
  }

  # ── 3.1.1  Process Piping ─────────────────────────────────────────────────────
  if (grepl("^3\\.1\\.1$", wbs)) {
    qty <- piping$proc_pipe_length %||% NA_real_
    ds  <- piping$proc_pipe_diam   %||% NA_real_      # in. diam
    uc  <- if (!is.null(piping$proc_pipe_cost) && !is.na(qty) && qty > 0)
             piping$proc_pipe_cost / qty
           else NA_real_
    tc  <- piping$proc_pipe_cost   %||% NA_real_
  }

  # ── 3.2.1  Backwash Piping ────────────────────────────────────────────────────
  if (grepl("^3\\.2\\.1$", wbs)) {
    qty <- piping$back_pipe_length %||% NA_real_
    ds  <- piping$back_pipe_diam   %||% NA_real_      # in. diam
    uc  <- if (!is.null(piping$back_pipe_cost) && !is.na(qty) && qty > 0)
             piping$back_pipe_cost / qty
           else NA_real_
    tc  <- piping$back_pipe_cost   %||% NA_real_
  }

  # ── 3.3.1  Influent and Treated Water Piping ─────────────────────────────────
  if (grepl("^3\\.3\\.1$", wbs)) {
    qty <- piping$in_out_pipe_length %||% NA_real_
    ds  <- piping$in_out_pipe_diam   %||% NA_real_    # in. diam
    uc  <- if (!is.null(piping$in_out_pipe_cost) && !is.na(qty) && qty > 0)
             piping$in_out_pipe_cost / qty
           else NA_real_
    tc  <- piping$in_out_pipe_cost   %||% NA_real_
  }

  # ── 3.4.1  Residuals Piping ───────────────────────────────────────────────────
  if (grepl("^3\\.4\\.1$", wbs)) {
    qty <- piping$res_pipe_length          %||% NA_real_
    ds  <- piping$res_pipe_diam            %||% NA_real_  # in. diam
    uc  <- if (!is.null(piping$res_pipe_material_cost) && !is.na(qty) && qty > 0)
             piping$res_pipe_material_cost / qty
           else NA_real_
    tc  <- piping$res_pipe_material_cost   %||% NA_real_
  }

  # ── 3.4.2  Residuals Piping – Excavation ─────────────────────────────────────
  if (grepl("^3\\.4\\.2$", wbs)) {
    qty <- piping$res_trench_vol_cy %||% NA_real_
    uc  <- 30.879999999999995   # excavate_cost_cl (workbook col3 = 30.88)
    tc  <- if (!is.na(qty)) qty * uc else NA_real_
  }

  # ── 3.4.3  Residuals Piping – Bedding ────────────────────────────────────────
  if (grepl("^3\\.4\\.3$", wbs)) {
    qty <- piping$res_bedding_vol_cy %||% NA_real_
    uc  <- 45.35   # pipe_bedding_cost_cl (workbook = 45.35)
    tc  <- if (!is.na(qty)) qty * uc else NA_real_
  }

  # ── 3.4.5  Residuals Piping – Backfill and Compaction ────────────────────────
  # Workbook OUTPUT C96 = res_trench_vol (same as excavation, per workbook formula
  # backfill_cy = excavation_cy in cost equations and piping module).
  if (grepl("^3\\.4\\.5$", wbs)) {
    qty <- piping$res_trench_vol_cy  %||% NA_real_   # = res_trench_vol (workbook C96)
    uc  <- 18.65   # backfill_cost_cl (workbook col3 = 18.65)
    tc  <- if (!is.na(qty)) qty * uc else NA_real_
  }

  # ── 3.4.6  Residuals Piping – Thrust Blocks ──────────────────────────────────
  if (grepl("^3\\.4\\.6$", wbs)) {
    qty <- piping$res_block_vol_cy %||% NA_real_
    uc  <- 739.6055887474795   # conc_basin_cost_cl col3 ($/cy)
    tc  <- if (!is.na(qty)) qty * uc else NA_real_
  }

  # ── 4.1.1  MOVs – Process ────────────────────────────────────────────────────
  if (grepl("^4\\.1\\.1$", wbs)) {
    qty <- piping$proc_mov_qty  %||% 0
    ds  <- piping$proc_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$proc_mov_cost %||% 0) / qty else NA_real_
    tc  <- piping$proc_mov_cost %||% NA_real_
  }

  # ── 4.1.2  MOVs – Backwash ───────────────────────────────────────────────────
  if (grepl("^4\\.1\\.2$", wbs)) {
    qty <- piping$back_mov_qty  %||% 0
    ds  <- piping$back_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$back_mov_cost %||% 0) / qty else NA_real_
    tc  <- piping$back_mov_cost %||% NA_real_
  }

  # ── 4.1.3  MOVs – Residuals ──────────────────────────────────────────────────
  if (grepl("^4\\.1\\.3$", wbs)) {
    qty <- piping$res_mov_qty   %||% 0
    ds  <- piping$res_pipe_diam  %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$res_mov_cost %||% 0) / qty else NA_real_
    tc  <- piping$res_mov_cost  %||% NA_real_
  }

  # ── 4.2.1  Manual Valves – Influent/Treated Water ────────────────────────────
  if (grepl("^4\\.2\\.1$", wbs)) {
    qty <- piping$in_man_qty    %||% 0
    ds  <- piping$in_out_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$in_man_cost %||% 0) / qty else NA_real_
    tc  <- piping$in_man_cost   %||% NA_real_
  }

  # ── 4.2.2  Manual Valves – Process ───────────────────────────────────────────
  if (grepl("^4\\.2\\.2$", wbs)) {
    qty <- piping$proc_man_qty  %||% 0
    ds  <- piping$proc_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$proc_man_cost %||% 0) / qty else NA_real_
    tc  <- piping$proc_man_cost %||% NA_real_
  }

  # ── 4.2.3  Manual Valves – Backwash ──────────────────────────────────────────
  if (grepl("^4\\.2\\.3$", wbs)) {
    qty <- piping$back_man_qty  %||% 0
    ds  <- piping$back_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$back_man_cost %||% 0) / qty else NA_real_
    tc  <- piping$back_man_cost %||% NA_real_
  }

  # ── 4.2.4  Manual Valves – Residuals ─────────────────────────────────────────
  if (grepl("^4\\.2\\.4$", wbs)) {
    qty <- piping$res_man_qty   %||% 0
    ds  <- piping$res_pipe_diam  %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$res_man_cost %||% 0) / qty else NA_real_
    tc  <- piping$res_man_cost  %||% NA_real_
  }

  # ── 4.3.1  Check Valves – Backwash ───────────────────────────────────────────
  if (grepl("^4\\.3\\.1$", wbs)) {
    qty <- piping$back_chv_qty  %||% 0
    ds  <- piping$back_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$back_chv_cost %||% 0) / qty else NA_real_
    tc  <- piping$back_chv_cost %||% NA_real_
  }

  # ── 4.3.2  Check Valves – Residuals ──────────────────────────────────────────
  if (grepl("^4\\.3\\.2$", wbs)) {
    qty <- piping$res_chv_qty   %||% 0
    ds  <- piping$res_pipe_diam  %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$res_chv_cost %||% 0) / qty else NA_real_
    tc  <- piping$res_chv_cost  %||% NA_real_
  }

  # ── 4.3.5  Check Valves – Influent ───────────────────────────────────────────
  if (grepl("^4\\.3\\.5$", wbs)) {
    qty <- piping$in_chv_qty    %||% 0
    ds  <- piping$in_out_pipe_diam %||% NA_real_
    uc  <- if (!is.na(qty) && qty > 0) (piping$in_chv_cost %||% 0) / qty else NA_real_
    tc  <- piping$in_chv_cost   %||% NA_real_
  }

  # ── 5.1  Booster Pumps ───────────────────────────────────────────────────────
  # Workbook: pump_cost(Q) = -0.00067003*Q^2 + 14.80901498*Q + 4093.494684836
  pump_cost_eq <- function(Q) {
    Q <- as.numeric(Q %||% 0)
    if (is.na(Q) || Q <= 0) return(NA_real_)
    -0.00067003 * Q^2 + 14.80901498 * Q + 4093.494684836
  }
  if (grepl("^5\\.1$", wbs)) {
    qty <- pumps$service_pumps   %||% 0
    ds  <- pumps$pump_rating      %||% NA_real_
    uc  <- pump_cost_eq(pumps$pump_rating      %||% 0)
    tc  <- if (!is.na(qty) && qty > 0) qty * uc else NA_real_
  }

  # ── 5.2  Backwash Pumps ──────────────────────────────────────────────────────
  if (grepl("^5\\.2$", wbs)) {
    qty <- pumps$backwash_pumps  %||% 0
    ds  <- pumps$back_pump_rating %||% NA_real_
    uc  <- pump_cost_eq(pumps$back_pump_rating %||% 0)
    tc  <- if (!is.na(qty) && qty > 0) qty * uc else NA_real_
  }

  # ── 5.3  Residuals Pumps ─────────────────────────────────────────────────────
  if (grepl("^5\\.3$", wbs)) {
    qty <- pumps$residuals_pumps %||% 0
    ds  <- pumps$res_pump_rating  %||% NA_real_
    uc  <- pump_cost_eq(pumps$res_pump_rating  %||% 0)
    tc  <- if (!is.na(qty) && qty > 0) qty * uc else NA_real_
  }

  # ── 6.1.1  Flow Meters – Influent and Treated Water ──────────────────────────
  if (grepl("^6\\.1\\.1$", wbs)) {
    ds  <- piping$in_out_pipe_diam %||% NA_real_
    if (grepl("orifice",   item_lower)) { qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_op_uc   %||% 0 }
    else if (grepl("propeller", item_lower)) { qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_prop_uc %||% 0 }
    else if (grepl("venturi",   item_lower)) { qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_ven_uc  %||% 0 }
    else if (grepl("magnetic",  item_lower)) { qty <- controls$tot_fm_in %||% 0; uc <- controls$fm_in_mag_uc  %||% 0 }
    if (!is.na(qty)) tc <- qty * uc
  }

  # ── 6.2.1  Flow Meters – Process ─────────────────────────────────────────────
  if (grepl("^6\\.2\\.1$", wbs)) {
    ds  <- piping$proc_pipe_diam %||% NA_real_
    if (grepl("orifice",   item_lower)) { qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_op_uc   %||% 0 }
    else if (grepl("propeller", item_lower)) { qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_prop_uc %||% 0 }
    else if (grepl("venturi",   item_lower)) { qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_ven_uc  %||% 0 }
    else if (grepl("magnetic",  item_lower)) { qty <- controls$tot_fm_proc %||% 0; uc <- controls$fm_proc_mag_uc  %||% 0 }
    if (!is.na(qty)) tc <- qty * uc
  }

  # ── 6.3.1  Flow Meters – Backwash ────────────────────────────────────────────
  if (grepl("^6\\.3\\.1$", wbs)) {
    ds  <- piping$back_pipe_diam %||% NA_real_
    if (grepl("orifice",   item_lower)) { qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_op_uc   %||% 0 }
    else if (grepl("propeller", item_lower)) { qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_prop_uc %||% 0 }
    else if (grepl("venturi",   item_lower)) { qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_ven_uc  %||% 0 }
    else if (grepl("magnetic",  item_lower)) { qty <- controls$tot_fm_back %||% 0; uc <- controls$fm_back_mag_uc  %||% 0 }
    if (!is.na(qty)) tc <- qty * uc
  }

  # ── 6.4.1  Flow Meters – Residuals ───────────────────────────────────────────
  if (grepl("^6\\.4\\.1$", wbs)) {
    ds  <- piping$res_pipe_diam %||% NA_real_
    if (grepl("orifice",   item_lower)) { qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_op_uc   %||% 0 }
    else if (grepl("propeller", item_lower)) { qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_prop_uc %||% 0 }
    else if (grepl("venturi",   item_lower)) { qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_ven_uc  %||% 0 }
    else if (grepl("magnetic",  item_lower)) { qty <- controls$tot_fm_res %||% 0; uc <- controls$fm_res_mag_uc  %||% 0 }
    if (!is.na(qty)) tc <- qty * uc
  }

  # ── 6.5  Level Switches/Alarms ────────────────────────────────────────────────
  if (grepl("^6\\.5$", wbs)) {
    qty <- controls$tot_level_switch  %||% 0
    uc  <- controls$level_switch_uc   %||% 0
    tc  <- controls$level_switch_cost %||% 0
  }

  # ── 6.6  High/Low Alarms – Backwash Tanks ────────────────────────────────────
  if (grepl("^6\\.6$", wbs)) {
    qty <- controls$tot_back_alarm  %||% 0
    uc  <- controls$back_alarm_uc   %||% 0
    tc  <- controls$back_alarm_cost %||% 0
  }

  # ── 6.7  High/Low Alarm – Residuals Holding Tanks ────────────────────────────
  if (grepl("^6\\.7$", wbs)) {
    qty <- controls$tot_res_alarm  %||% 0
    uc  <- controls$res_alarm_uc   %||% 0
    tc  <- controls$res_alarm_cost %||% 0
  }

  # ── 6.8  pH Meters ───────────────────────────────────────────────────────────
  if (grepl("^6\\.8$", wbs)) {
    qty <- controls$pH_controls %||% 0
    uc  <- controls$pH_meter_uc %||% 0
    tc  <- controls$pH_cost     %||% 0
  }

  # ── 6.9  Temperature Meters ──────────────────────────────────────────────────
  if (grepl("^6\\.9$", wbs)) {
    qty <- controls$tot_temp_meters %||% 0
    uc  <- controls$temp_meter_uc   %||% 0
    tc  <- controls$temp_cost       %||% 0
  }

  # ── 6.10  Turbidity Meters ────────────────────────────────────────────────────
  if (grepl("^6\\.10$", wbs)) {
    qty <- controls$tot_turb_meters %||% 0
    uc  <- controls$turb_meter_uc   %||% 0
    tc  <- controls$turb_cost       %||% 0
  }

  # ── 6.11  Head Loss Sensors ───────────────────────────────────────────────────
  if (grepl("^6\\.11$", wbs)) {
    qty <- controls$tot_head_sens  %||% 0
    uc  <- controls$headloss_uc    %||% 0
    tc  <- controls$headloss_cost  %||% 0
  }

  # ── 6.12.1  Sampling Ports ───────────────────────────────────────────────────
  if (grepl("^6\\.12\\.1$", wbs)) {
    qty <- controls$ports       %||% 0
    uc  <- controls$sampling_uc %||% 0
    tc  <- if (!is.na(qty)) qty * uc else NA_real_
  }

  # ── 6.13  Electrical Enclosure ────────────────────────────────────────────────
  if (grepl("^6\\.13$", wbs)) {
    qty <- controls$elec_encl      %||% 0
    uc  <- controls$elec_encl_uc   %||% 0
    tc  <- controls$elec_encl_cost %||% 0
  }

  # ── 7.1.1  PLC racks/power supplies ──────────────────────────────────────────
  if (grepl("^7\\.1\\.1$", wbs)) { qty <- controls$qty_7_1_1 %||% 0; uc <- controls$uc_plc_rack              %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.2$", wbs)) { qty <- controls$qty_7_1_2 %||% 0; uc <- controls$uc_plc_cpu               %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.3$", wbs)) { qty <- controls$qty_7_1_3 %||% 0; uc <- controls$uc_plc_discrete_input    %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.4$", wbs)) { qty <- controls$qty_7_1_4 %||% 0; uc <- controls$uc_plc_discrete_output   %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.5$", wbs)) { qty <- controls$qty_7_1_5 %||% 0; uc <- controls$uc_plc_combination_analog%||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.6$", wbs)) { qty <- controls$qty_7_1_6 %||% 0; uc <- controls$uc_plc_ethernet          %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.7$", wbs)) { qty <- controls$qty_7_1_7 %||% 0; uc <- controls$uc_plc_base_expansion    %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.8$", wbs)) { qty <- controls$qty_7_1_8 %||% 0; uc <- controls$uc_plc_base_expansion_ctrl%||%0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.1\\.9$", wbs)) { qty <- controls$qty_7_1_9 %||% 0; uc <- controls$uc_ups                   %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.2\\.1$", wbs)) { qty <- controls$qty_7_2_1 %||% 0; uc <- controls$uc_switch                %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.2\\.2$", wbs)) { qty <- controls$qty_7_2_2 %||% 0; uc <- controls$uc_plc_op_interface      %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.2\\.3$", wbs)) { qty <- controls$qty_7_2_3 %||% 0; uc <- controls$uc_pc_workstation        %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.2\\.4$", wbs)) { qty <- controls$qty_7_2_4 %||% 0; uc <- controls$uc_laser_printer         %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.3\\.1$", wbs)) { qty <- controls$qty_7_3_1 %||% 0; uc <- controls$uc_op_interface_software %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.3\\.2$", wbs)) { qty <- controls$qty_7_3_2 %||% 0; uc <- controls$uc_plc_software          %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.3\\.3$", wbs)) { qty <- controls$qty_7_3_3 %||% 0; uc <- controls$uc_plc_data_software     %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }
  if (grepl("^7\\.3\\.4$", wbs)) { qty <- controls$qty_7_3_4 %||% 0; uc <- controls$uc_plant_intel_software  %||% 0; tc <- if (!is.na(qty)) qty * uc else NA_real_ }

  # ── 8.1.1  GAC Solids Transfer ───────────────────────────────────────────────
  if (grepl("^8\\.1\\.1$", wbs)) {
    qty <- chem_feed$qty_8_1_1 %||% 0
    ds  <- chem_feed$ds_8_1_1  %||% NA_real_  # transfer_rate (lbs/hr) or eductor_size (in. diam)
    uc  <- chem_feed$uc_8_1_1
    tc  <- chem_feed$tc_8_1_1
  }

  # ── 8.1.2  Residuals Holding Tank Transfer ────────────────────────────────────
  if (grepl("^8\\.1\\.2$", wbs)) {
    qty <- chem_feed$qty_8_1_2 %||% 0
    ds  <- chem_feed$ds_8_1_2  %||% NA_real_  # res_transfer_rate (lbs/hr) or res_eductor_size
    uc  <- chem_feed$uc_8_1_2
    tc  <- chem_feed$tc_8_1_2
  }

  # ── 8.2.1  Residuals Mixers ───────────────────────────────────────────────────
  if (grepl("^8\\.2\\.1$", wbs)) {
    qty <- chem_feed$qty_8_2_1 %||% 0
    ds  <- chem_feed$ds_8_2_1  %||% NA_real_  # hmix_size (hp)
    if      (grepl("portable", item_lower)) { uc <- chem_feed$uc_8_2_1_portable; tc <- chem_feed$tc_8_2_1_portable }
    else if (grepl("mounted",  item_lower)) { uc <- chem_feed$uc_8_2_1_mounted;  tc <- chem_feed$tc_8_2_1_mounted  }
    else if (grepl("impeller", item_lower)) { uc <- chem_feed$uc_8_2_1_impeller; tc <- chem_feed$tc_8_2_1_impeller }
  }

  # ── 8.4.1  Ferric Chloride Mixers ────────────────────────────────────────────
  if (grepl("^8\\.4\\.1$", wbs)) {
    qty <- chem_feed$qty_8_4_1 %||% 0
    ds  <- chem_feed$ds_8_4_1  %||% NA_real_  # coag_cmix_size (hp)
    if      (grepl("portable", item_lower)) { uc <- chem_feed$uc_8_4_1_portable; tc <- chem_feed$tc_8_4_1_portable }
    else if (grepl("mounted",  item_lower)) { uc <- chem_feed$uc_8_4_1_mounted;  tc <- chem_feed$tc_8_4_1_mounted  }
    else if (grepl("impeller", item_lower)) { uc <- chem_feed$uc_8_4_1_impeller; tc <- chem_feed$tc_8_4_1_impeller }
  }

  # ── 8.5.1  Polymer Mixers ────────────────────────────────────────────────────
  if (grepl("^8\\.5\\.1$", wbs)) {
    qty <- chem_feed$qty_8_5_1 %||% 0
    ds  <- chem_feed$ds_8_5_1  %||% NA_real_  # polymer_cmix_size (hp)
    if      (grepl("portable", item_lower)) { uc <- chem_feed$uc_8_5_1_portable; tc <- chem_feed$tc_8_5_1_portable }
    else if (grepl("mounted",  item_lower)) { uc <- chem_feed$uc_8_5_1_mounted;  tc <- chem_feed$tc_8_5_1_mounted  }
    else if (grepl("impeller", item_lower)) { uc <- chem_feed$uc_8_5_1_impeller; tc <- chem_feed$tc_8_5_1_impeller }
  }

  # ── 9.1  Initial GAC Charge ──────────────────────────────────────────────────
  # qty = total GAC mass (lbs) across ALL vessels including NRD standby, because
  #       every vessel is physically filled at installation (workbook OUTPUT J248).
  # total_gac_mass_lb_fill is computed in calculate_gac_system() using
  # total_contactors after AutoSize resolves the full vessel count.
  # Falls back to total_gac_mass_lb (operating-only) for backward compatibility.
  if (grepl("^9\\.1$", wbs)) {
    qty <- gac$total_gac_mass_lb_fill %||% gac$total_gac_mass_lb %||% NA_real_
    uc  <- gac$gac_unit_cost          %||% NA_real_
    tc  <- gac$initial_fill_cost      %||% NA_real_
  }

  # ── 14.1.1 / 14.2.1 / 14.3.1 / 14.4.1  Buildings ────────────────────────────
  # Design Size = building footprint (sf); unit cost from bpcost_ubc97 polynomial
  if (grepl("^14\\.[1-4]\\.1$", wbs)) {
    qty <- 1L                                          # always 1 building
    ds  <- site$building_footprint_sf %||% NA_real_   # sf
    uc  <- site$building_uc           %||% NA_real_   # $/sf equivalent (total/sf)
    tc  <- site$building_cost         %||% NA_real_
  }

  # ── 14.5  Concrete Pad ───────────────────────────────────────────────────────
  # qty = 1 unit; design_size = volume in cy; uc = $492.75/cy (VLOOKUP constant)
  if (grepl("^14\\.5$", wbs)) {
    qty <- site$concrete_pad_qty %||% 0
    ds  <- site$concrete_pad_qty %||% NA_real_         # cy (same as qty here)
    uc  <- site$concrete_pad_uc  %||% NA_real_
    tc  <- site$concrete_pad_tc  %||% NA_real_
  }

  # ── Workbook rule: qty == 0 → uc and tc become NA ("--" display) ─────────────
  if (!is.na(qty) && isTRUE(qty == 0)) {
    uc <- NA_real_
    tc <- NA_real_
  }

  list(quantity = qty, design_size = ds, unit_cost = uc, total_cost = tc,
       useful_life = ul)
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
  
  # ── Stage 1: Load Sheet23 and filter by priority selection ───────────────
  # Use priority_selection_table.csv to determine the selected material for
  # each WBS group (item with rank = 1 for the given size × cost combination).
  #
  # priority_selection_table.csv columns:
  #   WBS, Item, Small_Low, Small_Mid, Small_High,
  #              Medium_Low, Medium_Mid, Medium_High,
  #              Large_Low,  Large_Mid,  Large_High
  # Values are rank numbers (1 = selected for that combination).

  # Build the priority column name, e.g. "Small_Low"
  size_key_ps <- dplyr::case_when(
    grepl("small",  size_selection, ignore.case = TRUE) ~ "Small",
    grepl("medium", size_selection, ignore.case = TRUE) ~ "Medium",
    grepl("large",  size_selection, ignore.case = TRUE) ~ "Large",
    .default = "Small"
  )
  cost_key_ps <- stringr::str_to_title(trimws(cost_selection))  # "Low"/"Mid"/"High"
  priority_col_name <- paste0(size_key_ps, "_", cost_key_ps)    # e.g. "Small_Low"

  # Load the priority table (CSV ships with the app)
  ps_csv_path <- "priority_selection_table.csv"
  priorities_ps <- read.csv(ps_csv_path, stringsAsFactors = FALSE, check.names = FALSE)

  # For each WBS group, find the item whose rank == 1 (= the chosen material)
  selected_items_ps <- priorities_ps |>
    dplyr::mutate(ps_rank = suppressWarnings(
                    as.numeric(.data[[priority_col_name]]))) |>
    dplyr::filter(!is.na(ps_rank)) |>
    dplyr::group_by(WBS) |>
    dplyr::slice_min(ps_rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(WBS, Item)

  # ── Override WBS 1.1.1 with the effective vessel material ─────────────────
  # calculate_contactors returns `vessel_material` which already accounts for
  # the "contact vendor" cascade (e.g. FG priority-1 is skipped when vessel
  # volume > 901 gal, and the app uses CSP instead — matching CompSelect).
  # Use that result to ensure the WBS table shows the material that was actually
  # priced, not just the static priority-1 item from the CSV.
  eff_mat <- contactors$vessel_material
  if (!is.null(eff_mat) && !is.na(eff_mat) && nchar(trimws(eff_mat)) > 0) {
    material_to_item_1_1_1 <- c(
      "FG"  = "Fiberglass",
      "CSP" = "Carbon Steel - Plastic Internals",
      "CS"  = "Carbon Steel - Stainless Internals",
      "SS"  = "Stainless Steel"
    )
    eff_item <- material_to_item_1_1_1[eff_mat]
    if (!is.na(eff_item)) {
      selected_items_ps <- selected_items_ps |>
        dplyr::filter(WBS != "1.1.1") |>
        dplyr::bind_rows(
          data.frame(WBS = "1.1.1", Item = as.character(eff_item),
                     stringsAsFactors = FALSE)
        )
      message(sprintf("[WBS Stage1] WBS 1.1.1 overridden to '%s' (effective material: %s)",
                      eff_item, eff_mat))
    }
  }

  # Load Sheet23 (all WBS rows with cost data)
  df <- get_sheet_data("Sheet23", return_type = "table") |>
    janitor::clean_names() |>
    dplyr::mutate(row_index = dplyr::row_number())

  # Keep a row when:
  #   (a) its WBS group does not appear in the priority table at all
  #       (section headers, single-option items, etc.), OR
  #   (b) its (wbs, item) matches the priority-1 item for this size × cost.
  #
  # Matching is case-insensitive on both sides to tolerate minor capitalisation
  # differences between the CSV and the Google Sheet.
  wbs_in_ps <- tolower(trimws(selected_items_ps$WBS))
  selected_pairs_ps <- paste0(
    tolower(trimws(selected_items_ps$WBS)), "|||",
    tolower(trimws(selected_items_ps$Item))
  )

  df <- df |>
    dplyr::filter(
      !(tolower(trimws(wbs)) %in% wbs_in_ps) |
      (paste0(tolower(trimws(wbs)), "|||",
              tolower(trimws(item))) %in% selected_pairs_ps)
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
      design_quantity = purrr::map_dbl(values, ~ .x$quantity    %||% NA_real_),
      design_size     = purrr::map_dbl(values, ~ .x$design_size %||% NA_real_),
      unit_cost       = purrr::map_dbl(values, ~ .x$unit_cost   %||% NA_real_),
      total_cost      = purrr::map_dbl(values, ~ .x$total_cost  %||% NA_real_),
      useful_life     = purrr::map_dbl(values, ~ .x$useful_life %||% NA_real_)
    )

  # ── Post-process: 3.4.2 / 3.4.3 / 3.4.5 / 3.4.6 inherit UL from 3.4.1 ──
  # Workbook formula: IF(unit_cost="NA","N/A", use K of whichever 3.4.1
  # material is selected).  We just copy the UL we already computed for 3.4.1.
  ul_341 <- df$useful_life[grepl("^3\\.4\\.1$", trimws(df$wbs))]
  ul_341 <- if (length(ul_341) > 0 && !is.na(ul_341[1])) ul_341[1] else NA_real_
  df <- df |>
    dplyr::mutate(
      useful_life = dplyr::if_else(
        grepl("^3\\.4\\.[2356]$", trimws(wbs)) & !is.na(unit_cost),
        ul_341,
        useful_life
      )
    )

  # ── Join qty_unit / size_unit from static CSV ─────────────────────────────
  # wbs_design_units.csv maps each WBS number to the unit labels for
  # Design Quantity (e.g. "units", "lf", "cy") and Design Size ("gal", "in. diam").
  du_csv_path <- "wbs_design_units.csv"
  if (file.exists(du_csv_path)) {
    du <- read.csv(du_csv_path, stringsAsFactors = FALSE, check.names = FALSE) |>
      dplyr::mutate(wbs = trimws(as.character(wbs)))
    df <- df |>
      dplyr::left_join(du, by = "wbs")
  } else {
    df$qty_unit  <- NA_character_
    df$size_unit <- NA_character_
  }

  # ── Override size_used_in_estimate with computed design_size ──────────────
  # The workbook's "Size Used in Estimate" column equals Design Size for every
  # active row.  Replace the static Sheet23 value with the live calculated one.
  # Also duplicate size_unit for the "Size used in estimate" unit column — the
  # workbook repeats the same unit label in columns F and H (Design Size Units).
  df <- df |>
    dplyr::mutate(
      size_used_in_estimate = dplyr::if_else(
        !is.na(design_size), design_size, as.numeric(size_used_in_estimate)
      ),
      size_unit_sue = size_unit   # repeat size unit for Size used in estimate col
    )

  # Reorder to the exact workbook column sequence before rename, so that
  # format_wbs_table can hide unit columns by fixed numeric position.
  df <- df |>
    dplyr::select(
      wbs, item,
      design_quantity, qty_unit,
      design_size, size_unit,
      size_used_in_estimate, size_unit_sue,
      unit_cost, total_cost, useful_life,
      table, full_line_item_name, row_index
    ) |>
    dplyr::rename(
      WBS                          = wbs,
      Item                         = item,
      `Design Quantity`            = design_quantity,
      `Quantity Units`             = qty_unit,
      `Design Size`                = design_size,
      `Design Size Units`          = size_unit,
      `Size used in estimate`      = size_used_in_estimate,
      `Design Size Units.1`        = size_unit_sue,
      `Unit Cost`                  = unit_cost,
      `Total Cost`                 = total_cost,
      `Useful Life`                = useful_life
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