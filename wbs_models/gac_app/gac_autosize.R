# gac_autosize.R
# AutoSize inner-loop WBS evaluators for GAC system design
#
# These two functions are called exclusively from calculate_gac_system() during
# the iterative AutoSize search. Each evaluates the full WBS (contactors, GAC,
# pumps, tanks, piping, controls, site, capital, O&M) for a single candidate
# geometry, returning the annualised cost that the outer loop minimises.
#
# Workbook source: AutoSize sheet (VBA macros AutoSize_Opt / AutoSize_OptA)
# Objective:       OUTPUT C417  annualized_cost = -PMT(0.07, useful_life, system_cost) + OM_cost
#
# ── Workbook sheet abbreviations ──────────────────────────────────────────────
#   AutoSize  AutoSize sheet (VBA control + geometry candidate tables)
#   CC        Contactor Constraints
#   CDA       Critical Design Assumptions
#   PPS       Pumps Pipe Structure
#   B&R       Backwash and Regeneration
#   CE        Cost Equations
#   OUTPUT    OUTPUT sheet
# ─────────────────────────────────────────────────────────────────────────────


# calc_ann_for_n ---------------------------------------------------------------
#' Evaluate full WBS for one gravity basin AutoSize candidate
#'
#' Mirrors workbook VBA macro AutoSize_OptA. Called once per candidate n inside
#' the gravity AutoSize loop in calculate_gac_system(). Computes the full WBS
#' cost stack for (n_try basins, lw_try × lw_try ft square basins, bd_try ft
#' bed depth) and returns the annualised cost.
#'
#' Workbook references:
#'   AutoSize C151  extra_search = 5 (non-improvement steps before stopping)
#'   OUTPUT   C413  useful_life  = ROUND((direct_for_UL + addon + indirect) /
#'                                       (depr_direct + (addon+indirect)/20), 1)
#'   OUTPUT   C417  annualized_cost = total_project × CRF + OM_annual
#'   CDA      C25   target bed depth for gravity: 6 ft (flow ≤ 1 MGD), 8 ft (flow > 1 MGD)
#'
#' Component useful lives used in UL formula (gravity concrete basins):
#'   Basins/contactors 30 yr | Pumps 22 yr | Tanks 25 yr | Piping 17 yr
#'   Controls 8 yr | Building 20 yr | Concrete pad 37 yr
#'
#' @param n_try         Integer. Number of operating treatment basins to evaluate.
#' @param lw_try        Numeric. Basin side length (ft); basins are square (length = width).
#' @param bd_try        Numeric. GAC bed depth / basin operating depth (ft).
#' @param params        List. Full params list from calculate_gac_system() — passed by
#'                      reference so defaults have already been applied.
#' @param design_flow_mgd  Numeric. Design flow (MGD), pre-converted from params.
#' @param average_flow_mgd Numeric. Average flow (MGD), pre-converted from params.
#' @param ebct_num      Numeric. EBCT (minutes).
#' @param r_disc        Numeric. Discount rate (default 0.07). OUTPUT C415.
#'
#' @return List with:
#'   \item{ann}{Numeric. Annualised cost ($/yr); Inf if WBS failed.}
#'   \item{valid}{Logical. FALSE if any sub-function threw an error.}
calc_ann_for_n <- function(n_try, lw_try, bd_try,
                           params, design_flow_mgd, average_flow_mgd,
                           ebct_num, r_disc = 0.07) {
  tryCatch(suppressMessages({

    p <- params
    p$num_trains   <- as.integer(n_try)
    p$basin_width  <- lw_try
    p$basin_length <- lw_try
    p$basin_depth  <- bd_try
    p$bed_depth    <- bd_try
    p$use_autosize_a <- "no"

    # ── Apply parameter defaults ────────────────────────────────────────────
    p$service_pumps      <- as.numeric(get_value(p$service_pumps,      0))
    p$backwash_pumps     <- as.numeric(get_value(p$backwash_pumps,     0))
    p$residuals_pumps    <- as.numeric(get_value(p$residuals_pumps,    0))
    p$no_backwash        <- as.numeric(get_value(p$no_backwash,        0))
    p$no_backwash_tank   <- get_value(p$no_backwash_tank,   FALSE)
    p$backwash_interval  <- as.numeric(get_value(p$backwash_interval,  168))
    p$residuals_disposal <- get_value(p$residuals_disposal, "POTW")
    p$residuals_tank     <- get_value(p$residuals_tank,     "no holding tank")
    p$automation_level   <- get_value(p$automation_level,   "fully automated")
    p$manual_override    <- get_value(p$manual_override,    FALSE)
    p$include_buildings  <- get_value(p$include_buildings,  TRUE)
    p$include_hvac       <- get_value(p$include_hvac,       FALSE)
    p$include_land       <- get_value(p$include_land,       TRUE)
    p$include_permits    <- get_value(p$include_permits,    TRUE)
    p$include_pilot      <- get_value(p$include_pilot,      TRUE)
    p$retrofit           <- get_value(p$retrofit,           FALSE)
    p$regen_type         <- get_value(p$regen_type, "regeneration off-site (non-hazardous)")
    p$backwash_frequency <- as.numeric(get_value(p$backwash_frequency, 52))

    # ── Basin counts and geometry ───────────────────────────────────────────
    # CC C32-C35: op_num_basins, NRD_g, total_num_basins
    # NRD_g: CHOOSE(ss_cat2, 0, 1, 2) unless op_num_basins == 1 (always add 1)
    design_flow_gpm_c <- design_flow_mgd * 1e6 / 1440
    min_basin_vol_c   <- design_flow_gpm_c * ebct_num / 7.48
    op_num_c  <- ceiling(min_basin_vol_c / (lw_try * lw_try * bd_try))
    ss_cat2_c <- if (design_flow_mgd < 1) 1L else if (design_flow_mgd < 10) 2L else 3L
    nrd_i_c   <- suppressWarnings(as.numeric(p$redundancy))
    nrd_c     <- if (!is.na(nrd_i_c) && !is.null(p$redundancy) &&
                       !is.na(p$redundancy) && p$redundancy != "") {
      as.integer(nrd_i_c)
    } else if (op_num_c == 1L) { 1L
    } else { c(0L, 1L, 2L)[ss_cat2_c] }
    total_num_c <- op_num_c + nrd_c

    # PPS C58-C79: basin footprint and facility length for this candidate
    # basin_fp = (total_basins * side + (total_basins+1) * wall_thickness)
    #            * (side + 2 * wall_thickness)
    t_thick     <- 1.0  # CDA: basin wall thickness = 1 ft
    basin_fp_c  <- (total_num_c * lw_try + (total_num_c + 1L) * t_thick) *
                   (lw_try + 2 * t_thick)
    facil_len_c <- ceiling(sqrt(basin_fp_c) / 10) * 10  # PPS C81: ROUNDUP(sqrt(fp), -1)

    # ── Full WBS evaluation ─────────────────────────────────────────────────
    con_r <- calculate_contactors(
      design_flow = design_flow_mgd, ebct = ebct_num,
      geometry = p$tank_geometry, num_trains = as.integer(n_try),
      num_contactors_in_series = as.numeric(get_value(p$num_contactors_in_series, 1)),
      redundancy = p$redundancy, bed_depth = bd_try,
      diameter = p$vessel_diameter, height_length = p$vessel_height_length,
      basin_length = lw_try, basin_width = lw_try, basin_depth = bd_try,
      component_level = switch(
        tolower(trimws(as.character(p$automation_level %||% 1))),
        "1"="low","low"="low","low cost"="low","2"="mid","mid"="mid","medium"="mid","mid cost"="mid","3"="high","high"="high","high cost"="high","low"
      )
    )

    # Workbook: GAC_each = media_volume × num_treat_lines (operating trains only, no NRD)
    gac_r <- calculate_gac_requirements(
      total_volume    = con_r$gac_volume_per_contactor * as.integer(n_try),
      influent_conc   = p$influent_conc,
      effluent_target = p$effluent_target,
      average_flow    = average_flow_mgd,
      regen_type      = p$regen_type,
      freund_type     = p$freund_type %||% 4,
      freund_1        = p$freund_1    %||% 66600,
      freund_2        = p$freund_2    %||% NULL,
      design_type     = 2L,
      media_volume    = con_r$gac_volume_per_contactor,
      num_treat_lines = as.integer(n_try),
      BV_definition   = p$BV_definition %||% "EBCT per vessel",
      Num_tanks       = p$num_contactors_in_series %||% 1,
      op_num_basins   = op_num_c,
      bed_depth       = bd_try,
      basin_op_depth  = bd_try
    )

    pmp_r <- calculate_pumps(
      design_flow     = design_flow_mgd,
      num_trains      = as.integer(n_try),
      service_pumps   = p$service_pumps,
      backwash_pumps  = p$backwash_pumps,
      residuals_pumps = p$residuals_pumps,
      tank_geometry   = p$tank_geometry,
      no_backwash     = p$no_backwash,
      ss_cat2         = p$ss_cat2 %||% ss_cat2_c,
      water_flush_gpm = round(12 * lw_try * lw_try, 0)  # B&R C13: 12 gpm/ft²  × comm_SA
    )

    tnk_r <- calculate_tanks(
      design_flow       = design_flow_mgd,
      no_backwash       = p$no_backwash,
      no_backwash_tank  = p$no_backwash_tank,
      backwash_interval = p$backwash_interval,
      residuals_disposal = p$residuals_disposal,
      residuals_tank    = p$residuals_tank,
      num_contactors    = con_r$total_contactors,
      component_level   = 1,
      vessel_sa         = lw_try * lw_try  # comm_SA = basin_width × basin_length (gravity)
    )

    # Pass explicit facil_length so pipe lengths scale correctly with basin layout
    pip_r <- calculate_piping_valves(
      num_contactors    = con_r$total_contactors,
      num_trains        = as.integer(n_try),
      automation_level  = p$automation_level,
      design_flow_mgd   = design_flow_mgd,
      component_level   = 1,
      no_backwash       = p$no_backwash,
      facil_length      = facil_len_c,        # per-n facility length from basin footprint
      vessel_diameter   = p$vessel_diameter,
      vessel_length     = p$vessel_height_length,
      tank_geometry     = p$tank_geometry,
      params            = p,
      tanks             = tnk_r,
      backwash_pumps    = pmp_r$backwash_pumps  %||% 0,
      num_back_tanks    = tnk_r$num_back_tanks   %||% 0,
      num_booster_pumps = pmp_r$booster_pumps    %||% 0
    )

    ctl_r <- calculate_controls(
      automation_level  = p$automation_level,
      num_contactors    = con_r$total_contactors,
      num_trains        = as.integer(n_try),
      manual_override   = p$manual_override,
      ss_cat2           = p$ss_cat2 %||% ss_cat2_c,
      design_type       = p$design_type %||% 2,
      add_on            = p$add_on %||% 0,
      num_back_tanks    = tnk_r$num_back_tanks    %||% 0,
      res_holding       = p$res_holding            %||% "none",
      num_res_tanks     = tnk_r$num_residuals_tanks %||% 0,
      num_res_basins    = tnk_r$num_residuals_basins %||% 0,
      bp_pct            = p$bp_pct                %||% 0,
      in_out_pipe_diam  = pip_r$in_out_pipe_diam   %||% 1.5,
      proc_pipe_diam    = pip_r$proc_pipe_diam      %||% 1.5,
      back_pipe_diam    = pip_r$back_pipe_diam      %||% 2.0,
      res_pipe_diam     = pip_r$res_pipe_diam       %||% 2.0,
      tot_MOVs          = pip_r$mov_quantity         %||% 0,
      fm_lkp_io         = p$fm_lkp_io               %||% "flow_prop",
      fm_lkp_proc       = p$fm_lkp_proc             %||% "flow_mag",
      fm_lkp_back       = p$fm_lkp_back             %||% "flow_prop",
      fm_lkp_res        = p$fm_lkp_res              %||% "flow_prop",
      Operator_LOE      = p$Operator_LOE             %||% 40.607,
      booster_pumps     = pmp_r$booster_pumps        %||% 0,
      backwash_pumps    = pmp_r$backwash_pumps       %||% 0,
      res_pumps         = pmp_r$residuals_pumps      %||% 0,
      transfer_method      = p$transfer_method       %||% 3,
      res_transfer_method  = p$res_transfer_method   %||% 3,
      eductors             = p$eductors              %||% 0,
      res_slurry_pumps     = p$res_slurry_pumps      %||% 0,
      res_eductors         = p$res_eductors          %||% 0,
      mixers               = p$mixers                %||% 0
    )

    chm_r <- calculate_chemical_feed(
      transfer_method      = p$transfer_method      %||% 3,
      eductors             = p$eductors             %||% 0,
      transfer_rate        = p$transfer_rate        %||% 0,
      eductor_size         = p$eductor_size         %||% NA,
      res_holding          = p$res_holding          %||% "none",
      res_transfer_method  = p$res_transfer_method  %||% 3,
      res_slurry_pumps     = p$res_slurry_pumps     %||% 0,
      res_eductors         = p$res_eductors         %||% 0,
      res_transfer_rate    = p$res_transfer_rate    %||% 0,
      res_eductor_size     = p$res_eductor_size     %||% NA,
      hmixers              = p$hmixers              %||% 0,
      hmix_size            = p$hmix_size            %||% 0,
      coag_cmixers         = p$coag_cmixers         %||% 0,
      coag_cmix_size       = p$coag_cmix_size       %||% 0,
      polymer_cmixers      = p$polymer_cmixers      %||% 0,
      polymer_cmix_size    = p$polymer_cmix_size    %||% 0,
      curve                = p$curve                %||% 1
    )

    # Pass basin_fp_c as total_fp so sitework and land scale with basin layout
    sit_r <- calculate_site_buildings(
      include_buildings = p$include_buildings,
      include_hvac      = p$include_hvac,
      include_land      = p$include_land,
      retrofit          = p$retrofit,
      total_contactors  = con_r$total_contactors,
      design_flow       = design_flow_mgd,
      tank_geometry     = p$tank_geometry,
      piping_length_lf  = pip_r$piping_length_lf,
      total_fp          = basin_fp_c              # per-n basin footprint
    )

    cap_r <- compile_capital_costs(
      contactors          = con_r,
      gac                 = gac_r,
      pumps               = pmp_r,
      tanks               = tnk_r,
      piping              = pip_r,
      controls            = ctl_r,
      site                = sit_r,
      include_land        = p$include_land,
      include_permits     = p$include_permits,
      include_pilot       = p$include_pilot,
      retrofit            = p$retrofit,
      design_flow_mgd     = design_flow_mgd,
      residuals_disposal  = p$residuals_disposal %||% "potw"
    )

    om_r <- calculate_om_costs(
      design_flow_mgd    = design_flow_mgd,
      average_flow_mgd   = average_flow_mgd,
      gac_results        = gac_r,
      pump_results       = pmp_r,
      contactor_results  = con_r,
      tank_results       = tnk_r,
      site_results       = sit_r,
      regen_type         = p$regen_type %||% "regeneration off-site (non-hazardous)",
      design_type        = 2L,
      automation_level   = p$automation_level,
      residuals_disposal = p$residuals_disposal %||% "potw",
      retrofit           = p$retrofit,
      backwash_interval  = p$backwash_interval,
      num_trains         = as.numeric(n_try),
      total_num_basins   = as.numeric(total_num_c)
    )

    # ── Useful life (OUTPUT C413) ───────────────────────────────────────────
    # UL = ROUND((direct_for_UL + addon + indirect) /
    #            (depr_direct + (addon+indirect) / build_UL), 1)
    # direct_for_UL excludes GAC initial fill (media replaced separately).
    # Gravity basin component useful lives:
    #   Concrete basins 30yr | Pumps 22yr | Tanks 25yr | Piping 17yr
    #   Controls 8yr | Building 20yr | Concrete pad 37yr
    pad_cost_g  <- 492.75  # CE: concrete_pad_uc
    bldg_cost_g <- max(0, sit_r$building_cost - pad_cost_g)
    depr_direct_g <- (
      con_r$total_cost / 30 +
      pmp_r$total_cost / 22 +
      tnk_r$total_cost / 25 +
      pip_r$total_cost / 17 +
      ctl_r$total_cost / 8  +
      bldg_cost_g      / 20 +
      pad_cost_g       / 37
    )
    direct_for_UL_g <- (
      con_r$total_cost + pmp_r$total_cost + tnk_r$total_cost +
      pip_r$total_cost + ctl_r$total_cost + sit_r$building_cost
    )
    addon_indirect_g <- cap_r$addon_cost + cap_r$total_indirect
    ul_g <- max(1, min(round(
      (direct_for_UL_g + addon_indirect_g) /
      (depr_direct_g + addon_indirect_g / 20), 1), 40))
    crf_g_used <- r_disc * (1 + r_disc)^ul_g / ((1 + r_disc)^ul_g - 1)

    # OUTPUT C417: annualized_cost = total_project × CRF + OM_annual
    list(ann = cap_r$total_project * crf_g_used + om_r$total_annual, valid = TRUE)

  }), error = function(e) {
    message(sprintf("  gravity n=%d: WBS error — %s", n_try, conditionMessage(e)))
    list(ann = Inf, valid = FALSE)
  })
}


# calc_ann_pv ------------------------------------------------------------------
#' Evaluate full WBS for one pressure vessel AutoSize candidate
#'
#' Mirrors workbook VBA macro AutoSize_Opt. Called once per candidate n inside
#' the pressure vessel AutoSize loop in calculate_gac_system(). Geometry
#' (actual_d, actual_bd, actual_h) is pre-computed by the outer loop from
#' AutoSize sheet formulas before this function is called.
#'
#' Workbook references:
#'   AutoSize C58   raw_diam = 2 × (sqrt(SA_req / n / π) + Vessel_thickness)
#'   AutoSize E58   actual_d = ROUNDUP(2 × raw_d, 0) / 2  (0.5 ft increments)
#'   AutoSize E61   actual_bd = ROUNDUP(raw_bd, 1)         (0.1 ft increments)
#'   AutoSize E62   actual_h  = ROUNDUP(2 × raw_h, 0) / 2
#'   CC       C34   NRD = ROUNDUP(num_treat_lines / 4, 0)  (flow >= 1 MGD)
#'   OUTPUT   C413  useful_life formula (see below)
#'   OUTPUT   C417  annualized_cost = total_project × CRF + OM_annual
#'
#' Pressure vessel component useful lives (for UL formula):
#'   CSP/CS vessels 30 yr | FG vessels 20 yr | Pumps 22 yr | Tanks 25 yr
#'   Piping 17 yr | Controls 8 yr | Building 20 yr | Concrete pad 37 yr
#'   GAC initial fill excluded from UL calculation.
#'
#' @param n_try     Integer. Number of operating treatment trains to evaluate.
#' @param actual_d  Numeric. Vessel diameter (ft), rounded to 0.5 ft.
#' @param actual_bd Numeric. Bed depth (ft), rounded to 0.1 ft.
#' @param actual_h  Numeric. Vessel straight height (ft), rounded to 0.5 ft.
#' @param params         List. Full params list from calculate_gac_system().
#' @param design_flow_mgd  Numeric. Design flow (MGD).
#' @param average_flow_mgd Numeric. Average flow (MGD).
#' @param ebct_num         Numeric. EBCT (minutes).
#' @param num_series       Numeric. Contactors in series per train (Num_tanks). CC C18.
#' @param flow_num         Numeric. design_flow_mgd as plain numeric (for NRD logic).
#' @param redund_freq      Integer. NRD denominator; CDA = 4. CC C34.
#' @param r_disc           Numeric. Discount rate; OUTPUT C415 = 0.07.
#'
#' @return List with:
#'   \item{ann}{Numeric. Annualised cost ($/yr); Inf if WBS failed.}
#'   \item{valid}{Logical. FALSE if any sub-function threw an error.}
calc_ann_pv <- function(n_try, actual_d, actual_bd, actual_h,
                        params, design_flow_mgd, average_flow_mgd,
                        ebct_num, num_series, flow_num,
                        redund_freq = 4L, r_disc = 0.07) {
  tryCatch(suppressMessages({

    p <- params
    p$num_trains           <- as.integer(n_try)
    p$vessel_diameter      <- actual_d
    p$vessel_height_length <- actual_h
    p$bed_depth            <- actual_bd
    p$use_autosize_a       <- "no"

    # ── NRD (CC C34) ────────────────────────────────────────────────────────
    # Workbook CC C34: NRD = INT(num_treat_lines / redund_freq)
    # where redund_freq = CDA C16 = 4. Plain floor division, no flow-size
    # special case. Previous logic (NRD=1 when flow<1 MGD and n==1) did not
    # match the workbook and caused the optimizer to favour n=2 over n=1 for
    # small systems, inflating train count, contactor count, and carbon life.
    pv_nrd <- as.integer(n_try %/% redund_freq)
    p$redundancy <- pv_nrd

    # ── Apply parameter defaults ────────────────────────────────────────────
    p$service_pumps      <- as.numeric(get_value(p$service_pumps,      0))
    p$backwash_pumps     <- as.numeric(get_value(p$backwash_pumps,     0))
    p$residuals_pumps    <- as.numeric(get_value(p$residuals_pumps,    0))
    p$no_backwash        <- as.numeric(get_value(p$no_backwash,        0))
    p$no_backwash_tank   <- get_value(p$no_backwash_tank,   FALSE)
    p$backwash_interval  <- as.numeric(get_value(p$backwash_interval,  168))
    p$residuals_disposal <- get_value(p$residuals_disposal, "POTW")
    p$residuals_tank     <- get_value(p$residuals_tank,     "no holding tank")
    p$automation_level   <- get_value(p$automation_level,   "fully automated")
    p$manual_override    <- get_value(p$manual_override,    FALSE)
    p$include_buildings  <- get_value(p$include_buildings,  TRUE)
    p$include_hvac       <- get_value(p$include_hvac,       FALSE)
    p$include_land       <- get_value(p$include_land,       TRUE)
    p$include_permits    <- get_value(p$include_permits,    TRUE)
    p$include_pilot      <- get_value(p$include_pilot,      TRUE)
    p$retrofit           <- get_value(p$retrofit,           FALSE)
    p$regen_type         <- get_value(p$regen_type, "regeneration off-site (non-hazardous)")
    p$backwash_frequency <- as.numeric(get_value(p$backwash_frequency, 52))

    # ss_cat2: 1 = small (<1 MGD), 2 = medium (1–10 MGD), 3 = large (>10 MGD)
    ss_cat2_p <- if (design_flow_mgd < 1) 1L else if (design_flow_mgd < 10) 2L else 3L

    # ── Full WBS evaluation ─────────────────────────────────────────────────
    con_r <- calculate_contactors(
      design_flow = design_flow_mgd, ebct = ebct_num,
      geometry = p$tank_geometry, num_trains = as.integer(n_try),
      num_contactors_in_series = num_series,
      redundancy = p$redundancy, bed_depth = actual_bd,
      diameter = actual_d, height_length = actual_h,
      basin_length = NULL, basin_width = NULL, basin_depth = NULL,
      component_level = switch(
        tolower(trimws(as.character(p$automation_level %||% 1))),
        "1"="low","low"="low","low cost"="low","2"="mid","mid"="mid","medium"="mid","mid cost"="mid","3"="high","high"="high","high cost"="high","low"
      )
    )

    # Workbook: GAC_each = media_volume × num_treat_lines (operating trains only, no NRD)
    gac_r <- calculate_gac_requirements(
      total_volume    = con_r$gac_volume_per_contactor * as.integer(n_try),
      influent_conc   = p$influent_conc,
      effluent_target = p$effluent_target,
      average_flow    = average_flow_mgd,
      regen_type      = p$regen_type,
      freund_type     = p$freund_type %||% 4,
      freund_1        = p$freund_1    %||% 66600,
      freund_2        = p$freund_2    %||% NULL,
      design_type     = 1L,
      media_volume    = con_r$gac_volume_per_contactor,
      num_treat_lines = as.integer(n_try),
      BV_definition   = p$BV_definition %||% "EBCT per vessel",
      Num_tanks       = num_series
    )

    pmp_r <- calculate_pumps(
      design_flow     = design_flow_mgd,
      num_trains      = as.integer(n_try),
      service_pumps   = p$service_pumps,
      backwash_pumps  = p$backwash_pumps,
      residuals_pumps = p$residuals_pumps,
      tank_geometry   = p$tank_geometry,
      no_backwash     = p$no_backwash,
      ss_cat2         = p$ss_cat2 %||% ss_cat2_p,
      water_flush_gpm = round(12 * pi * (actual_d / 2)^2, 0)  # B&R C13: 12 gpm/ft² × comm_SA
    )

    tnk_r <- calculate_tanks(
      design_flow        = design_flow_mgd,
      no_backwash        = p$no_backwash,
      no_backwash_tank   = p$no_backwash_tank,
      backwash_interval  = p$backwash_interval,
      residuals_disposal = p$residuals_disposal,
      residuals_tank     = p$residuals_tank,
      num_contactors     = con_r$total_contactors,
      component_level    = 1,
      vessel_sa          = pi * (actual_d / 2)^2  # comm_SA = π(d/2)² for upright vessels
    )

    pip_r <- calculate_piping_valves(
      num_contactors    = con_r$total_contactors,
      num_trains        = as.integer(n_try),
      automation_level  = p$automation_level,
      design_flow_mgd   = design_flow_mgd,
      component_level   = 1,
      no_backwash       = p$no_backwash,
      facil_length      = NULL,        # derived internally from vessel footprint
      vessel_diameter   = actual_d,
      vessel_length     = actual_h,
      tank_geometry     = p$tank_geometry,
      params            = p,
      tanks             = tnk_r,
      backwash_pumps    = pmp_r$backwash_pumps  %||% 0,
      num_back_tanks    = tnk_r$num_back_tanks   %||% 0,
      num_booster_pumps = pmp_r$booster_pumps    %||% 0
    )

    ctl_r <- calculate_controls(
      automation_level = p$automation_level,
      num_contactors   = con_r$total_contactors,
      num_trains       = as.integer(n_try),
      manual_override  = p$manual_override,
      ss_cat2          = p$ss_cat2 %||% ss_cat2_p,
      design_type      = p$design_type %||% 1,
      add_on           = p$add_on %||% 0,
      num_back_tanks   = tnk_r$num_back_tanks    %||% 0,
      res_holding      = p$res_holding             %||% "none",
      num_res_tanks    = tnk_r$num_residuals_tanks %||% 0,
      num_res_basins   = tnk_r$num_residuals_basins %||% 0,
      bp_pct           = p$bp_pct                  %||% 0,
      in_out_pipe_diam = pip_r$in_out_pipe_diam     %||% 1.5,
      proc_pipe_diam   = pip_r$proc_pipe_diam        %||% 1.5,
      back_pipe_diam   = pip_r$back_pipe_diam        %||% 2.0,
      res_pipe_diam    = pip_r$res_pipe_diam         %||% 2.0,
      tot_MOVs         = pip_r$mov_quantity           %||% 0,
      fm_lkp_io        = p$fm_lkp_io                 %||% "flow_prop",
      fm_lkp_proc      = p$fm_lkp_proc               %||% "flow_mag",
      fm_lkp_back      = p$fm_lkp_back               %||% "flow_prop",
      fm_lkp_res       = p$fm_lkp_res                %||% "flow_prop",
      Operator_LOE     = p$Operator_LOE               %||% 40.607,
      booster_pumps    = pmp_r$booster_pumps          %||% 0,
      backwash_pumps   = pmp_r$backwash_pumps         %||% 0,
      res_pumps        = pmp_r$residuals_pumps        %||% 0,
      transfer_method      = p$transfer_method        %||% 3,
      res_transfer_method  = p$res_transfer_method    %||% 3,
      eductors             = p$eductors               %||% 0,
      res_slurry_pumps     = p$res_slurry_pumps       %||% 0,
      res_eductors         = p$res_eductors           %||% 0,
      mixers               = p$mixers                 %||% 0
    )

    chm_r <- calculate_chemical_feed(
      transfer_method      = p$transfer_method      %||% 3,
      eductors             = p$eductors             %||% 0,
      transfer_rate        = p$transfer_rate        %||% 0,
      eductor_size         = p$eductor_size         %||% NA,
      res_holding          = p$res_holding          %||% "none",
      res_transfer_method  = p$res_transfer_method  %||% 3,
      res_slurry_pumps     = p$res_slurry_pumps     %||% 0,
      res_eductors         = p$res_eductors         %||% 0,
      res_transfer_rate    = p$res_transfer_rate    %||% 0,
      res_eductor_size     = p$res_eductor_size     %||% NA,
      hmixers              = p$hmixers              %||% 0,
      hmix_size            = p$hmix_size            %||% 0,
      coag_cmixers         = p$coag_cmixers         %||% 0,
      coag_cmix_size       = p$coag_cmix_size       %||% 0,
      polymer_cmixers      = p$polymer_cmixers      %||% 0,
      polymer_cmix_size    = p$polymer_cmix_size    %||% 0,
      curve                = p$curve                %||% 1
    )

    sit_r <- calculate_site_buildings(
      include_buildings = p$include_buildings,
      include_hvac      = p$include_hvac,
      include_land      = p$include_land,
      retrofit          = p$retrofit,
      total_contactors  = con_r$total_contactors,
      design_flow       = design_flow_mgd,
      tank_geometry     = p$tank_geometry,
      piping_length_lf  = pip_r$piping_length_lf,
      # PPS C77: build1_fp = ROUNDUP(IF(fp_required<10000, fp_required, vessel_fp), -1)
      # PPS C76: fp_required = vessel_fp + back_tank_fp + pump_fp + office_fp
      # PPS C58: vessel_fp (large) = ROUNDUP((d+2s)² + (n-1)(d+2s)(d+s), 0)
      #          where s = MIN(d, max_space_vessels_cust=6)
      # PPS C62: back_tank_fp = ROUNDUP((bt_diam+2*sp_bt)², 0)
      #          bt_diam: ROUNDUP(2*MAX((2*bt_cf/PI())^(1/3), (4*bt_cf/PI()/12)^0.5), 0)
      # PPS C63-64: pump_fp from pump_dim_table_cl + space_pumps_cust=4ft
      # PPS C75: office_fp = VLOOKUP(flow, labor_table_cl, 3) × 100 sf/employee
      total_fp = {
        d_s   <- actual_d
        n_s   <- con_r$total_contactors
        sp_s  <- min(d_s, if (design_flow_mgd < 1) 1 else 6)
        vfp_s <- if (design_flow_mgd < 1)
          ceiling((d_s+2*sp_s)^2 + (n_s-1)*(d_s+2*sp_s)*(d_s+sp_s/2))  # PPS C59 skid
        else
          ceiling((d_s+2*sp_s)^2 + (n_s-1)*(d_s+2*sp_s)*(d_s+sp_s))    # PPS C58 custom
        if (design_flow_mgd < 1) {
          vfp_s  # ss_cat2=1: no backwash tank, no booster, no office space
        } else {
          wf_s    <- round(12 * pi * (d_s/2)^2, 0)          # B&R C13
          bv_s    <- wf_s * 10 / 7.48                        # ft³; backwash time = 10 min
          bt_d_s  <- ceiling(2 * max((2*bv_s/pi)^(1/3), sqrt(4*bv_s/pi/12)))  # PPS C45
          sp_bt_s <- min(bt_d_s, 6)
          bt_fp_s <- ceiling((bt_d_s + 2*sp_bt_s)^2)        # PPS C62
          sp_p_s        <- 4                                  # CDA: space_pumps_cust = 4 ft
          booster_gpm_s <- min(design_flow_mgd * 1e6 / 1440 * 1.25, 35000)
          pump_l_s <- if (booster_gpm_s<=350) 2.5 else if (booster_gpm_s<=1740) 3.75 else if (booster_gpm_s<=7000) 5 else 7.083
          boost_fp_s    <- ceiling((pump_l_s+sp_p_s)*(pump_l_s+2*sp_p_s))  # PPS C63
          bpft_s   <- wf_s * 1.25
          n_bop_s  <- max(1L, ceiling(bpft_s / 10000))
          n_btot_s <- n_bop_s + 1L                           # +1 NRD backwash pump
          bpr_s    <- bpft_s / n_bop_s
          bpl_s    <- if (bpr_s<=350) 2.5 else if (bpr_s<=1740) 3.75 else if (bpr_s<=7000) 5 else 7.083
          back_pump_fp_s <- ceiling((bpl_s+sp_p_s)*(bpl_s+2*sp_p_s) +
                                    (n_btot_s-1)*(bpl_s+2*sp_p_s)*(bpl_s+sp_p_s))  # PPS C64
          pump_fp_s <- boost_fp_s + back_pump_fp_s
          n_e_s <- if      (design_flow_mgd <= 0.124)  1.0  # PPS C75: labor_table_cl
                   else if (design_flow_mgd <= 0.74)   1.2
                   else if (design_flow_mgd <= 2.152)  1.6
                   else if (design_flow_mgd <= 7.365)  2.8
                   else if (design_flow_mgd <= 22.614) 3.8
                   else 7.8
          fp_req_s <- vfp_s + bt_fp_s + pump_fp_s + n_e_s * 100  # PPS C76
          if (fp_req_s >= 10000) vfp_s else fp_req_s
        }
      }
    )

    cap_r <- compile_capital_costs(
      contactors         = con_r,
      gac                = gac_r,
      pumps              = pmp_r,
      tanks              = tnk_r,
      piping             = pip_r,
      controls           = ctl_r,
      site               = sit_r,
      include_land       = p$include_land,
      include_permits    = p$include_permits,
      include_pilot      = p$include_pilot,
      retrofit           = p$retrofit,
      design_flow_mgd    = design_flow_mgd,
      residuals_disposal = p$residuals_disposal %||% "potw"
    )

    om_r <- calculate_om_costs(
      design_flow_mgd    = design_flow_mgd,
      average_flow_mgd   = average_flow_mgd,
      gac_results        = gac_r,
      pump_results       = pmp_r,
      contactor_results  = con_r,
      tank_results       = tnk_r,
      site_results       = sit_r,
      regen_type         = p$regen_type %||% "regeneration off-site (non-hazardous)",
      design_type        = 1L,
      automation_level   = p$automation_level,
      residuals_disposal = p$residuals_disposal %||% "potw",
      retrofit           = p$retrofit,
      backwash_interval  = p$backwash_interval,
      num_trains         = as.numeric(n_try),
      total_num_basins   = NULL
    )

    # ── Useful life (OUTPUT C413) ───────────────────────────────────────────
    # UL = ROUND((direct_for_UL + addon + indirect) /
    #            (depr_direct + (addon+indirect) / build_UL), 1)
    # Vessel UL: volume > 901 gal → cascades to CSP (30 yr); FG only for ≤ 901 gal (20 yr)
    # GAC initial fill is excluded from direct_for_UL and depreciation.
    vol_gal_p   <- pi * (actual_d/2)^2 * actual_h * 7.48052
    vessel_ul_p <- if (vol_gal_p > 901) 30 else 20
    pad_cost_p  <- 492.75  # CE: concrete_pad_uc
    bldg_cost_p <- max(0, sit_r$building_cost - pad_cost_p)
    depr_direct_p <- (
      con_r$total_cost / vessel_ul_p +
      pmp_r$total_cost / 22 +
      tnk_r$total_cost / 25 +
      pip_r$total_cost / 17 +
      ctl_r$total_cost / 8  +
      bldg_cost_p      / 20 +
      pad_cost_p       / 37
    )
    direct_for_UL_p <- (
      con_r$total_cost + pmp_r$total_cost + tnk_r$total_cost +
      pip_r$total_cost + ctl_r$total_cost + sit_r$building_cost
    )
    addon_indirect_p <- cap_r$addon_cost + cap_r$total_indirect
    ul_p <- max(1, min(round(
      (direct_for_UL_p + addon_indirect_p) /
      (depr_direct_p + addon_indirect_p / 20), 1), 40))
    crf_used <- r_disc * (1 + r_disc)^ul_p / ((1 + r_disc)^ul_p - 1)

    # ── Debug output ────────────────────────────────────────────────────────
    ann_val <- cap_r$total_project * crf_used + om_r$total_annual
    cat(sprintf(
      "[CALC_ANN_PV] n=%d d=%.1f bd=%.1f h=%.1f | vessels=%d | contactor=$%.0f | gac_init=$%.0f | pump=$%.0f | tank=$%.0f | piping=$%.0f | controls=$%.0f | site=$%.0f\n",
      n_try, actual_d, actual_bd, actual_h,
      con_r$total_contactors, con_r$total_cost, gac_r$initial_fill_cost,
      pmp_r$total_cost, tnk_r$total_cost, pip_r$total_cost,
      ctl_r$total_cost, sit_r$total_cost))
    cat(sprintf(
      "[CALC_ANN_PV] n=%d | direct=$%.0f | indirect=$%.0f | addon=$%.0f | total_project=$%.0f | om_annual=$%.0f | ul=%.1fyr | ann=$%.2f\n",
      n_try, cap_r$total_direct, cap_r$total_indirect, cap_r$addon_cost,
      cap_r$total_project, om_r$total_annual, ul_p, ann_val))
    cat(sprintf(
      "[CALC_ANN_PV_OM] n=%d | gac_makeup=$%.0f | gac_regen=$%.0f | bldg_maint=$%.0f | labor=$%.0f | misc=$%.0f | other=$%.0f\n",
      n_try,
      om_r$gac_makeup_cost %||% 0,
      om_r$gac_regen_cost  %||% 0,
      om_r$building_maint  %||% 0,
      (om_r$manager_labor_cost  %||% 0) +
      (om_r$clerical_labor_cost %||% 0) +
      (om_r$operator_labor_cost %||% 0),
      om_r$misc_allowance  %||% 0,
      om_r$total_annual -
        (om_r$gac_makeup_cost     %||% 0) - (om_r$gac_regen_cost    %||% 0) -
        (om_r$building_maint      %||% 0) -
        (om_r$manager_labor_cost  %||% 0) - (om_r$clerical_labor_cost %||% 0) -
        (om_r$operator_labor_cost %||% 0) - (om_r$misc_allowance      %||% 0)))

    # OUTPUT C417
    list(ann = ann_val, valid = TRUE)

  }), error = function(e) {
    message(sprintf("  pv n=%d: WBS error — %s", n_try, conditionMessage(e)))
    list(ann = Inf, valid = FALSE)
  })
}
