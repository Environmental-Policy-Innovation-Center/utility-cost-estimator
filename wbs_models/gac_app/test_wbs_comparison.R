# =============================================================================
# GAC App – WBS Line-Item Comparison Test Script
# =============================================================================
# PURPOSE
#   Run a defined set of test cases through calculate_gac_system() and emit a
#   structured CSV with every WBS line item mirroring the workbook OUTPUT sheet.
#   The WB_* columns are left blank — fill them from the workbook to pinpoint
#   any remaining discrepancies.
#
# HOW TO RUN
#   setwd("wbs_models/gac_app")
#   source("test_wbs_comparison.R")
#
# OUTPUT
#   wbs_comparison_results.csv   — one row per WBS metric per test case
#   wbs_inputs_summary.csv       — confirmed inputs to replicate in the workbook
#
# TEST CASES
#   TC-01  PFAS         | Pressure vessel | 0.124 MGD  (small)
#   TC-02  PFAS         | Pressure vessel | 1.0 MGD    (medium boundary)
#   TC-03  PFAS         | Pressure vessel | 10.0 MGD   (large boundary)
#   TC-04  TCE          | Pressure vessel | 0.124 MGD  (small)
#   TC-05  TCE          | Pressure vessel | 1.0 MGD    (medium)
#   TC-06  PFAS         | Gravity basin   | 1.0 MGD    (medium)
#   TC-07  UVAOP Quench | Pressure vessel | 0.124 MGD  (small, add-on)
#   TC-08  UVAOP Quench | Pressure vessel | 0.74 MGD   (medium, add-on)
#   TC-09  Other        | Pressure vessel | 0.124 MGD  (40,000 BV, EBCT=7.5)
#   TC-10  Other        | Pressure vessel | 1.0 MGD    (40,000 BV, EBCT=7.5)
#   TC-11  Other        | Pressure vessel | 10.0 MGD   (40,000 BV, new backwash, large)
#   TC-12  Other        | Pressure vessel | 0.5 MGD    (20,000 BV, EBCT=10, backwash tank)
#   TC-13  Other        | Pressure vessel | 0.124 MGD  (carbon life = 12 months)
#   TC-14  Other        | Pressure vessel | 1.0 MGD    (Freundlich isotherm, on-site regen)
#   TC-15  Other        | Gravity basin   | 1.0 MGD    (40,000 BV)
#   TC-16  Other        | Pressure vessel | 5.0 MGD    (66,600 BV, EBCT=20, mid cost)
# =============================================================================

cat("=== GAC WBS Comparison Test ===\n")
cat("Loading libraries and sourcing app files...\n")

# ── 0. Environment setup ──────────────────────────────────────────────────────
app_dir <- normalizePath(getwd())
stopifnot("Run from wbs_models/gac_app/" =
            file.exists(file.path(app_dir, "calculations_headers.R")))

suppressPackageStartupMessages({
  source(file.path(app_dir, "load_libraries.R"))
})

source(file.path(app_dir, "utils.R"))
source(file.path(app_dir, "populate_wbs_table.R"))
source(file.path(app_dir, "cost_equations.R"))

googlesheets4::gs4_deauth()

# ── 1. Load caches ───────────────────────────────────────────────────────────
cat("Loading cost coefficients...\n")
.gac_coeff_table <- load_cost_coefficients()
options(gac.coeff_table = .gac_coeff_table)

cat("Caching Google Sheets reference data...\n")
load_gac_sheet_cache()
load_critical_design_assumptions_sheet_cache()
source(file.path(app_dir, "calculations_headers.R"))

# ── 2. Discover available contaminants and design flows ───────────────────────
contam_list    <- get_contam_type()
std_inputs_all <- get_sheet_data("standard_inputs", return_type = "table")

cat("\nAvailable contaminants:\n")
for (i in seq_along(contam_list)) cat(sprintf("  [%2d] %s\n", i, contam_list[i]))

# ── Helpers ───────────────────────────────────────────────────────────────────
find_contam_idx <- function(pattern) {
  hits <- grep(pattern, contam_list, ignore.case = TRUE)
  if (length(hits) == 0) stop(sprintf("No contaminant matching '%s'", pattern))
  if (length(hits) > 1)
    message(sprintf("Multiple matches for '%s': using [%d] %s",
                    pattern, hits[1], contam_list[hits[1]]))
  hits[1]
}

find_design_number <- function(contam_idx, design_type_idx, target_mgd) {
  rows <- std_inputs_all |>
    dplyr::filter(contaminant_selection == contam_idx,
                  design_type           == design_type_idx)
  if (nrow(rows) == 0)
    stop(sprintf("No standard inputs for contam=%d, design_type=%d",
                 contam_idx, design_type_idx))
  flows <- suppressWarnings(as.numeric(rows$design_flow_i))
  best  <- which.min(abs(flows - target_mgd))
  list(design_number = rows$design[best],
       actual_flow   = flows[best],
       matched_row   = rows[best, , drop = FALSE])
}

std_to_params <- function(contam_idx, design_type_idx, design_number) {
  std <- get_standard_inputs(contam_idx, design_type_idx, design_number)
  if (is.null(std)) stop("get_standard_inputs returned NULL")

  # Resolve add_on flag (sheet stores "add-on" text)
  addon_raw <- tolower(trimws(as.character(std$addon %||% "0")))
  addon_val <- {
    n <- suppressWarnings(as.numeric(addon_raw))
    if (!is.na(n)) as.integer(n)
    else if (addon_raw %in% c("add-on","addon","yes","true")) 1L
    else 0L
  }

  list(
    design_flow              = as.numeric(std$design_flow),
    design_flow_units        = std$design_flow_units %||% "MGD",
    average_flow             = suppressWarnings(as.numeric(std$average_flow)),
    average_flow_units       = std$average_flow_units %||% "MGD",
    contaminant              = contam_list[contam_idx],
    design_type              = design_type_idx,
    ebct_type                = suppressWarnings(as.numeric(std$ebct_input_type)),
    ebct                     = suppressWarnings(as.numeric(std$ebct)),
    freund_type              = std$freund_type,          # keep raw; app normalises
    freund_1                 = suppressWarnings(as.numeric(std$freund_1)),
    freund_2                 = suppressWarnings(as.numeric(std$freund_2)),
    influent_conc            = suppressWarnings(as.numeric(std$C_0)),
    effluent_target          = suppressWarnings(as.numeric(std$C_b)),
    tank_geometry            = std$tank_geom_I %||% "upright",
    num_trains               = NULL,
    num_contactors_in_series = suppressWarnings(as.numeric(std$Num_tanks_I) %||% 1),
    redundancy               = suppressWarnings(as.numeric(std$NRD_I)),
    bed_depth                = suppressWarnings(as.numeric(std$bed_depth)),
    vessel_diameter          = suppressWarnings(as.numeric(std$comm_diam)),
    vessel_height_length     = suppressWarnings(as.numeric(std$comm_height_length)),
    basin_length             = suppressWarnings(as.numeric(std$basin_length)),
    basin_width              = suppressWarnings(as.numeric(std$basin_width)),
    basin_depth              = suppressWarnings(as.numeric(std$basin_op_depth)),
    no_backwash              = std$no_backwash_I %||% 0,
    backwash_interval        = suppressWarnings(as.numeric(std$back_interval_I)),
    no_backwash_tank         = isTRUE(tolower(std$no_back_tank_I) %in% c("yes","true","1")),
    regen_type               = std$regen_type_I,
    residuals_disposal       = std$res_s2_opt_I,
    residuals_tank           = std$res_s1_opt_I,
    transfer_method          = std$transfer_method_I,
    solids_hazardous         = std$solids_haz_I,
    service_pumps            = suppressWarnings(as.numeric(std$lines_pump_I) %||% 0),
    backwash_pumps           = 0,
    residuals_pumps          = 0,
    automation_level         = std$component_level_I,
    manual_override          = isTRUE(tolower(std$manual_I) %in% c("yes","true","1")),
    include_buildings        = !isTRUE(tolower(std$include_buildings_I) %in% c("no","false","0")),
    include_hvac             = isTRUE(tolower(std$include_HVAC_I) %in% c("yes","true","1")),
    include_land             = !isTRUE(tolower(std$include_land_I) %in% c("no","false","0")),
    include_permits          = TRUE,
    include_pilot            = TRUE,
    retrofit                 = isTRUE(tolower(std$retrofit_I) %in% c("yes","true","1")),
    add_on                   = addon_val,
    standard_inputs          = std
  )
}

# ── 3. Define test cases ─────────────────────────────────────────────────────
other_base_params <- list(
  design_flow_units        = "MGD",
  average_flow_units       = "MGD",
  contaminant              = "Other",
  design_type              = 1L,
  ebct_type                = 3L,
  freund_type              = 4L,
  freund_1                 = 40000,
  freund_2                 = NA_real_,
  influent_conc            = NA_real_,
  effluent_target          = NA_real_,
  tank_geometry            = "upright",
  num_trains               = NULL,
  num_contactors_in_series = 1L,
  redundancy               = NA_real_,
  bed_depth                = NULL,
  vessel_diameter          = NULL,
  vessel_height_length     = NULL,
  no_backwash              = 0,
  backwash_interval        = 72,
  no_backwash_tank         = FALSE,
  regen_type               = "regeneration off-site (non-hazardous)",
  residuals_disposal       = "POTW",
  residuals_tank           = "no holding tank",
  transfer_method          = "manual transfer",
  solids_hazardous         = NA_character_,
  service_pumps            = 0,
  backwash_pumps           = 0,
  residuals_pumps          = 0,
  automation_level         = "fully automated",
  manual_override          = FALSE,
  include_buildings        = TRUE,
  include_hvac             = FALSE,
  include_land             = TRUE,
  include_permits          = TRUE,
  include_pilot            = TRUE,
  retrofit                 = FALSE,
  add_on                   = 0L,
  standard_inputs          = NULL
)

test_case_defs <- list(

  TC01 = list(
    id          = "TC-01",
    label       = "PFAS | Pressure Vessel | 0.124 MGD",
    contam_pat  = "PFAS|PFOA|perfluoro",
    design_type = 1L,
    target_mgd  = 0.124
  ),

  TC02 = list(
    id          = "TC-02",
    label       = "PFAS | Pressure Vessel | 1.0 MGD",
    contam_pat  = "PFAS|PFOA|perfluoro",
    design_type = 1L,
    target_mgd  = 1.0
  ),

  TC03 = list(
    id          = "TC-03",
    label       = "PFAS | Pressure Vessel | 10.0 MGD",
    contam_pat  = "PFAS|PFOA|perfluoro",
    design_type = 1L,
    target_mgd  = 10.0
  ),

  TC04 = list(
    id          = "TC-04",
    label       = "TCE/PCE | Pressure Vessel | 0.124 MGD",
    contam_pat  = "TCE|PCE|trichloroethyl|tetrachloroethyl",
    design_type = 1L,
    target_mgd  = 0.124
  ),

  TC05 = list(
    id          = "TC-05",
    label       = "TCE/PCE | Pressure Vessel | 1.0 MGD",
    contam_pat  = "TCE|PCE|trichloroethyl|tetrachloroethyl",
    design_type = 1L,
    target_mgd  = 1.0
  ),

  TC06 = list(
    id          = "TC-06",
    label       = "PFAS | Gravity Basin | 1.0 MGD",
    contam_pat  = "PFAS|PFOA|perfluoro",
    design_type = 2L,
    target_mgd  = 1.0
  ),

  TC07 = list(
    id          = "TC-07",
    label       = "UVAOP Quench (Add-on) | Pressure Vessel | 0.124 MGD",
    contam_pat  = "UVAOP|UV.AOP|quench",
    design_type = 1L,
    target_mgd  = 0.124
  ),

  TC08 = list(
    id          = "TC-08",
    label       = "UVAOP Quench (Add-on) | Pressure Vessel | 0.74 MGD",
    contam_pat  = "UVAOP|UV.AOP|quench",
    design_type = 1L,
    target_mgd  = 0.74
  ),

  # ── Other: BV-based carbon life, various flows / settings ───────────────────
  # Average flows match the standard_inputs values at the nearest design flow bin.
  # Source: PFAS standard inputs (the most complete reference set):
  #   0.124 MGD → 0.035 MGD avg  (28% — small system, limited peak demand)
  #   0.305 MGD → 0.094 MGD avg  (31%)
  #   0.500 MGD → 0.148 MGD avg  (30% interpolated)
  #   0.740 MGD → 0.251 MGD avg  (34%)
  #   1.0   MGD → 0.251 MGD avg  (same bin as 0.74 in standard inputs)
  #   5.0   MGD → 2.000 MGD avg  (40% interpolated)
  #   7.365 MGD → 3.200 MGD avg  (44%)
  #  10.0   MGD → 3.200 MGD avg  (same bin as 7.365 in standard inputs)

  TC09 = list(
    id          = "TC-09",
    label       = "Other | Pressure Vessel | 0.124 MGD | 40,000 BV | EBCT=7.5",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(design_flow = 0.124, average_flow = 0.035, ebct = 7.5)
    )
  ),

  TC10 = list(
    id          = "TC-10",
    label       = "Other | Pressure Vessel | 1.0 MGD | 40,000 BV | EBCT=7.5",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(design_flow = 1.0, average_flow = 0.251, ebct = 7.5)
    )
  ),

  # Large system (>1 MGD): triggers new backwash pumps, large-system NRD,
  # larger pipe diameters, and medium/large controls suite.
  TC11 = list(
    id          = "TC-11",
    label       = "Other | Pressure Vessel | 10.0 MGD | 40,000 BV | new backwash",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow   = 10.0,
        average_flow  = 3.200,   # standard inputs: 7.365 MGD design → 3.2 MGD avg
        ebct          = 7.5,
        no_backwash   = "new pumps",
        backwash_interval = 168
      )
    )
  ),

  # Medium flow with shorter BV, longer EBCT, and a backwash holding tank.
  TC12 = list(
    id          = "TC-12",
    label       = "Other | Pressure Vessel | 0.5 MGD | 20,000 BV | EBCT=10 | backwash tank",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow      = 0.5,
        average_flow     = 0.148,  # ~30% of 0.5 MGD, consistent with standard inputs
        ebct             = 10.0,
        freund_1         = 20000,
        no_backwash      = "new pumps",
        backwash_interval= 168,
        residuals_tank   = "backwash holding tank"
      )
    )
  ),

  # Carbon life specified in months (freund_type=1) — tests the direct
  # bed-life path rather than the BV/EBCT calculation.
  TC13 = list(
    id          = "TC-13",
    label       = "Other | Pressure Vessel | 0.124 MGD | carbon life = 12 months",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow  = 0.124,
        average_flow = 0.035,  # standard inputs: 0.124 MGD design → 0.035 MGD avg
        ebct         = 10.0,
        freund_type  = 1L,
        freund_1     = 12
      )
    )
  ),

  # Freundlich isotherm (freund_type=2) with on-site regeneration — tests
  # the Freundlich bed-life branch and regen cost path.
  TC14 = list(
    id          = "TC-14",
    label       = "Other | Pressure Vessel | 1.0 MGD | Freundlich isotherm | on-site regen",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow      = 1.0,
        average_flow     = 0.251,  # standard inputs: ~1 MGD design → 0.251 MGD avg
        ebct             = 20.0,
        freund_type      = 2L,
        freund_1         = 150,
        freund_2         = 0.4,
        influent_conc    = 100,
        effluent_target  = 10,
        regen_type       = "regeneration on-site",
        no_backwash      = "new pumps",
        backwash_interval= 168
      )
    )
  ),

  # Gravity basin — exercises the basin AutoSize code path (AutoSize_OptA),
  # basin contactors, concrete cost, and different piping/controls logic.
  TC15 = list(
    id          = "TC-15",
    label       = "Other | Gravity Basin | 1.0 MGD | 40,000 BV",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow   = 1.0,
        average_flow  = 0.251,
        ebct          = 7.5,
        tank_geometry = "basin",
        design_type   = 2L,
        no_backwash   = "new pumps",
        backwash_interval = 168
      )
    )
  ),

  # Mid-size system at mid component level — tests the medium-cost equipment
  # equations, medium NRD formula, and mid-tier controls.
  TC16 = list(
    id          = "TC-16",
    label       = "Other | Pressure Vessel | 5.0 MGD | 66,600 BV | EBCT=20 | mid cost",
    contam_pat  = NA,
    override_params = c(
      other_base_params,
      list(
        design_flow      = 5.0,
        average_flow     = 2.000,  # ~40% of 5.0 MGD, interpolated from standard inputs
        ebct             = 20.0,
        freund_1         = 66600,
        automation_level = "mid cost",
        no_backwash      = "new pumps",
        backwash_interval= 168,
        residuals_disposal = "POTW"
      )
    )
  )
)

# ── 4. Run each test case ─────────────────────────────────────────────────────
run_test_case <- function(tc) {
  cat(sprintf("\n--- Running %s: %s ---\n", tc$id, tc$label))

  params <- if (!is.na(tc$contam_pat)) {
    cidx <- tryCatch(find_contam_idx(tc$contam_pat),
                     error = function(e) { message("  SKIP: ", e$message); NULL })
    if (is.null(cidx)) return(NULL)
    dn <- find_design_number(cidx, tc$design_type, tc$target_mgd)
    cat(sprintf("  Contaminant [%d]: %s | design=%s | actual_flow=%.3f MGD\n",
                cidx, contam_list[cidx], dn$design_number, dn$actual_flow))
    std_to_params(cidx, tc$design_type, dn$design_number)
  } else {
    tc$override_params
  }
  if (is.null(params)) return(NULL)

  result <- tryCatch(
    suppressMessages(calculate_gac_system(params)),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(result) || !isTRUE(result$success)) {
    message("  Calculation failed.")
    return(NULL)
  }

  cat(sprintf("  --> Direct=$%.0f | Indirect=$%.0f | AddOn=$%.0f | Total=$%.0f\n",
              result$capital_costs$total_direct   %||% NA,
              result$capital_costs$total_indirect %||% NA,
              result$capital_costs$addon_cost     %||% NA,
              result$capital_costs$total_project  %||% NA))

  list(id = tc$id, label = tc$label, params = params, result = result)
}

all_runs        <- lapply(test_case_defs, run_test_case)
successful_runs <- Filter(Negate(is.null), all_runs)
cat(sprintf("\n%d / %d test cases succeeded.\n",
            length(successful_runs), length(test_case_defs)))

# ── 5. Extract metrics ───────────────────────────────────────────────────────
# Returns a named CHARACTER vector so both text and numeric rows can coexist.
# Numeric values are formatted with appropriate precision; text rows store the
# value directly.  WB_ fill-in columns are NA.
extract_metrics <- function(run) {
  r  <- run$result
  p  <- run$params
  co <- r$contactors    %||% list()
  gac<- r$gac           %||% list()
  pu <- r$pumps         %||% list()
  tn <- r$tanks         %||% list()
  pi <- r$piping        %||% list()
  ct <- r$controls      %||% list()
  si <- r$site          %||% list()
  ca <- r$capital_costs %||% list()
  om <- r$om_costs      %||% list()

  is_basin <- isTRUE(tolower(p$tank_geometry %||% "") == "basin")
  n_series  <- max(1, as.numeric(p$num_contactors_in_series %||% 1))

  # ── Derived summary values ────────────────────────────────────────────────
  design_flow_mgd <- as.numeric(p$design_flow %||% NA)
  avg_flow_mgd    <- as.numeric(p$average_flow %||% NA)

  ss_cat <- if (!is.na(design_flow_mgd))
    if (design_flow_mgd < 1) "small" else if (design_flow_mgd <= 10) "medium" else "large"
  else NA_character_

  # Fixed 20-year CRF at 7% (matches workbook "over 20 years at 7%")
  crf_20 <- 0.07 * (1.07^20) / ((1.07^20) - 1)
  total_project   <- as.numeric(ca$total_project %||% NA)
  total_om        <- as.numeric(om$total_annual  %||% NA)
  ann_capital     <- if (!is.na(total_project)) total_project * crf_20 else NA_real_
  total_ann       <- if (!is.na(ann_capital) && !is.na(total_om)) ann_capital + total_om else NA_real_

  # Per-unit costs (workbook: 300 gpd/household, 365.25 days)
  gal_yr          <- if (!is.na(avg_flow_mgd)) avg_flow_mgd * 1e6 * 365.25 else NA_real_
  per_1000_gal    <- if (!is.na(total_ann) && !is.na(gal_yr) && gal_yr > 0)
    total_ann * 1000 / gal_yr else NA_real_
  per_household   <- if (!is.na(per_1000_gal))
    per_1000_gal * 300 * 365.25 / 1000 else NA_real_

  # Annualised capital share %
  cap_pct <- if (!is.na(ann_capital) && !is.na(total_ann) && total_ann > 0)
    round(ann_capital / total_ann * 100) else NA_real_
  om_pct  <- if (!is.na(cap_pct)) 100 - cap_pct else NA_real_

  # Component level / automation (mapped from automation_level field)
  auto_raw   <- tolower(trimws(as.character(p$automation_level %||% "")))
  comp_level <- if (grepl("low|1",  auto_raw)) "low cost"
                else if (grepl("mid|2", auto_raw)) "mid cost"
                else if (grepl("high|3",auto_raw)) "high cost"
                else if (nchar(auto_raw) > 0) auto_raw
                else NA_character_
  sys_auto   <- if (grepl("fully",  auto_raw) || grepl("3|high", auto_raw)) "fully automated"
                else if (grepl("semi",  auto_raw) || grepl("2|mid",  auto_raw)) "semi-automated"
                else if (grepl("manual",auto_raw) || grepl("1|low",  auto_raw)) "manual"
                else if (nchar(auto_raw) > 0) auto_raw
                else NA_character_

  retrofit_yn <- if (isTRUE(p$retrofit)) "yes" else "no"

  # Helper: format number or return blank
  fmt  <- function(x, digits = 0) if (is.null(x)||length(x)==0||is.na(x)) "" else
    formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")
  fmtd <- function(x, digits = 1) fmt(x, digits)  # one decimal
  fmts <- function(x) if (is.null(x)||length(x)==0||is.na(x)) "" else as.character(x)

  # Return a named CHARACTER vector so text and numeric rows coexist cleanly.
  c(
    # ══ OUTPUT SUMMARY — in workbook order ════════════════════════════════════
    SUM_contaminant       = fmts(p$contaminant %||% "Other"),
    SUM_system_size       = fmts(ss_cat),
    SUM_technology        = "GAC",
    SUM_design_type       = if (is_basin) "Gravity" else "Pressure",
    SUM_design_flow       = fmt(p$design_flow, 3),
    SUM_average_flow      = fmt(p$average_flow %||% NA, 3),
    SUM_n_trains          = fmt(r$params$num_trains %||% p$num_trains %||% NA),
    SUM_n_in_series       = fmt(n_series),
    SUM_n_contactors      = fmt(co$total_contactors %||% NA),
    SUM_ebct_total        = fmt(p$ebct %||% NA, 1),
    SUM_ebct_per_contactor= fmt((p$ebct %||% NA) / n_series, 1),
    SUM_carbon_life       = fmt(gac$bed_life_months %||% NA, 1),
    SUM_bed_depth         = fmt(if (is_basin) (r$params$basin_depth %||% p$basin_depth)
                                else (r$params$bed_depth %||% p$bed_depth), 1),
    SUM_vessel_geometry   = fmts(r$params$tank_geometry %||% p$tank_geometry %||% "upright"),
    SUM_height_straight   = fmt(if (is_basin) (r$params$basin_length %||% p$basin_length)
                                else (r$params$vessel_height_length %||% p$vessel_height_length), 1),
    SUM_diameter          = fmt(if (is_basin) (r$params$basin_width %||% p$basin_width)
                                else (r$params$vessel_diameter %||% p$vessel_diameter), 1),
    SUM_component_level   = fmts(comp_level),
    SUM_system_automation = fmts(sys_auto),
    SUM_retrofit          = retrofit_yn,
    SUM_new_carbon_life   = "not applicable",   # fill from result if retrofit = TRUE
    SUM_direct_capital    = fmt(ca$total_direct %||% NA),
    SUM_addon_cost        = fmt(ca$addon_cost   %||% NA),
    SUM_indirect_capital  = fmt(ca$total_indirect %||% NA),
    SUM_total_capital     = fmt(total_project),
    SUM_ann_capital       = fmt(ann_capital),
    SUM_annual_om         = fmt(total_om),
    SUM_total_annualized  = fmt(total_ann),
    SUM_per_1000_gal      = fmt(per_1000_gal, 2),
    SUM_per_household     = fmt(per_household),

    # ══ WBS DETAIL (for line-by-line comparison) ══════════════════════════════
    # ── Design inputs ─────────────────────────────────────────────────────────
    INP_add_on                 = fmt(p$add_on %||% 0),

    # ── WBS 1: GAC Contactors ─────────────────────────────────────────────────
    WBS1_n_vessels             = fmt(co$total_contactors %||% NA),
    WBS1_vol_per_vessel_gal    = fmt(co$volume_per_contactor_gal %||% NA),
    WBS1_unit_cost             = fmt(co$unit_cost %||% NA),
    WBS1_total_cost            = fmt(co$total_cost %||% NA),

    # ── WBS 2.1: Backwash Tanks ───────────────────────────────────────────────
    WBS2_1_n_backwash_tanks    = fmt(tn$num_backwash_tanks %||% 0),
    WBS2_1_vol_gal             = fmt(tn$backwash_tank_volume %||% NA),
    WBS2_1_cost                = fmt(tn$backwash_tank_cost %||% 0),
    WBS2_2_n_residuals_tanks   = fmt(tn$num_residuals_tanks %||% 0),
    WBS2_2_vol_gal             = fmt(tn$residuals_tank_volume %||% NA),
    WBS2_2_cost                = fmt(tn$residuals_tank_cost %||% 0),
    WBS2_total_cost            = fmt(tn$total_cost %||% 0),

    # ── WBS 3: Piping ─────────────────────────────────────────────────────────
    WBS3_1_proc_len_lf         = fmt(pi$proc_pipe_length %||% NA),
    WBS3_1_proc_diam_in        = fmtd(pi$proc_pipe_diam %||% NA),
    WBS3_1_proc_cost           = fmt(pi$proc_pipe_cost %||% NA),
    WBS3_2_back_len_lf         = fmt(pi$back_pipe_length %||% NA),
    WBS3_2_back_diam_in        = fmtd(pi$back_pipe_diam %||% NA),
    WBS3_2_back_cost           = fmt(pi$back_pipe_cost %||% NA),
    WBS3_3_io_len_lf           = fmt(pi$in_out_pipe_length %||% NA),
    WBS3_3_io_diam_in          = fmtd(pi$in_out_pipe_diam %||% NA),
    WBS3_3_io_cost             = fmt(pi$in_out_pipe_cost %||% NA),
    WBS3_4_1_res_pipe_len_lf   = fmt(pi$res_pipe_length %||% NA),
    WBS3_4_1_res_pipe_diam_in  = fmtd(pi$res_pipe_diam %||% NA),
    WBS3_4_1_res_pipe_cost     = fmt(pi$res_pipe_material_cost %||% NA),
    WBS3_4_2_res_excav_cy      = fmt(pi$res_trench_vol_cy %||% NA, 2),
    WBS3_4_3_res_bedding_cy    = fmt(pi$res_bedding_vol_cy %||% NA, 2),
    WBS3_4_5_res_backfill_cy   = fmt(pi$res_trench_vol_cy %||% NA, 2),
    WBS3_4_6_res_thrust_cy     = fmt(pi$res_block_vol_cy %||% NA, 2),
    WBS3_excav_cy              = fmt(pi$excavation_cy %||% NA, 2),
    WBS3_bedding_cy            = fmt(pi$bedding_cy %||% NA, 2),
    WBS3_backfill_cy           = fmt(pi$backfill_cy %||% NA, 2),
    WBS3_thrust_cy             = fmt(pi$thrust_block_cy %||% NA, 2),
    WBS3_install_cost          = fmt(pi$piping_installation_cost %||% NA),

    # ── WBS 4: Valves ─────────────────────────────────────────────────────────
    WBS4_1_1_proc_mov_qty      = fmt(pi$proc_mov_qty %||% NA),
    WBS4_1_1_proc_mov_cost     = fmt(pi$proc_mov_cost %||% NA),
    WBS4_1_2_back_mov_qty      = fmt(pi$back_mov_qty %||% NA),
    WBS4_1_2_back_mov_cost     = fmt(pi$back_mov_cost %||% NA),
    WBS4_1_3_res_mov_qty       = fmt(pi$res_mov_qty %||% NA),
    WBS4_1_3_res_mov_cost      = fmt(pi$res_mov_cost %||% NA),
    WBS4_2_1_in_man_qty        = fmt(pi$in_man_qty %||% NA),
    WBS4_2_1_in_man_cost       = fmt(pi$in_man_cost %||% NA),
    WBS4_2_2_proc_man_qty      = fmt(pi$proc_man_qty %||% NA),
    WBS4_2_2_proc_man_cost     = fmt(pi$proc_man_cost %||% NA),
    WBS4_2_3_back_man_qty      = fmt(pi$back_man_qty %||% NA),
    WBS4_2_3_back_man_cost     = fmt(pi$back_man_cost %||% NA),
    WBS4_2_4_res_man_qty       = fmt(pi$res_man_qty %||% NA),
    WBS4_2_4_res_man_cost      = fmt(pi$res_man_cost %||% NA),
    WBS4_3_1_back_chv_qty      = fmt(pi$back_chv_qty %||% NA),
    WBS4_3_1_back_chv_cost     = fmt(pi$back_chv_cost %||% NA),
    WBS4_3_2_res_chv_qty       = fmt(pi$res_chv_qty %||% NA),
    WBS4_3_2_res_chv_cost      = fmt(pi$res_chv_cost %||% NA),
    WBS4_3_5_in_chv_qty        = fmt(pi$in_chv_qty %||% NA),
    WBS4_3_5_in_chv_cost       = fmt(pi$in_chv_cost %||% NA),
    WBS4_total_valve_cost      = fmt(pi$valve_cost %||% NA),

    # ── WBS 5: Pumps ──────────────────────────────────────────────────────────
    WBS5_1_n_booster           = fmt(pu$service_pumps %||% 0),
    WBS5_1_booster_gpm         = fmt(pu$pump_rating %||% NA),
    WBS5_1_booster_cost        = fmt(pu$service_cost %||% pu$booster_cost %||% 0),
    WBS5_2_n_backwash          = fmt(pu$backwash_pumps %||% 0),
    WBS5_2_backwash_gpm        = fmt(pu$back_pump_rating %||% NA),
    WBS5_2_backwash_cost       = fmt(pu$backwash_cost %||% pu$back_cost %||% 0),
    WBS5_3_n_residuals         = fmt(pu$residuals_pumps %||% 0),
    WBS5_3_residuals_gpm       = fmt(pu$res_pump_rating %||% NA),
    WBS5_3_residuals_cost      = fmt(pu$residuals_cost %||% pu$res_cost %||% 0),
    WBS5_total_cost            = fmt(pu$total_cost %||% 0),

    # ── WBS 6: Instrumentation ────────────────────────────────────────────────
    WBS6_1_fm_in_qty           = fmt(ct$tot_fm_in %||% NA),
    WBS6_1_fm_in_size_in       = fmtd(ct$in_meter_size %||% NA),
    WBS6_1_fm_in_cost          = fmt(ct$fm_in_prop_tc %||% NA),
    WBS6_2_fm_proc_qty         = fmt(ct$tot_fm_proc %||% NA),
    WBS6_2_fm_proc_cost        = fmt(ct$fm_proc_prop_tc %||% NA),
    WBS6_3_fm_back_qty         = fmt(ct$tot_fm_back %||% NA),
    WBS6_3_fm_back_size_in     = fmtd(ct$back_meter_size %||% NA),
    WBS6_3_fm_back_cost        = fmt(ct$fm_back_prop_tc %||% NA),
    WBS6_4_fm_res_qty          = fmt(ct$tot_fm_res %||% NA),
    WBS6_4_fm_res_size_in      = fmtd(ct$res_meter_size %||% NA),
    WBS6_4_fm_res_cost         = fmt(ct$fm_res_prop_tc %||% NA),
    WBS6_ports_qty             = fmt(ct$ports %||% NA),
    WBS6_ports_cost            = fmt(ct$ports_ss_cost %||% NA),

    # ── WBS 7: System Controls ────────────────────────────────────────────────
    WBS7_1_1_plc_rack_qty      = fmt(ct$qty_7_1_1 %||% NA),
    WBS7_1_1_plc_rack_cost     = fmt((ct$qty_7_1_1 %||% 0) * (ct$uc_plc_rack %||% 0)),
    WBS7_1_2_plc_cpu_qty       = fmt(ct$qty_7_1_2 %||% NA),
    WBS7_1_2_plc_cpu_cost      = fmt((ct$qty_7_1_2 %||% 0) * (ct$uc_plc_cpu %||% 0)),
    WBS7_1_3_plc_di_qty        = fmt(ct$qty_7_1_3 %||% NA),
    WBS7_1_3_plc_di_cost       = fmt((ct$qty_7_1_3 %||% 0) * (ct$uc_plc_discrete_input %||% 0)),
    WBS7_1_4_plc_do_qty        = fmt(ct$qty_7_1_4 %||% NA),
    WBS7_1_4_plc_do_cost       = fmt((ct$qty_7_1_4 %||% 0) * (ct$uc_plc_discrete_output %||% 0)),
    WBS7_1_5_plc_analog_qty    = fmt(ct$qty_7_1_5 %||% NA),
    WBS7_1_5_plc_analog_cost   = fmt((ct$qty_7_1_5 %||% 0) * (ct$uc_plc_combination_analog %||% 0)),
    WBS7_1_6_plc_eth_qty       = fmt(ct$qty_7_1_6 %||% NA),
    WBS7_1_6_plc_eth_cost      = fmt((ct$qty_7_1_6 %||% 0) * (ct$uc_plc_ethernet %||% 0)),
    WBS7_1_9_ups_qty           = fmt(ct$qty_7_1_9 %||% NA),
    WBS7_1_9_ups_cost          = fmt((ct$qty_7_1_9 %||% 0) * (ct$uc_ups %||% 0)),
    WBS7_2_1_switches_qty      = fmt(ct$qty_7_2_1 %||% NA),
    WBS7_2_1_switches_cost     = fmt((ct$qty_7_2_1 %||% 0) * (ct$uc_switch %||% 0)),
    WBS7_2_2_op_interface_qty  = fmt(ct$qty_7_2_2 %||% NA),
    WBS7_2_2_op_interface_cost = fmt((ct$qty_7_2_2 %||% 0) * (ct$uc_plc_op_interface %||% 0)),
    WBS67_total_cost           = fmt(ct$total_cost %||% NA),

    # ── WBS 8: Chemical Feed ──────────────────────────────────────────────────
    WBS8_total_cost            = fmt(r$chem_feed$total_cost %||% 0),

    # ── WBS 9: Initial GAC ────────────────────────────────────────────────────
    WBS9_gac_mass_fill_lb      = fmt(gac$total_gac_mass_lb_fill %||% NA),
    WBS9_gac_mass_om_lb        = fmt(gac$total_gac_mass_lb %||% NA),
    WBS9_gac_unit_cost_per_lb  = fmt(gac$gac_unit_cost %||% NA, 2),
    WBS9_initial_fill_cost     = fmt(gac$initial_fill_cost %||% NA),

    # ── WBS 14: Building / Site ───────────────────────────────────────────────
    WBS14_building_fp_sf       = fmt(si$building_footprint_sf %||% NA),
    WBS14_building_cost        = fmt(si$building_cost %||% 0),
    WBS14_pad_cost             = fmt(si$concrete_pad_tc %||% 0),
    WBS_sitework_cost          = fmt(si$site_work_cost %||% 0),
    WBS_yard_piping_cost       = fmt(ca$yard_piping %||% 0),
    WBS_land_cost              = fmt(si$land_cost %||% 0),

    # ── Capital roll-ups ──────────────────────────────────────────────────────
    CAP_equipment_cost         = fmt(ca$equipment_cost %||% NA),
    CAP_materials_cost         = fmt(ca$materials_cost %||% NA),
    CAP_piping_install_cost    = fmt(ca$piping_install_cost %||% NA),
    CAP_controls_cost          = fmt(ca$controls_cost %||% NA),
    CAP_sitework_direct        = fmt(ca$site_work_cost %||% NA),
    CAP_building_cost          = fmt(ca$building_cost %||% NA),
    CAP_total_direct           = fmt(ca$total_direct %||% NA),
    CAP_mobilization           = fmt(ca$mobilization %||% NA),
    CAP_architectural          = fmt(ca$architectural_fees %||% NA),
    CAP_installation           = fmt(ca$installation_transp %||% NA),
    CAP_sitework_indirect      = fmt(ca$sitework_indirect %||% NA),
    CAP_yard_piping_indirect   = fmt(ca$yard_piping %||% NA),
    CAP_geotechnical           = fmt(ca$geotechnical %||% NA),
    CAP_standby_power          = fmt(ca$standby_power %||% NA),
    CAP_electrical             = fmt(ca$electrical %||% NA),
    CAP_instrumentation        = fmt(ca$instrumentation %||% NA),
    CAP_contingency            = fmt(ca$contingency %||% NA),
    CAP_process_engineering    = fmt(ca$process_engineering %||% NA),
    CAP_misc_allowance         = fmt(ca$misc_allowance %||% NA),
    CAP_legal_fiscal           = fmt(ca$legal_fiscal %||% NA),
    CAP_sales_tax              = fmt(ca$sales_tax %||% NA),
    CAP_financing              = fmt(ca$financing %||% NA),
    CAP_construction_mgmt      = fmt(ca$construction_mgmt %||% NA),
    CAP_total_indirect         = fmt(ca$total_indirect %||% NA),
    CAP_permits                = fmt(ca$permit_cost %||% 0),
    CAP_pilot                  = fmt(ca$pilot_cost %||% 0),
    CAP_land_addon             = fmt(ca$land_cost %||% 0),
    CAP_addon_total            = fmt(ca$addon_cost %||% 0),
    CAP_total_project          = fmt(total_project),

    # ── O&M ───────────────────────────────────────────────────────────────────
    OM_manager_hrs             = fmt(om$Manager_LOE %||% NA, 1),
    OM_manager_cost            = fmt(om$labor_manager %||% NA),
    OM_clerical_hrs            = fmt(om$Clerical_LOE %||% NA, 1),
    OM_clerical_cost           = fmt(om$labor_clerical %||% NA),
    OM_operator_hrs            = fmt(om$Operator_LOE %||% NA, 1),
    OM_operator_cost           = fmt(om$labor_operator %||% NA),
    OM_labor_total             = fmt((om$labor_manager %||% 0) + (om$labor_clerical %||% 0) +
                                     (om$labor_operator %||% 0)),
    OM_booster_pump_mtl        = fmt(om$pump_mtl %||% 0),
    OM_backwash_pump_mtl       = fmt(om$back_pump_mtl %||% 0),
    OM_residuals_pump_mtl      = fmt(om$res_pump_mtl %||% 0),
    OM_contactor_mtl           = fmt(om$filter_materials %||% 0),
    OM_building_maint          = fmt(om$bldg_maint_cost %||% 0),
    OM_makeup_gac_lbs          = fmt(om$GAC_makeup_lbs %||% NA),
    OM_makeup_gac_cost         = fmt(om$makeup_gac_cost %||% 0),
    OM_regen_lbs               = fmt(om$regen_yr_lbs %||% NA),
    OM_regen_cost              = fmt(om$off_regen_cost %||% 0),
    OM_booster_energy          = fmt(om$pump_energy_cost %||% 0),
    OM_backwash_energy         = fmt(om$back_pump_energy_cost %||% 0),
    OM_residuals_energy        = fmt(om$res_pump_energy_cost %||% 0),
    OM_lighting                = fmt(om$lighting_cost %||% 0),
    OM_ventilation             = fmt(om$ventilation_cost %||% 0),
    OM_potw_fee                = fmt(om$potw_fee %||% 0),
    OM_misc_allowance          = fmt(om$misc_allowance %||% 0),
    OM_total_annual            = fmt(om$total_annual %||% NA)
  )
}

# ── 6. Human-readable labels ──────────────────────────────────────────────────
metric_labels <- c(
  # ── Output Summary (workbook order) ─────────────────────────────────────────
  SUM_contaminant       = "Contaminant",
  SUM_system_size       = "System Size Category",
  SUM_technology        = "Technology",
  SUM_design_type       = "Design Type",
  SUM_design_flow       = "Design Flow",
  SUM_average_flow      = "Average Flow",
  SUM_n_trains          = "# of treatment trains",
  SUM_n_in_series       = "# of contactors in series",
  SUM_n_contactors      = "# of contactors",
  SUM_ebct_total        = "Total EBCT",
  SUM_ebct_per_contactor= "EBCT per contactor",
  SUM_carbon_life       = "Carbon life",
  SUM_bed_depth         = "Bed depth",
  SUM_vessel_geometry   = "Vessel geometry",
  SUM_height_straight   = "Height (straight)",
  SUM_diameter          = "Diameter",
  SUM_component_level   = "Component level",
  SUM_system_automation = "System automation",
  SUM_retrofit          = "Retrofit (operational modification)?",
  SUM_new_carbon_life   = "New carbon life after retrofit",
  SUM_direct_capital    = "Direct Capital Cost",
  SUM_addon_cost        = "Add-on Cost",
  SUM_indirect_capital  = "Indirect Capital Cost",
  SUM_total_capital     = "Total Capital Cost",
  SUM_ann_capital       = "Annualized Capital Cost (per year over 20 years at 7%)",
  SUM_annual_om         = "Annual O&M Cost (per year)",
  SUM_total_annualized  = "Total Annualized Cost (per year)",
  SUM_per_1000_gal      = "Annualized cost per 1,000 gallons average flow",
  SUM_per_household     = "Annualized cost per household per year",
  # ── WBS detail ──────────────────────────────────────────────────────────────
  INP_add_on                 = "Add-on flag (0/1)",
  WBS1_n_vessels             = "1. # contactors (vessels / basins)",
  WBS1_vol_per_vessel_gal    = "1. Volume per contactor (gal)",
  WBS1_unit_cost             = "1. Unit cost per contactor ($)",
  WBS1_total_cost            = "1. CONTACTOR TOTAL ($)",
  WBS2_1_n_backwash_tanks    = "2.1 # backwash tanks",
  WBS2_1_vol_gal             = "2.1 Backwash tank volume (gal)",
  WBS2_1_cost                = "2.1 Backwash tank cost ($)",
  WBS2_2_n_residuals_tanks   = "2.2 # residuals tanks",
  WBS2_2_vol_gal             = "2.2 Residuals tank volume (gal)",
  WBS2_2_cost                = "2.2 Residuals tank cost ($)",
  WBS2_total_cost            = "2. TANK TOTAL ($)",
  WBS3_1_proc_len_lf         = "3.1 Process pipe length (lf)",
  WBS3_1_proc_diam_in        = "3.1 Process pipe diameter (in)",
  WBS3_1_proc_cost           = "3.1 Process piping cost ($)",
  WBS3_2_back_len_lf         = "3.2 Backwash pipe length (lf)",
  WBS3_2_back_diam_in        = "3.2 Backwash pipe diameter (in)",
  WBS3_2_back_cost           = "3.2 Backwash piping cost ($)",
  WBS3_3_io_len_lf           = "3.3 Influent/treated pipe length (lf)",
  WBS3_3_io_diam_in          = "3.3 Influent/treated pipe diameter (in)",
  WBS3_3_io_cost             = "3.3 Influent/treated piping cost ($)",
  WBS3_4_1_res_pipe_len_lf   = "3.4.1 Residuals pipe length (lf)",
  WBS3_4_1_res_pipe_diam_in  = "3.4.1 Residuals pipe diameter (in)",
  WBS3_4_1_res_pipe_cost     = "3.4.1 Residuals piping cost ($)",
  WBS3_4_2_res_excav_cy      = "3.4.2 Residuals excav. (cy)",
  WBS3_4_3_res_bedding_cy    = "3.4.3 Residuals bedding (cy)",
  WBS3_4_5_res_backfill_cy   = "3.4.5 Residuals backfill (cy)",
  WBS3_4_6_res_thrust_cy     = "3.4.6 Residuals thrust blocks (cy)",
  WBS3_excav_cy              = "3.x.2 Main pipe excav. (cy)",
  WBS3_bedding_cy            = "3.x.3 Main pipe bedding (cy)",
  WBS3_backfill_cy           = "3.x.5 Main pipe backfill (cy)",
  WBS3_thrust_cy             = "3.x.6 Main pipe thrust blocks (cy)",
  WBS3_install_cost          = "3. Piping installation cost ($)",
  WBS4_1_1_proc_mov_qty      = "4.1.1 Process MOVs (qty)",
  WBS4_1_1_proc_mov_cost     = "4.1.1 Process MOV cost ($)",
  WBS4_1_2_back_mov_qty      = "4.1.2 Backwash MOVs (qty)",
  WBS4_1_2_back_mov_cost     = "4.1.2 Backwash MOV cost ($)",
  WBS4_1_3_res_mov_qty       = "4.1.3 Residuals MOVs (qty)",
  WBS4_1_3_res_mov_cost      = "4.1.3 Residuals MOV cost ($)",
  WBS4_2_1_in_man_qty        = "4.2.1 Influent manual valves (qty)",
  WBS4_2_1_in_man_cost       = "4.2.1 Influent manual valve cost ($)",
  WBS4_2_2_proc_man_qty      = "4.2.2 Process manual valves (qty)",
  WBS4_2_2_proc_man_cost     = "4.2.2 Process manual valve cost ($)",
  WBS4_2_3_back_man_qty      = "4.2.3 Backwash manual valves (qty)",
  WBS4_2_3_back_man_cost     = "4.2.3 Backwash manual valve cost ($)",
  WBS4_2_4_res_man_qty       = "4.2.4 Residuals manual valves (qty)",
  WBS4_2_4_res_man_cost      = "4.2.4 Residuals manual valve cost ($)",
  WBS4_3_1_back_chv_qty      = "4.3.1 Backwash check valves (qty)",
  WBS4_3_1_back_chv_cost     = "4.3.1 Backwash check valve cost ($)",
  WBS4_3_2_res_chv_qty       = "4.3.2 Residuals check valves (qty)",
  WBS4_3_2_res_chv_cost      = "4.3.2 Residuals check valve cost ($)",
  WBS4_3_5_in_chv_qty        = "4.3.5 Influent check valves (qty)",
  WBS4_3_5_in_chv_cost       = "4.3.5 Influent check valve cost ($)",
  WBS4_total_valve_cost      = "4. VALVE TOTAL ($)",
  WBS5_1_n_booster           = "5.1 # booster pumps",
  WBS5_1_booster_gpm         = "5.1 Booster pump rating (gpm)",
  WBS5_1_booster_cost        = "5.1 Booster pump cost ($)",
  WBS5_2_n_backwash          = "5.2 # backwash pumps",
  WBS5_2_backwash_gpm        = "5.2 Backwash pump rating (gpm)",
  WBS5_2_backwash_cost       = "5.2 Backwash pump cost ($)",
  WBS5_3_n_residuals         = "5.3 # residuals pumps",
  WBS5_3_residuals_gpm       = "5.3 Residuals pump rating (gpm)",
  WBS5_3_residuals_cost      = "5.3 Residuals pump cost ($)",
  WBS5_total_cost            = "5. PUMP TOTAL ($)",
  WBS6_1_fm_in_qty           = "6.1 Influent FM (qty)",
  WBS6_1_fm_in_size_in       = "6.1 Influent FM size (in)",
  WBS6_1_fm_in_cost          = "6.1 Influent FM cost ($)",
  WBS6_2_fm_proc_qty         = "6.2 Process FM (qty)",
  WBS6_2_fm_proc_cost        = "6.2 Process FM cost ($)",
  WBS6_3_fm_back_qty         = "6.3 Backwash FM (qty)",
  WBS6_3_fm_back_size_in     = "6.3 Backwash FM size (in)",
  WBS6_3_fm_back_cost        = "6.3 Backwash FM cost ($)",
  WBS6_4_fm_res_qty          = "6.4 Residuals FM (qty)",
  WBS6_4_fm_res_size_in      = "6.4 Residuals FM size (in)",
  WBS6_4_fm_res_cost         = "6.4 Residuals FM cost ($)",
  WBS6_ports_qty             = "6.12 Sampling ports (qty)",
  WBS6_ports_cost            = "6.12 Sampling ports cost ($)",
  WBS7_1_1_plc_rack_qty      = "7.1.1 PLC rack (qty)",
  WBS7_1_1_plc_rack_cost     = "7.1.1 PLC rack cost ($)",
  WBS7_1_2_plc_cpu_qty       = "7.1.2 PLC CPU (qty)",
  WBS7_1_2_plc_cpu_cost      = "7.1.2 PLC CPU cost ($)",
  WBS7_1_3_plc_di_qty        = "7.1.3 PLC DI module (qty)",
  WBS7_1_3_plc_di_cost       = "7.1.3 PLC DI module cost ($)",
  WBS7_1_4_plc_do_qty        = "7.1.4 PLC DO module (qty)",
  WBS7_1_4_plc_do_cost       = "7.1.4 PLC DO module cost ($)",
  WBS7_1_5_plc_analog_qty    = "7.1.5 PLC combo analog (qty)",
  WBS7_1_5_plc_analog_cost   = "7.1.5 PLC combo analog cost ($)",
  WBS7_1_6_plc_eth_qty       = "7.1.6 PLC ethernet (qty)",
  WBS7_1_6_plc_eth_cost      = "7.1.6 PLC ethernet cost ($)",
  WBS7_1_9_ups_qty           = "7.1.9 UPS (qty)",
  WBS7_1_9_ups_cost          = "7.1.9 UPS cost ($)",
  WBS7_2_1_switches_qty      = "7.2.1 Drive controllers (qty)",
  WBS7_2_1_switches_cost     = "7.2.1 Drive controller cost ($)",
  WBS7_2_2_op_interface_qty  = "7.2.2 Operator interface (qty)",
  WBS7_2_2_op_interface_cost = "7.2.2 Operator interface cost ($)",
  WBS67_total_cost           = "6+7. CONTROLS TOTAL ($)",
  WBS8_total_cost            = "8. Chemical feed / GAC transfer ($)",
  WBS9_gac_mass_fill_lb      = "9. Initial GAC fill (lbs)",
  WBS9_gac_mass_om_lb        = "9. Operating GAC mass (lbs)",
  WBS9_gac_unit_cost_per_lb  = "9. GAC unit cost ($/lb)",
  WBS9_initial_fill_cost     = "9. INITIAL GAC CHARGE ($)",
  WBS14_building_fp_sf       = "14. Building footprint (sf)",
  WBS14_building_cost        = "14. Building cost ($)",
  WBS14_pad_cost             = "14.5 Concrete pad cost ($)",
  WBS_sitework_cost          = "Site work cost ($)",
  WBS_yard_piping_cost       = "Yard piping cost ($)",
  WBS_land_cost              = "Land cost ($)",
  CAP_equipment_cost         = "  Equipment cost ($)",
  CAP_materials_cost         = "  Materials cost ($)",
  CAP_piping_install_cost    = "  Piping installation cost ($)",
  CAP_controls_cost          = "  Controls cost ($)",
  CAP_sitework_direct        = "  Site work cost ($)",
  CAP_building_cost          = "  Building cost ($)",
  CAP_total_direct           = ">>> TOTAL DIRECT CAPITAL ($)",
  CAP_mobilization           = "  Indirect: Mobilization ($)",
  CAP_architectural          = "  Indirect: Architectural fees ($)",
  CAP_installation           = "  Indirect: Installation/transportation ($)",
  CAP_sitework_indirect      = "  Indirect: Site work ($)",
  CAP_yard_piping_indirect   = "  Indirect: Yard piping ($)",
  CAP_geotechnical           = "  Indirect: Geotechnical ($)",
  CAP_standby_power          = "  Indirect: Standby power ($)",
  CAP_electrical             = "  Indirect: Electrical ($)",
  CAP_instrumentation        = "  Indirect: Instrumentation & control ($)",
  CAP_contingency            = "  Indirect: Contingency ($)",
  CAP_process_engineering    = "  Indirect: Process engineering ($)",
  CAP_misc_allowance         = "  Indirect: Misc allowance ($)",
  CAP_legal_fiscal           = "  Indirect: Legal/fiscal ($)",
  CAP_sales_tax              = "  Indirect: Sales tax ($)",
  CAP_financing              = "  Indirect: Financing ($)",
  CAP_construction_mgmt      = "  Indirect: Construction management ($)",
  CAP_total_indirect         = ">>> TOTAL INDIRECT ($)",
  CAP_permits                = "  Add-on: Permits ($)",
  CAP_pilot                  = "  Add-on: Pilot study ($)",
  CAP_land_addon             = "  Add-on: Land ($)",
  CAP_addon_total            = ">>> TOTAL ADD-ON ($)",
  CAP_total_project          = ">>> GRAND TOTAL PROJECT COST ($)",
  OM_manager_hrs             = "O&M: Manager (hrs/yr)",
  OM_manager_cost            = "O&M: Manager cost ($/yr)",
  OM_clerical_hrs            = "O&M: Clerical (hrs/yr)",
  OM_clerical_cost           = "O&M: Clerical cost ($/yr)",
  OM_operator_hrs            = "O&M: Operator (hrs/yr)",
  OM_operator_cost           = "O&M: Operator cost ($/yr)",
  OM_labor_total             = "O&M: Total labor ($/yr)",
  OM_booster_pump_mtl        = "O&M: Booster pump materials ($/yr)",
  OM_backwash_pump_mtl       = "O&M: Backwash pump materials ($/yr)",
  OM_residuals_pump_mtl      = "O&M: Residuals pump materials ($/yr)",
  OM_contactor_mtl           = "O&M: GAC contactor materials ($/yr)",
  OM_building_maint          = "O&M: Building/HVAC maintenance ($/yr)",
  OM_makeup_gac_lbs          = "O&M: Makeup GAC (lbs/yr)",
  OM_makeup_gac_cost         = "O&M: Makeup GAC cost ($/yr)",
  OM_regen_lbs               = "O&M: Off-site regen GAC (lbs/yr)",
  OM_regen_cost              = "O&M: Off-site regen cost ($/yr)",
  OM_booster_energy          = "O&M: Booster pump energy ($/yr)",
  OM_backwash_energy         = "O&M: Backwash pump energy ($/yr)",
  OM_residuals_energy        = "O&M: Residuals pump energy ($/yr)",
  OM_lighting                = "O&M: Lighting energy ($/yr)",
  OM_ventilation             = "O&M: Ventilation energy ($/yr)",
  OM_potw_fee                = "O&M: POTW discharge fee ($/yr)",
  OM_misc_allowance          = "O&M: Misc allowance ($/yr)",
  OM_total_annual            = ">>> TOTAL ANNUAL O&M ($/yr)"
)

# ── 7. Build comparison table ─────────────────────────────────────────────────
# Values are character strings; WB_ fill-in columns are empty strings.
metrics_list <- lapply(successful_runs, function(run) {
  vals <- tryCatch(extract_metrics(run), error = function(e) {
    message(sprintf("  extract_metrics failed for %s: %s", run$id, e$message)); NULL
  })
  if (is.null(vals)) return(NULL)
  data.frame(metric_key = names(vals), stringsAsFactors = FALSE) |>
    dplyr::mutate(!!run$id := unname(vals))
})
metrics_list <- Filter(Negate(is.null), metrics_list)

if (length(metrics_list) == 0) stop("No metrics extracted — all runs failed.")

comparison_wide <- Reduce(
  function(a, b) dplyr::full_join(a, b, by = "metric_key"),
  metrics_list
)

# Add blank WB_ columns for workbook fill-in
tc_ids <- sapply(successful_runs, `[[`, "id")
for (tc_id in tc_ids) {
  comparison_wide[[paste0("WB_", tc_id)]] <- ""
}

# Add Metric label and Section grouping; preserve metric_key order
comparison_wide <- comparison_wide |>
  dplyr::mutate(
    Metric  = dplyr::coalesce(metric_labels[metric_key], metric_key),
    Section = dplyr::case_when(
      grepl("^SUM_",    metric_key) ~ "0. Output Summary",
      grepl("^INP_",    metric_key) ~ "1. Design Inputs",
      grepl("^WBS1_",   metric_key) ~ "2. Contactors",
      grepl("^WBS2_",   metric_key) ~ "3. Tanks",
      grepl("^WBS3_",   metric_key) ~ "4. Piping",
      grepl("^WBS4_",   metric_key) ~ "5. Valves",
      grepl("^WBS5_",   metric_key) ~ "6. Pumps",
      grepl("^WBS6_",   metric_key) ~ "7. Instrumentation",
      grepl("^WBS7_|^WBS67_", metric_key) ~ "8. System Controls",
      grepl("^WBS8_",   metric_key) ~ "9. Chemical Feed",
      grepl("^WBS9_",   metric_key) ~ "10. Initial GAC",
      grepl("^WBS1[2-4]_|^WBS_", metric_key) ~ "11. Buildings/Site",
      grepl("^CAP_",    metric_key) ~ "12. Capital Roll-ups",
      grepl("^OM_",     metric_key) ~ "13. O&M",
      TRUE ~ "Other"
    ),
    .before = 1
  )

# ── 8. Inputs summary table ───────────────────────────────────────────────────
inputs_rows <- lapply(successful_runs, function(run) {
  p <- run$params
  data.frame(
    TestCase        = run$id,
    Label           = run$label,
    Contaminant     = as.character(p$contaminant %||% "Other"),
    DesignType      = if (isTRUE(tolower(p$tank_geometry) == "basin")) "Gravity" else "Pressure",
    AddOn           = as.integer(p$add_on %||% 0),
    DesignFlow_MGD  = as.numeric(p$design_flow),
    AverageFlow_MGD = as.numeric(p$average_flow %||% NA),
    EBCT_min        = as.numeric(p$ebct %||% NA),
    CarbonLifeType  = as.character(p$freund_type %||% NA),
    CarbonLifeValue = as.numeric(p$freund_1 %||% NA),
    SpentCarbon     = as.character(p$regen_type %||% NA),
    BackwashInterval= as.numeric(p$backwash_interval %||% NA),
    ResidualDisposal= as.character(p$residuals_disposal %||% NA),
    TransferMethod  = as.character(p$transfer_method %||% NA),
    AutomationLevel = as.character(p$automation_level %||% NA),
    stringsAsFactors = FALSE
  )
})
inputs_summary <- do.call(rbind, inputs_rows)

# ── 9. Write outputs ──────────────────────────────────────────────────────────
out_comparison <- file.path(app_dir, "wbs_comparison_results.csv")
out_inputs     <- file.path(app_dir, "wbs_inputs_summary.csv")

write.csv(comparison_wide, out_comparison, row.names = FALSE, na = "")
write.csv(inputs_summary,  out_inputs,     row.names = FALSE, na = "")

cat(sprintf("\n=== Output files written ===\n"))
cat(sprintf("  Comparison : %s\n", out_comparison))
cat(sprintf("  Inputs     : %s\n", out_inputs))
cat(sprintf("\nTest cases run: %s\n", paste(tc_ids, collapse = ", ")))
cat("\nColumns for each test case:\n")
cat("  TC-xx    = app-computed value\n")
cat("  WB_TC-xx = workbook value (fill in manually to spot discrepancies)\n")
cat("\nKey summary rows to check first:\n")
cat("  CAP_total_direct, CAP_total_indirect, CAP_addon_total,\n")
cat("  CAP_total_project, OM_total_annual\n")
