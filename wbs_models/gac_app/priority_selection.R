# Priority-Based Component Selection System
# Implements Excel's CompSelect macro logic

#' Load priority selection table
#' @return Data frame with equipment priorities
load_priority_table <- function() {
  # Try multiple locations
  possible_paths <- c(
    "/mnt/user-data/outputs/priority_selection_table.csv",
    "priority_selection_table.csv",
    "data/priority_selection_table.csv",
    "wbs_models/gac_app/priority_selection_table.csv"
  )
  
  priority_file <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      priority_file <- path
      break
    }
  }
  
  if (is.null(priority_file)) {
    stop("Priority selection table not found. Tried: ", 
         paste(possible_paths, collapse = ", "))
  }
  
  priorities <- read.csv(priority_file, stringsAsFactors = FALSE)
  return(priorities)
}

#' Determine system size category
#' @param design_flow_mgd Design flow in MGD
#' @return "Small", "Medium", or "Large"
get_system_size <- function(design_flow_mgd) {
  if (design_flow_mgd < 1) {
    "Small"
  } else if (design_flow_mgd <= 10) {
    "Medium"
  } else {
    "Large"
  }
}

#' Get priority column name based on size and component level
#' @param system_size "Small", "Medium", or "Large"
#' @param component_level "Low", "Mid", or "High"
#' @return Column name like "Small_Low"
get_priority_column <- function(system_size, component_level) {
  paste(system_size, component_level, sep = "_")
}

#' Select component based on priorities
#' @param wbs_category WBS category (e.g., "1.1.1" for pressure vessels)
#' @param design_flow_mgd Design flow in MGD
#' @param component_level "Low", "Mid", or "High"
#' @return List with selected item and its properties
select_component <- function(wbs_category, design_flow_mgd, component_level = "Low") {
  
  # Load priority table
  priorities <- load_priority_table()
  
  # Filter to this WBS category
  category_items <- priorities[priorities$WBS == wbs_category, ]
  
  if (nrow(category_items) == 0) {
    warning(paste("No items found for WBS category:", wbs_category))
    return(NULL)
  }
  
  # Determine system size
  system_size <- get_system_size(design_flow_mgd)
  
  # Get priority column
  priority_col <- get_priority_column(system_size, component_level)
  
  # Get priorities for each item
  category_items$priority <- as.numeric(category_items[[priority_col]])
  
  # Remove items with no priority (NA or empty)
  category_items <- category_items[!is.na(category_items$priority) & 
                                   category_items$priority != "", ]
  
  if (nrow(category_items) == 0) {
    warning(paste("No valid priorities for WBS:", wbs_category, 
                  "Size:", system_size, "Level:", component_level))
    return(NULL)
  }
  
  # Select item with LOWEST priority number (1 = best)
  selected_row <- category_items[which.min(category_items$priority), ]
  
  return(list(
    wbs = selected_row$WBS,
    item = selected_row$Item,
    priority = selected_row$priority,
    system_size = system_size,
    component_level = component_level
  ))
}

#' Select pressure vessel material
#' @param design_flow_mgd Design flow in MGD
#' @param component_level "Low", "Mid", or "High"
#' @return Material code ("FG", "CS", "CSP", "SS")
select_vessel_material <- function(design_flow_mgd, component_level = "Low") {
  
  selected <- select_component("1.1.1", design_flow_mgd, component_level)
  
  if (is.null(selected)) {
    # Fallback to simplified approach
    if (design_flow_mgd < 0.5) return("FG")
    if (design_flow_mgd < 2.0) return("CS")
    return("SS")
  }
  
  # Map item name to material code
  item_lower <- tolower(selected$item)
  
  if (grepl("fiberglass", item_lower)) {
    return("FG")
  } else if (grepl("stainless steel", item_lower) && !grepl("carbon", item_lower)) {
    return("SS")
  } else if (grepl("carbon.*stainless", item_lower)) {
    return("CS")  # CS with SS internals
  } else if (grepl("carbon.*plastic", item_lower)) {
    return("CSP")  # CS with plastic internals
  } else {
    return("CS")  # Default to carbon steel
  }
}

#' Select piping material for a given piping type
#' @param piping_wbs WBS code for piping type (e.g., "3.1" for process piping)
#' @param design_flow_mgd Design flow in MGD
#' @param component_level "Low", "Mid", or "High"
#' @return Material code ("PVC", "CPVC", "DI", "SS", "ST")
select_piping_material <- function(piping_wbs, design_flow_mgd, component_level = "Low") {
  
  selected <- select_component(piping_wbs, design_flow_mgd, component_level)
  
  if (is.null(selected)) {
    # Fallback
    if (design_flow_mgd < 1.0) return("PVC")
    return("DI")
  }
  
  # Map item name to material code
  item_lower <- tolower(selected$item)
  
  if (grepl("pvc", item_lower) && !grepl("cpvc", item_lower)) {
    return("PVC")
  } else if (grepl("cpvc", item_lower)) {
    return("CPVC")
  } else if (grepl("ductile", item_lower)) {
    return("DI")
  } else if (grepl("stainless", item_lower)) {
    return("SS")
  } else if (grepl("steel", item_lower)) {
    return("ST")
  } else {
    return("PVC")  # Default
  }
}

#' Test the priority selection system
test_priority_selection <- function() {
  
  cat("\n=== Testing Priority Selection System ===\n")
  
  # Test vessel selection for different scenarios
  scenarios <- list(
    list(flow = 0.03, level = "Low", expected = "FG"),
    list(flow = 0.03, level = "Mid", expected = "CSP"),
    list(flow = 0.03, level = "High", expected = "SS"),
    list(flow = 1.0, level = "Low", expected = "FG"),
    list(flow = 5.0, level = "Low", expected = "CS")
  )
  
  for (scenario in scenarios) {
    result <- select_vessel_material(scenario$flow, scenario$level)
    match <- if (result == scenario$expected) "✓" else "✗"
    cat(sprintf("%s Flow: %.2f MGD, Level: %s → %s (expected %s)\n",
                match, scenario$flow, scenario$level, result, scenario$expected))
  }
  
  cat("\n")
}
