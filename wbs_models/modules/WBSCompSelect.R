# # WBSCompSelect.bas module analysis; 324 lines 
# This module looks at worksheet "OUTPUT"
#ss_cat2 from HVAC sheet, $B$5
#component_level from Instrumentation and Control, $B$5
# Purpose: 
# Selects the best component from alternatives based on priority and cost, filling a "Use" column with values indicating whether each component should be used.
 
# Key Logic:
# Determines priority lookup column based on system size and component level
# Groups components by WBS (Work Breakdown Structure) number
# For each WBS group, selects the component with the lowest priority number that has a valid cost
# Marks selected components with 1, rejected with 0, or error codes

# Data Flow:
# Reads from "OUTPUT" worksheet range "output_db"
# Uses named ranges: "ss_cat2" (system size), "component_level"

# Priority columns start at column 14, organized by size (small/mid/large) and level (lo/mid/hi)

# Component Selection Module - R Translation
# Translated from VBA WBSCompSelect module

# Key Changes in Translation

# Data Structure: R uses data frames instead of Excel ranges/variants
# Indexing: R is 1-based like VBA, making translation straightforward
# Error Handling: Excel error checking adapted to handle NA values and error strings
# Function Style: More functional/vectorized approach where appropriate

# Usage Notes
# Input Data Structure: Your data frame should have columns at these positions:

# Column 1: WBS number
# Column 3: Quantity
# Column 10: Total Cost
# Column 14+: Priority columns (9 total: 3 sizes × 3 levels)

# Key Functions:

# comp_select() - Main entry point
# get_pri_lookup_col() - Determines which priority column to use
# get_component_row() - Selects best component from a WBS group
# Helper functions handle safe type conversion


# Constants
USE_YES <- 1
USE_NO <- 0
USE_UNAVAIL <- "x"
USE_COSTERR <- "ec"
USE_PRIERR <- "ep"

COL_WBS <- 1
COL_QTY <- 3
COL_TOT_COST <- 10
COL_USE <- 12
COL_PRI_START <- 14

#' Main Component Selection Function
#'
#' @param output_data Data frame containing component data
#' @param system_size System size category (1=small, 2=mid, 3=large)
#' @param component_level Component level (0=lo, 1=mid, 2=hi)
#' @return Data frame with Use column populated
#' @export
comp_select <- function(output_data, system_size, component_level) {
  
  # Validate inputs
  if (!system_size %in% 1:3) {
    stop("Component Selection: Invalid system size. Must be 1, 2, or 3.")
  }
  if (!component_level %in% 0:2) {
    stop("Component Selection: Invalid component level. Must be 0, 1, or 2.")
  }
  
  # Determine priority lookup column
  pri_lookup_col <- get_pri_lookup_col(system_size, component_level)
  
  # Initialize Use column
  output_data$Use <- NA
  n_rows <- nrow(output_data)
  
  # Process components by WBS groups
  first_row <- 1
  
  while (first_row <= n_rows) {
    did_increment <- FALSE
    
    # Check if current row has nonzero quantity
    qty <- safe_numeric(output_data[first_row, COL_QTY])
    
    if (!is.na(qty) && qty > 0) {
      # Get WBS for this row
      this_wbs <- safe_string(output_data[first_row, COL_WBS])
      
      if (nchar(this_wbs) > 0) {
        # Find all rows with same WBS
        last_row <- first_row
        while (last_row < n_rows && 
               safe_string(output_data[last_row + 1, COL_WBS]) == this_wbs) {
          last_row <- last_row + 1
        }
        
        # Select component for this WBS group
        output_data <- get_component_row(
          output_data, 
          pri_lookup_col, 
          first_row, 
          last_row
        )
        
        did_increment <- TRUE
      }
    }
    
    # Move to next group
    if (!did_increment) {
      last_row <- first_row
    }
    first_row <- last_row + 1
  }
  
  return(output_data)
}

#' Determine Priority Lookup Column
#'
#' @param size_cat System size category (1=small, 2=mid, 3=large)
#' @param comp_level Component level (0=lo, 1=mid, 2=hi)
#' @return Column index for priority lookup
get_pri_lookup_col <- function(size_cat, comp_level) {
  
  # Find base column for system size
  pri_col <- switch(
    as.character(size_cat),
    "1" = COL_PRI_START,
    "2" = COL_PRI_START + 3,
    "3" = COL_PRI_START + 6,
    stop("Invalid system size category")
  )
  
  # Adjust for component level
  pri_col <- pri_col + comp_level
  
  return(pri_col)
}

#' Select Component from Group
#'
#' @param output_data Data frame with component data
#' @param pri_column Column index for priority values
#' @param first_row First row of WBS group
#' @param last_row Last row of WBS group
#' @return Updated data frame with Use column filled for this group
get_component_row <- function(output_data, pri_column, first_row, last_row) {
  
  rows <- first_row:last_row
  n_rows <- length(rows)
  
  # Extract priorities and costs
  priorities <- numeric(n_rows)
  costs <- numeric(n_rows)
  found_zero <- FALSE
  
  for (i in seq_along(rows)) {
    row_idx <- rows[i]
    
    # Get priority
    pri_result <- safe_numeric_detailed(output_data[row_idx, pri_column])
    priorities[i] <- pri_result$value
    
    if (pri_result$xl_error) {
      output_data$Use[row_idx] <- USE_PRIERR
    } else if (!pri_result$blank && pri_result$value == 0) {
      found_zero <- TRUE
    }
    
    # Get cost
    cost_result <- safe_numeric_detailed(output_data[row_idx, COL_TOT_COST])
    costs[i] <- cost_result$value
    
    # Cost error overwrites priority error
    if (cost_result$xl_error) {
      output_data$Use[row_idx] <- USE_COSTERR
    }
  }
  
  # Find best available component
  ba_priority <- 1
  is_comp_chosen <- FALSE
  chosen_row <- NA
  
  while (!is_comp_chosen && ba_priority <= n_rows) {
    for (i in seq_along(rows)) {
      if (priorities[i] == ba_priority && costs[i] > 0) {
        chosen_row <- rows[i]
        is_comp_chosen <- TRUE
        break
      }
    }
    ba_priority <- ba_priority + 1
  }
  
  # Fill Use column for this WBS group
  for (row_idx in rows) {
    # Don't overwrite error codes
    if (is.na(output_data$Use[row_idx])) {
      if (is_comp_chosen) {
        # Mark chosen vs discarded
        output_data$Use[row_idx] <- ifelse(
          row_idx == chosen_row, 
          USE_YES, 
          USE_NO
        )
      } else {
        # No component chosen
        output_data$Use[row_idx] <- ifelse(
          found_zero, 
          USE_NO, 
          USE_UNAVAIL
        )
      }
    }
  }
  
  return(output_data)
}

#' Safe Numeric Conversion
#'
#' @param x Value to convert
#' @return Numeric value or NA
safe_numeric <- function(x) {
  if (is.na(x) || is.null(x) || !is.finite(suppressWarnings(as.numeric(x)))) {
    return(NA)
  }
  return(as.numeric(x))
}

#' Safe String Conversion
#'
#' @param x Value to convert
#' @return String value or empty string
safe_string <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("")
  }
  return(as.character(x))
}

#' Safe Numeric with Detailed Error Info
#'
#' @param x Value to convert
#' @return List with value, blank flag, and error flag
safe_numeric_detailed <- function(x) {
  result <- list(
    value = 0,
    blank = FALSE,
    xl_error = FALSE
  )
  
  # Check for NA/NULL (blank cell)
  if (is.na(x) || is.null(x) || (is.character(x) && nchar(x) == 0)) {
    result$blank <- TRUE
    return(result)
  }
  
  # Check for Excel error codes
  if (is.character(x) && grepl("^#", x)) {
    result$xl_error <- TRUE
    return(result)
  }
  
  # Try numeric conversion
  num_val <- suppressWarnings(as.numeric(x))
  if (is.na(num_val) || !is.finite(num_val)) {
    return(result)
  }
  
  result$value <- num_val
  return(result)
}

#' Example Usage
#'
#' Demonstrates how to use the component selection functions
example_usage <- function() {
  # Create sample data
  sample_data <- data.frame(
    WBS = c("1.1", "1.1", "1.1", "2.1", "2.1"),
    Component = c("CompA", "CompB", "CompC", "CompD", "CompE"),
    Qty = c(1, 1, 1, 2, 2),
    TotalCost = c(100, 150, 120, 200, 180),
    Pri_Small_Lo = c(1, 2, 3, 1, 2),
    Pri_Small_Mid = c(2, 1, 3, 2, 1),
    Pri_Small_Hi = c(3, 1, 2, 1, 2)
  )
  
  # Adjust column indices if needed
  # Run component selection
  result <- comp_select(
    output_data = sample_data,
    system_size = 1,      # Small
    component_level = 0   # Lo
  )
  
  print(result)
  return(result)
}

# Run example
# example_usage()