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
