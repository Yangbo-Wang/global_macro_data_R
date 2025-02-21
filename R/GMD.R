#' Download Global Macro Data
#'
#' This function downloads a dataset from the Global Macro Database.
#' If no year and month are provided, it fetches the default dataset from "GMD.csv".
#'
#' @param year A numeric value representing the desired year (e.g., 2025). If NULL, the latest default dataset is used.
#' @param month A numeric value representing the month (1, 4, 7, or 10). If NULL, the latest default dataset is used.
#' @param country A string specifying the ISO3 country code (e.g., "CHN" for China). If NULL, returns all countries.
#' @return A data frame containing the requested data.
#' @export
#' @importFrom utils download.file read.csv
GMD <- function(year = NULL, month = NULL, country = NULL) {
  ISO3 <- NULL
  # Base URL for the data source
  base_url <- "https://www.globalmacrodata.com"

  # Default behavior: download GMD.csv when no year and month are provided
  if (is.null(year) || is.null(month)) {
    data_url <- paste0(base_url, "/GMD.csv")
  } else {
    # User-specified year and month: construct the corresponding version UR
    month <- sprintf("%02d", as.integer(month))  # Ensure month is in two-digit format
    if (!month %in% c("01", "04", "07", "10")) {
      stop("Error: Month must be one of 1 (Q1), 4 (Q2), 7 (Q3), or 10 (Q4).")
    }
    version <- sprintf("%d_%s", year, month)
    data_url <- sprintf("%s/GMD_%s.csv", base_url, version)
  }

  # Check if the URL exists before attempting download
  if (!RCurl::url.exists(data_url)) {
    warning(sprintf("Error: Data file not found at %s", data_url))
    return(NULL)
  }

  # Download the CSV file
  temp_file <- tempfile(fileext = ".csv")
  download.file(data_url, temp_file, mode = "wb")

  # Read the CSV file into a data frame
  data <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Error downloading or reading the file.")
    return(NULL)
  })

  # Filter data by country if specified
  if (!is.null(country)) {
    country <- toupper(country)

    # Check if the 'ISO3' column exists before filtering
    if (!"ISO3" %in% colnames(data)) {
      warning("Error: Country filtering failed because 'ISO3' column is missing.")
      return(data)
    }

    # Check if the provided country code is valid
    if (!country %in% unique(data$ISO3)) {
      warning(sprintf("Error: Invalid country code '%s'. Returning full dataset.", country))
      return(data)
    }

    data <- subset(data, ISO3 == country)
    message(sprintf("Filtered data for country: %s", country))
  }

  return(data)
}

