#' Download and Filter Global Macro Data
#'
#' This function downloads a dataset from the Global Macro Database based on the provided year, quarter, and country.
#'
#' @param year A numeric value representing the desired year (e.g., 2025). If NULL, the latest available dataset is used.
#' @param quarter A numeric value representing the quarter (1, 3, 6, or 9). If NULL, the latest available dataset is used.
#' @param country A character string specifying the ISO3 country code (e.g., "CHN"). If NULL, returns all countries.
#' @return A data frame containing the requested data.
#' @export
#' @importFrom utils download.file read.csv
GMD <- function(year = NULL, quarter = NULL, country = NULL) {
  ISO3 <- NULL
  # Base URL
  base_url <- "https://www.globalmacrodata.com"

  # Validate year input
  if (!is.null(year) && (!is.numeric(year) || year < 2020 || year > 2050)) {
    stop("Error: Year must be a numeric value between 2020 and 2050.")
  }

  # Validate quarter input
  valid_quarters <- c(1, 3, 6, 9, 12)
  if (!is.null(quarter)) {
    if (!quarter %in% valid_quarters) {
      stop("Error: Quarter must be one of 1, 3, 6, 9, or 12.")
    }
    quarter <- sprintf("%02d", as.integer(quarter))  # Format quarter as "01", "03", etc.
  }

  # Automatically find the latest available dataset if year and quarter are NULL
  if (is.null(year) || is.null(quarter)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    current_quarter <- c(12, 9, 6, 3, 1)  # Order of checking
    found <- FALSE
    for (y in current_year:2020) {
      for (q in current_quarter) {
        url <- sprintf("%s/GMD_%d_%02d.csv", base_url, y, q)
        if (RCurl::url.exists(url)) {
          year <- y
          quarter <- sprintf("%02d", q)
          found <- TRUE
          break
        }
      }
      if (found) break
    }

    if (!found) {
      stop("Error: No available dataset found on the server.")
    }
  }

  # Construct the final URL
  data_url <- sprintf("%s/GMD_%d_%s.csv", base_url, year, quarter)

  # Check if the URL exists
  if (!RCurl::url.exists(data_url)) {
    stop(sprintf("Error: Data file not found at %s", data_url))
  }

  # Download the CSV file
  temp_file <- tempfile(fileext = ".csv")
  download.file(data_url, temp_file, mode = "wb")

  # Read CSV file into a data frame
  data <- tryCatch({
    read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error downloading or reading the file.")
  })

  # Ensure required columns exist
  required_columns <- c("year", "ISO3", "countryname")
  if (!all(required_columns %in% colnames(data))) {
    stop("Error: Required columns missing in dataset.")
  }

  # Filter by country
  if (!is.null(country)) {
    country <- toupper(country)
    if (!country %in% unique(data$ISO3)) {
      stop(sprintf("Error: Invalid country code '%s'.", country))
    }
    data <- subset(data, ISO3 == country)
    message(sprintf("Filtered data for country: %s", country))
  }

  # Show dataset info
  message(sprintf("Final dataset: %d observations of %d variables", nrow(data), ncol(data)))

  return(data)
}
