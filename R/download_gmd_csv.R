#' Download Global Macro Data CSV
#'
#' This function downloads the Global Macro Data CSV file and allows filtering.
#'
#' @param start_year Numeric. The starting year for filtering.
#' @param end_year Numeric. The ending year for filtering.
#' @param country Character. The country name to filter.
#' @param ISO3 Character. The three-letter country code (ISO3) to filter.
#' @return A filtered data frame.
#' @export
#'
#' @examples
#' df <- download_gmd_csv(start_year = 2000, end_year = 2020, country = "United States")
download_gmd_csv <- function(start_year = NULL, end_year = NULL, country = NULL, ISO3 = NULL) {
  url <- "https://www.globalmacrodata.com/GMD.csv"
  temp_file <- tempfile(fileext = ".csv")

  tryCatch({
    download.file(url, temp_file, mode = "wb")
    data <- read.csv(temp_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to download CSV file: ", e)
  })

  if (!all(c("year", "countryname", "ISO3") %in% names(data))) {
    stop("Missing columns: 'year', 'countryname', 'ISO3'")
  }

  # Filter data
  if (!is.null(start_year)) data <- data[data$Year >= start_year, ]
  if (!is.null(end_year)) data <- data[data$Year <= end_year, ]
  if (!is.null(country)) data <- data[data$Country == country, ]
  if (!is.null(ISO3)) data <- data[data$ISO3 == ISO3, ]

  return(data)
}

