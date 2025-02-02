#' Download Global Macro Data CSV
#' @export
download_gmd <- function() {
  url <- "https://www.globalmacrodata.com/GMD.csv"
  data <- read.csv(url)
  return(data)
}

