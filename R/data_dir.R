#' Get the right data directory
#'
#' The govukhugo package assumes data is stored in R/data, when processing
#' Rmarkdown files this folder is copied to a temporary folder. This is a helper
#' function to easily work both interactively when editing an Rmarkdown file
#' and when running the `build_hugo_rmd()` function.
#'
#' @param alt_path a non-standard location
#'
#' @return the location of the data folder
#' @export
data_dir <- function(alt_path = NULL) {

  if (!is.null(alt_path)) {
    # return supplied path if provided
    out_path <- path
  } else if (interactive()) {
    # if editing Rmarkdown set as the R/data folder
    out_path <- here::here("R", "data")
  } else {
    # if kniting Rmarkdown set as the data folder inside temporary folder
    out_path <- here::here("data")
  }

  return(out_path)

}
