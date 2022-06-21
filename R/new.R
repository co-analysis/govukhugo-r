#' Open a new editor with the Rmarkdown skeleton
#'
#' The govukhugo package includes a skeleton Rmarkdown file,
#' which can be easily selected via the RStudio GUI. `new_rmd()`
#' allows you to open a new RStudio editor with a copy of the
#' skeleton via the console.
#'
#' @param title The page title
#' @param date The page date
#' @param section The section the page will go in
#' @param weight The page weight
#' @param summary A summary of the page
#'
#' @export
new_rmd <- function(title = NULL, date = NULL, section = NULL,
                    weight = NULL, summary = NULL) {

  if(!rstudioapi::isAvailable()){
    stop("This function only works inside the RStudio IDE")
  } else if (!rstudioapi::isAvailable(version_needed = "1.2.640")) {
    stop("RStudio version 1.2.640 or greater needed")
  }

  doc_id <- rstudioapi::documentNew(
    text = readLines(
      system.file("rmarkdown", "templates", "govuk-hugo", "skeleton",
                  "skeleton.Rmd", package = "govukhugo")),
    type = "rmarkdown",
    rstudioapi::document_position(8,0),
    execute = FALSE
  )

  if (!is.null(title)) {
    if (is.character(title) & length(title) == 1) {
      rstudioapi::insertText(
        location = rstudioapi::document_range(
          rstudioapi::document_position(2, 9),
          rstudioapi::document_position(2, 19)),
        text = title,
        id = doc_id)
    } else {
      warning("title ignored: must be a character vector of length 1")
    }
  }

  if (!is.null(date)) {
    if (is.character(date) & length(date) == 1) {
      if (grepl("^\\d{4}-\\d{2}-\\d{2}", date)) {

        ds <- as.numeric(unlist(strsplit(date, "-")))

        if (ds[2] < 1 | ds[2] > 12) {
          warning("date ignored: month must be between 01 and 12")
        } else if (ds[3] < 1 | ds[3] > 31) {
          warning("date ignored: day must be between 01 and 31")
        } else {
          rstudioapi::insertText(
            location = rstudioapi::document_range(
              rstudioapi::document_position(3, 7),
              rstudioapi::document_position(3, 17)),
            text = date,
            id = doc_id)
        }

      } else {
        warning("date ignored: must be in the format YYYY-MM-DD")
      }
    } else {
      warning("date ignored: must be a character vector of length 1")
    }
  }

  if (!is.null(section)) {
    if (is.character(section) & length(section) == 1) {
      rstudioapi::insertText(
        location = rstudioapi::document_range(
          rstudioapi::document_position(4, 10),
          rstudioapi::document_position(4, 18)),
        text = section,
        id = doc_id)
    } else {
      warning("section ignored: must be a character vector of length 1")
    }
  }

  if (!is.null(weight)) {
    if (is.numeric(weight) & length(weight) == 1) {
      rstudioapi::insertText(
        location = rstudioapi::document_range(
          rstudioapi::document_position(5, 9),
          rstudioapi::document_position(5, 12)),
        text = as.character(weight),
        id = doc_id)
    } else {
      warning("weight ignored: must be a numeric vector of length 1")
    }
  }

  if (!is.null(summary)) {
    if (is.character(summary) & length(summary) == 1) {
      rstudioapi::insertText(
        location = rstudioapi::document_range(
          rstudioapi::document_position(6, 11),
          rstudioapi::document_position(6, 39)),
        text = summary,
        id = doc_id)
    } else {
      warning("summary ignored: must be a character vector of length 1")
    }
  }

  invisible(TRUE)

}
