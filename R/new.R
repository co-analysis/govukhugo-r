#' Open a new editor with the Rmarkdown skeleton
#'
#' The govukhugo package includes a skeleton Rmarkdown file,
#' which can be easily selected via the RStudio GUI. `new_rmd()`
#' allows you to open a new RStudio editor with a copy of the
#' skeleton via the console.
#'
#' @export
new_rmd <- function() {

  if(!rstudioapi::isAvailable()){
    stop("This function only works inside the RStudio IDE")
  } else if (!rstudioapi::isAvailable(version_needed = "1.2.640")) {
    stop("RStudio version 1.2.640 or greater needed")
  }

  rstudioapi::documentNew(
    text = readLines(
      system.file("rmarkdown", "templates", "govuk-hugo", "skeleton",
                  "skeleton.Rmd", package = "govukhugo")),
    type = "rmarkdown",
    rstudioapi::document_position(8,0),
    execute = FALSE
  )

  invisible(TRUE)

}
