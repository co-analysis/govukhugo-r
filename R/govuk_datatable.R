#' datatables for GOV.UK
#'
#' `govuk_datatable` is a wrapper for [DT::datatable()] that will enable the
#' govuk-hugo theme to apply GOV.UK CSS classes to the output.
#'
#' @param data the data frame to display
#' @param element_id optionally, an id for the resulting HTML chunk
#' @param col_names optionally, a vector of column names, otherwise the names of the data object
#' @param page_length the number of rows to show per page, default is 10
#' @param small_text whether to render the table content with a smaller font
#' @param buttons whether to include copy and download buttons
#' @param col_defs optionally, a list of column definitions
#' @param options optionally, a list of additional options to pass to [DT::datatable()]
#'
#' @export
govuk_datatable <- function(data,
                            element_id = NULL,
                            col_names = NULL,
                            page_length = 10,
                            search = TRUE,
                            small_text = FALSE,
                            buttons = TRUE,
                            col_defs = NULL,
                            options = NULL) {

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The DT package is required. ",
      "Install from CRAN using install.packages(\"DT\")"
    )
  }

  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("data must be a data frame or matrix")
  }


  if (small_text) {
    dt_class <- "govuk-table govuk-!-font-size-14"
  } else {
    dt_class <- "govuk-table"
  }

  if (is.null(col_names)) {
    col_names <- names(data)
  }

  dt_options <- list(pageLength = page_length)

  if (buttons & search) {
    dt_options <- append(dt_options, list(buttons = list("copy", "csv")))
    dt_options <- append(dt_options, list(dom = "<f><t><\"govuk_dt_footer\"B<\"govuk_dt_nav\"ip>>"))
    dt_extensions <- "Buttons"
  } else if (!buttons & search) {
    dt_options <- append(dt_options, list(dom = "<f><t><\"govuk_dt_footer\"<\"govuk_dt_nav\"ip>>"))
    dt_extensions <- character()
  } else if (buttons & !search) {
    dt_options <- append(dt_options, list(buttons = list("copy", "csv")))
    dt_options <- append(dt_options, list(dom = "<t><\"govuk_dt_footer\"B<\"govuk_dt_nav\"ip>>"))
    dt_extensions <- "Buttons"
  } else {
    dt_options <- append(dt_options, list(dom = "<t><\"govuk_dt_footer\"<\"govuk_dt_nav\"ip>>"))
    dt_extensions <- character()
  }

  if (!is.null(col_defs)) {
    dt_options <- append(dt_options, list(columnDefs = col_defs))
  }

  if (!is.null(options)) {
    dt_options <- append(dt_options, options)
  }

  x <- DT::datatable(data,
                     style = "jqueryui",
                     class = dt_class,
                     elementId = element_id,
                     selection = "none",
                     rownames = FALSE,
                     colnames = col_names,
                     extensions = dt_extensions,
                     options = dt_options
  )

  return(x)

}
