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

  if (!is.data.frame(data) & !is.matrix(data) &
      !crosstalk::is.SharedData(data)) {
    stop("data must be a data frame, matrix or crosstalk::SharedData")
  }


  if (small_text) {
    dt_class <- "govuk-table govuk-!-font-size-14"
  } else {
    dt_class <- "govuk-table"
  }

  if (is.null(col_names)) {
    col_names <- names(data)
  }

  if (crosstalk::is.SharedData(data)) {
    col_names <- names(data$data())
    nr <- nrow(data$data())
  } else {
    col_names <- names(data)
    nr <- nrow(data)
  }

  dt_options <- list(pageLength = page_length)

  dt_buttons_spec <- list(
    "copy",
    list(
      extend = "csv",
      text = "Download"
    )
  )

  if (nr < page_length) {
    dom_pi <- ""
  } else {
    dom_pi <- "pi"
  }

  if (buttons) {
    dt_options <- append(dt_options, list(buttons = dt_buttons_spec))
    dt_extensions <- "Buttons"
    dom_buttons <- "B"
  } else {
    dom_buttons <- ""
    dt_extensions <- character()
  }

  dom_nav <- paste0(dom_pi, dom_buttons)

  if (length(dom_nav) != 0) {
    dom_nav <- paste0("<\"govuk_dt_nav\"", dom_nav, ">")
  }

  if (search) {
    dom_search <- "<\"govuk_dt_search\"f>"
  } else {
    dom_search <- ""
  }

  dom_full <- paste0(dom_search, "<\"govuk_dt_table\"t>", dom_nav)

  dt_options <- append(dt_options, list(dom = dom_full))

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
