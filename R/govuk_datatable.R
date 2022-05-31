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
                            title = NULL,
                            element_id = NULL,
                            col_names = NULL,
                            page_length = 10,
                            search = TRUE,
                            small_text = FALSE,
                            buttons = TRUE,
                            col_defs = NULL,
                            copy_info = NULL,
                            export_file = NULL,
                            options = NULL) {

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "The {DT} package is required to run govuk_datatable().",
      call. = FALSE
    )
  }

  if (!is.data.frame(data) & !is.matrix(data) &
      !crosstalk::is.SharedData(data)) {
    stop("data must be a data frame, matrix or crosstalk::SharedData")
  }

  # Set table classes
  if (small_text) {
    dt_class <- "govuk-table govuk-!-font-size-14"
  } else {
    dt_class <- "govuk-table"
  }

  # get names of the data frame and size
  if (crosstalk::is.SharedData(data)) {
    data_names <- names(data$data())
    nr <- nrow(data$data())
  } else {
    data_names <- names(data)
    nr <- nrow(data)
  }

  if (is.null(col_names)) {
    col_names <- data_names
  }

  # hard-code page length
  dt_options <- list(pageLength = page_length)

  # define copy specifications
  if (!is.null(copy_info)) {
    copy_spec <- list(
      extend = "copy",
      title = copy_info
    )
  } else {
    copy_spec <- "copy"
  }

  # define csv specifications
  csv_spec <- list(
    extend = "csv",
    text = "Download"
  )

  # add filename if provided
  if (!is.null(export_file)) {

    if (tolower(tools::file_ext(export_file)) != "csv") {
      export_file <- paste0(tools::file_path_sans_ext(export_file), ".csv")
    }

    csv_spec <- append(csv_spec, list(filename = export_file))

  }

  # combine button specs
  dt_buttons_spec <- list(
    copy_spec,
    csv_spec
  )

  # define button specifications
  if (buttons) {
    dt_options <- append(dt_options, list(buttons = dt_buttons_spec))
    dt_extensions <- "Buttons"
    dom_buttons <- "B"
  } else {
    dom_buttons <- ""
    dt_extensions <- character()
  }

  # hide pagination/table info if single page
  if (nr <= page_length) {
    dom_pi <- ""
  } else {
    dom_pi <- "pi"
  }

  # navigation dom elements
  dom_nav <- paste0(dom_pi, dom_buttons)

  # wrap navigation elements in named div
  if (length(dom_nav) != 0) {
    dom_nav <- paste0("<\"govuk_dt_nav\"", dom_nav, ">")
  }

  # wrap search in own div
  if (search) {
    dom_search <- "<\"govuk_dt_search\"f>"
  } else {
    dom_search <- ""
  }

  # combine dom elements and add to options
  dom_full <- paste0(dom_search, "<\"govuk_dt_table\"t>", dom_nav)
  dt_options <- append(dt_options, list(dom = dom_full))

  # add col_defs to options
  if (!is.null(col_defs)) {
    dt_options <- append(dt_options, list(columnDefs = col_defs))
  }

  if (!is.null(options)) {
    dt_options <- append(dt_options, options)
  }

  # build table
  x <- DT::datatable(data,
                     style = "jqueryui",
                     caption = title,
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
