#' Remove bootstrap css from crosstalk components
#'
#' The crosstalk injects [Bootstrap](https://getbootstrap.com/) into rendered
#' HTML. `unstrap()` removes the Bootsrap dependencies from the components.
#'
#' @param x a crosstalk component, such as [`crosstalk::filter_select()`]
#'
#' @return The component `x` but with any `"boostrap"` dependency removed from its
#' `html_dependencies` attribute.
#'
#' @examples
#' # The fs object will inject css into your page.
#' if (requireNamespace("crosstalk", quietly = TRUE)) {
#'   df <- crosstalk::SharedData$new(mtcars)
#'
#'   fs <- crosstalk::filter_select(
#'       id = "myselector",
#'       label = "select something",
#'       sharedData = df,
#'       group = ~cyl
#'     )
#'
#'   # The fs_nobootstrap object won't inject css into your page.
#'   fs_nobootstrap <- unstrap(fs)
#' }
#' @export
unstrap <- function(x) {
  attr(x, "html_dependencies") <-
    Filter(
      function(dependency) {dependency$name != "bootstrap"},
      attr(x, "html_dependencies")
    )
  x
}


#' Clear crosstalk filters
#'
#' Insert a "clear filters" option to a set of filter controls. This adds an
#' HTML `div` element that works with jQuery in govukhugo to clear crosstalk
#' filters on a page, note that this clears all filters on a page including
#' any DT search boxes.
#'
#' @param text The text to display (defaults to "Clear filters")
#'
#' @export
clear_filters <- function(text = "Clear filters") {
  htmltools::div(
    id = "clear-filters", class = "govuk-!-font-size-14",
    text
  )
}

