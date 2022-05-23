#' @export
govuk_colours <- c(
  "red" = "#d4351c",
  "yellow" = "#ffdd00",
  "green" = "#00703c",
  "blue" = "#1d70b8",
  "dark-blue" = "#003078",
  "light-blue" = "#5694ca",
  "purple" = "#4c2c92",
  "black" = "#0b0c0c",
  "dark-grey" = "#505a5f",
  "mid-grey" = "#b1b4b6",
  "light-grey" = "#f3f2f1",
  "white" = "#ffffff",
  "light-purple" = "#6f72af",
  "bright-purple" = "#912b88",
  "pink" = "#d53880",
  "light-pink" = "#f499be",
  "orange" = "#f47738",
  "brown" = "#b58840",
  "light-green" = "#85994b",
  "turquoise" = "#28a197",
  "chart-white" = "#f9f8f8",
  "govuk-text-colour" = "#0b0c0c",
  "govuk-secondary-text-colour" = "#505a5f",
  "govuk-link-colour" = "#1d70b8",
  "govuk-link-hover-colour" = "#003078",
  "govuk-link-visited-colour" = "#4c2c92",
  "govuk-link-active-colour" = "#0b0c0c",
  "govuk-border-colour" = "#b1b4b6",
  "govuk-input-border-colour" = "#0b0c0c",
  "govuk-focus-colour" = "#ffdd00",
  "govuk-focus-text-colour" = "#0b0c0c",
  "govuk-error-colour" = "#d4351c",
  "govuk-success-colour" = "#00703c",
  "govuk-brand-colour" = "#1d70b8"
)

govuk_categorical_palette <- c(
  "blue" = "#1d70b8",
  "orange" = "#f47738",
  "turquoise" = "#28a197",
  "bright-purple" = "#912b88",
  "yellow" = "#ffdd00",
  "dark-blue" = "#003078"
)

govuk_sequential_blue <- c(
  "dark-blue" = "#003078",
  "blue" = "#1d70b8",
  "light-blue" = "#5694ca"
)

govuk_divergents <- list(
  blrd = c(
    "blue" = "#1d70b8",
    "chart-white" = "#f9f8f8",
    "red" = "#d4351c"
  ),
  blrd_dark = c(
    "dark-blue" = "#003078",
    "chart-white" = "#f9f8f8",
    "dark-red" = "#a12815"
  ),
  blyl = c(
    "blue" = "#1d70b8",
    "chart-white" = "#f9f8f8",
    "yellow" = "#ffdd00"
  ),
  putq = c(
    "purple" = "#4c2c92",
    "chart-white" = "#f9f8f8",
    "turquoise" = "#28a197"
  )
)

#' Use GOV.UK colours
#'
#' Access the GOV.UK design system colour palette
#'
#' The GOV.UK design system includes a colour palette, these can be accessed
#' via the `govuk_colours` vector, it includes both Sass variable colour names
#' (e.g. `govuk-text-colour`) as well as the named colour palette.
#'
#' The GOV.UK colour scheme was not designed with data visualisation in mind.
#' The `govuk_palette` function provides an opinionated selection of GOV.UK
#' colours for use in charts (e.g. via [ggplot2::scale_fill_manual()]).
#'
#' The `categorical` palette (the default) provides an opinonated set of six
#' GOV.UK colours for use in categorical/qualitative palettes.
#'
#' The `blues` palette orders the GOV.UK colour palettes blues from dark to
#' light and can be used for sequential colour palettes. Alternatively you can
#' supply a name from the `govuk_colours` vector to generate the end-points for
#' a sequential palette using that colour (where the lightest colour is roughly
#' a third lighter than the input colour).
#'
#' There are also four opinionated palettes for use in a divergent scale, these
#' all use an off-white (the mid-colour between GOV.UK light grey and white)
#'  * `blrd` which use GOV.UK blue and GOV.UK red as its end-points
#'  * `blrd_dark` which uses GOV.UK dark blue as its blue end-point and a darker
#'    red of the same hue as GOV.UK red as its red end-point
#'  * `blyl` which uses GOV.UK blue and GOV.UK yellow as its end-points
#'  * `putq` which uses GOV.UK purple and GOV.UK turquoise as its end-points
#'
#' The categorical and divergent palettes have been checked for general support
#' for users with colour blindness.
#'
#' @details
#'
#' @param pal One of `categorical` (the default), `blue`, `blrd`, `bldrd_dark`,
#' `blyl`, `putq`, or a colour name from `govuk_colours`
#'
#' @return A set of hexadecimal colours
#' @export
govuk_palette <- function(pal = "categorical") {

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "The {scales} package is required for govuk_palette()",
      call. = FALSE
    )
  }

  if (pal == "categorical") {
    out_pal <- unname(govuk_categorical_palette)
  } else if (pal == "blue") {
    out_pal <- unname(govuk_sequential_blue)
  } else {
    if (pal %in% names(govuk_divergents)) {
      out_pal <- unname(govuk_divergents[[pal]])
    } else if (pal == "chart-white") {
      stop("chart-white cannot be used for sequential palettes")
    } else if (pal %in% names(govuk_colours)) {
      out_pal <- scales::gradient_n_pal(
        c(govuk_colours[pal], govuk_colours["chart-white"]),
        values = c(1, 0))(c(1, 1/3))
    } else {
      stop("pal must be one of: categorical, blue, blrd, blrd_dark, blyl, putq,",
           " or the name of a colour in govuk_colours.")
    }
  }

  return(out_pal)

}

