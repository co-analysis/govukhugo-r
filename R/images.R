#' Render ggplot2 objects as in-line SVG
#'
#' @description
#' `render_svg()` converts ggplot2 objects to SVG code to render them as in-line
#' SVG code, and provides options for providing ARIA labels for assistive
#' technologies.
#'
#' @details
#' When rendering to HTML, Rmarkdown will render plots as `<img>` tags,
#' including when the output device is set to svg or svglite. This is
#' problematic for accessibility reasons, as while scalable the plot continues
#' to be rendered by the browser as an image meaning that any text within the
#' plot is not selectable by the user and cannot be read by a screen reader.
#'
#' `render_svg()` uses [ggplot2::ggsave()] to convert the plot to an chunk of
#' SVG code, rendering text as words (via the [svglite::svglite()] device) and
#' returning that code rather than producing a plot. If the `alt_title`,
#' `alt_desc` are used then these will embedded within the SVG code,
#' these are encoded as ARIA labels for the SVG chunk and `alt_title` will also
#' present as a tooltip. Optionally, a caption can be provided which will be
#' inserted below the plot, if the `caption` argument is set as `alt_title` or
#' `alt_desc` then it will take the value of that argument.
#'
#' @param plot A [ggplot2::ggplot()] object
#' @param width The desired width of the object
#' @param height The desired height of the object
#' @param units The units of width and height, default is px (pixels)
#' @param alt_title Short alt text (will show as tool-tip)
#' @param alt_desc Longer alt text (embedded within SVG)
#' @param caption A caption to display to all users, can also be set to
#'                `"alt_title"` or `"alt_desc"` to match their value
#' @param dpi Dots per inch, default of 96 for screen resolution, switch to
#'            300 if using physical units (mm, cm or inches)
#'
#' @export
render_svg <- function(plot, width, height, units = "px",
                       alt_title = NULL, alt_desc = NULL, caption = NULL,
                       dpi = 96) {

  # {ggplot2} is in suggests to reduce install bloat
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("render_svg() requires package {ggplot2} to be installed.",
         call. = FALSE)
  }

  # {svglite} is in suggests to reduce install bloat
  if (!requireNamespace("svglite", quietly = TRUE)) {
    stop("render_svg() requires package {svglite} to be installed.",
         call. = FALSE)
  }

  # warn if no alt text provided
  if (is.null(alt_title) & is.null(alt_desc)) {
    warning("You have not provided any alt text for the plot, please reconsider.")
  } else if (is.null(alt_title) & !is.null(alt_desc)) {
    warning("You have set an alt text description but not a title, are you sure about this?")
  }

  # check caption argument is ok
  if (!is.null(caption)) {
    if (caption == "alt_title" & is.null(alt_title)) {
      stop("Caption set to \"alt_title\" but no alt_title provided.")
    } else if (caption == "alt_desc" & is.null(alt_desc)) {
      stop("Caption set to \"alt_desc\" but no alt_desc provided.")
    } else if (caption == "alt_text") {
      warning("Caption set to \"alt_text\", alt_text is not an argument, did you mean alt_title?")
    }
  }

  # create a temporary file
  svg_file <- paste0(tempfile(),".svg")

  # check ggplot2 version
  # if less than 3.3.5 convert px units to mm
  ggplot_version <- as.character(packageVersion("ggplot2"))

  if (compareVersion(ggplot_version, "3.3.5") == -1) {
    if (units == "px") {
      width <- width * (25.4/dpi)
      height <- height * (25.4/dpi)
      units <- "mm"
    }
  }

  # render ggplot as an svg
  ggplot2::ggsave(svg_file, plot, device = "svg",
                  width = width, height = height, units = units, dpi = dpi)

  # read the svg, drop the DOCTYPE declaration
  x <- readLines(svg_file)[-1]

  # generate a unique id for the object
  unique_id <- paste0(c(sample(letters, 5), replicate(5, sample(0:9, 3))),
                   collapse = "")

  # set collectors if alt arguments are not null
  if (!is.null(alt_title) | !is.null(alt_desc)) {
    described_by <- character()
    alt_insert <- character()
  }

  # create tags for alt_title
  if (!is.null(alt_title)) {
    described_by <- c(described_by, paste0(unique_id, "-title"))
    alt_insert <- c(alt_insert,
                    paste0("<title id='", unique_id, "-title'>",
                           alt_title,
                           "</title>"))
  }

  # create tags for alt_desc
  if (!is.null(alt_desc)) {
    described_by <- c(described_by, paste0(unique_id, "-description"))
    alt_insert <- c(alt_insert,
                    paste0("<description id='", unique_id, "-description'>",
                           alt_desc,
                           "</description>"))
  }

  # insert alt text tags into svg, if no alt_text still set role='img'
  if (!is.null(alt_title) | !is.null(alt_desc)) {
    described_by <- paste0(" aria-labelledby='",
                           paste(described_by, collapse = " "),
                           "'",
                           collapse = "")
    x1 <- gsub(">", paste0(described_by," role='img'>"), x[1])
    new_svg <- c(x1, alt_insert, x[2:length(x)])
  } else {
    x1 <- gsub(">", " role='img'>", x[1])
    new_svg <- c(x1, x[2:length(x)])
  }

  # generate output
  if (!is.null(caption)) {

    # assign alt text to caption if set
    if (caption == "alt_title") {
      caption <- alt_title
    } else if (caption == "alt_desc") {
      caption <- alt_desc
    }

    # collapse SVG block and add caption
    out_chunk <- paste(c(paste("<figcaption class=\"govuk-heading-m\">",
                               caption, "</figcaption>", sep = ""),
                         paste(new_svg, sep = "\n")),
                       collapse = "\n"
                       )

  } else {
    # if no caption then collapse SVG block
    out_chunk <- paste(new_svg, sep = "\n")
  }

  # render as an HTML object and wrap in a <figure> tag
  out_html <- htmltools::HTML(
    paste(c(paste("<figure id=\"", unique_id, "-figure\", class=\"govuk_chart\">", sep = ""),
            out_chunk,
            "</figure>"),
          collapse = "\n"
    )
  )

  # output html
  return(out_html)

}
