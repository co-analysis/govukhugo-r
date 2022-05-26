#' Hugo-ify an R Markdown document
#'
#' @description
#' Hugo is largely used to convert plain markdown files into HTML, but it also
#' supports HTML documents as input files. `render_rmd()` renders Rmd files
#' using [rmarkdown::html_document()] and the converts the HTML to work with
#' Hugo.
#'
#' By default `render_rmd()` will read the YAML front-matter of an Rmd for
#' a section parameter and use that as a sub-directory of the content folder
#' as the output location.
#'
#' @param rmd_file the .rmd file to convert
#' @param tmp_dir a temporary directory for processing
#' @param out_dir override the default output directory
#' @param quiet keep knitr quiet, defaults to TRUE, set to FALSE to see
#' knitr messages
#'
#' @export
render_rmd <- function(rmd_file, tmp_dir = tempdir(), out_dir = NULL, quiet = TRUE) {

  # extract the YAML front matter from the Rmd
  rmd_yml <- rmarkdown::yaml_front_matter(rmd_file)

  # remove section from the YAML
  new_yml <- rmd_yml[!(names(rmd_yml) == "section")]

  # add rmarkdown flag to YAML
  new_yml <- append(new_yml, c("rmarkdown" = "true"))

  # copy rmd to a temporary folder
  tmp_rmd <- file.path(tmp_dir, "Rmd", basename(rmd_file))
  file.copy(rmd_file, tmp_rmd, overwrite = TRUE)

  # render the rmd to html
  tmp_content <- rmarkdown::render(
    tmp_rmd,
    output_format =  rmarkdown::html_document(
      theme = NULL,
      mathjax = NULL,
      self_contained = TRUE,
      df_print = "kable",
      md_extensions = "-smart"
    ),
    output_file = file.path(tmp_dir, "content.html"),
    quiet = quiet
  )

  # hugo-ify the html
  tmp_html <- hugo_html(tmp_content)

  # build YAML front-matter for the HTML
  yml_content <- c("---",
                   paste0(names(new_yml), ": ", new_yml),
                   "---")

  # combine the YAML and hugo-ified html
  tmp_out <- file.path(tmp_dir, gsub(".Rmd$|.rmd$", ".html", basename(rmd_file)))
  writeLines(c(yml_content, tmp_html), tmp_out)

  # set the output folder to the section sub-folder of content
  # unless out_dir is specified in the arguments
  if (is.null(out_dir)) {

    # if defined extract the section from YAML
    if (!is.null(rmd_yml$section)) {
      out_dir <- file.path(rmd_yml$section, "")
    } else {
      out_dir <- ""
    }

    # section as sub-folder of content
    out_dir <- file.path("content", out_dir)

  }

  # check if out_dir exists
  # if not create, but warn user
  if (!dir.exists(out_dir)) {
    warning(out_dir, " did not exist, creating")
    dir.create(out_dir)
  }

  # copy the html to the outdir and inform user
  out_file <- paste0(out_dir, basename(tmp_out))
  file.copy(tmp_out, out_file, overwrite = TRUE)
  message(rmd_file, " hugo-ified as: ", out_file)

}

# hugo_html takes the html produced by rmarkdown::html_document(), extracts
# the <body> content, inserts hugo shortcodes for code highlighting and
# resets the gt and lt HTML entities
hugo_html <- function(rmd_html) {

  # read the rmd html file
  in_html <- readLines(rmd_html)

  # find the <body> tags, and the Rmd rendered title and date
  # add/subtract 1 to exclude lines
  # body_start:title_line captures everything after the <body> tag
  # up to but not including the rendered title (hugo will create this from YAML)
  # date_line:body_end captures everything after the rendered date
  body_start <- which(in_html == "<body>") + 1
  title_line <- which(grepl("<h1 class=\"title", in_html)) - 1
  date_line <- which(grepl("<h4 class=\"date", in_html)) + 1
  body_end <- which(in_html == "</body>") - 1

  # handle missing/multiple title lines
  if (length(date_line) == 0) {
    title_line <- body_start
  } else if (length(date_line) != 1) {
    warning("Multiple title lines detected, using the first")
    title_line <- title_line[1]
  }

  # handle missing/multiple date lines
  if (length(date_line) == 0) {
    date_line <- title_line + 1
  } else if (length(date_line) != 1) {
    warning("Multiple date lines detected, using the first")
    date_line <- date_line[1]
  }

  # init out_html
  out_html <- in_html

  # reset gt and lt
  out_html <- gsub("&gt;", ">", out_html)
  out_html <- gsub("&lt;", "<", out_html)

  # reset quotations
  out_html <- gsub("&quot;", "\"", out_html)

  # replace <pre><code> with hugo highlight shortcode
  out_html <- gsub("<pre class=\"(r)\"><code>", "{{< highlight \\1 >}}", out_html)
  out_html <- gsub("<pre><code>", "{{< highlight txt >}}", out_html)
  out_html <- gsub("</code></pre>", "{{< /highlight >}}", out_html)

  # extract html excluding title/date lines
  # date_line needs to be after title_line
  if (date_line > title_line) {
    out_html <- out_html[c(body_start:title_line, date_line:body_end)]
  } else {
    stop(paste(
      "Title and date lines are not next to each other.",
      "This is an unexpected problem, please report it at:",
      "http://github.com/co-analysis/govuk-hugo-r/issues."))
  }

  # hack plotly size fixing
  plotly_lines <- which(grepl("class=\"plotly", out_html))
  out_html[plotly_lines] <- gsub("style=.*class", "class", out_html[plotly_lines])

  return(out_html)

}

