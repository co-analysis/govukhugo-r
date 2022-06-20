#' Serve the site
#'
#' This is a wrapper around [blogdown::serve_site()]. It's main
#' purpose is to ensure that the blogdown re-knitting process doesn't
#' run when files are saved.
#'
#' @param ... Arguments passed to [servr::server_config()]
#' @param .site_dir Directory to search for site configuration file, defaults to [base::getwd()].
#'
#' @export
serve_site <- function(build_rmd = TRUE, rmd_folder = "R/Rmd",
                       rebuild = FALSE) {

  if (build_rmd) {
    build_hugo_rmd(rmd_folder = rmd_folder, rebuild = rebuild)
  }

  knit_opt <- getOption("blogdown.knit.on_save")

  if (is.null(knit_opt)) {
    options("blogdown.knit.on_save" = FALSE)
    cli::cli_warn("Global option {.var blogdown.knit.on_save} has been set to {.val FALSE}")
  } else if (knit_opt) {
    options("blogdown.knit.on_save" = FALSE)
    cli::cli_warn("Global option {.var blogdown.knit.on_save} has been set to {.val FALSE}")
  }

  blogdown::serve_site()

}
