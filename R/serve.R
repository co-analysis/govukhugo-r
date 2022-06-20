#' Serve the site
#'
#' These are wrappers around [blogdown::serve_site()] and [blogdown::stop_server()].
#' The main purpose is to ensure that the blogdown re-knitting process doesn't
#' run when files are saved. `stop_server()` is a simple re-export.
#'
#' @param ... Arguments passed to [servr::server_config()]
#' @param .site_dir Directory to search for site configuration file, defaults to [base::getwd()].
#'
#' @export
serve_site <- function(build_rmd = TRUE, rmd_folder = "R/Rmd",
                       rebuild = FALSE, drafts = TRUE, future = TRUE) {

  if (build_rmd) {
    build_hugo_rmd(rmd_folder = rmd_folder, rebuild = rebuild)
  }

  options("blogdown.knit.on_save" = FALSE)

  if (drafts & future) {
    options("blogdown.hugo.server" = c("-D", "-F"))
  } else if (drafts) {
    options("blogdown.hugo.server" = c("-D"))
  } else if (future) {
    options("blogdown.hugo.server" = c("-F"))
  } else {
    options("blogdown.hugo.server" = "")
  }


  blogdown::serve_site()

}

#' @describeIn serve_site Stop serving a Hugo site
#' @export
stop_server <- function() {
  blogdown::stop_server()
}
