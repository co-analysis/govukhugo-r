#' Bulk Hugo-ify Rmd files
#'
#' `build_hugo_rmd()` converts all Rmd files within a folder to Hugo-ified
#' HTML, it will place the Hugo-ified HTML files into the content sections
#' designated in the Rmd YAML front matter. Use [render_rmd()] if you want
#' to override this. By default it expects Rmd files in the `R/Rmd` folder.
#'
#' @param rmd_folder path to the folder containing Rmd files
#'
#' @export
build_hugo_rmd <- function(rmd_folder = "R/Rmd") {

  # get rmd files
  rmd_files <- dir(rmd_folder, pattern = "\\.Rmd", full.names = TRUE,
                   recursive = TRUE)

  # set a common temp directory
  tmp_dir <- tempdir()

  # create Rmd processing sub-folder
  if (!dir.exists(file.path(tmp_dir, "Rmd"))) {
    dir.create(file.path(tmp_dir, "Rmd"))
  }

  # create data processing sub-folder
  if (!dir.exists(file.path(tmp_dir, "data"))) {
    dir.create(file.path(tmp_dir, "data"))
  }

  # if R/data exists copy to the tmp_dir
  if (dir.exists("R/data")) {
    R.utils::copyDirectory("R/data",
                           file.path(tmp_dir, "data"),
                           overwrite = TRUE)
  }

  # render files
  for (rmd in rmd_files) {
    render_rmd(rmd, tmp_dir = tmp_dir)
  }

}

#' Build a Hugo site
#'
#' `build_hugo()` Hugo-ifies Rmd files and then calls Hugo to build the
#' static site.
#'
#' @param with_rmd logical flag for whether to build Rmd files
#' @param rmd_folder path to the folder containing Rmd files
#'
#' @export
build_hugo <- function(with_rmd = TRUE, rmd_folder = "R/Rmd") {

  # convert rmds
  if (with_rmd) {
    build_hugo_rmd(rmd_folder)
  }

  # call hugo
  system("hugo")

}
