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
build_hugo_rmd <- function(rmd_folder = "R/Rmd", rebuild = FALSE) {

  # get rmd files
  rmd_files <- dir(rmd_folder, pattern = "\\.Rmd", full.names = TRUE,
                   recursive = TRUE)

  if (length(rmd_files) == 0) {
    message("No Rmarkdown files detected")
    return(NULL)
  }

  # get the MD5 checksums log
  if (file.exists(file.path(rmd_folder, "rmd.log"))) {
    rmd_log <- read_md5_log(file.path(rmd_folder, "rmd.log"))
  } else {
    rmd_log <- NULL
  }

  new_hashes <- tools::md5sum(rmd_files)

  no_build <- TRUE
  for (hash in new_hashes) {
    if (!hash %in% rmd_log) {
      no_build <- FALSE
    }
  }

  if (rebuild == TRUE) {
    no_build <- FALSE
    message("Site rebuild requested, unchanged Rmd files will undergo hugo-ification")
  }

  if (no_build) {
    message("All Rmarkdown files unchanged, skipping hugo-ification")
    return(NULL)
  }

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

    # if md5sum unchanged don't render
    if (rebuild) {
      render_rmd(rmd, tmp_dir = tmp_dir)
    } else if (check_md5(rmd, rmd_log)) {
      message(rmd, " unchanged, skipping hugo-ification")
    } else {
      render_rmd(rmd, tmp_dir = tmp_dir)
    }

  }

  # re-generate hash values and write to log
  out_hashes <- paste0(rmd_files, ": ", new_hashes)
  writeLines(out_hashes, file.path(rmd_folder, "rmd.log"))

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
build_hugo <- function(with_rmd = TRUE, rmd_folder = "R/Rmd", rebuild = FALSE) {

  # convert rmds
  if (with_rmd) {
    build_hugo_rmd(rmd_folder, rebuild)
  }

  # call hugo
  system("hugo")

}

# function to read the hashlog as a vector
read_md5_log <- function(file) {
  x <- readLines(file)
  y <- gsub("(^.*)(: )(.*)", "\\3", x)
  names(y) <- gsub("(^.*)(: )(.*)", "\\1", x)
  return(y)
}

# function to check the md5sum of an rmd file against log
check_md5 <- function(file, hash_log) {

  if (is.null(hash_log)) {
    FALSE
  } else if (!(file %in% names(hash_log))) {
    FALSE
  } else {
    log_hash <- hash_log[file]
    new_hash <- tools::md5sum(file)
    log_hash == new_hash
  }

}
