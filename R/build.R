#' Bulk Hugo-ify Rmd files
#'
#' `build_hugo_rmd()` converts all Rmd files within a folder to Hugo-ified
#' HTML, it will place the Hugo-ified HTML files into the content sections
#' designated in the Rmd YAML front matter. Use [render_rmd()] if you want
#' to override this. By default it expects Rmd files in the `R/Rmd` folder.
#' The default behaviour is to only build files that are changed, which is
#' tracked using the `rmd.log` file that is created/stored in the `rmd_folder`,
#' you can force the building of all pages by setting `rebuild = TRUE`. If
#' working in RStudio the process will save all open source editors windows
#' before running.
#'
#' @param rmd_folder path to the folder containing Rmd files
#' @param rebuild whether to rebuild all Rmd files
#' @param save whether to save open RStudio source files before build
#'
#' @export
build_hugo_rmd <- function(rmd_folder = "R/Rmd", rebuild = FALSE, save = TRUE) {

  # check if in RStudio
  if (Sys.getenv("RSTUDIO") == "1") {
    rstudioapi::documentSaveAll()
  }

  # get rmd files
  rmd_files <- dir(rmd_folder, pattern = "\\.Rmd", full.names = TRUE,
                   recursive = TRUE)

  if (length(rmd_files) == 0) {
    cli::cli_alert_info("No Rmarkdown files detected")
    return(invisible(NULL))
  }

  cli::cli_h1("Start Hugo processing")

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
    cli::cli_alert_warning(
      "Site rebuild requested, all Rmd files will be re-processed"
    )
  }

  if (no_build) {
    cli::cli_alert_info(
      "All Rmd files are unchanged, skipping hugo processing"
    )
    return(invisible(NULL))
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
    R.utils::copyDirectory("~/r/acses-example/R/data",
                           file.path(tmp_dir, "data"),
                           overwrite = TRUE)
  }

  unchanged_files <- 0
  processed_out <- character(0)
  processed_rmd <- character(0)

  cli::cli_progress_bar("Processing Rmd files", total = length(rmd_files), )

  # render files
  for (rmd in rmd_files) {

    # if md5sum unchanged don't render
    if (rebuild | !check_md5(rmd, rmd_log)) {
      out <- govukhugo::render_rmd(rmd, tmp_dir = tmp_dir)
      processed_out <- c(processed_out, out)
      processed_rmd <- c(processed_rmd, rmd)
    } else {
      unchanged_files <- unchanged_files + 1
    }

    cli::cli_progress_update()

  }

  cli::cli_progress_done()

  # re-generate hash values and write to log
  out_hashes <- paste0(rmd_files, ": ", new_hashes)
  writeLines(out_hashes, file.path(rmd_folder, "rmd.log"))

  processed_files <- processed_out
  names(processed_files) <- gsub(paste0(rmd_folder,"/"), "", processed_rmd)

  cli::cli({
    cli::cli_status_clear()
    cli::cli_h1("Hugo processing complete")
    cli::cli_alert_info("{unchanged_files}{cli::qty(unchanged_files)} Rmd file{?s} {?was/were} unchanged and skipped processing for govukhugo")
    cli::cli_alert_success("{length(processed_files)}{cli::qty(processed_files)} Rmd file{?s} {?was/were} processed for govukhugo")
    if (length(processed_files) > 0) {
      cli::cli({
        cli::cli_h2("Processed files")
        cli::cli_dl(processed_files)
      })
    }
  })

  return(invisible(processed_files))

}

#' Build a Hugo site
#'
#' `build_hugo()` Hugo-ifies Rmd files and then calls Hugo to build the
#' static site.
#'
#' @param with_rmd logical flag for whether to build Rmd files
#' @param rmd_folder path to the folder containing Rmd files
#' @param rebuild whether to rebuild all files
#'
#' @export
build_hugo <- function(with_rmd = TRUE, rmd_folder = "R/Rmd", rebuild = FALSE) {

  # convert rmds
  if (with_rmd) {
    build_hugo_rmd(rmd_folder, rebuild)
  }

  cli::cli_h1("Building Hugo site")

  # call hugo
  blogdown::hugo_build()

}

#' @describeIn build_hugo_rmd Don't directly knit govukhugo documents
#' @param input recieved Rmd file
#' @param ... knitr fluff
#' @export
govukhugo_knit <- function(input, ...) {
  curr_rlang_backtrace <- getOption(
    "rlang_backtrace_on_error"
  )
  options(
    rlang_backtrace_on_error = "none"
  )
  cli::cli(
    cli::cli_abort(
      c(x = "You cannot knit govukhugo Rmd files",
        i = "Use govukhugo::build_hugo_rmd() instead"),

    )
  )
  options(
    rlang_backtrace_on_error = curr_rlang_backtrace
  )
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
