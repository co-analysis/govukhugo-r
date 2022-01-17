#' Initiate a Hugo site using the govuk-hugo theme
#'
#' `init_govuk_hugo()` calls Hugo to create a new site scaffold,
#' and adds govuk-hugo as a theme. Note that this function only
#' works inside of RStudio with an RStudio project. It is
#' recommended that your project also uses git for version control.
#'
#' @export
#'
init_govuk_hugo <- function() {

  # check if interactive
  if (!interactive()) {
    stop("init_govuk_hugo() must be run interactively")
  }

  # check the user knows what they're doing
  continue <- utils::menu(
    choices = c("No", "No, but yeah, but no", "Yes"),
    title = paste0("init_govuk_hugo() will initate Hugo in your ",
                   "current directory:\n",
                   getwd(),
                   "\nAre you sure you want to continue?"))

  if (continue != 3) {
    stop("Hugo build stopped")
  }

  # find out if you're in RStudio
  rstudio_available <- rstudioapi::isAvailable()

  # stop if not using RStudio or in an RStudio project
  if (!rstudio_available | is.null(rstudioapi::getActiveProject())) {
    stop("It is recommended that you use an RStudio project.\n",
         "To use outside RStudio initiate Hugo yourself by following\n",
         "the getting started information at https:://gohugo.io.")
  }

  setwd(rstudioapi::getActiveProject())

  # check if using git
  if (!dir.exists(".git")) {
    git_in_use <- FALSE
    warning("It is recommended that you use git")
  } else {
    git_in_use <- TRUE
  }

  # create hugo site
  message("Creating Hugo directory scaffold")
  system("hugo new site . --force -f \"yaml\"")

  # replace the config file with the template
  config_template <- system.file("hugo", "config.yaml", package = "govukhugo")
  file.copy(config_template, "config.yaml", overwrite = TRUE)

  message("Adding govukhugo theme")

  # should the theme be added as a submodule
  if (git_in_use) {
    theme_submodule <- utils::menu(
      choices = c("Yes (recommended for easy updates)",
                  "No (copies version bundled in the package)"),
      title = "Do you want to add the govuk-hugo theme as a submodule?")
  } else {
    theme_submodule <- 0
  }

  # add theme as a submodule or copy from version bundled in the package
  if (theme_submodule == 1) {

    system("git submodule add https://github.com/co-analysis/govuk-hugo.git themes/govukhugo")

    message("govuk-hugo theme installed as a submodule at themes/govukhugo")

  } else {

    theme_download <- utils::menu(
      choices = c("Yes",
                  "No"),
      title = "You are not using git. Do you want to download and install the theme?")

    if (theme_download == 1) {
      tmp_theme <- tempfile()
      download.file("https://github.com/co-analysis/govuk-hugo/archive/refs/heads/main.zip",
                    destfile = tmp_theme)
      utils::unzip(tmp_theme, exdir = "themes")
      fs::file_move("themes/govuk-hugo-main", "themes/govukhugo")
    } else{
      message("Visit https://github.com/co-analysis/govuk-hugo/ to download the theme/n",
              " 1. Extract the zip file",
              " 2. Copy the extracted folder to the themes folder",
              " 3. Rename the folder from 'govuk-hugo-main' to 'govukhugo'")
    }

  }

  # create a folder for saving Rmd files to
  if (!dir.exists("R/Rmd")) {
    dir.create("R/Rmd", recursive = TRUE)
  }

  # open the config file to edit
  if (rstudio_available) {
    rstudioapi::navigateToFile("config.yaml")
  }

  # tell the user the build is complete
  message("govuk-hugo build complete, please edit the config.yaml file")

}
