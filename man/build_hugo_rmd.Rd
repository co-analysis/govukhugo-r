% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R
\name{build_hugo_rmd}
\alias{build_hugo_rmd}
\alias{govukhugo_knit}
\title{Bulk Hugo-ify Rmd files}
\usage{
build_hugo_rmd(rmd_folder = "R/Rmd", rebuild = FALSE, save = TRUE)

govukhugo_knit(input, ...)
}
\arguments{
\item{rmd_folder}{path to the folder containing Rmd files}

\item{rebuild}{whether to rebuild all Rmd files}

\item{save}{whether to save open RStudio source files before build}

\item{input}{recieved Rmd file}

\item{...}{knitr fluff}
}
\description{
\code{build_hugo_rmd()} converts all Rmd files within a folder to Hugo-ified
HTML, it will place the Hugo-ified HTML files into the content sections
designated in the Rmd YAML front matter. Use \code{\link[=render_rmd]{render_rmd()}} if you want
to override this. By default it expects Rmd files in the \code{R/Rmd} folder.
The default behaviour is to only build files that are changed, which is
tracked using the \code{rmd.log} file that is created/stored in the \code{rmd_folder},
you can force the building of all pages by setting \code{rebuild = TRUE}. If
working in RStudio the process will save all open source editors windows
before running.
}
\section{Functions}{
\itemize{
\item \code{govukhugo_knit}: Don't directly knit govukhugo documents
}}

