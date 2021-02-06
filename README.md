
# {govukhugo}

<!-- badges: start -->
<!-- badges: end -->

The `{govukhugo}` package provides functions to use R Markdown files with the [`govuk-hugo`](https://github.com/co-analysis/govuk-hugo) theme for [Hugo](https://gohugo.io).

`govuk-hugo` is a theme that brings the GOV.UK design system to the Hugo static site builder. Hugo is used by blogdown, however `govuk-hugo` is not compatible with blogdown's processes for rendering Rmd files, hence the functions within the {govukhugo} package.

Online documentation is provided in the cross-package [`govuk-hugo-demo`](https://co-analysis.github.io/govuk-hugo-demo/) site.

## Installation

Note that both the `{govukhugo}` package and the `govuk-hugo` theme are still experimental works in progress, it should work on *nix (Mac/Linux) systems but has not yet been tested on Windows builds. Extreme caution is advised.

You can install {govukhugo} as follows:

``` r
remotes::install_github("co-analysis/govukhugo")
```

## Usage

There are two main functions:

- `init_govuk_hugo()` - this creates the scaffold of a hugo site with the `govuk-hugo` theme.
- `build_hugo()` - this will render Rmd files and run hugo's site building

### Pages without R content
Where possible it is recommended that plain markdown files (`.md`) are used - i.e. if there is no R content in a page then create it as a plain markdown file. These should be stored within the `content` folder. It is also recommended that all sections (i.e. subfolders within `content`) should have their own `_index.md` file, see the `govuk-hugo` [documentation](https://co-analysis.github.io/govuk-hugo-demo/section/) for more details.


### Rmd files
It is recommended that Rmd files are stored in an `R/Rmd` folder. This is the default folder that `build_hugo()` looks in for `.Rmd` files, it will also search any sub-directories.

Rmd files for `govuk-hugo` should have simple YAML headers:

```
---
title: My R Markdown file
date: 2021-01-28
section: section
---
```

`section` should be the sub-folder of the content directory that you want `build_govuk_hugo()` to render the Rmd file into.

More detailed documentation on working with R Markdown files is provided in the cross-package `govuk-hugo` [documentation](https://co-analysis.github.io/govuk-hugo-demo/section/rmarkdown/), along with tests of [static] and interactive R Markdown files.
