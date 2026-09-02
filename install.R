# Run this once before working through the book.
#
#   source("install.R")
#
# Every chapter also loads its own packages with pacman::p_load(), which installs anything
# missing. So this script is not strictly required. It is here so that installation happens
# once, up front, rather than in the middle of a lesson: some of these take a few minutes,
# and a workshop is a bad time to discover that.
#
# Nothing here is needed to *read* the published book, only to run the code yourself.

pkgs <- c(
  # Neotoma
  "neotoma2",     # the Neotoma API client, used throughout
  # spatial
  "sf",           # simple features, for spatial objects
  "geojsonsf",    # converts GeoJSON to sf
  "leaflet",      # interactive maps
  # age-depth modelling
  "Bchron",       # calibration and Bayesian age-depth models
  # community ecology and palaeo
  "analogue",     # dissimilarity, the modern analogue technique
  "vegan",        # ordination and distance measures
  "rioja",        # stratigraphic diagrams, CONISS
  # data wrangling and plotting
  "dplyr", "tidyr", "ggplot2", "readr", "stringr", "scales",
  "tidyverse",    # the no-analogue chapter loads the whole meta-package
  "readxl",       # reads the Whitmore modern pollen spreadsheet
  "fuzzyjoin",    # approximate string matching for taxon names
  # tables and widgets
  "DT", "htmlwidgets",
  # required by code-link: true in _quarto.yml, which turns function names in code blocks
  # into links to their documentation. Without these, Quarto warns once per chapter and
  # silently renders the code unlinked.
  "downlit", "xml2"
)

# Not listed above, on purpose:
#   splines, mgcv  ship with R, no installation needed
#   pacman         bootstrapped below
#   raster, terra  the book's own code never calls them, and the neotoma2 chapter used to
#                  load raster for no reason. They are still installed, because leaflet
#                  lists raster in its Imports. Note leaflet loads fine without it (it only
#                  reaches for raster inside functions we never call, such as
#                  addRasterImage), so this is completeness rather than necessity.

# --- reproducibility note ------------------------------------------------------------
# If a package update breaks something, you can install the versions the book was written
# against by pointing at a dated snapshot before running the install. Uncomment and set a
# date at or before the one in the book's last release:
#
# options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2026-08-01"))
#
# This affects what gets installed from here on, not what you already have.
# See .notes/DEPENDENCIES.md for the reasoning.
# -------------------------------------------------------------------------------------

if (is.null(getOption("repos")[["CRAN"]]) || getOption("repos")[["CRAN"]] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

installed_now <- function() rownames(installed.packages())

# Install the packages above *and their whole dependency tree*.
#
# Why the tree matters: install.packages() normally handles dependencies for you, but if one
# of them fails part-way (a large binary timing out, say) you get a package that appears
# installed and then refuses to load. That is how a missing `stringi` showed up as a broken
# `Bchron`, which is a confusing place to start debugging. Resolving the closure up front and
# checking it means the missing piece is named directly.
message("Resolving dependencies...")
deps <- tryCatch(
  unique(unlist(tools::package_dependencies(
    pkgs, recursive = TRUE, which = c("Depends", "Imports", "LinkingTo")))),
  error = function(e) { message("  could not reach CRAN to resolve dependencies; ",
                               "falling back to a plain install"); character(0) }
)

wanted  <- unique(c(pkgs, deps))
missing <- setdiff(wanted, installed_now())

if (length(missing) == 0) {
  message("All ", length(pkgs), " packages and their dependencies are present.")
} else {
  message("Installing ", length(missing), " package(s):\n  ",
          paste(missing, collapse = ", "))
  install.packages(missing)
}

# Verify by actually loading each package, and report *why* anything fails rather than just
# that it did.
problems <- list()
for (p in pkgs) {
  err <- tryCatch({ loadNamespace(p); NULL }, error = function(e) conditionMessage(e))
  if (!is.null(err)) problems[[p]] <- err
}

if (length(problems) == 0) {
  message("All packages load. You are ready to render the book.")
} else {
  message("\n", length(problems), " package(s) did not load:\n")
  for (p in names(problems)) message("  ", p, ": ", problems[[p]])

  # If the errors name packages that are simply absent, offer the one-liner that fixes it.
  named <- unlist(regmatches(
    unlist(problems),
    gregexpr("(?<=there is no package called ).{1,60}?(?=$|\\n)", unlist(problems), perl = TRUE)
  ))
  named <- unique(gsub("^[\"'‘’“”]|[\"'‘’“”]$", "",
                       trimws(named)))
  named <- setdiff(named, installed_now())
  if (length(named)) {
    message("\nThose errors point at missing packages. Install them with:\n",
            '  install.packages(c("', paste(named, collapse = '", "'), '"))\n',
            "then run source(\"install.R\") again.")
  } else {
    message("\nOn Linux, sf and terra need system libraries (GDAL, PROJ, GEOS).")
  }
}
