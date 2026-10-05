suppressPackageStartupMessages(library(dplyr))

# Inspect and execute the actual summary-loop bodies without loading archived data
# or running the scripts' plots and full permutation analyses.
find_window_loops <- function(node) {
  if (missing(node)) return(list())
  found <- list()
  if (is.call(node) && identical(node[[1]], as.name("for"))) {
    body <- node[[4]]
    if (is.call(body) && identical(body[[1]], as.name("{"))) {
      statements <- as.list(body)[-1]
      assigns <- function(expr, name) {
        is.call(expr) && identical(expr[[1]], as.name("<-")) &&
          identical(expr[[2]], as.name(name))
      }
      lower <- Filter(function(x) assigns(x, "loRng"), statements)
      upper <- Filter(function(x) assigns(x, "hiRng"), statements)
      if (length(lower) == 1 && length(upper) == 1) {
        found <- list(list(body = body, lower = lower[[1]], upper = upper[[1]]))
      }
    }
  }
  if (is.call(node) || is.expression(node)) {
    for (child in as.list(node)) {
      found <- c(found, find_window_loops(child))
    }
  }
  found
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
checked <- 0L
for (file in "dataAnalysisPVal.R") {
  loops <- find_window_loops(parse(file.path(repo_root, file)))
  stopifnot(length(loops) == 6)
  for (loop in loops) {
    env <- new.env()
    env$window <- 50
    env$t <- 25:9975
    eval(loop$lower, env)
    eval(loop$upper, env)
    stopifnot(
      all(env$loRng >= 0), all(env$hiRng <= 1),
      all(abs(env$hiRng - env$loRng - 0.005) < 1e-12),
      all(abs((env$hiRng + env$loRng) / 2 - env$t / 10000) < 1e-12)
    )

    # Check actual grouping, means, quantiles, and inclusive boundary membership.
    for (center in c(0.0025, 0.5, 0.9975)) {
      lower <- center - 0.0025
      upper <- center + 0.0025
      # Use the same exact grid coordinates at the inclusive boundaries.
      bounds <- (round(center * 10000) + c(-25, 25)) / 10000
      data <- data.frame(privDex = c(bounds[1], center, bounds[2]), googPct = c(10, 20, 30))
      if (lower > 0) data <- rbind(data, data.frame(privDex = lower - 0.0001, googPct = 100))
      if (upper < 1) data <- rbind(data, data.frame(privDex = upper + 0.0001, googPct = 100))
      data <- bind_rows(lapply(c(0, 1, 2, 4), function(category) {
        transform(data, category = category, permCat = category)
      }))
      env$agtVPN <- env$agtDel <- env$agtSharing <- env$permAgt <- data
      env$t <- round(center * 10000)
      env$datList <- list()
      eval(loop$body, env)
      result <- env$datList[[1]]
      stopifnot(nrow(result) == 4, all(result$mn == 20), all(result$privDex == center))
      if ("q50" %in% names(result)) {
        stopifnot(all(result$q50 == 20), all(abs(result$q05 - 11) < 1e-12),
          all(abs(result$q95 - 29) < 1e-12))
      }
    }
    checked <- checked + 1L
  }
}
cat("Passed:", checked, "moving-average loops; full-grid bounds and synthetic summaries.\n")
