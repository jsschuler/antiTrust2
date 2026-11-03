suppressPackageStartupMessages({
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  repo_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
  source(file.path(repo_root, "analysisFunctions.R"))
})
options(dplyr.summarise.inform = FALSE)

near <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-9))

# Window centers cover the full grid with inclusive bounds of the right width.
stopifnot(length(windowCenters) == 9951, near(range(windowCenters), c(0.0025, 0.9975)))

# Agents sit exactly on window boundaries, just outside them, and at random.
set.seed(42)
gridPts <- c(0, 0.0049, 0.005, 0.0051, 0.5, 0.4975, 0.5025, 0.9951, 1)
privDex <- sort(c(gridPts, runif(3000)))
googPct <- runif(length(privDex), 0, 100)
selected <- runif(length(privDex)) < 0.5
bounds <- windowBounds(privDex)
fastMeans <- windowMeans(googPct, selected, bounds)
fastSmry <- windowSummary(googPct, selected, bounds)

# Reference: the original filter-per-window loop.
checked <- 0L
for (t in c(25, 26, 50, 51, 4975, 5000, 5025, 9951, 9975)){
  loRng <- (t - window / 2) / 10000
  hiRng <- (t + window / 2) / 10000
  inWin <- privDex >= loRng & privDex <= hiRng & selected
  j <- t - 24
  if (any(inWin)){
    x <- googPct[inWin]
    stopifnot(near(fastMeans[j], mean(x)))
    row <- fastSmry[abs(privDex - t / 10000) < 1e-12]
    stopifnot(nrow(row) == 1, near(row$mn, mean(x)), near(row$q05, quantile(x, .05)),
              near(row$q50, median(x)), near(row$q95, quantile(x, .95)))
  } else {
    stopifnot(is.nan(fastMeans[j]))
  }
  checked <- checked + 1L
}

# Nearest-center lookup in mcArea matches the original which.min search.
curveY <- abs(sin(windowCenters * 20))
set.seed(7)
fast <- mcArea(curveY, nSamps = 2000)
set.seed(7)
xSamps <- runif(2000, min(windowCenters), max(windowCenters))
ySamps <- runif(2000, 0, max(curveY))
slow <- (max(windowCenters) - min(windowCenters)) * max(curveY) *
  mean(sapply(1:2000, function(i) ySamps[i] <= curveY[which.min(abs(windowCenters - xSamps[i]))]))
stopifnot(near(fast, slow))

# Agent-level test runs, and its permutations keep each privacy level's treated-run count.
runs <- as.data.table(data.frame(key = paste0("k", 1:40), privacyVal = rep(c(0.5, 1.0), each = 20),
                                 treated = rep(c(TRUE, FALSE), 20)))
agt <- runs[rep(1:40, each = 25)][, `:=`(privDex = runif(.N), googPct = runif(.N, 0, 100))]
res <- agentLevelTest(agt, 1, 2, nPerm = 20)
stopifnot(length(res$null) == 20, all(res$null >= 0), res$obs >= 0,
          nrow(res$treated) > 0, nrow(res$untreated) > 0)
set.seed(2)
for (i in 1:20){
  permLab <- permuteWithin(runs$treated, runs$privacyVal)
  stopifnot(sum(permLab[runs$privacyVal == 0.5]) == 10, sum(permLab[runs$privacyVal == 1.0]) == 10)
}
# lopsided strata: the pooled shuffle would move treated runs between levels
lopsided <- rep(c(TRUE, FALSE), c(5, 35))
lev <- rep(1:2, each = 20)
stopifnot(all(replicate(50, sum(permuteWithin(lopsided, lev)[lev == 1])) == 5))

cat("Passed:", checked, "window checks against the filter loop, Monte Carlo lookup, and stratified agent permutations.\n")
