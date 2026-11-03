################################################################################
#             Analysis Functions                                               #
#             Anti-Trust Model                                                 #
#             Loading, permutation tests and plot helpers shared by            #
#             dataAnalysisPVal.R and the tests in test/                        #
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(data.table)

basePoint <- "black"
bgFill <- "white"

# the treatment tick every experiment switches its intervention on at
treatTick <- 50

plotTheme <- function(){
  theme(
    panel.background = element_rect(fill = bgFill),
    plot.title = element_text(color = basePoint, hjust = 0.5),
    plot.background = element_rect(fill = bgFill),
    panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.3)),
    axis.text = element_text(color = basePoint),
    axis.title = element_text(color = basePoint),
    legend.background = element_rect(fill = bgFill),
    legend.text = element_text(color = basePoint))
}

#### LOADING ####

# Google share per run and tick, run (from tick 50 on) and agent (from tick 50
# on) for one output file. Each agent is observed once per tick, so the run
# share equals the mean of its agents' shares.
summarizeOutput <- function(outDat){
  outDat <- as.data.table(outDat)
  list(
    timePct = outDat[, .(googPct = 100 * mean(currEngine == "google")), by = .(key, tick)],
    runPct = outDat[tick >= treatTick, .(googPct = 100 * mean(currEngine == "google")), by = key],
    agentPct = outDat[tick >= treatTick, .(googPct = 100 * mean(currEngine == "google")), by = .(key, agent)]
  )
}

# Read and stack the control file, agent file and summarized output of the given
# experiments. Each experiment (baseline, vpn, deletion, sharing) is its own sweep
# with ctrl.csv and the run CSVs in <dataRoot>/<experiment>/data. Output files are
# summarized one at a time so the 100M-row agent-tick table is never in memory.
loadExperiments <- function(dataRoot, exps){
  dirs <- file.path(dataRoot, exps, "data")
  control <- rbindlist(lapply(file.path(dirs, "ctrl.csv"), fread))
  control[, privacyVal := as.numeric(privacyVal)]
  control[, privalVal := factor(if_else(privacyVal == 1.0, "Low", "High"), levels = c("Low", "High"))]
  control[, category := 1 * (vpnTick == treatTick) + 2 * (deletionTick == treatTick) + 4 * (sharingTick == treatTick)]

  agentFi <- list.files(dirs, pattern = "^agents.*\\.csv$", full.names = TRUE)
  agentDat <- rbindlist(lapply(agentFi, fread, header = FALSE,
                               col.names = c("key", "agent", "blissPoint", "selfExp", "unifExp")))

  outFi <- list.files(dirs, pattern = "^output.*\\.csv$", full.names = TRUE)
  outCols <- c("key", "tick", "agent", "currEngine", "duckTick", "vpnTick", "delTick", "shareTick", "optOut")
  smryList <- lapply(outFi, function(fi) summarizeOutput(fread(fi, header = FALSE, col.names = outCols)))

  list(
    control = control,
    agentDat = agentDat,
    timePct = rbindlist(lapply(smryList, `[[`, "timePct")),
    runPct = rbindlist(lapply(smryList, `[[`, "runPct")),
    agentPct = rbindlist(lapply(smryList, `[[`, "agentPct"))
  )
}

#### PERMUTATION TESTS ####

# p-values of an observed statistic against its permutation null
perm_pvals <- function(obsVal, nullVals){
  B <- length(nullVals)
  extremeCnt <- sum(abs(nullVals) >= abs(obsVal))
  pApprox <- extremeCnt / B
  if (pApprox == 0){
    floorTxt <- paste0("< ", signif(1 / B, 3))
  } else {
    floorTxt <- paste0("= ", sprintf("%.3f", pApprox))
  }
  # rank of the observed value among null and observed, ties placed after the null
  rankFrac <- (sum(nullVals <= obsVal) + 1) / (B + 1)
  return(list(
    B = B,
    p_rank = min(rankFrac, 1 - rankFrac),
    p_approx = pApprox,
    p_plus1 = (extremeCnt + 1) / (B + 1),
    p_floor = floorTxt
  ))
}

# one row of the effect-size table
effectRow <- function(label, obsVal, nullVals){
  pTmp <- perm_pvals(obsVal, nullVals)
  data.frame(
    label = label,
    effect_size = obsVal,
    null_q025 = unname(quantile(nullVals, .025)),
    null_q975 = unname(quantile(nullVals, .975)),
    p_rank = pTmp$p_rank,
    p_approx = pTmp$p_approx,
    p_plus1 = pTmp$p_plus1,
    p_report = pTmp$p_floor,
    n_permutations = pTmp$B
  )
}

# difference in mean run-level Google share, treated minus untreated, with one
# label permutation per run as the null
runLevelTest <- function(googPct, treated, seed){
  meanGap <- function(lab) mean(googPct[lab]) - mean(googPct[!lab])
  set.seed(seed)
  nullVals <- sapply(seq_along(treated), function(i) meanGap(sample(treated, length(treated), replace = FALSE)))
  list(obs = meanGap(treated), null = nullVals)
}

#### AGENT-LEVEL MOVING WINDOWS ####

# window centers on the privacy index: 50/10000 wide, stepping by 1/10000
window <- 50
windowCenters <- ((window / 2):(10000 - window / 2)) / 10000

# first and last position of each window [center - w/2, center + w/2] (both ends
# inclusive) in the sorted privacy index; an empty window has last < first
windowBounds <- function(sortedDex){
  t <- round(windowCenters * 10000)
  loRng <- (t - window / 2) / 10000
  hiRng <- (t + window / 2) / 10000
  list(first = findInterval(loRng, sortedDex, left.open = TRUE) + 1,
       last = findInterval(hiRng, sortedDex))
}

# mean Google share of the selected agents in each window, from cumulative sums
# over agents sorted by privacy index; NaN where the window holds none of them
windowMeans <- function(googPct, selected, bounds){
  csSum <- c(0, cumsum(googPct * selected))
  csCnt <- c(0, cumsum(selected))
  (csSum[bounds$last + 1] - csSum[bounds$first]) / (csCnt[bounds$last + 1] - csCnt[bounds$first])
}

# quantile and mean curves of the selected agents across windows, for plotting
windowSummary <- function(googPct, selected, bounds){
  rbindlist(lapply(seq_along(windowCenters), function(j){
    inWin <- if (bounds$last[j] >= bounds$first[j]) bounds$first[j]:bounds$last[j] else integer(0)
    x <- googPct[inWin][selected[inWin]]
    if (length(x) == 0) return(NULL)
    q <- quantile(x, c(.05, .25, .5, .75, .95), names = FALSE)
    data.table(privDex = windowCenters[j], q05 = q[1], q25 = q[2], q50 = q[3], mn = mean(x), q75 = q[4], q95 = q[5])
  }))
}

# Monte Carlo estimate of the area under a curve sampled on windowCenters, with
# each random point read off at its nearest window center
mcArea <- function(curveY, nSamps = 10000){
  curveY[is.na(curveY)] <- 0
  xLo <- min(windowCenters)
  xHi <- max(windowCenters)
  yHi <- max(curveY)
  xSamps <- runif(nSamps, xLo, xHi)
  ySamps <- runif(nSamps, 0, yHi)
  nearest <- pmin(pmax(round((xSamps - xLo) * 10000) + 1, 1), length(windowCenters))
  (xHi - xLo) * yHi * mean(ySamps <= curveY[nearest])
}

# shuffle labels among the runs of each stratum separately
permuteWithin <- function(labels, strata){
  for (idx in split(seq_along(labels), strata)) labels[idx] <- labels[idx][sample.int(length(idx))]
  labels
}

# area between the treated and untreated moving-average curves, observed and
# under permutation of the run labels within each privacy level
agentLevelTest <- function(agt, obsSeed, permSeed, nPerm = 1000){
  agt <- agt[order(privDex)]
  bounds <- windowBounds(agt$privDex)
  runs <- unique(agt[, .(key, treated, privacyVal)])
  runIdx <- match(agt$key, runs$key)
  curveGap <- function(treatedAgt){
    abs(windowMeans(agt$googPct, treatedAgt, bounds) - windowMeans(agt$googPct, !treatedAgt, bounds))
  }

  set.seed(obsSeed)
  obsVal <- mcArea(curveGap(agt$treated))
  set.seed(permSeed)
  nullVals <- sapply(1:nPerm, function(i){
    mcArea(curveGap(permuteWithin(runs$treated, runs$privacyVal)[runIdx]))
  })
  list(obs = obsVal, null = nullVals,
       untreated = windowSummary(agt$googPct, !agt$treated, bounds),
       treated = windowSummary(agt$googPct, agt$treated, bounds))
}

#### TIME TRENDS ####

# area between the two median curves, summed directly over ticks
medianGap <- function(googMat, treated){
  sum(abs(apply(googMat[treated, , drop = FALSE], 2, median) -
          apply(googMat[!treated, , drop = FALSE], 2, median)))
}

# median-gap test within one privacy level; labels are permuted among runs of
# this privacy level only, since pooling the two levels puts the pooled median
# in the gap between them and leaves the permutation with almost no power
timeTest <- function(timeDat, seed, nPerm = 1000){
  wide <- dcast(timeDat, key + treated ~ tick, value.var = "googPct")
  googMat <- as.matrix(wide[, -(1:2)])
  treated <- wide$treated
  set.seed(seed)
  list(obs = medianGap(googMat, treated),
       null = sapply(1:nPerm, function(i) medianGap(googMat, sample(treated))))
}

timeSummary <- function(timeDat){
  timeDat[, .(q05 = quantile(googPct, .05), median = quantile(googPct, .5), q95 = quantile(googPct, .95)),
          by = .(tick, treated)][order(tick, treated)]
}
