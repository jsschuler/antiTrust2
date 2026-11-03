################################################################################
#             Final Analysis Code                                              #
#             Anti-Trust Model                                                 #
#             Nov 2026 Update                                                  #
#             John S. Schuler                                                  #
#             This version uses privacy index rather than bliss point          #
################################################################################

# Each intervention (VPN, deletion, sharing) is compared against the
# DuckDuckGo-entry-only baseline at three levels:
#   run level   - mean Google share from tick 50 on, within each privacy level
#   agent level - moving average of agent Google share along the privacy index,
#                 within each privacy level
#   time        - median Google share per tick, within each privacy level

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
codeDir <- if (length(script_arg) == 1) dirname(normalizePath(sub("^--file=", "", script_arg))) else getwd()
source(file.path(codeDir, "analysisFunctions.R"))

dataRoot <- "~/ResearchCode/antitrust2-data"
imageDir <- "~/ResearchCode/antiTrustImages"
dir.create(imageDir, recursive = TRUE, showWarnings = FALSE)

# category codes from loadExperiments: 0 baseline, 1 VPN, 2 deletion, 4 sharing
interventions <- list(
  list(name = "VPN", cat = 1, plotName = "VPN", file = "vpn", timeFile = "vpnTimeByPrivacy.png",
       runSeeds = c(Low = 1234, High = 67869), agentSeeds = c(123443, 98765), timeSeed = 1357922),
  list(name = "Deletion", cat = 2, plotName = "Del", file = "deletion", timeFile = "DeletionTimeByPrivacy.png",
       runSeeds = c(Low = 2468, High = 243), agentSeeds = c(56789, 15333443), timeSeed = 2468013),
  list(name = "Sharing", cat = 4, plotName = "Sharing", file = "sharing", timeFile = "SharingTimeByPrivacy.png",
       runSeeds = c(Low = 1357, High = 1357), agentSeeds = c(2468032, 3443), timeSeed = 3579124)
)
privLabs <- c(Low = "Lower", High = "Higher")

dat <- loadExperiments(dataRoot, c("baseline", "vpn", "deletion", "sharing"))
control <- dat$control

#### RUN LEVEL ####

merge(control, dat$runPct, by = "key") -> controlSmry

runEffects <- list()
for (iv in interventions){
  controlSmry[category %in% c(0, iv$cat)][, category := factor(if_else(category == 0, "False", "True"))] -> pairDat
  assign(paste0(tolower(iv$file), "Dat"), as.data.frame(pairDat))
  x0 <- min(pairDat$googPct)
  x1 <- max(pairDat$googPct)
  for (lev in c("Low", "High")){
    # base (locale) key order, as the original merge-based version had, keeps
    # each seed producing the same permutations
    levDat <- pairDat[privalVal == lev]
    levDat <- levDat[base::order(levDat$key)]
    test <- runLevelTest(levDat$googPct, levDat$category == "True", iv$runSeeds[[lev]])
    effectRow(paste0(tolower(lev), " privacy - ", iv$name), test$obs, test$null) -> row
    runEffects[[length(runEffects) + 1]] <- row

    ggplot(levDat) + geom_density(aes(x = googPct, linetype = category), alpha = .2) +
      xlim(x0, x1) +
      labs(x = "% Hoogle Usage", y = "Density", linetype = iv$name,
           title = paste0(privLabs[[lev]], " Privacy Preference (d=", round(row$effect_size, 1), "pp, p ", row$p_report, ")")) +
      plotTheme() -> densityPlot
    assign(paste0(if (lev == "Low") "lo" else "hi", iv$plotName), densityPlot)
  }
  plot_grid(get(paste0("lo", iv$plotName)), get(paste0("hi", iv$plotName)), align = "v", axis = "lr", ncol = 1)
  ggsave(file.path(imageDir, paste0(iv$file, "Dist.png")), width = 7, height = 7, bg = bgFill)
}

#### AGENT LEVEL ####

merge(dat$agentDat, dat$agentPct, by = c("key", "agent")) %>%
  merge(control[, .(key, privacyVal, category)], by = "key") -> agtSmry
agtSmry[, privDex := blissPoint / unifExp]

agentPanel <- function(smry, title){
  ggplot(smry) + geom_line(aes(x = privDex, y = mn), linewidth = 0.4) +
    geom_line(aes(x = privDex, y = q05), alpha = .8, linetype = "dotted", linewidth = 0.25) +
    geom_line(aes(x = privDex, y = q95), alpha = .8, linetype = "dotted", linewidth = 0.25) +
    geom_ribbon(aes(x = privDex, ymin = q25, ymax = q75), alpha = .4) +
    geom_line(aes(x = privDex, y = q50), linetype = "dotted", linewidth = 0.4) +
    plotTheme() +
    labs(x = "Agent Privacy Index", y = "% Hoogle Usage", title = title) + ylim(0, 100)
}

titledGrid <- function(label, grid){
  title <- ggdraw() + draw_label(label, fontface = "bold", size = 14, hjust = 0.5)
  plot_grid(title, grid, ncol = 1, rel_heights = c(0.12, 1))
}

# each intervention is tested separately within each privacy level, since an
# intervention can move privacy-minded and privacy-indifferent agents in
# opposite directions and the pooled effect then depends on the population mix
agentEffects <- list()
for (iv in interventions){
  levelGrids <- list()
  basePanels <- list()
  for (lev in c("Low", "High")){
    privacy <- if (lev == "Low") 1.0 else 0.5
    agt <- agtSmry[privacyVal == privacy & category %in% c(0, iv$cat)][, treated := category == iv$cat]
    test <- agentLevelTest(agt, iv$agentSeeds[1] + (lev == "High"), iv$agentSeeds[2] + (lev == "High"))
    effectRow(paste0("Privacy - ", iv$name, " (", tolower(privLabs[[lev]]), " privacy)"), test$obs, test$null) -> row
    agentEffects[[length(agentEffects) + 1]] <- row

    basePanels[[lev]] <- agentPanel(test$untreated, paste(privLabs[[lev]], "Privacy Preference"))
    plot_grid(agentPanel(test$untreated, paste("No", iv$name)), agentPanel(test$treated, iv$name),
              align = "h", axis = "lr", ncol = 2) -> grid
    levelGrids[[lev]] <- titledGrid(paste0(privLabs[[lev]], " Privacy Preference (d=", round(row$effect_size, 3),
                                           ", p ", row$p_report, ")"), grid)
  }

  # the untreated runs are the baseline, so any pair gives the baseline curves
  if (iv$cat == 1){
    titledGrid("Hoogle Usage With Respect to Agent Privacy Preference",
               plot_grid(basePanels$Low, basePanels$High, align = "h", axis = "lr", ncol = 2))
    ggsave(file.path(imageDir, "base.png"), width = 12, height = 6, bg = bgFill)
  }

  plot_grid(levelGrids$Low, levelGrids$High, ncol = 1)
  ggsave(file.path(imageDir, paste0(iv$file, "Agent.png")), width = 12, height = 12, bg = bgFill)
}

#### TIME TRENDS ####

merge(dat$timePct, control[, .(key, privacyVal, category)], by = "key") -> timeDat

timeEffects <- list()
timeSmryList <- list()
for (iv in interventions){
  timePlots <- list()
  for (lev in c("Low", "High")){
    privacy <- if (lev == "Low") 1.0 else 0.5
    sub <- timeDat[privacyVal == privacy & category %in% c(0, iv$cat)][, treated := category == iv$cat]
    test <- timeTest(sub, iv$timeSeed + (lev == "High"))
    effectRow(paste0("Time - ", iv$name, " (", tolower(privLabs[[lev]]), " privacy)"), test$obs, test$null) -> row
    timeEffects[[length(timeEffects) + 1]] <- row

    timeSummary(sub)[, category := if_else(treated, iv$name, paste("No", iv$name))] -> newData
    timeSmryList[[length(timeSmryList) + 1]] <- newData[, `:=`(intervention = iv$name, privalVal = lev)]

    # event labels sit at the bottom of the band, entry label left of its line
    # and intervention label right of its, so the two never collide
    yBot <- min(newData$q05)
    ggplot(data = newData) + geom_line(aes(x = tick, y = median, linetype = category), color = "black") +
      geom_ribbon(aes(x = tick, ymin = q05, ymax = q95, linetype = category), alpha = .2) +
      geom_vline(xintercept = 30, color = "black", alpha = .4) +
      geom_vline(xintercept = treatTick, color = "black", alpha = .4) +
      plotTheme() +
      labs(x = "Time", y = "% Hoogle Usage", linetype = paste(iv$name, "Access"),
           title = paste0(privLabs[[lev]], " Privacy Preference (d=", round(row$effect_size, 1), ", p ", row$p_report, ")")) +
      scale_linetype_manual(values = setNames(c("solid", "dotted"), c(iv$name, paste("No", iv$name)))) +
      annotate("text", label = "Duck Duck Go", x = 30, y = yBot, angle = 90, vjust = -.4, hjust = 0,
               color = "black", size = 4, family = "Helvetica") +
      annotate("text", label = iv$name, x = treatTick, y = yBot, angle = 90, vjust = 1.4, hjust = 0,
               color = "black", size = 4, family = "Helvetica") -> timePlots[[lev]]
  }
  plot_grid(timePlots$Low, timePlots$High, align = "v", axis = "lr", ncol = 1)
  ggsave(filename = file.path(imageDir, iv$timeFile), width = 7, height = 10, bg = bgFill)
}

#### EFFECT TABLE AND ARTIFACTS ####

# rows 1-6 run level, 7-12 agent level, 13-18 time; replot_from_artifacts.R
# relies on the run-level rows coming first
effectTable <- rbind(rbindlist(runEffects), rbindlist(agentEffects), rbindlist(timeEffects))
print(effectTable)
write.csv(effectTable, file.path(dataRoot, "effect_size_summary.csv"), row.names = FALSE)

timeSmry <- rbindlist(timeSmryList)

# Persist plot-ready artifacts so plots can be edited without rerunning analysis.
artifactRoot <- file.path(dataRoot, "analysis_artifacts")
artifactDir <- file.path(artifactRoot, format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(artifactDir, recursive = TRUE, showWarnings = FALSE)

for (objName in c("effectTable", "controlSmry", "vpnDat", "deletionDat", "sharingDat", "timeSmry")){
  saveRDS(get(objName), file.path(artifactDir, paste0(objName, ".rds")))
  write.csv(get(objName), file.path(artifactDir, paste0(objName, ".csv")), row.names = FALSE)
}
# plot objects (can be re-opened and restyled with ggplot_build/ggplotGrob)
for (objName in c("loVPN", "hiVPN", "loDel", "hiDel", "loSharing", "hiSharing")){
  saveRDS(get(objName), file.path(artifactDir, paste0(objName, ".rds")))
}

writeLines(artifactDir, con = file.path(artifactRoot, "LATEST_ARTIFACT_DIR.txt"))
message(paste0("Artifacts written to: ", artifactDir))
