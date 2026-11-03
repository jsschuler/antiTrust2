suppressPackageStartupMessages({
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  repo_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))
  source(file.path(repo_root, "analysisFunctions.R"))
})
options(dplyr.summarise.inform = FALSE)

near <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-12))

# Four runs: tick 1 has Google shares 0%, 50%, 0%, 100%; tick 2 is all DuckDuckGo;
# tick 3 is all Google. Runs C and D are treated.
joint_data <- expand.grid(key = LETTERS[1:4], tick = 1:3, agent = 1:4, stringsAsFactors = FALSE)
google_count <- ifelse(joint_data$tick == 2, 0,
  ifelse(joint_data$tick == 3, 4, c(A = 0, B = 2, C = 0, D = 4)[joint_data$key]))
joint_data$currEngine <- ifelse(joint_data$agent <= google_count, "google", "duckDuckGo")
treatedKeys <- c("C", "D")

# Also test absent run/tick tuples and datasets containing just one engine.
scenarios <- list(joint_data,
  subset(joint_data, !(key == "B" & tick == 3)),
  transform(joint_data, currEngine = "duckDuckGo"),
  transform(joint_data, currEngine = "google"))
checked <- 0L
for (data in scenarios){
  timePct <- summarizeOutput(data)$timePct
  expected <- data %>% group_by(key, tick) %>%
    summarise(googPct = 100 * mean(currEngine == "google"), .groups = "drop")
  joined <- merge(expected, timePct, by = c("key", "tick"))
  # every observed run/tick is kept, including runs with no Google users
  stopifnot(nrow(timePct) == nrow(expected), nrow(joined) == nrow(expected),
            near(joined$googPct.x, joined$googPct.y))

  timePct[, treated := key %in% treatedKeys]
  smry <- timeSummary(timePct)
  expected_summary <- expected %>% mutate(treated = key %in% treatedKeys) %>% group_by(tick, treated) %>%
    summarise(q05 = quantile(googPct, .05), median = median(googPct), q95 = quantile(googPct, .95), .groups = "drop")
  both <- merge(expected_summary, smry, by = c("tick", "treated"))
  stopifnot(nrow(both) == nrow(expected_summary))
  for (column in c("q05", "median", "q95")){
    stopifnot(near(both[[paste0(column, ".x")]], both[[paste0(column, ".y")]]))
  }
  if (identical(data, joint_data)){
    stopifnot(near(smry[tick == 1 & !treated]$median, 25))
  }
  checked <- checked + 1L
}

# Median gap: untreated tick-1 median 25 vs treated 50, identical at ticks 2 and 3.
timePct <- summarizeOutput(joint_data)$timePct[, treated := key %in% treatedKeys]
wide <- dcast(timePct, key + treated ~ tick, value.var = "googPct")
stopifnot(near(medianGap(as.matrix(wide[, -(1:2)]), wide$treated), 25))
test <- timeTest(timePct, 1, nPerm = 50)
stopifnot(near(test$obs, 25), length(test$null) == 50, all(test$null >= 0), all(test$null <= 100))

# Run level: from tick 50 on only, and the permutation test matches the
# original dplyr loop for the same seed.
late <- transform(joint_data, tick = tick + 48)
runPct <- summarizeOutput(late)$runPct
stopifnot(near(runPct[key == "B"]$googPct, 100 * mean(late$currEngine[late$key == "B" & late$tick >= 50] == "google")))
set.seed(3)
googPct <- runif(30, 0, 100)
lab <- rep(c(TRUE, FALSE), 15)
res <- runLevelTest(googPct, lab, 99)
set.seed(99)
long <- data.frame(googPct = googPct, category = if_else(lab, "True", "False"))
reference <- sapply(seq_len(nrow(long)), function(i){
  long$permCat <- sample(long$category, nrow(long), replace = FALSE)
  long %>% group_by(permCat) %>% summarise(mn = mean(googPct)) %>%
    pivot_wider(names_from = permCat, values_from = mn) %>% transform(delta = True - False) %>% pull(delta)
})
stopifnot(near(res$null, reference), near(res$obs, mean(googPct[lab]) - mean(googPct[!lab])))

cat("Passed:", checked, "summary scenarios, median-gap and run-level permutation checks.\n")
