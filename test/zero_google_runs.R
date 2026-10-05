suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
options(dplyr.summarise.inform = FALSE)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo_root <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg))))

# Four runs: tick 1 has Google shares 0%, 50%, 0%, 100%; tick 2 is all DuckDuckGo;
# tick 3 is all Google. Both logical treatment labels are represented.
joint_data <- expand.grid(key = LETTERS[1:4], tick = 1:3, agent = 1:4,
                          stringsAsFactors = FALSE)
joint_data$category <- joint_data$key %in% c("C", "D")
google_count <- ifelse(joint_data$tick == 2, 0,
  ifelse(joint_data$tick == 3, 4,
    c(A = 0, B = 2, C = 0, D = 4)[joint_data$key]))
joint_data$currEngine <- ifelse(joint_data$agent <= google_count, "google", "duckDuckGo")

assignment_to <- function(expr, name) {
  is.call(expr) && identical(expr[[1]], as.name("<-")) && identical(expr[[2]], as.name(name))
}
near <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-12))

pipelines_checked <- 0L
permutations_checked <- 0L
for (file in c("dataAnalysisPVal.R", "newAnalysis.R")) {
  expressions <- as.list(parse(file.path(repo_root, file)))
  starts <- which(vapply(expressions, assignment_to, logical(1), name = "byCat"))
  stopifnot(length(starts) == 3)
  for (start in starts) {
    # Also test absent run/tick tuples and datasets containing just one engine.
    scenarios <- list(joint_data,
      subset(joint_data, !(key == "B" & tick == 3)),
      transform(joint_data, currEngine = "duckDuckGo"),
      transform(joint_data, currEngine = "google"))
    for (data in scenarios) {
      env <- new.env()
      env$jointDat <- data
      for (expr in expressions[start:length(expressions)]) {
        if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
            any(all.names(expr[[2]]) %in% c("byCat", "denom", "jointDenom", "modSmry", "modDenom", "modJoint", "newData"))) {
          eval(expr, env)
          if (assignment_to(expr, "newData")) break
        }
      }

      # Compute an independent reference directly from agent-level indicator values.
      expected <- data %>% group_by(key, tick, category) %>%
        summarise(googPct = 100 * mean(currEngine == "google"), .groups = "drop")
      actual <- env$modJoint %>% filter(currEngine == "google") %>%
        transmute(key, tick, category, googPct = 100 * cnt / total)
      joined <- merge(expected, actual, by = c("key", "tick", "category"))
      stopifnot(nrow(actual) == nrow(expected), nrow(joined) == nrow(expected),
                near(joined$googPct.x, joined$googPct.y))

      expected_summary <- expected %>% group_by(tick, category) %>%
        summarise(q05 = quantile(googPct, .05), median = median(googPct),
                  q95 = quantile(googPct, .95), .groups = "drop")
      summary <- merge(expected_summary, env$newData, by = c("tick", "category"))
      stopifnot(nrow(summary) == nrow(expected_summary))
      for (column in c("q05", "median", "q95")) {
        stopifnot(near(summary[[paste0(column, ".x")]], summary[[paste0(column, ".y")]]))
      }

      expected_pooled <- data %>% group_by(tick, category) %>%
        summarise(googPCt = mean(currEngine == "google"), .groups = "drop")
      pooled <- merge(expected_pooled, env$jointDenom, by = c("tick", "category"))
      stopifnot(nrow(pooled) == nrow(expected_pooled), near(pooled$googPCt.x, pooled$googPCt.y))

      if (identical(data, joint_data)) {
        stopifnot(near(env$newData$median[env$newData$tick == 1 & !env$newData$category], 25))
        baseline_mod_joint <- env$modJoint
      }
    }
    pipelines_checked <- pipelines_checked + 1L
  }

  definitions <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function")) &&
      "modJoint" %in% all.names(expr[[3]])
  }, expressions)
  for (definition in definitions) {
    env <- new.env()
    env$modJoint <- baseline_mod_joint
    # Deterministically swap treatment labels while preserving each run's trajectory.
    env$sample <- function(x, size, replace) rev(x)
    eval(definition, env)
    fn <- get(as.character(definition[[2]]), env)
    for (expr in as.list(body(fn))[-1]) {
      eval(expr, env)
      if (assignment_to(expr, "permDiffFrame")) break
    }
    stopifnot(nrow(env$permModPct) == 12,
      near(env$permDiffFrame$absDiff[match(1:3, env$permDiffFrame$tick)], c(25, 0, 0)))
    value <- fn(1)
    stopifnot(is.finite(value), value >= 0, value <= 50)
    permutations_checked <- permutations_checked + 1L
  }
}
stopifnot(pipelines_checked == 6, permutations_checked == 3)
cat("Passed:", pipelines_checked, "summary pipelines across four scenarios and",
    permutations_checked, "time-trend permutation functions.\n")
