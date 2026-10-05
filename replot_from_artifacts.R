#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
})

# Usage:
#   Rscript replot_from_artifacts.R
#   Rscript replot_from_artifacts.R /abs/path/to/analysis_artifacts/YYYYMMDD_HHMMSS
#   Rscript replot_from_artifacts.R /abs/path/to/artifacts /abs/path/to/output_dir

args <- commandArgs(trailingOnly = TRUE)
repo_root <- getwd()
artifacts_root <- file.path(repo_root, "analysis_artifacts")
latest_ptr <- file.path(artifacts_root, "LATEST_ARTIFACT_DIR.txt")

if (length(args) >= 1) {
  artifact_dir <- args[[1]]
} else {
  if (!file.exists(latest_ptr)) {
    stop("Could not find LATEST_ARTIFACT_DIR.txt. Pass artifact dir as first argument.")
  }
  artifact_dir <- readLines(latest_ptr, warn = FALSE)[1]
}

if (!dir.exists(artifact_dir)) {
  stop(paste0("Artifact directory does not exist: ", artifact_dir))
}

if (length(args) >= 2) {
  out_dir <- args[[2]]
} else {
  out_dir <- file.path(artifact_dir, "replots")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_obj <- function(name) {
  p <- file.path(artifact_dir, paste0(name, ".rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

# Load plots
loVPN <- read_obj("loVPN")
hiVPN <- read_obj("hiVPN")
loDel <- read_obj("loDel")
hiDel <- read_obj("hiDel")
loSharing <- read_obj("loSharing")
hiSharing <- read_obj("hiSharing")

# Load table for labels (optional)
effectTable <- read_obj("effectTable")

title_with_p <- function(default_title, idx) {
  if (!is.null(effectTable) && nrow(effectTable) >= idx) {
    d <- round(effectTable$effect_size[idx], 1)
    p <- effectTable$p_report[idx]
    return(paste0(default_title, " (d=", d, "pp, p ", p, ")"))
  }
  default_title
}

maybe_retitle <- function(p, ttl) {
  if (is.null(p)) return(NULL)
  p + ggtitle(ttl)
}

save_plot <- function(p, name, w = 7, h = 7) {
  if (is.null(p)) return(FALSE)
  ggsave(filename = file.path(out_dir, name), plot = p, width = w, height = h, bg = "white")
  TRUE
}

# Single-panel density plots
loVPN2 <- maybe_retitle(loVPN, title_with_p("Lower Privacy Preference", 1))
hiVPN2 <- maybe_retitle(hiVPN, title_with_p("Higher Privacy Preference", 2))
loDel2 <- maybe_retitle(loDel, title_with_p("Lower Privacy Preference", 3))
hiDel2 <- maybe_retitle(hiDel, title_with_p("Higher Privacy Preference", 4))
loSharing2 <- maybe_retitle(loSharing, title_with_p("Lower Privacy Preference", 5))
hiSharing2 <- maybe_retitle(hiSharing, title_with_p("Higher Privacy Preference", 6))

save_plot(loVPN2, "vpn_low_density.png")
save_plot(hiVPN2, "vpn_high_density.png")
save_plot(loDel2, "deletion_low_density.png")
save_plot(hiDel2, "deletion_high_density.png")
save_plot(loSharing2, "sharing_low_density.png")
save_plot(hiSharing2, "sharing_high_density.png")

# Combined panels
if (!is.null(loVPN2) && !is.null(hiVPN2)) {
  vpnDist <- plot_grid(loVPN2, hiVPN2, align = "v", axis = "lr", ncol = 1)
  save_plot(vpnDist, "vpnDist.png")
}
if (!is.null(loDel2) && !is.null(hiDel2)) {
  delDist <- plot_grid(loDel2, hiDel2, align = "v", axis = "lr", ncol = 1)
  save_plot(delDist, "deletionDist.png")
}
if (!is.null(loSharing2) && !is.null(hiSharing2)) {
  sharingDist <- plot_grid(loSharing2, hiSharing2, align = "v", axis = "lr", ncol = 1)
  save_plot(sharingDist, "sharingDist.png")
}

cat(paste0("Replots written to: ", out_dir, "\n"))
