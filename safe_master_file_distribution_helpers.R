# safe_master_file_distribution_helpers.R
# Current as of: 2026-03-16
#
# Purpose:
#   Audit and safely distribute shared "standard files" from a master folder to
#   project folders under ~/R Working Directory without accidentally overwriting
#   newer local versions.
#
# Core philosophy:
#   1. Compare first
#   2. Copy only when justified
#   3. Protect newer destination files by default
#   4. Optionally back up any file before replacement
#
# Main functions:
#   - default_distribution_files()
#   - list_project_directories_safe()
#   - audit_distribution_conflicts()
#   - summarize_distribution_audit()
#   - write_distribution_audit_excel()
#   - distribute_project_files_safe()
#
# Suggested workflow:
#   master_code_dir <- path.expand("~/R Working Directory/Master File Distribution Code")
#   source(master_code_dir |> file.path("safe_master_file_distribution_helpers.R"))
#
#   audit <- audit_distribution_conflicts()
#   summary <- summarize_distribution_audit(audit)
#   write_distribution_audit_excel(audit, summary = summary)
#
#   # Only after review:
#   distribute_project_files_safe(dry_run = FALSE)

default_distribution_files <- function(profile = c("analysis", "quarto_site", "minimal")) {
  profile <- match.arg(profile)

  common <- c(
    ".gitattributes",
    ".gitignore",
    ".Rbuildignore",
    "swart.css",
    "xaringan-themer.css",
    "header.html",
    "r-colors.css",
    "reference-backlinks.js",
    "tachyons.min.css"
  )

  analysis_extra <- c(
    "repo_file_audit_helpers_with_pdf_and_excel.R",
    "safe_master_file_distribution_helpers.R"
  )

  quarto_site_extra <- c(
    "repo_file_audit_helpers_with_pdf_and_excel.R"
  )

  if (profile == "analysis") {
    unique(c(common, analysis_extra))
  } else if (profile == "quarto_site") {
    unique(c(common, quarto_site_extra))
  } else {
    common
  }
}

list_project_directories_safe <- function(
  root_dir = "~/R Working Directory",
  exclude_dirs = c("Master File Distribution Code"),
  include_root = TRUE
) {
  root_dir <- normalizePath(path.expand(root_dir), winslash = "/", mustWork = TRUE)

  subdirs <- list.dirs(root_dir, full.names = TRUE, recursive = FALSE)
  subdirs <- normalizePath(subdirs, winslash = "/", mustWork = FALSE)

  visible_dirs <- subdirs[!grepl("/\\.", subdirs)]
  visible_dirs <- setdiff(visible_dirs, file.path(root_dir, exclude_dirs))

  out <- visible_dirs

  if (include_root) {
    out <- c(root_dir, out)
  }

  out
}

.get_file_metadata <- function(path) {
  exists <- file.exists(path)

  if (!exists) {
    return(list(
      exists = FALSE,
      size_bytes = NA_real_,
      mtime = as.POSIXct(NA),
      md5 = NA_character_
    ))
  }

  info <- file.info(path)
  md5_val <- unname(tools::md5sum(path))

  list(
    exists = TRUE,
    size_bytes = as.numeric(info$size),
    mtime = info$mtime,
    md5 = md5_val
  )
}

.classify_file_comparison <- function(src, dest) {
  if (!src$exists) return("missing_source")
  if (!dest$exists) return("missing_destination")

  if (!is.na(src$md5) && !is.na(dest$md5) && identical(src$md5, dest$md5)) {
    return("identical")
  }

  if (!is.na(src$mtime) && !is.na(dest$mtime)) {
    if (src$mtime > dest$mtime) return("different_source_newer")
    if (src$mtime < dest$mtime) return("different_destination_newer")
  }

  "different_same_or_unknown_time"
}

.recommend_action <- function(status) {
  dplyr::case_when(
    status == "missing_source" ~ "Fix source file first",
    status == "missing_destination" ~ "Copy",
    status == "identical" ~ "Skip",
    status == "different_source_newer" ~ "Copy",
    status == "different_destination_newer" ~ "Review: destination newer",
    status == "different_same_or_unknown_time" ~ "Review: contents differ",
    TRUE ~ "Review"
  )
}

audit_distribution_conflicts <- function(
  root_dir = "~/R Working Directory",
  source_files_dir = "~/R Working Directory/Master File Distribution Code",
  files_to_copy = default_distribution_files("analysis"),
  exclude_dirs = c("Master File Distribution Code"),
  include_root = TRUE,
  include_hidden_files = TRUE,
  return_tibble = TRUE
) {
  root_dir <- normalizePath(path.expand(root_dir), winslash = "/", mustWork = TRUE)
  source_files_dir <- normalizePath(path.expand(source_files_dir), winslash = "/", mustWork = TRUE)

  destinations <- list_project_directories_safe(
    root_dir = root_dir,
    exclude_dirs = exclude_dirs,
    include_root = include_root
  )

  if (!include_hidden_files) {
    files_to_copy <- files_to_copy[substr(files_to_copy, 1, 1) != "."]
  }

  rows <- vector("list", length(destinations) * length(files_to_copy))
  idx <- 1L

  for (dest_dir in destinations) {
    repo_name <- if (normalizePath(dest_dir, winslash = "/", mustWork = FALSE) == root_dir) {
      "R Working Directory"
    } else {
      basename(dest_dir)
    }

    for (file in files_to_copy) {
      src_path <- file.path(source_files_dir, file)
      dest_path <- file.path(dest_dir, file)

      src_meta <- .get_file_metadata(src_path)
      dest_meta <- .get_file_metadata(dest_path)
      status <- .classify_file_comparison(src_meta, dest_meta)

      rows[[idx]] <- data.frame(
        repo_name = repo_name,
        destination_dir = dest_dir,
        file_name = file,
        source_path = src_path,
        destination_path = dest_path,
        source_exists = src_meta$exists,
        destination_exists = dest_meta$exists,
        source_size_bytes = src_meta$size_bytes,
        destination_size_bytes = dest_meta$size_bytes,
        source_mtime = as.character(src_meta$mtime),
        destination_mtime = as.character(dest_meta$mtime),
        source_md5 = src_meta$md5,
        destination_md5 = dest_meta$md5,
        compare_status = status,
        recommended_action = .recommend_action(status),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }

  rows <- rows[seq_len(idx - 1L)]
  out <- do.call(rbind, rows)

  out$source_mtime <- as.POSIXct(out$source_mtime, tz = Sys.timezone())
  out$destination_mtime <- as.POSIXct(out$destination_mtime, tz = Sys.timezone())

  out$source_size_mb <- round(out$source_size_bytes / (1024^2), 3)
  out$destination_size_mb <- round(out$destination_size_bytes / (1024^2), 3)

  out <- out[, c(
    "repo_name", "file_name", "compare_status", "recommended_action",
    "source_exists", "destination_exists",
    "source_size_mb", "destination_size_mb",
    "source_mtime", "destination_mtime",
    "source_path", "destination_path",
    "source_md5", "destination_md5",
    "destination_dir"
  )]

  out <- out[order(out$file_name, out$repo_name), ]
  rownames(out) <- NULL

  if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
    out <- tibble::as_tibble(out)
  }

  out
}

summarize_distribution_audit <- function(audit_report) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install dplyr to use summarize_distribution_audit().")
  }

  dplyr::as_tibble(audit_report) |>
    dplyr::group_by(repo_name) |>
    dplyr::summarise(
      n_files = dplyr::n(),
      n_copy = sum(recommended_action == "Copy", na.rm = TRUE),
      n_skip = sum(recommended_action == "Skip", na.rm = TRUE),
      n_review_newer_dest = sum(compare_status == "different_destination_newer", na.rm = TRUE),
      n_review_different = sum(compare_status == "different_same_or_unknown_time", na.rm = TRUE),
      n_missing_source = sum(compare_status == "missing_source", na.rm = TRUE),
      n_missing_destination = sum(compare_status == "missing_destination", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_review_newer_dest), dplyr::desc(n_review_different), dplyr::desc(n_copy))
}

write_distribution_audit_excel <- function(
  audit_report,
  output_dir = ".",
  summary = NULL,
  prefix = "master_file_distribution_audit",
  date_prefix = format(Sys.Date(), "%Y_%m_%d"),
  freeze_panes = TRUE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Please install openxlsx to use write_distribution_audit_excel().")
  }

  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  wb <- openxlsx::createWorkbook()

  if (!is.null(summary)) {
    openxlsx::addWorksheet(wb, "summary")
    openxlsx::writeDataTable(wb, "summary", summary, withFilter = TRUE)
    if (freeze_panes) openxlsx::freezePane(wb, "summary", firstRow = TRUE)
    openxlsx::setColWidths(wb, "summary", cols = 1:ncol(summary), widths = "auto")
  }

  openxlsx::addWorksheet(wb, "detail")
  openxlsx::writeDataTable(wb, "detail", audit_report, withFilter = TRUE)
  if (freeze_panes) openxlsx::freezePane(wb, "detail", firstRow = TRUE)
  openxlsx::setColWidths(wb, "detail", cols = 1:ncol(audit_report), widths = "auto")

  out_file <- file.path(output_dir, paste0(date_prefix, "_", prefix, ".xlsx"))
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  out_file
}

distribute_project_files_safe <- function(
  root_dir = "~/R Working Directory",
  source_files_dir = "~/R Working Directory/Master File Distribution Code",
  files_to_copy = default_distribution_files("analysis"),
  exclude_dirs = c("Master File Distribution Code"),
  include_root = TRUE,
  include_hidden_files = TRUE,
  overwrite_mode = c("if_source_newer", "missing_only", "force"),
  protect_newer_destination = TRUE,
  backup_before_overwrite = TRUE,
  backup_suffix = format(Sys.time(), "%Y_%m_%d_%H%M%S"),
  dry_run = TRUE,
  verbose = TRUE
) {
  overwrite_mode <- match.arg(overwrite_mode)

  audit <- audit_distribution_conflicts(
    root_dir = root_dir,
    source_files_dir = source_files_dir,
    files_to_copy = files_to_copy,
    exclude_dirs = exclude_dirs,
    include_root = include_root,
    include_hidden_files = include_hidden_files,
    return_tibble = FALSE
  )

  choose_copy <- function(status) {
    if (overwrite_mode == "missing_only") {
      return(status == "missing_destination")
    }
    if (overwrite_mode == "if_source_newer") {
      return(status %in% c("missing_destination", "different_source_newer"))
    }
    if (overwrite_mode == "force") {
      return(status %in% c(
        "missing_destination",
        "different_source_newer",
        "different_destination_newer",
        "different_same_or_unknown_time"
      ))
    }
    FALSE
  }

  audit$will_copy <- vapply(audit$compare_status, choose_copy, logical(1))

  if (protect_newer_destination) {
    audit$will_copy[audit$compare_status == "different_destination_newer"] <- FALSE
  }

  audit$copy_result <- NA_character_
  audit$backup_path <- NA_character_

  if (dry_run) {
    if (verbose) {
      cat("Dry run only. No files copied.\n")
      cat("Files marked for copy:", sum(audit$will_copy, na.rm = TRUE), "\n")
      cat("Protected newer destination files:",
          sum(audit$compare_status == "different_destination_newer", na.rm = TRUE), "\n")
    }
    return(audit)
  }

  for (i in seq_len(nrow(audit))) {
    if (!isTRUE(audit$will_copy[i])) {
      audit$copy_result[i] <- "Skipped"
      next
    }

    src <- audit$source_path[i]
    dest <- audit$destination_path[i]

    if (!file.exists(src)) {
      audit$copy_result[i] <- "Failed: source missing"
      next
    }

    if (file.exists(dest) && backup_before_overwrite && audit$compare_status[i] != "missing_destination") {
      backup_path <- paste0(dest, ".bak_", backup_suffix)
      ok_backup <- file.copy(dest, backup_path, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
      if (ok_backup) {
        audit$backup_path[i] <- backup_path
      } else {
        audit$copy_result[i] <- "Failed: could not create backup"
        next
      }
    }

    ok_copy <- file.copy(src, dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    audit$copy_result[i] <- if (ok_copy) "Copied" else "Failed: copy unsuccessful"
  }

  if (verbose) {
    cat("Distribution complete.\n")
    cat("Copied:", sum(audit$copy_result == "Copied", na.rm = TRUE), "\n")
    cat("Skipped:", sum(audit$copy_result == "Skipped", na.rm = TRUE), "\n")
    cat("Failed:", sum(grepl("^Failed", audit$copy_result), na.rm = TRUE), "\n")
  }

  audit
}

print_distribution_help <- function() {
  cat(
"Suggested workflow:

1. Audit first
   audit <- audit_distribution_conflicts()

2. Summarize
   summary <- summarize_distribution_audit(audit)

3. Save to Excel
   write_distribution_audit_excel(audit, summary = summary)

4. Review any rows with:
   - compare_status == 'different_destination_newer'
   - compare_status == 'different_same_or_unknown_time'

5. Dry run the copy plan
   distribute_project_files_safe(dry_run = TRUE)

6. Copy only when ready
   distribute_project_files_safe(dry_run = FALSE)

Recommended defaults:
- overwrite_mode = 'if_source_newer'
- protect_newer_destination = TRUE
- backup_before_overwrite = TRUE

This setup is designed to avoid overwriting a newer local standard file with
an older master copy.
", sep = "")
}
