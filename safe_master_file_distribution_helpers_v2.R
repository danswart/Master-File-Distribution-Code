# safe_master_file_distribution_helpers_v2.R
# Current as of: 2026-03-16
#
# This is a new replacement helper file.
# It folds the 'find the most recent candidate source copy first' step into
# the distribution workflow.

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
    "safe_master_file_distribution_helpers_v2.R"
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

find_most_recent_shared_files <- function(
  root_dir = "~/R Working Directory",
  file_names = default_distribution_files("analysis"),
  exclude_dirs = c("Master File Distribution Code"),
  include_root = TRUE,
  include_hidden_files = TRUE,
  return_tibble = TRUE
) {
  root_dir <- normalizePath(path.expand(root_dir), winslash = "/", mustWork = TRUE)

  destinations <- list_project_directories_safe(
    root_dir = root_dir,
    exclude_dirs = exclude_dirs,
    include_root = include_root
  )

  if (!include_hidden_files) {
    file_names <- file_names[substr(file_names, 1, 1) != "."]
  }

  rows <- vector("list", length(destinations) * length(file_names))
  idx <- 1L

  for (dest_dir in destinations) {
    repo_name <- if (normalizePath(dest_dir, winslash = "/", mustWork = FALSE) == root_dir) {
      "R Working Directory"
    } else {
      basename(dest_dir)
    }

    for (fname in file_names) {
      fpath <- file.path(dest_dir, fname)

      if (file.exists(fpath)) {
        finfo <- file.info(fpath)

        rows[[idx]] <- data.frame(
          repo_name = repo_name,
          file_name = fname,
          path = normalizePath(fpath, winslash = "/", mustWork = FALSE),
          size_bytes = as.numeric(finfo$size),
          size_mb = round(as.numeric(finfo$size) / (1024^2), 4),
          modified_time = finfo$mtime,
          md5 = unname(tools::md5sum(fpath)),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1L
      }
    }
  }

  rows <- rows[seq_len(idx - 1L)]

  if (length(rows) == 0) {
    out <- data.frame(
      repo_name = character(),
      file_name = character(),
      path = character(),
      size_bytes = numeric(),
      size_mb = numeric(),
      modified_time = as.POSIXct(character()),
      md5 = character(),
      stringsAsFactors = FALSE
    )
    if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(list(all_matches = out, most_recent = out))
  }

  all_matches <- do.call(rbind, rows)

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install dplyr to use find_most_recent_shared_files().")
  }

  most_recent <- dplyr::as_tibble(all_matches) |>
    dplyr::arrange(file_name, dplyr::desc(modified_time), dplyr::desc(size_bytes)) |>
    dplyr::group_by(file_name) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  duplicate_newest <- dplyr::as_tibble(all_matches) |>
    dplyr::group_by(file_name) |>
    dplyr::mutate(max_time = max(modified_time, na.rm = TRUE)) |>
    dplyr::filter(modified_time == max_time) |>
    dplyr::summarise(n_at_latest_time = dplyr::n(), .groups = "drop")

  most_recent <- dplyr::left_join(most_recent, duplicate_newest, by = "file_name")

  if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
    all_matches <- tibble::as_tibble(all_matches)
  }

  list(
    all_matches = all_matches,
    most_recent = most_recent
  )
}

write_most_recent_shared_files_excel <- function(
  scan_result,
  output_dir = ".",
  prefix = "most_recent_shared_files",
  date_prefix = format(Sys.Date(), "%Y_%m_%d"),
  freeze_panes = TRUE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Please install openxlsx to use write_most_recent_shared_files_excel().")
  }

  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "most_recent")
  openxlsx::writeDataTable(wb, "most_recent", scan_result$most_recent, withFilter = TRUE)
  if (freeze_panes) openxlsx::freezePane(wb, "most_recent", firstRow = TRUE)
  openxlsx::setColWidths(wb, "most_recent", cols = 1:ncol(scan_result$most_recent), widths = "auto")

  openxlsx::addWorksheet(wb, "all_matches")
  openxlsx::writeDataTable(wb, "all_matches", scan_result$all_matches, withFilter = TRUE)
  if (freeze_panes) openxlsx::freezePane(wb, "all_matches", firstRow = TRUE)
  openxlsx::setColWidths(wb, "all_matches", cols = 1:ncol(scan_result$all_matches), widths = "auto")

  out_file <- file.path(output_dir, paste0(date_prefix, "_", prefix, ".xlsx"))
  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  out_file
}

promote_files_to_master <- function(
  selected_paths,
  source_files_dir = "~/R Working Directory/Master File Distribution Code",
  backup_existing = TRUE,
  backup_suffix = format(Sys.time(), "%Y_%m_%d_%H%M%S"),
  dry_run = TRUE,
  verbose = TRUE
) {
  source_files_dir <- path.expand(source_files_dir)
  if (!dir.exists(source_files_dir)) dir.create(source_files_dir, recursive = TRUE)

  selected_paths <- path.expand(selected_paths)

  result <- data.frame(
    selected_path = selected_paths,
    file_name = basename(selected_paths),
    master_path = file.path(source_files_dir, basename(selected_paths)),
    exists_selected = file.exists(selected_paths),
    exists_master = file.exists(file.path(source_files_dir, basename(selected_paths))),
    backup_path = NA_character_,
    copy_result = NA_character_,
    stringsAsFactors = FALSE
  )

  if (dry_run) {
    result$copy_result <- ifelse(result$exists_selected, "Would copy", "Missing selected path")
    if (verbose) cat("Dry run only. No files promoted to master.\n")
    return(result)
  }

  for (i in seq_len(nrow(result))) {
    src <- result$selected_path[i]
    dest <- result$master_path[i]

    if (!file.exists(src)) {
      result$copy_result[i] <- "Failed: selected path missing"
      next
    }

    if (file.exists(dest) && backup_existing) {
      backup_path <- paste0(dest, ".bak_", backup_suffix)
      ok_backup <- file.copy(dest, backup_path, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
      if (!ok_backup) {
        result$copy_result[i] <- "Failed: could not back up existing master file"
        next
      }
      result$backup_path[i] <- backup_path
    }

    ok_copy <- file.copy(src, dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    result$copy_result[i] <- if (ok_copy) "Copied to master" else "Failed: copy unsuccessful"
  }

  if (verbose) {
    cat("Promotion to master complete.\n")
    cat("Copied:", sum(result$copy_result == "Copied to master", na.rm = TRUE), "\n")
    cat("Failed:", sum(grepl("^Failed", result$copy_result), na.rm = TRUE), "\n")
  }

  result
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
      cat("Protected newer destination files:", sum(audit$compare_status == "different_destination_newer", na.rm = TRUE), "\n")
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

STEP 1. Find the most recent candidate source copy for each shared file
   scan <- find_most_recent_shared_files()

STEP 2. Save the scan to Excel and review it
   write_most_recent_shared_files_excel(scan)

STEP 3. Promote chosen files into Master File Distribution Code
   promote_files_to_master(selected_paths = scan$most_recent$path, dry_run = TRUE)

STEP 4. Audit distribution conflicts
   audit <- audit_distribution_conflicts()

STEP 5. Save the distribution audit to Excel
   summary <- summarize_distribution_audit(audit)
   write_distribution_audit_excel(audit, summary = summary)

STEP 6. Dry run the distribution plan
   distribute_project_files_safe(dry_run = TRUE)

STEP 7. Run actual distribution only when satisfied
   distribute_project_files_safe(dry_run = FALSE)

This helper is designed to reduce the risk that an outdated master file will overwrite a newer local standard file.
",
  sep = ""
  )
}
