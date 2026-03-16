# repo_file_audit_helpers_with_pdf_and_excel.R
# Current as of: 2026-03-16
#
# Purpose:
#   Scan a root folder (for example ~/R Working Directory) for .csv, .xlsx,
#   and .pdf files, report file sizes, infer repo names, and optionally check
#   whether each file is currently tracked by Git.
#
# Practical rules used here:
#   CSV:
#     < 10 MB   -> usually OK to track
#     10-50 MB  -> review before tracking
#     50-100 MB -> keep local or use Git LFS
#     >=100 MB  -> do NOT track in normal Git
#
#   XLSX:
#     < 5 MB    -> usually OK to track
#     5-50 MB   -> review before tracking
#     50-100 MB -> keep local or use Git LFS
#     >=100 MB  -> do NOT track in normal Git
#
#   PDF:
#     < 5 MB    -> review carefully; may be OK if truly important
#     5-25 MB   -> review carefully; likely keep local
#     25-50 MB  -> strong presumption to keep local
#     50-100 MB -> keep local or use Git LFS
#     >=100 MB  -> do NOT track in normal Git

repo_file_audit <- function(
  root_dir = "~/R Working Directory",
  include_extensions = c("csv", "xlsx", "pdf"),
  csv_track_mb = 10,
  xlsx_track_mb = 5,
  pdf_track_mb = 5,
  pdf_review_mb = 25,
  review_mb = 50,
  hard_limit_mb = 100,
  check_git_tracking = TRUE,
  drop_git_dirs = TRUE,
  return_tibble = TRUE
) {
  stopifnot(length(include_extensions) >= 1)

  root_dir <- path.expand(root_dir)

  if (!dir.exists(root_dir)) {
    stop("Directory does not exist: ", root_dir)
  }

  all_files <- list.files(
    path = root_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE,
    include.dirs = FALSE
  )

  all_files <- normalizePath(all_files, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)

  if (drop_git_dirs) {
    all_files <- all_files[!grepl("/\\.git/", all_files)]
  }

  ext <- tolower(tools::file_ext(all_files))
  keep <- ext %in% tolower(include_extensions)
  target_files <- all_files[keep]

  if (length(target_files) == 0) {
    out <- data.frame(
      repo_name = character(),
      filename = character(),
      extension = character(),
      file_type_group = character(),
      size_mb = numeric(),
      size_kb = numeric(),
      size_bytes = numeric(),
      recommendation = character(),
      git_tracked = logical(),
      folder = character(),
      path = character(),
      stringsAsFactors = FALSE
    )
    if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
      out <- tibble::as_tibble(out)
    }
    return(out)
  }

  info <- file.info(target_files)
  out <- data.frame(
    path = rownames(info),
    extension = tolower(tools::file_ext(rownames(info))),
    size_bytes = info$size,
    stringsAsFactors = FALSE
  )

  out$size_kb <- round(out$size_bytes / 1024, 2)
  out$size_mb <- round(out$size_bytes / (1024^2), 2)
  out$filename <- basename(out$path)
  out$folder <- dirname(out$path)

  rel_path <- sub(paste0("^", root_norm, "/?"), "", out$path)
  out$repo_name <- sub("/.*$", "", rel_path)

  out$file_type_group <- ifelse(
    out$extension == "csv", "data_text",
    ifelse(out$extension == "xlsx", "data_binary",
           ifelse(out$extension == "pdf", "document_pdf", "other"))
  )

  out$recommendation <- NA_character_

  is_csv  <- out$extension == "csv"
  is_xlsx <- out$extension == "xlsx"
  is_pdf  <- out$extension == "pdf"

  out$recommendation[is_csv & out$size_mb < csv_track_mb] <- "Track normally"
  out$recommendation[is_csv & out$size_mb >= csv_track_mb & out$size_mb < review_mb] <- "Review before tracking"
  out$recommendation[is_csv & out$size_mb >= review_mb & out$size_mb < hard_limit_mb] <- "Keep local or use Git LFS"
  out$recommendation[is_csv & out$size_mb >= hard_limit_mb] <- "Do NOT track in normal Git"

  out$recommendation[is_xlsx & out$size_mb < xlsx_track_mb] <- "Track normally"
  out$recommendation[is_xlsx & out$size_mb >= xlsx_track_mb & out$size_mb < review_mb] <- "Review before tracking"
  out$recommendation[is_xlsx & out$size_mb >= review_mb & out$size_mb < hard_limit_mb] <- "Keep local or use Git LFS"
  out$recommendation[is_xlsx & out$size_mb >= hard_limit_mb] <- "Do NOT track in normal Git"

  out$recommendation[is_pdf & out$size_mb < pdf_track_mb] <- "Review carefully; maybe OK to track"
  out$recommendation[is_pdf & out$size_mb >= pdf_track_mb & out$size_mb < pdf_review_mb] <- "Review carefully; likely keep local"
  out$recommendation[is_pdf & out$size_mb >= pdf_review_mb & out$size_mb < review_mb] <- "Strong presumption to keep local"
  out$recommendation[is_pdf & out$size_mb >= review_mb & out$size_mb < hard_limit_mb] <- "Keep local or use Git LFS"
  out$recommendation[is_pdf & out$size_mb >= hard_limit_mb] <- "Do NOT track in normal Git"

  out$pdf_rendered_flag <- ifelse(
    is_pdf &
      grepl(
        "(analysis|report|rendered|output|appendix|book|budget|final|with_buttons|refactored)",
        tolower(out$path)
      ),
    TRUE,
    FALSE
  )

  out$git_tracked <- NA

  if (check_git_tracking) {
    split_paths <- split(out$path, out$repo_name)

    tracked_lookup <- lapply(names(split_paths), function(repo) {
      repo_dir <- file.path(root_norm, repo)

      if (!dir.exists(file.path(repo_dir, ".git"))) {
        return(data.frame(path = character(), git_tracked = logical(), stringsAsFactors = FALSE))
      }

      git_files <- tryCatch(
        system2(
          command = "git",
          args = c("-C", repo_dir, "ls-files"),
          stdout = TRUE,
          stderr = FALSE
        ),
        error = function(e) character()
      )

      if (length(git_files) == 0) {
        return(data.frame(path = character(), git_tracked = logical(), stringsAsFactors = FALSE))
      }

      git_files_full <- normalizePath(file.path(repo_dir, git_files), winslash = "/", mustWork = FALSE)

      data.frame(
        path = git_files_full,
        git_tracked = TRUE,
        stringsAsFactors = FALSE
      )
    })

    tracked_lookup <- do.call(rbind, tracked_lookup)

    if (nrow(tracked_lookup) > 0) {
      out <- merge(
        out,
        tracked_lookup,
        by = "path",
        all.x = TRUE,
        suffixes = c("", ".y")
      )
      out$git_tracked <- ifelse(is.na(out$git_tracked.y), FALSE, out$git_tracked.y)
      out$git_tracked.y <- NULL
    } else {
      out$git_tracked <- FALSE
    }
  }

  out <- out[, c(
    "repo_name", "filename", "extension", "file_type_group", "size_mb", "size_kb",
    "size_bytes", "recommendation", "pdf_rendered_flag", "git_tracked", "folder", "path"
  )]

  out <- out[order(-out$size_mb, out$repo_name, out$filename), ]
  rownames(out) <- NULL

  if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
    out <- tibble::as_tibble(out)
  }

  out
}

summarize_repo_file_audit <- function(report) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install dplyr to use summarize_repo_file_audit().")
  }

  dplyr::as_tibble(report) |>
    dplyr::group_by(repo_name) |>
    dplyr::summarise(
      n_files = dplyr::n(),
      n_csv = sum(extension == "csv", na.rm = TRUE),
      n_xlsx = sum(extension == "xlsx", na.rm = TRUE),
      n_pdf = sum(extension == "pdf", na.rm = TRUE),
      n_pdf_rendered = sum(pdf_rendered_flag %in% TRUE, na.rm = TRUE),
      n_tracked = sum(git_tracked %in% TRUE, na.rm = TRUE),
      largest_mb = max(size_mb, na.rm = TRUE),
      n_review = sum(grepl("Review", recommendation), na.rm = TRUE),
      n_keep_local = sum(grepl("keep local|Keep local", recommendation), na.rm = TRUE),
      n_blocked = sum(recommendation == "Do NOT track in normal Git", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_blocked), dplyr::desc(n_keep_local), dplyr::desc(largest_mb))
}

flag_repo_file_actions <- function(report) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install dplyr to use flag_repo_file_actions().")
  }

  dplyr::as_tibble(report) |>
    dplyr::mutate(
      action = dplyr::case_when(
        recommendation == "Do NOT track in normal Git" & git_tracked ~ "URGENT: remove from Git tracking/history",
        recommendation == "Do NOT track in normal Git" & !git_tracked ~ "Leave local; do not add",
        recommendation %in% c("Keep local or use Git LFS", "Strong presumption to keep local") & git_tracked ~ "Review now: probably remove from normal Git",
        recommendation %in% c("Keep local or use Git LFS", "Strong presumption to keep local") & !git_tracked ~ "Keep local or use Git LFS",
        recommendation == "Review carefully; likely keep local" & git_tracked ~ "Tracked but likely should be local only",
        recommendation == "Review carefully; likely keep local" & !git_tracked ~ "Probably leave local",
        recommendation == "Review carefully; maybe OK to track" & git_tracked ~ "Tracked; keep only if truly important",
        recommendation == "Review carefully; maybe OK to track" & !git_tracked ~ "Add only if truly important",
        recommendation == "Review before tracking" & git_tracked ~ "Tracked but review whether it belongs in repo",
        recommendation == "Review before tracking" & !git_tracked ~ "Review before adding",
        recommendation == "Track normally" & git_tracked ~ "OK as tracked",
        recommendation == "Track normally" & !git_tracked ~ "OK to add if needed",
        TRUE ~ "Review manually"
      )
    ) |>
    dplyr::arrange(dplyr::desc(size_mb))
}

write_repo_file_audit_reports <- function(
  report,
  output_dir = ".",
  summary = NULL,
  actions = NULL,
  prefix = "repo_file_audit"
) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Please install readr to use write_repo_file_audit_reports().")
  }

  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  report_path <- file.path(output_dir, paste0(prefix, "_detail.csv"))
  readr::write_csv(report, report_path)

  out <- list(detail = report_path)

  if (!is.null(summary)) {
    summary_path <- file.path(output_dir, paste0(prefix, "_summary.csv"))
    readr::write_csv(summary, summary_path)
    out$summary <- summary_path
  }

  if (!is.null(actions)) {
    actions_path <- file.path(output_dir, paste0(prefix, "_actions.csv"))
    readr::write_csv(actions, actions_path)
    out$actions <- actions_path
  }

  out
}

write_repo_file_audit_excel <- function(
  report,
  output_dir = ".",
  summary = NULL,
  actions = NULL,
  prefix = "repo_large_file_audit",
  date_prefix = format(Sys.Date(), "%Y_%m_%d"),
  freeze_panes = TRUE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Please install openxlsx to use write_repo_file_audit_excel().")
  }

  output_dir <- path.expand(output_dir)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  wb <- openxlsx::createWorkbook()

  if (!is.null(summary)) {
    openxlsx::addWorksheet(wb, "summary")
    openxlsx::writeDataTable(wb, "summary", summary, withFilter = TRUE)
    if (freeze_panes) openxlsx::freezePane(wb, "summary", firstRow = TRUE)
    openxlsx::setColWidths(wb, "summary", cols = 1:ncol(summary), widths = "auto")
  }

  if (!is.null(actions)) {
    openxlsx::addWorksheet(wb, "actions")
    openxlsx::writeDataTable(wb, "actions", actions, withFilter = TRUE)
    if (freeze_panes) openxlsx::freezePane(wb, "actions", firstRow = TRUE)
    openxlsx::setColWidths(wb, "actions", cols = 1:ncol(actions), widths = "auto")
  }

  openxlsx::addWorksheet(wb, "detail")
  openxlsx::writeDataTable(wb, "detail", report, withFilter = TRUE)
  if (freeze_panes) openxlsx::freezePane(wb, "detail", firstRow = TRUE)
  openxlsx::setColWidths(wb, "detail", cols = 1:ncol(report), widths = "auto")

  out_file <- file.path(
    output_dir,
    paste0(date_prefix, "_", prefix, ".xlsx")
  )

  openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
  out_file
}

print_repo_file_audit_help <- function() {
  cat(
"Suggested workflow:

1. Run the scan
   report <- repo_file_audit()

2. Summarize by repo
   summary <- summarize_repo_file_audit(report)

3. Add suggested actions
   actions <- flag_repo_file_actions(report)

4. View the biggest problems first
   print(summary, n = 100)
   print(actions, n = 200)

5. Save CSV reports
   write_repo_file_audit_reports(report, summary = summary, actions = actions)

6. Save one Excel workbook
   write_repo_file_audit_excel(report, summary = summary, actions = actions)

Notes:
- root_dir defaults to ~/R Working Directory
- git_tracked = TRUE means the file is currently tracked by Git
- pdf_rendered_flag is a simple filename/path heuristic, not a guarantee
- recommendation is based on size thresholds
- action adds a plain-English next step
",
  sep = ""
  )
}
