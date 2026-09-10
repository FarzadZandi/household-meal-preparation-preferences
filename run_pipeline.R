required <- c("targets")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing, collapse = ", "),
    ". Run renv::restore() first.",
    call. = FALSE
  )
}

targets::tar_make()

