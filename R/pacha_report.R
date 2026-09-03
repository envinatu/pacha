.pacha_report_quiet <- function(fn) {
  value <- NULL
  utils::capture.output(value <- fn())
  value
}

#' Generate a per-species Markdown report
#'
#' Concatenates the Markdown output of [pacha_sc_full_name_md()],
#' [common_names_pacha_md()], and [sustainable_uses_pacha_md()] into a single
#' Markdown-formatted string, with one block per species in `species`,
#' suitable for a `results = "asis"` knitr chunk in an automated report.
#'
#' The heading for each species always comes from
#' [pacha_sc_full_name_md()], which takes no argument beyond `species` and
#' resolves against its own dataset and cache, independently of
#' `language` and `source`; see `?pacha_sc_full_name_md`. `language` and
#' `source` are forwarded to both [common_names_pacha_md()] and
#' [sustainable_uses_pacha_md()]; `use` is forwarded only to
#' [sustainable_uses_pacha_md()]; `refresh` is forwarded to both
#' [common_names_pacha_md()] and [sustainable_uses_pacha_md()], but not to
#' [pacha_sc_full_name_md()], as scientific names rely on a separate cache
#' unaffected by this parameter.
#'
#' Each component is produced by capturing, not re-implementing, the
#' output of the corresponding `_md` accessor: its internal `cat()` call
#' is suppressed and its invisible return value is reused directly, so
#' the combined report is emitted exactly once regardless of the number
#' of species or components involved. As documented for the underlying
#' accessors, a component with no data for a given species contributes
#' nothing to that species' block: there is no "no data" placeholder or
#' empty heading, since each `_md` function returns `""` in that case.
#' [pacha_sc_full_name_md()] has no such empty case: it always renders a
#' heading, falling back to the requested name verbatim when resolution
#' fails.
#'
#' A structurally invalid element of `species` (missing, or lacking at
#' least a genus and a specific epithet) raises an error via the same
#' validation used by the underlying accessors; species preceding the
#' invalid one in the vector are not processed once that error is
#' raised.
#'
#' @param species Character vector of one or more scientific names, each
#'   containing at least a genus and a specific epithet.
#' @param language Optional output-language code or alias, forwarded to
#'   [common_names_pacha_md()] and [sustainable_uses_pacha_md()].
#' @param source Optional source override (`"api"`/`"web"` or
#'   `"coldp"`/`"local"`), forwarded to the same two functions. `NULL`
#'   uses each function's configured default.
#' @param use Optional sustainable-use category filter, forwarded to
#'   [sustainable_uses_pacha_md()]. `NULL` includes every category.
#' @param refresh Logical scalar. If `TRUE`, bypasses the cache of
#'   [common_names_pacha_md()] and [sustainable_uses_pacha_md()] and
#'   re-fetches those two components for every species.
#' @param print Logical scalar. If `TRUE` (the default), the combined
#'   Markdown is emitted via `cat()`, as required in a
#'   `results = "asis"` knitr chunk. If `FALSE`, the string is built and
#'   returned without being printed.
#'
#' @return A character scalar with the combined Markdown for every
#'   element of `species`, one block per species separated by a blank
#'   line. Returned invisibly when `print = TRUE`, visibly when
#'   `print = FALSE`.
#'
#' @examples
#' \dontrun{
#' # Inside a knitr chunk with `results = "asis"`:
#' pacha_report("Bidens andicola")
#' pacha_report(c("Bidens andicola", "Bomarea multiflora"), language = "en")
#' pacha_report("Bidens andicola", source = "coldp", use = "medicinal")
#' }
#' @export
pacha_report <- function(species, language = NULL, source = NULL, use = NULL,
                         refresh = FALSE, print = TRUE) {
  if (!is.character(species) || !length(species) || anyNA(species)) {
    stop("`species` must be one or more non-missing scientific names.", call. = FALSE)
  }
  if (!is.logical(print) || length(print) != 1L || is.na(print)) {
    stop("`print` must be TRUE or FALSE.", call. = FALSE)
  }

  blocks <- vapply(species, function(one_species) {
    heading <- .pacha_report_quiet(function() pacha_sc_full_name_md(one_species))
    names_block <- .pacha_report_quiet(function() {
      common_names_pacha_md(one_species, language = language, source = source, refresh = refresh)
    })
    uses_block <- .pacha_report_quiet(function() {
      sustainable_uses_pacha_md(one_species, use = use, language = language, source = source, refresh = refresh)
    })
    parts <- c(heading, names_block, uses_block)
    paste(parts[nzchar(parts)], collapse = "\n")
  }, character(1), USE.NAMES = FALSE)

  report <- paste(blocks, collapse = "\n\n")

  if (isTRUE(print)) {
    cat(report, "\n", sep = "")
    return(invisible(report))
  }
  report
}
