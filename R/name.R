.pacha_fn_config <- local({
  cfg <- new.env(parent = emptyenv())
  cfg$dataset  <- "3LXR"
  cfg$base_url <- "https://api.checklistbank.org"
  cfg
})

.pacha_fn_cache <- new.env(parent = emptyenv())

.pacha_fn_http_json <- function(url, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required by the pacha module's ChecklistBank adapter.", call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = "application/json")
  request <- httr2::req_user_agent(request, "pacha R module")
  request <- httr2::req_timeout(request, seconds = 30)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  request <- httr2::req_retry(request, max_tries = 3, is_transient = function(response) {
    httr2::resp_status(response) %in% c(429L, 500L, 502L, 503L, 504L)
  })
  query <- query[!vapply(query, is.null, logical(1))]
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  response <- tryCatch(httr2::req_perform(request), error = function(e) NULL)
  if (is.null(response)) return(NULL)
  status <- httr2::resp_status(response)
  if (identical(status, 404L)) return(NULL)
  if (status < 200L || status >= 300L) return(NULL)
  tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e) NULL)
}

.pacha_fn_checklist_get <- function(..., query = list()) {
  path <- vapply(c("dataset", .pacha_fn_config$dataset, ...), .pacha_url_encode, character(1))
  .pacha_fn_http_json(paste(c(.pacha_fn_config$base_url, path), collapse = "/"), query)
}

.pacha_fn_rows <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (!is.list(x)) return(list())
  if (!is.null(x$result)) x <- x$result
  if (is.null(x) || !length(x)) return(list())
  if (!is.list(x)) return(list())
  fields <- c("id", "name", "scientificName", "rank", "usage", "property", "value", "language")
  if (!is.null(names(x)) && any(names(x) %in% fields)) return(list(x))
  if (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))) return(x)
  list(x)
}

.pacha_fn_first_field <- function(x, fields, default = NA_character_) {
  if (!is.list(x) || is.null(names(x))) return(default)
  lower_names <- tolower(names(x))
  for (field in fields) {
    index <- which(lower_names == tolower(field))[1L]
    if (!is.na(index)) {
      value <- .pacha_scalar(x[[index]], NA_character_)
      if (!is.na(value)) return(value)
    }
  }
  default
}

.pacha_fn_search_usage <- function(species) {
  candidates <- .pacha_fn_rows(.pacha_fn_checklist_get("nameusage", "search", query = list(q = species, limit = 100)))
  if (!length(candidates)) return(NULL)
  scientific <- vapply(candidates, .pacha_fn_first_field, character(1),
                       fields = c("scientificName", "name", "label"))
  status <- vapply(candidates, .pacha_fn_first_field, character(1), fields = "status")
  exact <- which(tolower(scientific) == tolower(species))
  accepted_exact <- exact[tolower(status[exact]) == "accepted"]
  selected <- if (length(accepted_exact)) accepted_exact[1L] else if (length(exact)) {
    exact[1L]
  } else {
    accepted <- which(tolower(status) == "accepted")
    if (length(accepted)) accepted[1L] else 1L
  }
  candidates[[selected]]
}

.pacha_fn_resolve_usage <- function(species) {
  match <- .pacha_fn_checklist_get("match", "nameusage", query = list(q = species, verbose = "false"))
  usage <- if (is.list(match)) match$usage else NULL
  if (is.null(usage) || !is.list(usage)) usage <- .pacha_fn_search_usage(species)
  if (is.null(usage)) return(NULL)
  id <- .pacha_fn_first_field(usage, c("id", "usage.id"))
  if (is.na(id)) return(NULL)
  list(
    id = id,
    name = .pacha_fn_first_field(usage, c("scientificName", "name", "label")),
    authorship = .pacha_fn_first_field(usage, c("authorship", "author"))
  )
}

.pacha_fn_fetch_record <- function(species) {
  resolution <- tryCatch(.pacha_fn_resolve_usage(species), error = function(e) NULL)
  if (is.null(resolution)) {
    return(list(scientific_name = species, authorship = NA_character_, retrieved_at = Sys.time()))
  }
  list(
    scientific_name = .pacha_scalar(resolution$name, species),
    authorship = .pacha_scalar(resolution$authorship, NA_character_),
    retrieved_at = Sys.time()
  )
}

.pacha_fn_record <- function(species, refresh = FALSE) {
  species <- .pacha_validate_species(species)
  key <- tolower(species)
  if (isTRUE(refresh) || !exists(key, envir = .pacha_fn_cache, inherits = FALSE)) {
    assign(key, .pacha_fn_fetch_record(species), envir = .pacha_fn_cache)
  }
  get(key, envir = .pacha_fn_cache, inherits = FALSE)
}

.pacha_fn_markdown_escape_text <- function(x) {
  if (is.na(x)) return(x)
  escaped <- .pacha_markdown_escape(x)
  gsub("\\\\([()])", "\\1", escaped)
}

#' Full scientific name of a species
#'
#' Resolves \code{species} against ChecklistBank's Catalogue of Life
#' dataset (\code{"3LXR"}) via
#' \code{match/nameusage}, falling back to \code{nameusage/search}
#' (\code{limit = 100}) when no direct match is returned. Among search
#' candidates the best match is chosen by preference: an exact
#' case-insensitive name match with status \code{"accepted"}; else any
#' exact name match; else the first \code{"accepted"} candidate; else the
#' first candidate returned. \code{pacha_sc_full_name} returns the name as
#' plain text; \code{pacha_sc_full_name_md} renders it as a Markdown
#' heading (see "Functions"). This resolution always uses dataset
#' \code{"3LXR"} and ignores \code{pacha_configure()}.
#'
#' Results are cached in memory per lower-cased, whitespace-normalised
#' species string, shared by both functions, so repeat lookups skip the
#' HTTP call.
#'
#' Failures never raise an error: network/HTTP errors, timeouts, malformed
#' JSON, no match, or a missing \pkg{httr2} dependency all fall back to
#' \code{species} verbatim with no authorship -- the same output produced
#' for a name that resolved but has no authorship of its own. \code{stop()}
#' is only reached for a structurally invalid \code{species}.
#'
#' @param species Character scalar: a validated binomial name (non-NA,
#'   non-empty, at least two whitespace-separated tokens). Internal
#'   whitespace is collapsed and the string trimmed before use.
#'
#' @return Character scalar: \code{"Genus species Authorship"} if
#'   authorship resolved, otherwise \code{"Genus species"} (or the
#'   original \code{species} if resolution failed). See "Functions" for
#'   the Markdown variant.
#'
#' @examples
#' \dontrun{
#' pacha_sc_full_name("Bidens andicola")
#' pacha_sc_full_name_md("Bidens andicola")
#' }
#'
#' @export
pacha_sc_full_name <- function(species) {
  record <- .pacha_fn_record(species)
  if (is.na(record$authorship)) {
    record$scientific_name
  } else {
    paste(record$scientific_name, record$authorship)
  }
}

#' @describeIn pacha_sc_full_name Same resolution, rendered as a
#'   level-one Markdown heading with the name italicised and the
#'   authorship appended as plain text (e.g. \code{"# *Bidens andicola*
#'   Kunth"}). Markdown-sensitive characters in both parts are
#'   backslash-escaped to keep the output well-formed. Printed via
#'   \code{cat()} and returned invisibly.
#' @export
pacha_sc_full_name_md <- function(species) {
  record <- .pacha_fn_record(species)
  markdown <- paste0(
    "# *", .pacha_fn_markdown_escape_text(record$scientific_name), "*",
    if (!is.na(record$authorship)) paste0(" ", .pacha_fn_markdown_escape_text(record$authorship)) else ""
  )
  .pacha_emit_md(markdown)
}
