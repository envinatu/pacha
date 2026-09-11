.pacha_state <- local({
  state <- new.env(parent = emptyenv())
  state$config <- new.env(parent = emptyenv())
  state$config$dataset <- "313479"
  state$config$base_url <- "https://api.checklistbank.org"
  state$config$source_name <- "Ecuador ChecklistBank"
  state$config$source <- "api"
  state$config$language <- "es"
  state$config$fetcher <- NULL
  state$config$use_exclude_pattern <- "habito|h\u00e1bito|habit|etnia|ethnic"
  state$config$label_overrides <- list()
  state$config$coldp_zip_file <- NULL
  state$config$coldp_country <- "EC"
  state$config$timeout <- 30
  state$api_cache <- new.env(parent = emptyenv())
  state$coldp_cache <- new.env(parent = emptyenv())
  state$coldp_tables <- new.env(parent = emptyenv())
  state$language_cache <- new.env(parent = emptyenv())
  state$tracker <- new.env(parent = emptyenv())
  state$tracker$current <- NULL
  state
})

.pacha_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x) & tolower(x) != "null"]
  if (length(x)) x[[1L]] else default
}

.pacha_text <- function(x, argument) {
  value <- .pacha_scalar(x)
  if (is.na(value)) stop(sprintf("`%s` must be a non-empty character string.", argument), call. = FALSE)
  value
}

.pacha_validate_species <- function(species) {
  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    stop("`species` must be one non-missing scientific name.", call. = FALSE)
  }
  species <- gsub("\\s+", " ", trimws(species))
  if (!nzchar(species) || length(strsplit(species, " ", fixed = TRUE)[[1L]]) < 2L) {
    stop("`species` must include at least a genus and a specific epithet.", call. = FALSE)
  }
  species
}

.pacha_resolve_source <- function(source = NULL) {
  if (is.null(source)) return(.pacha_state$config$source)
  source <- tolower(.pacha_text(source, "source"))
  aliases <- c(
    api = "api", web = "api", checklistbank = "api", remote = "api",
    coldp = "coldp", local = "coldp", local_coldp = "coldp", archive = "coldp"
  )
  if (!(source %in% names(aliases))) {
    stop("`source` must be one of: 'api', 'web', 'coldp', or 'local'.", call. = FALSE)
  }
  unname(aliases[[source]])
}

.pacha_language_dir <- function() {
  installed <- tryCatch(system.file("lang", package = "pacha"), error = function(e) "")
  if (nzchar(installed) && dir.exists(installed)) return(installed)
  development <- file.path("inst", "lang")
  if (dir.exists(development)) return(development)
  stop("The package language directory 'inst/lang' could not be located.", call. = FALSE)
}

.pacha_language_files <- function() {
  key <- ".files"
  cache <- .pacha_state$language_cache
  if (!exists(key, cache, inherits = FALSE)) {
    files <- list.files(.pacha_language_dir(), pattern = "\\.ya?ml$", full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) stop("No YAML language dictionaries were found in 'inst/lang'.", call. = FALSE)
    names(files) <- tolower(tools::file_path_sans_ext(basename(files)))
    assign(key, files, envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

.pacha_language_data <- function(language) {
  key <- paste0("language:", language)
  cache <- .pacha_state$language_cache
  if (!exists(key, cache, inherits = FALSE)) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Package 'yaml' is required to read language dictionaries.", call. = FALSE)
    }
    files <- .pacha_language_files()
    if (!(language %in% names(files))) {
      stop(sprintf("No language dictionary exists for '%s'.", language), call. = FALSE)
    }
    dictionary <- tryCatch(yaml::read_yaml(files[[language]]), error = function(e) NULL)
    if (is.null(dictionary) || !is.list(dictionary)) {
      stop(sprintf("Could not parse language dictionary '%s'.", files[[language]]), call. = FALSE)
    }
    assign(key, dictionary, envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

.pacha_language_aliases <- function() {
  key <- ".aliases"
  cache <- .pacha_state$language_cache
  if (!exists(key, cache, inherits = FALSE)) {
    aliases <- character()
    for (code in names(.pacha_language_files())) {
      dictionary <- .pacha_language_data(code)
      candidates <- c(code, unlist(dictionary$aliases, recursive = TRUE, use.names = FALSE))
      candidates <- tolower(trimws(as.character(candidates)))
      candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
      for (candidate in candidates) if (!(candidate %in% names(aliases))) aliases[[candidate]] <- code
    }
    aliases <- c(aliases, spanish = aliases[["es"]], english = aliases[["en"]])
    aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
    assign(key, aliases, envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

.pacha_resolve_language <- function(language = NULL) {
  if (is.null(language)) return(.pacha_state$config$language)
  language <- tolower(.pacha_text(language, "language"))
  aliases <- .pacha_language_aliases()
  if (!(language %in% names(aliases))) {
    stop(sprintf("Unsupported `language`: '%s'.", language), call. = FALSE)
  }
  unname(aliases[[language]])
}

.pacha_label <- function(key, language = NULL, required = FALSE) {
  language <- .pacha_resolve_language(language)
  dictionary <- .pacha_language_data(language)
  dictionary_key <- if (identical(key, "common_names")) "common_names_pacha" else key
  value <- .pacha_scalar(dictionary$labels_data[[dictionary_key]])
  override <- .pacha_state$config$label_overrides[[language]][[key]]
  override <- .pacha_scalar(override)
  if (!is.na(override)) value <- override
  if (is.na(value)) {
    stop(sprintf("No '%s' label is defined in the '%s' language dictionary.", key, language), call. = FALSE)
  }
  value
}

.pacha_set_labels <- function(labels) {
  if (!is.list(labels) || is.null(names(labels)) || any(!nzchar(names(labels)))) {
    stop("`labels` must be a named list of named character vectors.", call. = FALSE)
  }
  for (language in names(labels)) {
    values <- labels[[language]]
    if (is.list(values)) values <- unlist(values, recursive = TRUE, use.names = TRUE)
    if (!is.character(values) || is.null(names(values)) || any(!nzchar(names(values)))) {
      stop("Each `labels` element must be a named character vector.", call. = FALSE)
    }
    language <- .pacha_resolve_language(language)
    current <- .pacha_state$config$label_overrides[[language]]
    if (is.null(current)) current <- character()
    current[names(values)] <- as.character(values)
    .pacha_state$config$label_overrides[[language]] <- current
  }
}

.pacha_snapshot <- function() as.list(.pacha_state$config)

.pacha_clear_environments <- function() {
  .pacha_state$api_cache <- new.env(parent = emptyenv())
  .pacha_state$coldp_cache <- new.env(parent = emptyenv())
  .pacha_state$coldp_tables <- new.env(parent = emptyenv())
  invisible(NULL)
}

#' Configure the pacha data module
#'
#' Gets or updates the configuration shared by every public accessor in this
#' module. The accessors are source-agnostic: they query any
#' ChecklistBank-compatible web API, or read any compatible local Catalogue
#' of Life Data Package (ColDP) archive, provided the source exposes name
#' usages and, where relevant, vernacular names, taxon properties, and
#' distributions. The defaults set here -- the ChecklistBank web API as the
#' active `source`, dataset `"313479"` (the Ecuadorian checklist of plants
#' susceptible to sustainable use), and `coldp_country = "EC"` -- identify the
#' dataset used in this file's `\dontrun` examples; any other ChecklistBank
#' dataset or compatible ColDP archive can be configured in its place without
#' modifying the module itself.
#'
#' `fetcher` preserves the existing extension contract for the web source. It
#' receives `(species, config)` and must return a list with optional
#' `common_names`, `sustainable_uses`, `indexation_urls`, `establishment`,
#' `threat_status`, `transport_error`, and `transport_messages` elements. A
#' custom fetcher applies only when `source = "api"`; `source = "coldp"`
#' always reads the archive. For example:
#'
#' ```r
#' my_fetcher <- function(species, config) {
#'   list(
#'     common_names = list(es = c("nombre comun")),
#'     sustainable_uses = list(Medicinal = c("uso medicinal registrado")),
#'     indexation_urls = c(source = "https://example.org/taxon/123"),
#'     establishment = "native",
#'     threat_status = "LC",
#'     transport_error = FALSE,
#'     transport_messages = character()
#'   )
#' }
#' pacha_configure(fetcher = my_fetcher)
#' ```
#'
#' @param dataset Character scalar. ChecklistBank dataset identifier used by
#'   the default API adapter.
#' @param base_url Character scalar. HTTP(S) base URL of the ChecklistBank API.
#' @param source_name Character scalar identifying the web source in
#'   connection warnings.
#' @param language Character scalar. A language-dictionary file stem or one
#'   of its aliases. Controls labels and presentation, not the source data.
#' @param fetcher Optional custom web adapter. Pass `NULL` explicitly to
#'   restore the built-in ChecklistBank adapter; omit this argument to leave
#'   the current adapter unchanged.
#' @param use_exclude_pattern Character scalar regular expression identifying
#'   taxon-property names that must not be treated as sustainable-use classes.
#' @param labels Optional named list of named character vectors overriding
#'   the default labels by language.
#' @param source Default source: `"api"`/`"web"` or `"coldp"`/`"local"`.
#' @param coldp_zip_file Path to a local ColDP ZIP archive. It must contain
#'   `NameUsage.tsv`; `VernacularName.tsv`, `TaxonProperty.tsv`, and
#'   `Distribution.tsv` are read when present. `NULL` leaves the configured
#'   path unchanged. The `pacha_COLDP_ZIP` environment variable is used lazily
#'   when no path is set.
#' @param coldp_country ISO country code used to select country-specific
#'   records from the local ColDP archive: vernacular names in
#'   `VernacularName.tsv`, and the preferred distribution row in
#'   `Distribution.tsv` for establishment and threat status. Defaults to
#'   `"EC"`.
#' @param timeout Positive numeric HTTP timeout, in seconds, for web requests.
#'
#' @return A named configuration list, returned visibly when called with no
#'   arguments, and invisibly after an update.
#'
#' @examples
#' \dontrun{
#' pacha_configure()
#' pacha_configure(source = "coldp", coldp_zip_file = "C:/data/ColDP.zip")
#' pacha_configure(source = "api", dataset = "313479", language = "es")
#' }
#' @export
pacha_configure <- function(dataset = NULL, base_url = NULL, source_name = NULL,
                            language = NULL, fetcher, use_exclude_pattern = NULL,
                            labels = NULL, source = NULL, coldp_zip_file = NULL,
                            coldp_country = NULL, timeout = NULL) {
  query_only <- missing(fetcher) && is.null(dataset) && is.null(base_url) &&
    is.null(source_name) && is.null(language) && is.null(use_exclude_pattern) &&
    is.null(labels) && is.null(source) && is.null(coldp_zip_file) &&
    is.null(coldp_country) && is.null(timeout)
  changed <- FALSE
  config <- .pacha_state$config
  if (!is.null(dataset)) {
    dataset <- .pacha_text(dataset, "dataset")
    changed <- changed || !identical(config$dataset, dataset)
    config$dataset <- dataset
  }
  if (!is.null(base_url)) {
    base_url <- sub("/+$", "", .pacha_text(base_url, "base_url"))
    if (!grepl("^https?://", base_url, ignore.case = TRUE)) {
      stop("`base_url` must start with http:// or https://.", call. = FALSE)
    }
    changed <- changed || !identical(config$base_url, base_url)
    config$base_url <- base_url
  }
  if (!is.null(source_name)) config$source_name <- .pacha_text(source_name, "source_name")
  if (!is.null(language)) config$language <- .pacha_resolve_language(language)
  if (!is.null(labels)) .pacha_set_labels(labels)
  if (!is.null(source)) config$source <- .pacha_resolve_source(source)
  if (!is.null(use_exclude_pattern)) {
    use_exclude_pattern <- .pacha_text(use_exclude_pattern, "use_exclude_pattern")
    valid <- tryCatch({ grepl(use_exclude_pattern, "test", perl = TRUE); TRUE }, error = function(e) FALSE)
    if (!valid) stop("`use_exclude_pattern` is not a valid regular expression.", call. = FALSE)
    changed <- changed || !identical(config$use_exclude_pattern, use_exclude_pattern)
    config$use_exclude_pattern <- use_exclude_pattern
  }
  if (!is.null(coldp_zip_file)) {
    coldp_zip_file <- path.expand(.pacha_text(coldp_zip_file, "coldp_zip_file"))
    if (!file.exists(coldp_zip_file)) {
      stop(sprintf("ColDP archive not found at '%s'.", coldp_zip_file), call. = FALSE)
    }
    changed <- changed || !identical(config$coldp_zip_file, coldp_zip_file)
    config$coldp_zip_file <- coldp_zip_file
  }
  if (!is.null(coldp_country)) {
    coldp_country <- toupper(.pacha_text(coldp_country, "coldp_country"))
    changed <- changed || !identical(config$coldp_country, coldp_country)
    config$coldp_country <- coldp_country
  }
  if (!is.null(timeout)) {
    timeout <- suppressWarnings(as.numeric(timeout))
    if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
      stop("`timeout` must be one positive number of seconds.", call. = FALSE)
    }
    changed <- changed || !identical(config$timeout, timeout)
    config$timeout <- timeout
  }
  if (!missing(fetcher)) {
    if (!is.null(fetcher) && !is.function(fetcher)) {
      stop("`fetcher` must be NULL or a function with arguments `(species, config)`.", call. = FALSE)
    }
    changed <- changed || !identical(config$fetcher, fetcher)
    config$fetcher <- fetcher
  }
  if (changed) .pacha_clear_environments()
  configuration <- .pacha_snapshot()
  if (query_only) configuration else invisible(configuration)
}

#' Clear cached source records
#'
#' Removes cached records for both sources. With `species = NULL`, also
#' discards the cached ColDP tables, so subsequent local queries reread the
#' ZIP archive. Supplying a species removes only that species from the API
#' and ColDP record caches.
#'
#' @param species Optional scientific name containing at least a genus and
#'   specific epithet. `NULL` clears every record and local-table cache.
#'
#' @return Invisibly, `NULL`.
#'
#' @examples
#' pacha_clear_cache()
#' pacha_clear_cache("Bidens andicola")
#' @export
pacha_clear_cache <- function(species = NULL) {
  if (is.null(species)) return(.pacha_clear_environments())
  species <- .pacha_validate_species(species)
  key <- tolower(species)
  for (cache in list(.pacha_state$api_cache, .pacha_state$coldp_cache)) {
    if (exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
  }
  invisible(NULL)
}

.pacha_with_tracker <- function(fn) {
  state <- new.env(parent = emptyenv())
  state$failed <- FALSE
  state$messages <- character()
  previous <- .pacha_state$tracker$current
  .pacha_state$tracker$current <- state
  on.exit(.pacha_state$tracker$current <- previous, add = TRUE)
  list(value = fn(), failed = state$failed, messages = state$messages)
}

.pacha_track_error <- function(message) {
  state <- .pacha_state$tracker$current
  if (!is.null(state)) {
    state$failed <- TRUE
    state$messages <- unique(c(state$messages, as.character(message)[[1L]]))
  }
  invisible(NULL)
}

.pacha_rows <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  if (!is.list(x)) return(list())
  if (!is.null(x$result)) x <- x$result
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  if (!is.list(x)) return(list())
  fields <- c("id", "name", "scientificName", "property", "value", "language", "country", "link", "url")
  if (!is.null(names(x)) && any(names(x) %in% fields)) return(list(x))
  if (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))) return(x)
  list(x)
}

.pacha_first_field <- function(x, fields, default = NA_character_) {
  if (is.data.frame(x)) x <- as.list(x[1L, , drop = FALSE])
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

.pacha_url_encode <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

.pacha_http_json <- function(url, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required by the default ChecklistBank adapter.", call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = "application/json")
  request <- httr2::req_user_agent(request, "pacha R package")
  request <- httr2::req_timeout(request, seconds = .pacha_state$config$timeout)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  request <- httr2::req_retry(request, max_tries = 3, is_transient = function(response) {
    httr2::resp_status(response) %in% c(429L, 500L, 502L, 503L, 504L)
  })
  query <- query[!vapply(query, is.null, logical(1))]
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  response <- tryCatch(httr2::req_perform(request), error = function(e) {
    .pacha_track_error(conditionMessage(e))
    NULL
  })
  if (is.null(response)) return(NULL)
  status <- httr2::resp_status(response)
  if (identical(status, 404L)) return(NULL)
  if (status < 200L || status >= 300L) {
    .pacha_track_error(sprintf("HTTP %s from %s", status, url))
    return(NULL)
  }
  tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e) {
    .pacha_track_error(conditionMessage(e))
    NULL
  })
}

.pacha_api_get <- function(dataset, ..., query = list()) {
  path <- vapply(c("dataset", dataset, ...), .pacha_url_encode, character(1))
  .pacha_http_json(paste(c(.pacha_state$config$base_url, path), collapse = "/"), query)
}

.pacha_api_search_usage <- function(species, dataset) {
  candidates <- .pacha_rows(.pacha_api_get(
    dataset, "nameusage", "search", query = list(q = species, limit = 100)
  ))
  if (!length(candidates)) return(NULL)
  scientific <- vapply(candidates, .pacha_first_field, character(1), fields = c("scientificName", "name", "label"))
  status <- vapply(candidates, .pacha_first_field, character(1), fields = "status")
  exact <- which(tolower(scientific) == tolower(species))
  accepted_exact <- exact[tolower(status[exact]) == "accepted"]
  selected <- if (length(accepted_exact)) accepted_exact[[1L]] else if (length(exact)) {
    exact[[1L]]
  } else {
    accepted <- which(tolower(status) == "accepted")
    if (length(accepted)) accepted[[1L]] else 1L
  }
  candidates[[selected]]
}

.pacha_api_resolve_usage <- function(species, dataset) {
  usage <- .pacha_api_search_usage(species, dataset)
  if (is.null(usage)) {
    matched <- .pacha_api_get(dataset, "match", "nameusage", query = list(q = species, verbose = "false"))
    usage <- if (is.list(matched)) matched$usage else NULL
  }
  if (is.null(usage) || !is.list(usage)) return(NULL)
  identifier <- .pacha_first_field(usage, c("id", "usage.id", "taxonID"))
  if (is.na(identifier)) return(NULL)
  list(id = identifier, usage = usage)
}

.pacha_api_bundle <- function(resolution, dataset) {
  if (is.null(resolution) || is.na(resolution$id)) return(list())
  identifier <- resolution$id
  list(
    vernacular = .pacha_api_get(dataset, "taxon", identifier, "vernacular"),
    property = .pacha_api_get(dataset, "taxon", identifier, "property"),
    distribution = .pacha_api_get(dataset, "taxon", identifier, "distribution")
  )
}

.pacha_normalise_name_language <- function(language, country = NULL) {
  language <- tolower(.pacha_scalar(language, ""))
  country <- tolower(.pacha_scalar(country, ""))
  if (!nzchar(language)) return(".unspecified")
  if (grepl("castellano-kichwa|vacio", language) || (grepl("spa|es", language) && grepl("kichwa|qvi", language))) {
    return("castellano-kichwa")
  }
  if (grepl("kichwa|quechua|qvi|quz|que", language)) return("kichwa")
  if (grepl("spa|es|castellano|spanish", language)) return("es")
  if (nzchar(country)) paste0(language, "-", country) else language
}

.pacha_normalise_common_names <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.list(x) && !is.null(x$result)) x <- x$result
  if (is.character(x) && !is.null(names(x))) return(lapply(split(unname(x), names(x)), unique))
  if (is.data.frame(x) || (is.list(x) && (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))))) {
    x <- .pacha_rows(x)
  } else if (is.list(x) && !is.null(names(x)) && any(tolower(names(x)) %in% c("name", "vernacularname", "commonname"))) {
    x <- list(x)
  } else if (is.list(x) && !is.null(names(x))) {
    output <- lapply(x, function(values) unique(trimws(as.character(unlist(values, use.names = FALSE)))))
    output <- output[vapply(output, length, integer(1)) > 0L]
    return(output)
  }
  output <- list()
  for (row in x) {
    name <- .pacha_first_field(row, c("name", "vernacularName", "commonName", "col:name"))
    if (is.na(name)) next
    language <- .pacha_normalise_name_language(
      .pacha_first_field(row, c("language", "col:language")),
      .pacha_first_field(row, c("country", "area", "col:country"))
    )
    output[[language]] <- unique(c(output[[language]], name))
  }
  output
}

.pacha_sentence_case <- function(x) {
  x <- trimws(as.character(x)[[1L]])
  if (is.na(x) || !nzchar(x)) return(x)
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

.pacha_normalise_uses <- function(x, exclude_pattern = .pacha_state$config$use_exclude_pattern) {
  if (is.null(x) || !length(x)) return(list())
  if (is.list(x) && !is.null(x$result)) x <- x$result
  if (is.list(x) && !is.null(names(x)) && !any(tolower(names(x)) %in% c("property", "key", "name", "value", "values"))) {
    output <- lapply(x, function(values) unique(trimws(as.character(unlist(values, use.names = FALSE)))))
    output <- output[vapply(output, length, integer(1)) > 0L]
    return(output)
  }
  rows <- .pacha_rows(x)
  output <- list()
  for (row in rows) {
    category <- .pacha_first_field(row, c("property", "key", "name"))
    value <- .pacha_first_field(row, c("value", "values"))
    if (is.na(category) || is.na(value) || grepl(exclude_pattern, category, ignore.case = TRUE, perl = TRUE)) next
    category <- .pacha_sentence_case(category)
    output[[category]] <- unique(c(output[[category]], value))
  }
  output
}

.pacha_distribution_value <- function(x, fields) {
  rows <- .pacha_rows(x)
  for (row in rows) {
    value <- .pacha_first_field(row, fields)
    if (!is.na(value)) return(value)
  }
  NA_character_
}

.pacha_url_key <- function(url) {
  url <- trimws(as.character(url))
  url <- sub("[).,;]+$", "", url)
  if (grepl("ipni\\.org", url, ignore.case = TRUE)) {
    identifier <- sub(".*(?:names:|/n/)([0-9-]+).*$", "\\1", url, perl = TRUE)
    if (grepl("^[0-9-]+$", identifier)) return(paste0("https://www.ipni.org/n/", identifier))
  }
  sub("/+$", "", url)
}

.pacha_urls_from_value <- function(x) {
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  candidates <- unlist(regmatches(values, gregexpr("https?://[^[:space:];,]+", values, perl = TRUE)), use.names = FALSE)
  if (!length(candidates)) return(character())
  candidates <- vapply(candidates, .pacha_url_key, character(1))
  candidates <- unique(candidates[nzchar(candidates)])
  labels <- vapply(candidates, function(url) {
    host <- tolower(sub("^https?://([^/]+).*$", "\\1", url, perl = TRUE))
    if (grepl("ipni\\.org", host)) "ipni" else if (grepl("powo\\.science", host)) "powo" else if (grepl("gbif\\.org", host)) "gbif" else if (grepl("checklistbank", host)) "checklistbank" else "url"
  }, character(1))
  labels <- make.unique(labels, sep = "_")
  stats::setNames(candidates, labels)
}

.pacha_api_fetcher <- function(species, config) {
  resolution <- .pacha_api_resolve_usage(species, config$dataset)
  bundle <- .pacha_api_bundle(resolution, config$dataset)
  urls <- if (is.null(resolution)) character() else .pacha_urls_from_value(resolution$usage)
  if (!is.null(resolution)) {
    catalogue <- sprintf("https://www.checklistbank.org/dataset/%s/taxon/%s", config$dataset, resolution$id)
    if (!(catalogue %in% unname(urls))) urls <- c(urls, checklistbank = catalogue)
  }
  list(
    found = !is.null(resolution),
    common_names = .pacha_normalise_common_names(bundle$vernacular),
    sustainable_uses = .pacha_normalise_uses(bundle$property, config$use_exclude_pattern),
    indexation_urls = urls,
    establishment = .pacha_distribution_value(bundle$distribution, c("establishmentMeans", "establishment")),
    threat_status = .pacha_distribution_value(bundle$distribution, c("threatStatus", "threat"))
  )
}

.pacha_normalise_record <- function(record, requested_species, source) {
  if (is.null(record)) record <- list()
  if (!is.list(record)) stop("A source fetcher must return a list.", call. = FALSE)
  messages <- record$transport_messages
  if (is.null(messages)) messages <- character()
  common_names <- .pacha_normalise_common_names(record$common_names)
  sustainable_uses <- .pacha_normalise_uses(record$sustainable_uses)
  indexation_urls <- .pacha_urls_from_value(record$indexation_urls)
  establishment <- .pacha_scalar(record$establishment)
  threat_status <- .pacha_scalar(record$threat_status)
  found <- record$found
  if (is.null(found)) found <- length(common_names) || length(sustainable_uses) || length(indexation_urls) || !is.na(establishment) || !is.na(threat_status)
  list(
    requested_species = requested_species,
    source = source,
    found = isTRUE(found),
    common_names = common_names,
    sustainable_uses = sustainable_uses,
    indexation_urls = indexation_urls,
    establishment = establishment,
    threat_status = threat_status,
    transport_error = isTRUE(record$transport_error),
    transport_messages = unique(as.character(messages)),
    match_type = .pacha_scalar(record$match_type),
    match_ambiguous = isTRUE(record$match_ambiguous),
    distribution_ambiguous = isTRUE(record$distribution_ambiguous),
    retrieved_at = Sys.time()
  )
}

.pacha_fetch_api_record <- function(species) {
  result <- .pacha_with_tracker(function() {
    fetcher <- .pacha_state$config$fetcher
    if (is.null(fetcher)) fetcher <- .pacha_api_fetcher
    tryCatch(fetcher(species, .pacha_snapshot()), error = function(e) {
      .pacha_track_error(conditionMessage(e))
      NULL
    })
  })
  record <- .pacha_normalise_record(result$value, species, "api")
  record$transport_error <- record$transport_error || result$failed
  record$transport_messages <- unique(c(record$transport_messages, result$messages))
  record
}

.pacha_coldp_zip_path <- function() {
  path <- .pacha_state$config$coldp_zip_file
  if (is.null(path)) {
    candidate <- Sys.getenv("pacha_COLDP_ZIP", unset = "")
    if (nzchar(candidate)) path <- path.expand(candidate)
  }
  if (is.null(path) || !nzchar(path)) {
    stop("No local ColDP archive is configured. Use pacha_configure(coldp_zip_file = ...) or set pacha_COLDP_ZIP.", call. = FALSE)
  }
  if (!file.exists(path)) stop(sprintf("ColDP archive not found at '%s'.", path), call. = FALSE)
  path
}

.pacha_coldp_read_table <- function(zip_file, table_name) {
  key <- paste(zip_file, table_name, sep = "::")
  cache <- .pacha_state$coldp_tables
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))
  entries <- utils::unzip(zip_file, list = TRUE)$Name
  candidate <- entries[entries == table_name]
  if (!length(candidate)) candidate <- entries[grepl(paste0("(^|/)", gsub("\\.", "\\\\.", table_name), "$"), entries)]
  table <- NULL
  if (length(candidate)) {
    table <- utils::read.delim(
      unz(zip_file, candidate[[1L]]), header = TRUE, sep = "\t", quote = "",
      comment.char = "", stringsAsFactors = FALSE, check.names = FALSE,
      fileEncoding = "UTF-8"
    )
    table[] <- lapply(table, as.character)
  }
  assign(key, table, envir = cache)
  table
}

.pacha_coldp_require_columns <- function(table, columns, table_name) {
  missing_columns <- setdiff(columns, names(table))
  if (length(missing_columns)) {
    stop(sprintf("'%s' is missing required column(s): %s.", table_name, paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

.pacha_coldp_nonempty <- function(x) !is.na(x) & nzchar(trimws(x)) & toupper(trimws(x)) != "NA"

.pacha_coldp_pick_distribution_row <- function(records, country) {
  if (nrow(records) <= 1L) return(list(row = records, ambiguous = FALSE))
  area_columns <- intersect(c("col:area", "col:gazetteer", "col:country"), names(records))
  if (length(area_columns)) {
    country_values <- toupper(trimws(records[[area_columns[[1L]]]]))
    country_records <- records[!is.na(country_values) & country_values == country, , drop = FALSE]
    if (nrow(country_records) == 1L) return(list(row = country_records, ambiguous = FALSE))
    if (nrow(country_records) > 1L) records <- country_records
  }
  if ("col:ID" %in% names(records)) records <- records[order(records[["col:ID"]], na.last = TRUE), , drop = FALSE]
  list(row = records[1L, , drop = FALSE], ambiguous = TRUE)
}

.pacha_coldp_fetcher <- function(species, config) {
  zip_file <- .pacha_coldp_zip_path()
  names_table <- .pacha_coldp_read_table(zip_file, "NameUsage.tsv")
  if (is.null(names_table)) stop("'NameUsage.tsv' is required in the ColDP archive.", call. = FALSE)
  .pacha_coldp_require_columns(names_table, c("col:ID", "col:scientificName", "col:status"), "NameUsage.tsv")
  scientific <- names_table[["col:scientificName"]]
  status <- names_table[["col:status"]]
  accepted <- tolower(trimws(status)) == "accepted"
  exact <- !is.na(scientific) & tolower(trimws(scientific)) == tolower(species)
  partial <- !is.na(scientific) & grepl(tolower(species), tolower(scientific), fixed = TRUE)
  candidates <- which(exact & accepted)
  match_type <- "exact_accepted"
  if (!length(candidates)) {
    candidates <- which(partial & accepted)
    match_type <- "partial_accepted"
  }
  if (!length(candidates)) {
    candidates <- which(exact | partial)
    match_type <- "fallback_any_status"
  }
  if (!length(candidates)) {
    return(list(found = FALSE, common_names = list(), sustainable_uses = list(), indexation_urls = character(), establishment = NA_character_, threat_status = NA_character_))
  }
  identifiers <- names_table[["col:ID"]][candidates]
  candidates <- candidates[order(identifiers, na.last = TRUE)]
  selected <- candidates[[1L]]
  taxon_id <- names_table[["col:ID"]][selected]
  vernacular <- list()
  vernacular_table <- .pacha_coldp_read_table(zip_file, "VernacularName.tsv")
  if (!is.null(vernacular_table)) {
    .pacha_coldp_require_columns(vernacular_table, c("col:taxonID", "col:name", "col:language"), "VernacularName.tsv")
    keep <- vernacular_table[["col:taxonID"]] == taxon_id & .pacha_coldp_nonempty(vernacular_table[["col:name"]])
    if ("col:country" %in% names(vernacular_table)) {
      keep <- keep & toupper(trimws(vernacular_table[["col:country"]])) == config$coldp_country
    }
    if (any(keep, na.rm = TRUE)) vernacular <- .pacha_normalise_common_names(vernacular_table[which(keep), , drop = FALSE])
  }
  properties <- list()
  property_table <- .pacha_coldp_read_table(zip_file, "TaxonProperty.tsv")
  if (!is.null(property_table)) {
    .pacha_coldp_require_columns(property_table, c("col:taxonID", "col:property", "col:value"), "TaxonProperty.tsv")
    keep <- property_table[["col:taxonID"]] == taxon_id &
      .pacha_coldp_nonempty(property_table[["col:property"]]) &
      .pacha_coldp_nonempty(property_table[["col:value"]])
    if (any(keep, na.rm = TRUE)) {
      properties <- .pacha_normalise_uses(
        data.frame(
          property = property_table[["col:property"]][which(keep)],
          value = property_table[["col:value"]][which(keep)],
          stringsAsFactors = FALSE
        ),
        config$use_exclude_pattern
      )
    }
  }
  establishment <- NA_character_
  threat_status <- NA_character_
  distribution_ambiguous <- FALSE
  distribution_table <- .pacha_coldp_read_table(zip_file, "Distribution.tsv")
  if (!is.null(distribution_table)) {
    .pacha_coldp_require_columns(distribution_table, "col:taxonID", "Distribution.tsv")
    keep <- distribution_table[["col:taxonID"]] == taxon_id
    if (any(keep, na.rm = TRUE)) {
      picked <- .pacha_coldp_pick_distribution_row(distribution_table[which(keep), , drop = FALSE], config$coldp_country)
      distribution_ambiguous <- picked$ambiguous
      if ("col:establishmentMeans" %in% names(picked$row)) establishment <- .pacha_scalar(picked$row[["col:establishmentMeans"]])
      if ("col:threatStatus" %in% names(picked$row)) threat_status <- .pacha_scalar(picked$row[["col:threatStatus"]])
    }
  }
  links <- if ("col:link" %in% names(names_table)) names_table[["col:link"]][selected] else character()
  list(
    found = TRUE,
    common_names = vernacular,
    sustainable_uses = properties,
    indexation_urls = .pacha_urls_from_value(links),
    establishment = establishment,
    threat_status = threat_status,
    match_type = match_type,
    match_ambiguous = length(candidates) > 1L,
    distribution_ambiguous = distribution_ambiguous
  )
}

.pacha_fetch_coldp_record <- function(species) {
  raw <- tryCatch(.pacha_coldp_fetcher(species, .pacha_snapshot()), error = function(e) {
    stop(conditionMessage(e), call. = FALSE)
  })
  .pacha_normalise_record(raw, species, "coldp")
}

.pacha_record <- function(species, source = NULL, refresh = FALSE) {
  species <- .pacha_validate_species(species)
  source <- .pacha_resolve_source(source)
  cache <- if (identical(source, "api")) .pacha_state$api_cache else .pacha_state$coldp_cache
  key <- tolower(species)
  if (isTRUE(refresh) || !exists(key, envir = cache, inherits = FALSE)) {
    record <- if (identical(source, "api")) .pacha_fetch_api_record(species) else .pacha_fetch_coldp_record(species)
    assign(key, record, envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

.pacha_warn_coldp_resolution <- function(record) {
  if (!identical(record$source, "coldp") || !isTRUE(record$found)) return(invisible(NULL))
  if (!is.na(record$match_type) && !identical(record$match_type, "exact_accepted")) {
    warning(sprintf("The local ColDP archive resolved '%s' with '%s'; confirm the selected taxon.", record$requested_species, record$match_type), call. = FALSE)
  }
  if (isTRUE(record$match_ambiguous)) {
    warning(sprintf("More than one ColDP name record matched '%s'; the lowest identifier was selected.", record$requested_species), call. = FALSE)
  }
  if (isTRUE(record$distribution_ambiguous)) {
    warning(sprintf("More than one ColDP distribution record matched '%s'; the configured country was preferred and the lowest identifier was selected when necessary.", record$requested_species), call. = FALSE)
  }
  invisible(NULL)
}

.pacha_no_data_or_warning <- function(record, language = NULL) {
  if (isTRUE(record$transport_error)) {
    warning(sprintf("pacha Ecuador (%s): %s", .pacha_state$config$source_name, .pacha_label("connection_error", language)), call. = FALSE)
    return(.pacha_label("connection_error", language))
  }
  .pacha_label("no_data", language)
}

.pacha_markdown_escape <- function(x) {
  gsub("([\\\\`*_{}<>#+.!|-])", "\\\\\\1", as.character(x), perl = TRUE)
}

.pacha_emit_md <- function(markdown) {
  if (!is.null(markdown) && length(markdown) && nzchar(markdown)) cat(markdown, "\n", sep = "")
  invisible(if (is.null(markdown)) "" else markdown)
}

.pacha_emit_plain <- function(text) {
  if (!is.null(text) && length(text) == 1L && !is.na(text) && nzchar(text)) cat(text, "\n", sep = "")
  invisible(if (is.null(text) || !length(text) || is.na(text)) "" else text)
}

.pacha_decode_html_entities <- function(x) {
  x <- as.character(x)
  decode_numeric <- function(value, pattern, base) {
    codes <- unique(regmatches(value, gregexpr(pattern, value, ignore.case = TRUE, perl = TRUE))[[1L]])
    for (code in codes) {
      digits <- gsub("[^0-9A-Fa-f]", "", code)
      number <- strtoi(digits, base = base)
      if (!is.na(number)) value <- gsub(code, intToUtf8(number), value, fixed = TRUE)
    }
    value
  }
  x <- vapply(x, decode_numeric, character(1), pattern = "&#x[0-9A-Fa-f]+;", base = 16L, USE.NAMES = FALSE)
  x <- vapply(x, decode_numeric, character(1), pattern = "&#[0-9]+;", base = 10L, USE.NAMES = FALSE)
  named <- c(
    aacute = "\u00e1", eacute = "\u00e9", iacute = "\u00ed", oacute = "\u00f3", uacute = "\u00fa",
    Aacute = "\u00c1", Eacute = "\u00c9", Iacute = "\u00cd", Oacute = "\u00d3", Uacute = "\u00da",
    ntilde = "\u00f1", Ntilde = "\u00d1", uuml = "\u00fc", Uuml = "\u00dc",
    quot = "\"", apos = "'", nbsp = " ", amp = "&", lt = "<", gt = ">"
  )
  for (entity in names(named)) x <- gsub(paste0("&", entity, ";"), named[[entity]], x, fixed = TRUE)
  x
}

.pacha_html_to_plain <- function(x) {
  x <- gsub("<[^>]*>", "", as.character(x), perl = TRUE)
  x <- .pacha_decode_html_entities(x)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\\s*\n\\s*", " ", x)
  trimws(x)
}

.pacha_common_names_text <- function(common_names, language = NULL) {
  if (!length(common_names)) return(character())
  vapply(names(common_names), function(name_language) {
    values <- paste(unique(common_names[[name_language]]), collapse = ", ")
    if (identical(name_language, "es")) values else {
      label <- if (identical(name_language, ".unspecified")) .pacha_label("unspecified_language", language) else name_language
      sprintf("%s (%s)", values, label)
    }
  }, character(1))
}

.pacha_filter_uses <- function(uses, use = NULL) {
  if (is.null(use)) return(uses)
  use <- tolower(.pacha_text(use, "use"))
  uses[grepl(use, tolower(names(uses)), fixed = TRUE)]
}

.pacha_format_uses_plain <- function(uses) {
  if (!length(uses)) return(NA_character_)
  blocks <- vapply(names(uses), function(category) {
    lines <- paste0("  ", uses[[category]])
    paste0(category, ":\n", paste(lines, collapse = "\n"))
  }, character(1))
  paste(blocks, collapse = "\n")
}

.pacha_format_uses_md <- function(uses, language = NULL) {
  if (!length(uses)) return("")
  blocks <- vapply(names(uses), function(category) {
    values <- paste0("- ", .pacha_markdown_escape(uses[[category]]), collapse = "\n")
    paste0("**", .pacha_markdown_escape(category), ":**\n\n", values)
  }, character(1))
  paste0("**", .pacha_label("sustainable_uses", language), "**\n\n", paste(blocks, collapse = "\n\n"), "\n")
}

.pacha_format_urls_plain <- function(urls) {
  if (!length(urls)) return(NA_character_)
  stats::setNames(unname(urls), names(urls))
}

.pacha_format_urls_md <- function(urls, language = NULL) {
  if (!length(urls)) return("")
  labels <- tools::toTitleCase(gsub("_", " ", names(urls)))
  links <- sprintf("[%s](%s)", .pacha_markdown_escape(labels), urls)
  sprintf("**%s:** %s.\n", .pacha_label("indexed_in", language), paste(links, collapse = "; "))
}

.pacha_status_value <- function(value, status_type, language = NULL) {
  if (is.na(value)) return(NA_character_)
  dictionary <- .pacha_language_data(.pacha_resolve_language(language))[[status_type]]
  key <- tolower(trimws(value))
  index <- if (is.null(dictionary) || is.null(names(dictionary))) NA_integer_ else which(tolower(names(dictionary)) == key)[1L]
  translated <- if (is.na(index)) NA_character_ else .pacha_scalar(dictionary[[index]])
  if (!is.na(translated)) translated else .pacha_sentence_case(value)
}

.pacha_accessor <- function(species, language, source, refresh,
                            extractor, is_missing, plain_fmt, md_fmt,
                            format = c("plain", "md"), emit_plain = TRUE) {
  format <- match.arg(format)
  record <- .pacha_record(species, source, refresh)
  value <- extractor(record)
  if (is_missing(value)) {
    if (identical(format, "plain")) {
      message <- .pacha_no_data_or_warning(record, language)
      return(if (isTRUE(emit_plain)) .pacha_emit_plain(message) else message)
    }
    return(.pacha_emit_md(""))
  }
  if (identical(format, "plain")) {
    .pacha_warn_coldp_resolution(record)
    result <- plain_fmt(value, language)
    return(if (isTRUE(emit_plain)) .pacha_emit_plain(result) else result)
  }
  .pacha_emit_md(md_fmt(value, language))
}

.pacha_common_names_accessor <- function() {
  list(
    extractor = function(record) record$common_names,
    is_missing = function(value) !length(value),
    plain_fmt = function(value, language) paste(.pacha_common_names_text(value, language), collapse = "; "),
    md_fmt = function(value, language) sprintf(
      "**%s:** %s.  \n", .pacha_label("common_names", language),
      .pacha_markdown_escape(paste(.pacha_common_names_text(value, language), collapse = "; "))
    )
  )
}

.pacha_sustainable_uses_accessor <- function(use) {
  list(
    extractor = function(record) .pacha_filter_uses(record$sustainable_uses, use),
    is_missing = function(value) !length(value),
    plain_fmt = function(value, language) .pacha_format_uses_plain(value),
    md_fmt = function(value, language) .pacha_format_uses_md(value, language)
  )
}

.pacha_indexation_urls_accessor <- function() {
  list(
    extractor = function(record) record$indexation_urls,
    is_missing = function(value) !length(value),
    plain_fmt = function(value, language) .pacha_format_urls_plain(value),
    md_fmt = function(value, language) .pacha_format_urls_md(value, language)
  )
}

.pacha_status_accessor <- function(field, status_type, label_key, language) {
  list(
    extractor = function(record) .pacha_status_value(record[[field]], status_type, language),
    is_missing = function(value) is.na(value),
    plain_fmt = function(value, language) value,
    md_fmt = function(value, language) sprintf(
      "**%s:** %s.  \n", .pacha_label(label_key, language), .pacha_markdown_escape(value)
    )
  )
}

#' Retrieve common names
#'
#' Retrieves vernacular names for a scientific name from the currently
#' configured source. The API source queries the configured ChecklistBank
#' dataset; the local source reads `VernacularName.tsv` from the configured
#' ColDP archive and, when a country column is present, keeps only the
#' entries matching `coldp_country`. Names are grouped by language in the
#' returned value. The examples below use the Ecuadorian checklist dataset
#' configured by default, but the same calls work against any compatible
#' ChecklistBank dataset or ColDP archive.
#'
#' @param species Character scalar containing at least a genus and specific
#'   epithet.
#' @param language Optional output-language code or alias used for labels.
#' @param source Optional source override: `"api"`/`"web"` or
#'   `"coldp"`/`"local"`. `NULL` uses the configured default (`"api"`).
#' @param refresh Logical scalar. If `TRUE`, fetches the selected source again
#'   instead of reusing its cached record.
#'
#' @return `common_names_pacha()` prints and invisibly returns a single
#'   character string of names grouped by language, or the localized
#'   no-data/connection message. `common_names_pacha_md()` returns, invisibly,
#'   the emitted Markdown string, or `""` when no names are available.
#'
#' @seealso [accessors-pacha] for how plain-text and Markdown accessors work.
#'
#' @examples
#' \dontrun{
#' common_names_pacha("Bidens andicola")
#' common_names_pacha("Bidens andicola", source = "coldp")
#' }
#' @rdname common_names_pacha
#' @export
common_names_pacha <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_common_names_accessor()
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "plain"
  )
}

#' @rdname common_names_pacha
#' @examples
#' \dontrun{
#' common_names_pacha_md("Bidens andicola")
#' }
#' @export
common_names_pacha_md <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_common_names_accessor()
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "md"
  )
}

#' Retrieve sustainable-use records
#'
#' Retrieves sustainable-use categories and values from the configured
#' source: the ChecklistBank taxon-property endpoint for the API source, or
#' `TaxonProperty.tsv` for the local source. In either case, categories
#' matching `use_exclude_pattern` are dropped. This keeps the function usable
#' with any compatible property-based dataset, while remaining well suited
#' to the Ecuadorian sustainable-use dataset used in the examples below.
#'
#' @inheritParams common_names_pacha
#' @param use Optional category name or fragment, matched case-insensitively.
#'   `NULL` returns every available category.
#'
#' @return `sustainable_uses_pacha()` prints and invisibly returns a
#'   `Category:` heading followed by one indented use per line, for every
#'   matching category, or the localized no-data/connection message.
#'   `sustainable_uses_pacha_md()` returns, invisibly, the emitted Markdown
#'   string, or `""` when no matching sustainable-use data are available.
#'
#' @seealso [accessors-pacha] for a detailed description of plain-text and Markdown accessors.
#'
#' @examples
#' \dontrun{
#' sustainable_uses_pacha("Bidens andicola")
#' sustainable_uses_pacha("Bidens andicola", use = "medicinal", source = "coldp")
#' }
#' @rdname sustainable_uses_pacha
#' @export
sustainable_uses_pacha <- function(species, use = NULL, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_sustainable_uses_accessor(use)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "plain"
  )
}

#' @rdname sustainable_uses_pacha
#' @examples
#' \dontrun{
#' sustainable_uses_pacha_md("Bidens andicola", use = "medicinal")
#' }
#' @export
sustainable_uses_pacha_md <- function(species, use = NULL, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_sustainable_uses_accessor(use)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "md"
  )
}

#' Retrieve indexation URLs
#'
#' Retrieves URLs associated with the resolved taxon. For the web source, the
#' result includes any URLs returned by ChecklistBank plus its canonical
#' taxon page. For the local source, URLs are extracted from
#' `NameUsage.tsv`; IPNI links are normalized to their canonical
#' `https://www.ipni.org/n/...` form.
#'
#' @inheritParams common_names_pacha
#'
#' @return `indexation_urls_pacha()` returns, visibly, a named character
#'   vector of URLs, or the localized no-data/connection message.
#'   `indexation_urls_pacha_md()` returns, invisibly, the emitted Markdown
#'   string, or `""` when no URLs are available.
#'
#' @seealso [accessors-pacha] for a detailed description of plain-text and Markdown accessors.
#'
#' @examples
#' \dontrun{
#' indexation_urls_pacha("Bidens andicola")
#' indexation_urls_pacha("Bidens andicola", source = "coldp")
#' }
#' @rdname indexation_urls_pacha
#' @export
indexation_urls_pacha <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_indexation_urls_accessor()
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "plain", emit_plain = FALSE
  )
}

#' @rdname indexation_urls_pacha
#' @examples
#' \dontrun{
#' indexation_urls_pacha_md("Bidens andicola")
#' }
#' @export
indexation_urls_pacha_md <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_indexation_urls_accessor()
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "md"
  )
}

#' Retrieve establishment status
#'
#' Retrieves the documented establishment means of a taxon -- native,
#' introduced, cultivated, naturalized, and so on. The API source reads the
#' ChecklistBank taxon-distribution endpoint. The local source reads
#' `Distribution.tsv` and prefers the entry matching the configured
#' `coldp_country`; when several equally relevant records remain, it selects
#' the lowest ColDP identifier and reports this through a warning raised by
#' the plain-text accessor.
#'
#' Values are translated through the selected language dictionary's
#' `establishment` mapping. Values absent from that mapping are returned in
#' sentence case rather than rejected, so datasets using additional
#' establishment categories remain usable.
#'
#' @inheritParams common_names_pacha
#' @param language Optional output-language code or alias used to translate
#'   the establishment category and labels.
#'
#' @return `establishment_pacha()` prints and invisibly returns a translated
#'   establishment category, or the localized no-data/connection message.
#'   `establishment_pacha_md()` returns, invisibly, the emitted Markdown
#'   string, or `""` when the source has no establishment record.
#'
#' @seealso [accessors-pacha] for a detailed description of plain-text and Markdown accessors.
#'
#' @examples
#' \dontrun{
#' establishment_pacha("Bidens andicola")
#' establishment_pacha("Bidens andicola", source = "coldp", language = "en")
#' }
#' @rdname establishment_pacha
#' @export
establishment_pacha <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_status_accessor("establishment", "establishment", "origin", language)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "plain"
  )
}

#' @rdname establishment_pacha
#' @examples
#' \dontrun{
#' establishment_pacha_md("Bidens andicola", language = "en")
#' }
#' @export
establishment_pacha_md <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_status_accessor("establishment", "establishment", "origin", language)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "md"
  )
}

#' Retrieve conservation threat status
#'
#' Retrieves the conservation-status value documented for a taxon. The API
#' source reads the ChecklistBank taxon-distribution endpoint; the local
#' source reads `Distribution.tsv` using the same country-aware,
#' deterministic selection rule as [establishment_pacha()]. Typical values are
#' IUCN categories such as `EX`, `EW`, `CR`, `EN`, `VU`, `NT`, `LC`, `DD`, and
#' `NE`, though the function accepts any status recorded in the underlying
#' dataset.
#'
#' Values are translated through the selected language dictionary's `threat`
#' mapping. Unmapped source values are returned in sentence case rather than
#' rejected, so alternative assessment schemes remain usable.
#'
#' @inheritParams common_names_pacha
#' @param language Optional output-language code or alias used to translate
#'   the threat category and labels.
#'
#' @return `threat_status_pacha()` prints and invisibly returns a translated
#'   conservation-status category, or the localized no-data/connection
#'   message. `threat_status_pacha_md()` returns, invisibly, the emitted
#'   Markdown string, or `""` when the source has no threat-status record.
#'
#' @seealso [accessors-pacha] for a detailed description of plain-text and Markdown accessors.
#'
#' @examples
#' \dontrun{
#' threat_status_pacha("Bidens andicola")
#' threat_status_pacha("Bidens andicola", source = "coldp", language = "en")
#' }
#' @rdname threat_status_pacha
#' @export
threat_status_pacha <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_status_accessor("threat_status", "threat", "threat", language)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "plain"
  )
}

#' @rdname threat_status_pacha
#' @examples
#' \dontrun{
#' threat_status_pacha_md("Bidens andicola", language = "en")
#' }
#' @export
threat_status_pacha_md <- function(species, language = NULL, source = NULL, refresh = FALSE) {
  accessor <- .pacha_status_accessor("threat_status", "threat", "threat", language)
  .pacha_accessor(
    species, language, source, refresh,
    accessor$extractor, accessor$is_missing, accessor$plain_fmt, accessor$md_fmt,
    format = "md"
  )
}

#' Report whether a species is listed in the configured dataset
#'
#' Resolves a scientific name against the currently configured source
#' (`api` or `coldp`) and reports, as a plain-text console message, whether
#' the species is listed in the working dataset. The message is taken from
#' the active language configuration and can be customized through the
#' package configuration; if the required messages are unavailable for the
#' selected language, the function raises an error. The `language` argument
#' is resolved in the same way as for the other accessors in this module,
#' accepting either a language alias or a language code. As with those
#' accessors, a taxonomic match resolved from the local ColDP archive that
#' required disambiguation raises one or more additional `warning()`s.
#'
#' @inheritParams common_names_pacha
#' @param detailed Logical scalar. If `TRUE` and the species is listed, the
#'   concrete sustainable-use records are appended, one category per block
#'   and one use per line, in plain text without Markdown formatting.
#'
#' @return Invisibly, the printed plain-text statement: the species name,
#'   the localized listed/not-listed statement, and, when `detailed = TRUE`
#'   and the species is listed, its sustainable uses.
#'
#' @examples
#' \dontrun{
#' is_listed_pacha("Bidens andicola")
#' is_listed_pacha("Bidens andicola", detailed = TRUE)
#' is_listed_pacha("Bidens andicola", language = "en", source = "coldp")
#' }
#' @export
is_listed_pacha <- function(species, language = NULL, source = NULL, refresh = FALSE, detailed = FALSE) {
  species <- .pacha_validate_species(species)
  if (!is.logical(detailed) || length(detailed) != 1L || is.na(detailed)) {
    stop("`detailed` must be TRUE or FALSE.", call. = FALSE)
  }
  record <- .pacha_record(species, source, refresh)
  if (isTRUE(record$transport_error)) {
    return(.pacha_emit_plain(.pacha_no_data_or_warning(record, language)))
  }
  .pacha_warn_coldp_resolution(record)
  status_key <- if (isTRUE(record$found)) "is_listed_pacha_true" else "is_listed_pacha_false"
  result <- sprintf("%s: %s", species, .pacha_label(status_key, language, required = TRUE))
  if (isTRUE(detailed) && isTRUE(record$found)) {
    uses_text <- .pacha_format_uses_plain(record$sustainable_uses)
    if (!is.na(uses_text) && nzchar(uses_text)) result <- paste(result, uses_text, sep = "\n")
  }
  .pacha_emit_plain(result)
}


.pacha_api_dataset_citation <- function(dataset, refresh = FALSE) {
  cache <- .pacha_state$api_cache
  key <- paste0(".dataset_citation:", dataset)
  if (isTRUE(refresh) && exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
  if (!exists(key, envir = cache, inherits = FALSE)) {
    info <- .pacha_http_json(paste(c(.pacha_state$config$base_url, "dataset", .pacha_url_encode(dataset)), collapse = "/"))
    citation <- if (is.list(info)) .pacha_scalar(info$citation) else NA_character_
    assign(key, citation, envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

.pacha_coldp_metadata <- function(zip_file) {
  key <- paste(zip_file, "metadata.yaml", sep = "::")
  cache <- .pacha_state$coldp_tables
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))
  entries <- utils::unzip(zip_file, list = TRUE)$Name
  candidate <- entries[grepl("(^|/)metadata\\.ya?ml$", entries, ignore.case = TRUE)]
  metadata <- NULL
  if (length(candidate) && requireNamespace("yaml", quietly = TRUE)) {
    metadata <- tryCatch(yaml::read_yaml(unz(zip_file, candidate[[1L]])), error = function(e) NULL)
  }
  assign(key, metadata, envir = cache)
  metadata
}

.pacha_coldp_dataset_citation <- function(refresh = FALSE) {
  zip_file <- .pacha_coldp_zip_path()
  if (isTRUE(refresh)) {
    key <- paste(zip_file, "metadata.yaml", sep = "::")
    cache <- .pacha_state$coldp_tables
    if (exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
  }
  metadata <- .pacha_coldp_metadata(zip_file)
  if (is.null(metadata)) return(NA_character_)
  direct <- .pacha_scalar(metadata$citation)
  if (!is.na(direct) && nzchar(direct)) return(direct)
  title <- .pacha_scalar(metadata$title)
  version <- .pacha_scalar(metadata$version)
  doi <- .pacha_scalar(metadata$doi)
  issued <- .pacha_scalar(metadata$issued)
  year <- if (!is.na(issued) && nzchar(issued)) {
    sub("-.*", "", issued)
  } else if (!is.na(version) && nzchar(version)) {
    sub("-.*", "", version)
  } else {
    NA_character_
  }
  creators <- metadata$creator
  authors <- if (is.list(creators)) {
    vapply(creators, function(entry) {
      org <- .pacha_scalar(entry$organisation)
      if (!is.na(org) && nzchar(org)) return(org)
      family <- .pacha_scalar(entry$family)
      given <- .pacha_scalar(entry$given)
      name <- trimws(paste(family, given))
      if (nzchar(name)) name else NA_character_
    }, character(1))
  } else {
    character(0)
  }
  authors <- authors[!is.na(authors) & nzchar(authors)]
  author_str <- if (length(authors)) paste(authors, collapse = "; ") else NA_character_
  if (is.na(title) || !nzchar(title) || is.na(author_str) || is.na(year) || !nzchar(year)) {
    return(NA_character_)
  }
  citation <- sprintf("%s. (%s). %s", author_str, year, title)
  if (!is.na(version) && nzchar(version)) citation <- sprintf("%s (Version %s)", citation, version)
  if (!is.na(doi) && nzchar(doi)) {
    citation <- sprintf("%s. https://doi.org/%s", citation, doi)
  } else {
    citation <- paste0(citation, ".")
  }
  citation
}

.pacha_dataset_citation <- function(source = NULL, refresh = FALSE) {
  resolved_source <- .pacha_resolve_source(source)
  if (identical(resolved_source, "api")) {
    result <- .pacha_with_tracker(function() {
      tryCatch(.pacha_api_dataset_citation(.pacha_state$config$dataset, refresh), error = function(e) {
        .pacha_track_error(conditionMessage(e))
        NA_character_
      })
    })
    citation <- result$value
    transport_error <- result$failed
  } else {
    citation <- tryCatch(.pacha_coldp_dataset_citation(refresh), error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    })
    transport_error <- FALSE
  }
  list(citation = citation, transport_error = transport_error, source = resolved_source)
}

#' Retrieve the source database citation
#'
#' Retrieves how the configured data source itself should be cited, as
#' distinct from any per-species bibliographic reference. For the API
#' source, this is the `citation` field of the configured ChecklistBank
#' dataset (`GET /dataset/{key}`), resolved once and cached, since it does
#' not vary by species. For the local source, this is the `citation` entry
#' of `metadata.yaml` inside the configured ColDP archive. Either source may
#' return the citation as CSL HTML (e.g. wrapped in `<div class="csl-entry">`,
#' with `<span style="font-style: italic">` and HTML entities); this function
#' strips all markup and decodes entities, returning clean, unformatted text.
#'
#' @param species Ignored. Accepted only so a positional call does not
#'   collide with `language`. Unlike the other accessors in this module,
#'   `reference_pacha()` does not return a species-specific citation: the
#'   citation identifies the configured dataset or archive as a whole, and
#'   is identical for every species queried against that source. Passing a
#'   non-`NULL` value here has no effect on the result and emits a warning.
#' @param language Optional output-language code or alias used for the
#'   localized no-data/connection message.
#' @param source Optional source override: `"api"`/`"web"` or
#'   `"coldp"`/`"local"`. `NULL` uses the configured default.
#' @param refresh Logical scalar. If `TRUE`, fetches the citation again
#'   instead of reusing its cached value.
#'
#' @return A single character string with the plain-text source citation, or
#'   the localized no-data/connection message.
#'
#' @examples
#' \dontrun{
#' reference_pacha()
#' reference_pacha(source = "coldp")
#' }
#' @export
reference_pacha <- function(species = NULL, language = NULL, source = NULL, refresh = FALSE) {
  if (!is.null(species)) {
    warning("`species` is ignored by reference_pacha(): the citation is a property of the configured dataset/archive, not of an individual species.", call. = FALSE)
  }
  record <- .pacha_dataset_citation(source, refresh)
  if (is.na(record$citation) || !nzchar(record$citation)) {
    if (isTRUE(record$transport_error)) {
      warning(sprintf("pacha Ecuador (%s): %s", .pacha_state$config$source_name, .pacha_label("connection_error", language)), call. = FALSE)
      return(.pacha_label("connection_error", language))
    }
    return(.pacha_label("no_data", language))
  }
  .pacha_html_to_plain(record$citation)
}


.pacha_compare_components <- c("common_names", "sustainable_uses", "indexation_urls", "establishment", "threat_status")

.pacha_resolve_components <- function(component) {
  if (missing(component) || is.null(component)) return(.pacha_compare_components)
  component <- tolower(trimws(as.character(component)))
  component <- gsub("[ -]", "_", component)
  aliases <- c(
    all = "all",
    common_name = "common_names", common_names = "common_names", names = "common_names",
    sustainable_use = "sustainable_uses", sustainable_uses = "sustainable_uses", uses = "sustainable_uses",
    indexation_url = "indexation_urls", indexation_urls = "indexation_urls", urls = "indexation_urls",
    establishment = "establishment", origin = "establishment",
    threat_status = "threat_status", threat = "threat_status", conservation_status = "threat_status", conservation = "threat_status"
  )
  if (!length(component) || any(is.na(component)) || any(!(component %in% names(aliases)))) {
    stop("`component` must be 'all', 'common_names', 'sustainable_uses', 'indexation_urls', 'establishment', or 'threat_status'.", call. = FALSE)
  }
  resolved <- unname(aliases[component])
  if ("all" %in% resolved) .pacha_compare_components else unique(resolved)
}

.pacha_compare_normalise <- function(values) {
  values <- trimws(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) return(character())
  transliterated <- suppressWarnings(iconv(values, from = "", to = "ASCII//TRANSLIT"))
  transliterated[is.na(transliterated)] <- values[is.na(transliterated)]
  values <- tolower(transliterated)
  values <- gsub("[^[:alnum:]]+", " ", values)
  values <- gsub("\\s+", " ", trimws(values))
  unique(values[nzchar(values)])
}

.pacha_component_values <- function(record, component) {
  raw <- switch(
    component,
    common_names = unlist(record$common_names, recursive = TRUE, use.names = FALSE),
    sustainable_uses = unlist(lapply(names(record$sustainable_uses), function(category) {
      paste(category, record$sustainable_uses[[category]], sep = " :: ")
    }), use.names = FALSE),
    indexation_urls = unname(record$indexation_urls),
    establishment = record$establishment,
    threat_status = record$threat_status
  )
  raw <- unique(trimws(as.character(raw)))
  raw <- raw[!is.na(raw) & nzchar(raw)]
  list(raw = raw, normalized = .pacha_compare_normalise(raw))
}

.pacha_compare_one <- function(component, api_record, coldp_record) {
  api_values <- .pacha_component_values(api_record, component)
  coldp_values <- .pacha_component_values(coldp_record, component)
  if (isTRUE(api_record$transport_error)) {
    status <- "api_unavailable"
  } else if (isTRUE(coldp_record$transport_error)) {
    status <- "coldp_unavailable"
  } else if (!length(api_values$normalized) && !length(coldp_values$normalized)) {
    status <- "both_missing"
  } else if (!length(api_values$normalized) || !length(coldp_values$normalized)) {
    status <- "one_source_missing"
  } else if (setequal(api_values$normalized, coldp_values$normalized)) {
    status <- "identical"
  } else if (length(intersect(api_values$normalized, coldp_values$normalized))) {
    status <- "partial_overlap"
  } else {
    status <- "discrepant"
  }
  list(
    component = component,
    status = status,
    api = api_values$raw,
    coldp = coldp_values$raw,
    shared = intersect(api_values$normalized, coldp_values$normalized),
    api_only = setdiff(api_values$normalized, coldp_values$normalized),
    coldp_only = setdiff(coldp_values$normalized, api_values$normalized)
  )
}

.pacha_print_comparison <- function(report) {
  cat(sprintf("pacha Ecuador source comparison: %s\n", report$species))
  for (item in report$details) {
    cat(sprintf("\n%s: %s\n", item$component, item$status))
    api <- if (length(item$api)) paste(item$api, collapse = " | ") else "<none>"
    coldp <- if (length(item$coldp)) paste(item$coldp, collapse = " | ") else "<none>"
    cat(sprintf("  API: %s\n  ColDP: %s\n", api, coldp))
    if (length(item$shared)) cat(sprintf("  Shared after normalization: %s\n", paste(item$shared, collapse = " | ")))
    if (length(item$api_only)) cat(sprintf("  API only: %s\n", paste(item$api_only, collapse = " | ")))
    if (length(item$coldp_only)) cat(sprintf("  ColDP only: %s\n", paste(item$coldp_only, collapse = " | ")))
  }
  invisible(report)
}

#' Compare the web API and local ColDP source
#'
#' Validates and compares the data returned by both sources for one species.
#' The comparison always queries the API and the local ColDP archive,
#' regardless of the configured default source. It normalizes case, accents,
#' punctuation, and whitespace before assessing agreement, while retaining
#' the original source values in the printed result and returned report.
#' Sustainable uses are compared as `category :: value` pairs, so a value
#' moved between categories is reported as a discrepancy rather than
#' overlooked.
#'
#' The function prints a concise, component-by-component result by default
#' and returns the full machine-readable report invisibly. Status values are
#' `"identical"`, `"partial_overlap"`, `"discrepant"`, `"one_source_missing"`,
#' `"both_missing"`, `"api_unavailable"`, and `"coldp_unavailable"`. With
#' `component = "all"`, it compares common names, sustainable uses,
#' indexation URLs, establishment status, and threat status.
#'
#' @param species Character scalar containing at least a genus and specific
#'   epithet.
#' @param component One or more components to compare. Use `"all"` (the
#'   default) for the full comparison, or select `"common_names"`,
#'   `"sustainable_uses"`, `"indexation_urls"`, `"establishment"`, and/or
#'   `"threat_status"` individually. Recognized aliases map to these
#'   standard components as follows: `"names"` to `"common_names"`;
#'   `"uses"` to `"sustainable_uses"`; `"urls"` to `"indexation_urls"`;
#'   `"origin"` to `"establishment"`; and `"threat"` and `"conservation"`
#'   to `"threat_status"`. Matching is case-insensitive and tolerant of
#'   spaces or hyphens in place of underscores.
#' @param language Optional output-language code or alias retained in the
#'   report metadata; the comparison itself is language-neutral.
#' @param refresh Logical scalar. If `TRUE`, forces both source records to be
#'   refreshed before comparison.
#' @param print Logical scalar. If `TRUE`, prints the comparison result.
#'
#' @return Invisibly, a list with `species`, `language`, `components`,
#'   `summary`, `details`, `api_record`, `coldp_record`, and `compared_at`.
#'   `summary` is a data frame with one row per selected component and its
#'   agreement status.
#'
#' @examples
#' \dontrun{
#' compare_pacha("Bidens andicola")
#' compare_pacha("Bidens andicola", component = "sustainable_uses")
#' compare_pacha("Bidens andicola", component = c("origin", "threat"))
#' compare_pacha("Bidens andicola", component = c("names", "urls"), print = FALSE)
#' }
#' @export
compare_pacha <- function(species, component = "all", language = NULL, refresh = FALSE, print = TRUE) {
  species <- .pacha_validate_species(species)
  components <- .pacha_resolve_components(component)
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) stop("`refresh` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(print) || length(print) != 1L || is.na(print)) stop("`print` must be TRUE or FALSE.", call. = FALSE)
  language <- .pacha_resolve_language(language)
  api_record <- .pacha_record(species, source = "api", refresh = refresh)
  coldp_record <- .pacha_record(species, source = "coldp", refresh = refresh)
  details <- lapply(components, .pacha_compare_one, api_record = api_record, coldp_record = coldp_record)
  summary <- data.frame(
    component = vapply(details, `[[`, character(1), "component"),
    status = vapply(details, `[[`, character(1), "status"),
    api_values = vapply(details, function(x) length(x$api), integer(1)),
    coldp_values = vapply(details, function(x) length(x$coldp), integer(1)),
    shared_values = vapply(details, function(x) length(x$shared), integer(1)),
    stringsAsFactors = FALSE
  )
  report <- list(
    species = species,
    language = language,
    components = components,
    summary = summary,
    details = stats::setNames(details, components),
    api_record = api_record,
    coldp_record = coldp_record,
    compared_at = Sys.time()
  )
  if (isTRUE(print)) .pacha_print_comparison(report)
  invisible(report)
}
