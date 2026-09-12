# tests/testthat/test-pacha.R
# Runs inside the package namespace, so internal ".pacha_*" objects are
# reachable directly (no ":::" needed).

testthat::skip_if_not_installed("withr")
testthat::skip_if_not_installed("zip")
testthat::skip_if_not_installed("yaml")

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

# Snapshot/restore .pacha_state$config. Bypasses pacha_configure()'s public
# API because coldp_zip_file = NULL means "leave unchanged" there, so it
# can't be used to undo a change; direct env manipulation can.
local_pacha_config <- function(..., .local_envir = parent.frame()) {
  old <- as.list(.pacha_state$config)
  withr::defer({
    for (field in names(old)) {
      assign(field, old[[field]], envir = .pacha_state$config)
    }
    .pacha_clear_environments()
  }, envir = .local_envir)

  dots <- list(...)
  if (length(dots)) do.call(pacha_configure, dots)
  invisible(NULL)
}

# Mock fetchers (no network)
mock_fetcher_found <- function(species, config) {
  list(
    found = TRUE,
    common_names = list(es = c("chilca", "chilco"), kichwa = "wiku"),
    sustainable_uses = list(
      Medicinal  = "Decoction of leaves for fever",
      Ornamental = "Cut flower"
    ),
    indexation_urls = c(
      gbif = "https://www.gbif.org/species/123456",
      ipni = "https://www.ipni.org/n/77098765-1"
    ),
    establishment = "Native",
    threat_status = "LC"
  )
}

mock_fetcher_not_found <- function(species, config) {
  list(
    found = FALSE, common_names = list(), sustainable_uses = list(),
    indexation_urls = character(), establishment = NA_character_,
    threat_status = NA_character_
  )
}

mock_fetcher_transport_error <- function(species, config) {
  list(found = FALSE, transport_error = TRUE,
       transport_messages = "Simulated HTTP 404 Not Found")
}

mock_fetcher_throws <- function(species, config) {
  stop("Simulated connection failure", call. = FALSE)
}

# Builds a minimal ColDP archive under a temp dir; `overrides` lets a test
# replace any table or omit a file to force a specific failure mode.
create_dummy_coldp_zip <- function(overrides = list(), include_metadata = TRUE,
                                   .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)

  name_usage <- overrides$name_usage
  if (is.null(name_usage)) {
    name_usage <- data.frame(
      `col:ID` = c("t1", "t2"),
      `col:scientificName` = c("Bidens andicola", "Bidens andicola"),
      `col:status` = c("accepted", "synonym"),
      `col:link` = c("https://www.ipni.org/n/77098765-1", ""),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  vernacular <- overrides$vernacular
  if (is.null(vernacular)) {
    vernacular <- data.frame(
      `col:taxonID` = c("t1", "t1"),
      `col:name` = c("chilca", "chilco"),
      `col:language` = c("spa", "spa"),
      `col:country` = c("EC", "EC"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  taxon_property <- overrides$taxon_property
  if (is.null(taxon_property)) {
    taxon_property <- data.frame(
      `col:taxonID` = c("t1", "t1"),
      `col:property` = c("medicinal", "habit"),
      `col:value` = c("Decoction of leaves", "shrub"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
  distribution <- overrides$distribution
  if (is.null(distribution)) {
    distribution <- data.frame(
      `col:taxonID` = c("t1"),
      `col:area` = c("EC"),
      `col:establishmentMeans` = c("Native"),
      `col:threatStatus` = c("LC"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  write_tsv <- function(df, file) {
    utils::write.table(df, file.path(dir, file), sep = "\t", quote = FALSE,
                       row.names = FALSE, fileEncoding = "UTF-8", na = "")
  }

  files <- character()
  if (!isTRUE(overrides$omit_name_usage)) {
    write_tsv(name_usage, "NameUsage.tsv"); files <- c(files, "NameUsage.tsv")
  }
  if (!isTRUE(overrides$omit_vernacular)) {
    write_tsv(vernacular, "VernacularName.tsv"); files <- c(files, "VernacularName.tsv")
  }
  if (!isTRUE(overrides$omit_property)) {
    write_tsv(taxon_property, "TaxonProperty.tsv"); files <- c(files, "TaxonProperty.tsv")
  }
  if (!isTRUE(overrides$omit_distribution)) {
    write_tsv(distribution, "Distribution.tsv"); files <- c(files, "Distribution.tsv")
  }
  if (isTRUE(include_metadata)) {
    yaml::write_yaml(
      list(citation = "Dummy Ecuador Checklist (2026). Test archive."),
      file.path(dir, "metadata.yaml")
    )
    files <- c(files, "metadata.yaml")
  }

  zip_path <- file.path(dir, "coldp.zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(zip_path, files = files)
  zip_path
}

# =======================================================================
# 1. pacha_configure(): pure validation, no network, no files
# =======================================================================

test_that("pacha_configure() returns a config list with expected fields", {
  local_pacha_config()
  config <- pacha_configure()
  expect_type(config, "list")
  expect_true(all(c("dataset", "base_url", "source", "language",
                    "coldp_country", "timeout") %in% names(config)))
})

test_that("pacha_configure() rejects a malformed base_url", {
  local_pacha_config()
  expect_error(pacha_configure(base_url = "ftp://not-http"), "http")
})

test_that("pacha_configure() rejects an empty dataset", {
  local_pacha_config()
  expect_error(pacha_configure(dataset = ""), "non-empty")
})

test_that("pacha_configure() rejects an unknown source", {
  local_pacha_config()
  expect_error(pacha_configure(source = "carrier_pigeon"), "source")
})

test_that("pacha_configure() resolves source aliases consistently", {
  local_pacha_config()
  pacha_configure(source = "web")
  expect_identical(pacha_configure()$source, "api")
  pacha_configure(source = "local")
  expect_identical(pacha_configure()$source, "coldp")
})

test_that("pacha_configure() rejects a non-positive timeout", {
  local_pacha_config()
  expect_error(pacha_configure(timeout = 0), "positive")
  expect_error(pacha_configure(timeout = -5), "positive")
})

test_that("pacha_configure() rejects an invalid use_exclude_pattern regex", {
  local_pacha_config()
  expect_error(suppressWarnings(pacha_configure(use_exclude_pattern = "(")), "regular expression")
})

test_that("pacha_configure() rejects a fetcher that is not NULL or a function", {
  local_pacha_config()
  expect_error(pacha_configure(fetcher = "not_a_function"), "function")
})

test_that("pacha_configure() rejects a coldp_zip_file that does not exist", {
  local_pacha_config()
  expect_error(
    pacha_configure(coldp_zip_file = file.path(tempdir(), "does-not-exist-123.zip")),
    "not found"
  )
})

test_that("species validation rejects a single-word name", {
  expect_error(common_names_pacha("Bidens", source = "api"), "genus and")
})

test_that("is_listed_pacha() rejects a non-logical `detailed`", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  expect_error(is_listed_pacha("Bidens andicola", detailed = "yes"), "TRUE or FALSE")
})

# =======================================================================
# 2. pacha_clear_cache()
# =======================================================================

test_that("pacha_clear_cache() clears one species and clears everything", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")

  suppressWarnings(common_names_pacha("Bidens andicola", source = "api"))
  expect_true(exists("bidens andicola", envir = .pacha_state$api_cache, inherits = FALSE))

  pacha_clear_cache("Bidens andicola")
  expect_false(exists("bidens andicola", envir = .pacha_state$api_cache, inherits = FALSE))

  suppressWarnings(common_names_pacha("Bidens andicola", source = "api"))
  pacha_clear_cache()
  expect_equal(length(ls(.pacha_state$api_cache)), 0L)
})

# =======================================================================
# 3. Accessors against a mocked fetcher (no network)
# =======================================================================

test_that("common_names_pacha() groups plain-text names by language", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  expect_output(result <- common_names_pacha("Bidens andicola"), "chilca")
  expect_match(result, "wiku \\(kichwa\\)")
})

test_that("common_names_pacha_md() emits Markdown with a bold label", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  md <- common_names_pacha_md("Bidens andicola")
  expect_match(md, "\\*\\*")
  expect_match(md, "chilca")
})

test_that("common_names_pacha_md() returns '' invisibly when no names are found", {
  local_pacha_config(fetcher = mock_fetcher_not_found, source = "api")
  expect_identical(common_names_pacha_md("Bidens andicola"), "")
})

test_that("sustainable_uses_pacha() filters by category via `use`", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")

  expect_output(all_uses <- sustainable_uses_pacha("Bidens andicola"), "Medicinal")
  expect_match(all_uses, "Ornamental")

  expect_output(
    medicinal_only <- sustainable_uses_pacha("Bidens andicola", use = "medic"),
    "Medicinal"
  )
  expect_false(grepl("Ornamental", medicinal_only))
})

test_that("indexation_urls_pacha() returns a named character vector, prints nothing", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  expect_silent(urls <- indexation_urls_pacha("Bidens andicola"))
  expect_true(is.character(urls))
  expect_true(any(grepl("ipni\\.org", urls)))
})

test_that("establishment_pacha() and threat_status_pacha() return non-empty values", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  expect_output(est <- establishment_pacha("Bidens andicola"), ".")
  expect_output(threat <- threat_status_pacha("Bidens andicola"), ".")
  expect_true(nzchar(est))
  expect_true(nzchar(threat))
})

test_that("is_listed_pacha() reports presence and, optionally, sustainable uses", {
  local_pacha_config(fetcher = mock_fetcher_found, source = "api")
  expect_output(is_listed_pacha("Bidens andicola"), "Bidens andicola")
  expect_output(is_listed_pacha("Bidens andicola", detailed = TRUE), "Medicinal")
})

# =======================================================================
# 4. Failure resilience (simulated network, missing data)
# =======================================================================

test_that("a simulated transport error surfaces the localized connection message", {
  local_pacha_config(fetcher = mock_fetcher_transport_error, source = "api")
  expected <- .pacha_label("connection_error")
  expect_warning(result <- common_names_pacha("Bidens andicola"))
  expect_identical(result, expected)
})

test_that("an R error thrown by a custom fetcher is caught as a connection failure", {
  local_pacha_config(fetcher = mock_fetcher_throws, source = "api")
  expect_warning(result <- common_names_pacha("Bidens andicola"))
  expect_identical(result, .pacha_label("connection_error"))
})

test_that("a genuinely absent species returns the no-data message, no warning", {
  local_pacha_config(fetcher = mock_fetcher_not_found, source = "api")
  expect_warning(result <- common_names_pacha("Bidens andicola"), regexp = NA)
  expect_identical(result, .pacha_label("no_data"))
})

test_that("_md accessors return '' on transport error instead of propagating it", {
  local_pacha_config(fetcher = mock_fetcher_transport_error, source = "api")
  expect_identical(indexation_urls_pacha_md("Bidens andicola"), "")
})

# =======================================================================
# 5. Real API tests (guarded: CRAN + offline)
# =======================================================================

test_that("common_names_pacha() works against the live ChecklistBank API", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  local_pacha_config(source = "api", fetcher = NULL)
  expect_error(
    result <- common_names_pacha("Bidens andicola", refresh = TRUE),
    NA
  )
  expect_true(is.character(result) && length(result) == 1L)
})

test_that("reference_pacha() fetches the real dataset citation", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  local_pacha_config(source = "api")
  expect_error(citation <- reference_pacha(refresh = TRUE), NA)
  expect_true(is.character(citation) && nzchar(citation))
})

# =======================================================================
# 6. Local ColDP archive: fixtures in tempdir() only
# =======================================================================

test_that("ColDP accessors read a well-formed local archive correctly", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path, coldp_country = "EC")

  expect_output(names_txt <- common_names_pacha("Bidens andicola", source = "coldp"), "chilca")
  expect_match(names_txt, "chilco")

  urls <- indexation_urls_pacha("Bidens andicola", source = "coldp")
  expect_true(any(grepl("^https://www\\.ipni\\.org/n/", urls)))

  expect_output(est <- establishment_pacha("Bidens andicola", source = "coldp"), ".")
  expect_true(nzchar(est))
})

test_that("ColDP source excludes properties matching use_exclude_pattern", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path,
                     use_exclude_pattern = "habit")

  expect_output(uses <- sustainable_uses_pacha("Bidens andicola", source = "coldp"), "Medicinal")
  expect_false(grepl("shrub", uses))
})

test_that("a non-exact/non-accepted ColDP match warns about disambiguation", {
  name_usage <- data.frame(
    `col:ID` = "t1",
    `col:scientificName` = "Bidens andicola var. andicola",
    `col:status` = "accepted",
    `col:link` = "",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  zip_path <- create_dummy_coldp_zip(overrides = list(name_usage = name_usage))
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path)

  expect_warning(
    utils::capture.output(is_listed_pacha("Bidens andicola", source = "coldp")),
    "confirm the selected taxon"
  )
})

test_that("ambiguous ColDP distribution rows warn and pick deterministically", {
  distribution <- data.frame(
    `col:taxonID` = c("t1", "t1"),
    `col:area` = c("PE", "CO"),
    `col:establishmentMeans` = c("Introduced", "Native"),
    `col:threatStatus` = c("NT", "VU"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  zip_path <- create_dummy_coldp_zip(overrides = list(distribution = distribution))
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path, coldp_country = "EC")

  expect_warning(
    utils::capture.output(establishment_pacha("Bidens andicola", source = "coldp")),
    "distribution record"
  )
})

test_that("a ColDP archive missing NameUsage.tsv fails with a clear error, not a crash", {
  zip_path <- create_dummy_coldp_zip(overrides = list(omit_name_usage = TRUE))
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path)

  expect_error(
    common_names_pacha("Bidens andicola", source = "coldp"),
    "NameUsage.tsv"
  )
})

test_that("a ColDP table missing a required column fails with a clear error", {
  broken_vernacular <- data.frame(
    `col:taxonID` = "t1", `col:name` = "chilca", # missing col:language
    check.names = FALSE, stringsAsFactors = FALSE
  )
  zip_path <- create_dummy_coldp_zip(overrides = list(vernacular = broken_vernacular))
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path)

  expect_error(
    common_names_pacha("Bidens andicola", source = "coldp"),
    "missing required column"
  )
})

test_that("a species absent from the ColDP archive reports 'not listed', no error", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(source = "coldp", coldp_zip_file = zip_path)

  expect_output(
    is_listed_pacha("Genus fictitious", source = "coldp"),
    "Genus fictitious"
  )
})

test_that("no ColDP archive and no env var yields an informative error", {
  local_pacha_config()
  assign("coldp_zip_file", NULL, envir = .pacha_state$config)
  withr::local_envvar(pacha_COLDP_ZIP = "")

  expect_error(
    common_names_pacha("Bidens andicola", source = "coldp"),
    "No local ColDP archive"
  )
})

test_that("pacha_COLDP_ZIP env var is used when no path is configured", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config()
  assign("coldp_zip_file", NULL, envir = .pacha_state$config)
  withr::local_envvar(pacha_COLDP_ZIP = zip_path)

  expect_output(
    result <- common_names_pacha("Bidens andicola", source = "coldp"),
    "chilca"
  )
})

test_that("reference_pacha() reads the citation from a local metadata.yaml", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(coldp_zip_file = zip_path)

  citation <- reference_pacha(source = "coldp")
  expect_match(citation, "Dummy Ecuador Checklist")
})

test_that("reference_pacha() warns when `species` is supplied (it is ignored)", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(coldp_zip_file = zip_path)

  expect_warning(reference_pacha("Bidens andicola", source = "coldp"), "ignored")
})

test_that("reference_pacha() reports no-data when metadata.yaml has no citation", {
  zip_path <- create_dummy_coldp_zip(include_metadata = FALSE)
  local_pacha_config(coldp_zip_file = zip_path)

  expect_identical(reference_pacha(source = "coldp"), .pacha_label("no_data"))
})

# =======================================================================
# 7. compare_pacha(): mocked fetcher + mocked ColDP archive
# =======================================================================

test_that("compare_pacha() produces a valid summary data.frame, no network", {
  zip_path <- create_dummy_coldp_zip()
  local_pacha_config(fetcher = mock_fetcher_found, coldp_zip_file = zip_path,
                     coldp_country = "EC")

  report <- compare_pacha("Bidens andicola", component = "common_names", print = FALSE)
  expect_s3_class(report$summary, "data.frame")
  expect_true(
    report$summary$status[report$summary$component == "common_names"] %in%
      c("identical", "partial_overlap", "discrepant", "one_source_missing", "both_missing")
  )
})

test_that("compare_pacha() rejects an unknown `component`", {
  expect_error(compare_pacha("Bidens andicola", component = "nonsense"), "component")
})

test_that("compare_pacha() rejects non-logical `refresh`/`print`", {
  local_pacha_config(fetcher = mock_fetcher_found)
  expect_error(compare_pacha("Bidens andicola", refresh = "yes"), "TRUE or FALSE")
  expect_error(compare_pacha("Bidens andicola", print = "yes"), "TRUE or FALSE")
})
