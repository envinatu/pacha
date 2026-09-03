reset_pacha_cache <- function() {
  rm(list = ls(envir = .pacha_fn_cache), envir = .pacha_fn_cache)
}

# ---- Live API (network-dependent) -------------------------------------

test_that("pacha_sc_full_name resolves a real species from ChecklistBank", {
  skip_on_cran()
  skip_if_offline(host = "api.checklistbank.org")
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())

  result <- pacha_sc_full_name("Cinchona officinalis")

  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_true(nzchar(result))
})

test_that("pacha_sc_full_name_md renders a live Markdown heading", {
  skip_on_cran()
  skip_if_offline(host = "api.checklistbank.org")
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())

  expect_output(
    result <- pacha_sc_full_name_md("Cinchona officinalis"),
    "^# \\*"
  )
  expect_type(result, "character")
})

# ---- .pacha_fn_http_json: transport-layer resilience (mocked) ----------

test_that(".pacha_fn_http_json returns NULL on a simulated 404 response", {
  fake_response <- structure(list(status_code = 404L), class = "httr2_response")
  local_mocked_bindings(
    req_perform = function(req) fake_response,
    resp_status  = function(resp) 404L,
    .package = "httr2"
  )

  expect_null(.pacha_fn_http_json("https://api.checklistbank.org/dataset/3LXR/nameusage/search"))
})

test_that(".pacha_fn_http_json returns NULL on a simulated 5xx server error", {
  fake_response <- structure(list(status_code = 503L), class = "httr2_response")
  local_mocked_bindings(
    req_perform = function(req) fake_response,
    resp_status  = function(resp) 503L,
    .package = "httr2"
  )

  expect_null(.pacha_fn_http_json("https://api.checklistbank.org/dataset/3LXR/nameusage/search"))
})

test_that(".pacha_fn_http_json returns NULL when the connection itself fails", {
  local_mocked_bindings(
    req_perform = function(req) stop("simulated connection failure"),
    .package = "httr2"
  )

  expect_null(.pacha_fn_http_json("https://api.checklistbank.org/dataset/3LXR"))
})

test_that(".pacha_fn_http_json parses a simulated successful JSON response", {
  fake_response <- structure(list(status_code = 200L), class = "httr2_response")
  fake_body <- list(usage = list(id = "1", scientificName = "Cinchona officinalis"))
  local_mocked_bindings(
    req_perform    = function(req) fake_response,
    resp_status    = function(resp) 200L,
    resp_body_json = function(resp, simplifyVector = FALSE) fake_body,
    .package = "httr2"
  )

  result <- .pacha_fn_http_json("https://api.checklistbank.org/match/nameusage")
  expect_equal(result$usage$scientificName, "Cinchona officinalis")
})

test_that(".pacha_fn_http_json stops with an informative error when httr2 is unavailable", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )

  expect_error(
    .pacha_fn_http_json("https://api.checklistbank.org/dataset/3LXR"),
    "httr2"
  )
})

# ---- .pacha_fn_rows / .pacha_fn_first_field: pure parsing logic ---------

test_that(".pacha_fn_rows unwraps a top-level 'result' element", {
  x <- list(result = list(list(id = "1"), list(id = "2")))
  rows <- .pacha_fn_rows(x)

  expect_length(rows, 2)
  expect_equal(rows[[1]]$id, "1")
})

test_that(".pacha_fn_rows wraps a single named record in a one-element list", {
  x <- list(id = "1", scientificName = "Cinchona officinalis")
  rows <- .pacha_fn_rows(x)

  expect_length(rows, 1)
  expect_equal(rows[[1]]$scientificName, "Cinchona officinalis")
})

test_that(".pacha_fn_rows returns an empty list for NULL, empty, or non-list input", {
  expect_equal(.pacha_fn_rows(NULL), list())
  expect_equal(.pacha_fn_rows(list()), list())
  expect_equal(.pacha_fn_rows("not a list"), list())
})

test_that(".pacha_fn_first_field is case-insensitive and skips NA values", {
  x <- list(ScientificName = "Cinchona officinalis", Author = NA_character_)

  expect_equal(.pacha_fn_first_field(x, c("scientificName", "name")), "Cinchona officinalis")
  expect_true(is.na(.pacha_fn_first_field(x, c("author", "authorship"))))
})

test_that(".pacha_fn_first_field returns the default for non-named or malformed input", {
  expect_true(is.na(.pacha_fn_first_field(list(1, 2), "name")))
  expect_true(is.na(.pacha_fn_first_field(NULL, "name")))
})

# ---- .pacha_fn_search_usage: candidate-selection priority (mocked) -----

test_that("search selection prefers an exact, accepted match above all else", {
  fake_candidates <- list(
    list(scientificName = "Cinchona officinalis", status = "synonym"),
    list(scientificName = "Cinchona officinalis", status = "accepted"),
    list(scientificName = "Cinchona pubescens", status = "accepted")
  )
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(result = fake_candidates)
  )

  selected <- .pacha_fn_search_usage("Cinchona officinalis")
  expect_equal(selected$status, "accepted")
  expect_equal(selected$scientificName, "Cinchona officinalis")
})

test_that("search selection falls back to any exact match when none is 'accepted'", {
  fake_candidates <- list(
    list(scientificName = "Cinchona officinalis", status = "provisional"),
    list(scientificName = "Cinchona calisaya", status = "accepted")
  )
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(result = fake_candidates)
  )

  selected <- .pacha_fn_search_usage("Cinchona officinalis")
  expect_equal(selected$scientificName, "Cinchona officinalis")
  expect_equal(selected$status, "provisional")
})

test_that("search selection falls back to the first 'accepted' candidate when no exact match exists", {
  fake_candidates <- list(
    list(scientificName = "Cinchona pubescens", status = "synonym"),
    list(scientificName = "Cinchona calisaya", status = "accepted")
  )
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(result = fake_candidates)
  )

  selected <- .pacha_fn_search_usage("Cinchona officinalis")
  expect_equal(selected$scientificName, "Cinchona calisaya")
})

test_that("search selection returns the first candidate as a last resort", {
  fake_candidates <- list(
    list(scientificName = "Cinchona pubescens", status = "synonym"),
    list(scientificName = "Cinchona calisaya", status = "synonym")
  )
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(result = fake_candidates)
  )

  selected <- .pacha_fn_search_usage("Cinchona officinalis")
  expect_equal(selected$scientificName, "Cinchona pubescens")
})

test_that("search selection returns NULL when the API yields no candidates", {
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(result = list())
  )

  expect_null(.pacha_fn_search_usage("Cinchona officinalis"))
})

# ---- .pacha_fn_resolve_usage: match-then-search orchestration (mocked) -

test_that("resolve_usage uses the match endpoint directly when it returns a usage", {
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(..., query = list()) {
      args <- c(...)
      if (identical(args, c("match", "nameusage"))) {
        list(usage = list(id = "ABC123", scientificName = "Cinchona officinalis", authorship = "L."))
      } else {
        stop("search endpoint should not have been called")
      }
    }
  )

  resolution <- .pacha_fn_resolve_usage("Cinchona officinalis")
  expect_equal(resolution$id, "ABC123")
  expect_equal(resolution$name, "Cinchona officinalis")
  expect_equal(resolution$authorship, "L.")
})

test_that("resolve_usage falls back to search when the match endpoint returns no usage", {
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(..., query = list()) {
      args <- c(...)
      if (identical(args, c("match", "nameusage"))) {
        list(usage = NULL)
      } else {
        list(result = list(list(id = "XYZ9", scientificName = "Cinchona officinalis", status = "accepted")))
      }
    }
  )

  resolution <- .pacha_fn_resolve_usage("Cinchona officinalis")
  expect_equal(resolution$id, "XYZ9")
})

test_that("resolve_usage returns NULL when no usable id can be extracted", {
  local_mocked_bindings(
    .pacha_fn_checklist_get = function(...) list(usage = list(scientificName = "no id present"))
  )

  expect_null(.pacha_fn_resolve_usage("Cinchona officinalis"))
})

# ---- .pacha_fn_fetch_record / .pacha_fn_record: fallback and caching ----

test_that("fetch_record falls back to the input string when resolution errors out", {
  local_mocked_bindings(
    .pacha_fn_resolve_usage = function(...) stop("simulated network failure")
  )

  record <- .pacha_fn_fetch_record("Cinchona officinalis")
  expect_equal(record$scientific_name, "Cinchona officinalis")
  expect_true(is.na(record$authorship))
})

test_that("fetch_record falls back to the input string when no match is found", {
  local_mocked_bindings(
    .pacha_fn_resolve_usage = function(...) NULL
  )

  record <- .pacha_fn_fetch_record("Nonexistens fakespecies")
  expect_equal(record$scientific_name, "Nonexistens fakespecies")
  expect_true(is.na(record$authorship))
})

test_that(".pacha_fn_record caches results and does not re-resolve on repeat calls", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  call_count <- 0
  local_mocked_bindings(
    .pacha_fn_fetch_record = function(species) {
      call_count <<- call_count + 1
      list(scientific_name = species, authorship = "L.", retrieved_at = Sys.time())
    }
  )

  first  <- .pacha_fn_record("Cinchona officinalis")
  second <- .pacha_fn_record("Cinchona officinalis")

  expect_equal(call_count, 1)
  expect_equal(first$scientific_name, second$scientific_name)
})

test_that(".pacha_fn_record bypasses the cache when refresh = TRUE", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  call_count <- 0
  local_mocked_bindings(
    .pacha_fn_fetch_record = function(species) {
      call_count <<- call_count + 1
      list(scientific_name = species, authorship = "L.", retrieved_at = Sys.time())
    }
  )

  .pacha_fn_record("Cinchona officinalis")
  .pacha_fn_record("Cinchona officinalis", refresh = TRUE)

  expect_equal(call_count, 2)
})

# ---- pacha_sc_full_name / pacha_sc_full_name_md: public output (mocked) -

test_that("pacha_sc_full_name appends authorship when it was resolved", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  local_mocked_bindings(
    .pacha_fn_fetch_record = function(species) {
      list(scientific_name = "Cinchona officinalis", authorship = "L.", retrieved_at = Sys.time())
    }
  )

  expect_equal(pacha_sc_full_name("Cinchona officinalis"), "Cinchona officinalis L.")
})

test_that("pacha_sc_full_name omits authorship when none was resolved", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  local_mocked_bindings(
    .pacha_fn_fetch_record = function(species) {
      list(scientific_name = species, authorship = NA_character_, retrieved_at = Sys.time())
    }
  )

  expect_equal(pacha_sc_full_name("Nonexistens fakespecies"), "Nonexistens fakespecies")
})

test_that("pacha_sc_full_name_md prints a level-one heading with the italicised name", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  local_mocked_bindings(
    .pacha_fn_record = function(species, refresh = FALSE) {
      list(scientific_name = "Cinchona officinalis", authorship = "Linnaeus")
    }
  )

  expect_output(
    result <- pacha_sc_full_name_md("Cinchona officinalis"),
    "^# \\*Cinchona officinalis\\* Linnaeus$"
  )
  expect_equal(result, "# *Cinchona officinalis* Linnaeus")
})

test_that("pacha_sc_full_name_md omits the authorship segment when none was resolved", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  local_mocked_bindings(
    .pacha_fn_record = function(species, refresh = FALSE) {
      list(scientific_name = "Cinchona officinalis", authorship = NA_character_)
    }
  )

  expect_output(
    result <- pacha_sc_full_name_md("Cinchona officinalis"),
    "^# \\*Cinchona officinalis\\*$"
  )
  expect_equal(result, "# *Cinchona officinalis*")
})

test_that("pacha_sc_full_name_md escapes Markdown-sensitive authorship characters", {
  reset_pacha_cache()
  withr::defer(reset_pacha_cache())
  local_mocked_bindings(
    .pacha_fn_record = function(species, refresh = FALSE) {
      list(scientific_name = "Cinchona officinalis", authorship = "L. (1753)")
    }
  )

  expect_output(
    result <- pacha_sc_full_name_md("Cinchona officinalis"),
    "^# \\*Cinchona officinalis\\*"
  )
  expect_true(grepl("\\\\", result))
})

# ---- species argument validation: error resilience ---------------------

test_that("pacha_sc_full_name rejects a structurally invalid species argument", {
  expect_error(pacha_sc_full_name(NA_character_))
  expect_error(pacha_sc_full_name(""))
  expect_error(pacha_sc_full_name("Cinchona"))
})
