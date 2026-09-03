mock_pacha_accessors <- function(heading = function(species) paste0("# ", species),
                                names_md = function(...) "",
                                uses_md = function(...) "",
                                env = parent.frame()) {
  local_mocked_bindings(
    pacha_sc_full_name_md = heading,
    common_names_pacha_md = names_md,
    sustainable_uses_pacha_md = uses_md,
    .env = env
  )
}

test_that("validates `species` and `print`", {
  mock_pacha_accessors()

  bad_species <- list(character(0), NA_character_, c("Cinchona officinalis", NA), 123)
  for (sp in bad_species) {
    expect_error(
      pacha_report(sp, print = FALSE),
      "`species` must be one or more non-missing scientific names"
    )
  }

  bad_print <- list(NA, c(TRUE, FALSE), "TRUE", 1)
  for (p in bad_print) {
    expect_error(
      pacha_report("Cinchona officinalis", print = p),
      "`print` must be TRUE or FALSE"
    )
  }
})

test_that("combines components, omits empty ones, and always keeps the heading", {
  mock_pacha_accessors(
    names_md = function(...) "## Common names\ncascarilla",
    uses_md = function(...) "## Uses\nmedicinal"
  )
  expect_equal(
    pacha_report("Cinchona officinalis", print = FALSE),
    "# Cinchona officinalis\n## Common names\ncascarilla\n## Uses\nmedicinal"
  )

  mock_pacha_accessors(uses_md = function(...) "## Uses\nmedicinal")
  report_no_names <- pacha_report("Cinchona officinalis", print = FALSE)
  expect_equal(report_no_names, "# Cinchona officinalis\n## Uses\nmedicinal")
  expect_false(grepl("no data", report_no_names, ignore.case = TRUE))

  mock_pacha_accessors()
  expect_equal(pacha_report("Cinchona officinalis", print = FALSE), "# Cinchona officinalis")
})

test_that("separates multiple species with a blank line", {
  mock_pacha_accessors(
    heading = function(species) paste0("# ", species),
    names_md = function(species, ...) paste0("## Names for ", species),
    uses_md = function(species, ...) paste0("## Uses for ", species)
  )

  report <- pacha_report(c("Cinchona officinalis", "Bomarea multiflora"), print = FALSE)

  expect_equal(
    report,
    paste0(
      "# Cinchona officinalis\n## Names for Cinchona officinalis\n## Uses for Cinchona officinalis\n\n",
      "# Bomarea multiflora\n## Names for Bomarea multiflora\n## Uses for Bomarea multiflora"
    )
  )
})

test_that("forwards `language`/`source` to both accessors, `use` only to sustainable_uses_pacha_md(), and skips them for the heading", {
  calls <- new.env()

  mock_pacha_accessors(
    heading = function(species) {
      calls$heading <- list(species = species, extra_args = names(formals(sys.function())))
      "# Heading"
    },
    names_md = function(species, language = NULL, source = NULL, refresh = FALSE) {
      calls$names <- list(language = language, source = source, refresh = refresh)
      ""
    },
    uses_md = function(species, use = NULL, language = NULL, source = NULL, refresh = FALSE) {
      calls$uses <- list(use = use, language = language, source = source, refresh = refresh)
      ""
    }
  )

  pacha_report(
    "Cinchona officinalis",
    language = "en", source = "coldp", use = "medicinal", refresh = TRUE,
    print = FALSE
  )

  expect_equal(calls$heading$species, "Cinchona officinalis")
  expect_false("use" %in% calls$heading$extra_args)
  expect_equal(calls$names, list(language = "en", source = "coldp", refresh = TRUE))
  expect_equal(calls$uses, list(use = "medicinal", language = "en", source = "coldp", refresh = TRUE))
})

test_that("print = TRUE emits via cat() invisibly; print = FALSE returns visibly without printing", {
  mock_pacha_accessors()

  expect_output(
    printed <- withVisible(pacha_report("Cinchona officinalis")),
    "# Cinchona officinalis"
  )
  expect_false(printed$visible)
  expect_equal(printed$value, "# Cinchona officinalis")

  expect_silent(returned <- withVisible(pacha_report("Cinchona officinalis", print = FALSE)))
  expect_true(returned$visible)
  expect_equal(returned$value, "# Cinchona officinalis")
})

test_that("an invalid species aborts and stops processing earlier species", {
  mock_pacha_accessors(
    heading = function(species) {
      if (!grepl("^[A-Z][a-z]+ [a-z]+$", species)) stop("invalid scientific name", call. = FALSE)
      paste0("# ", species)
    }
  )

  expect_error(
    pacha_report(c("Cinchona officinalis", "not-a-name"), print = FALSE),
    "invalid scientific name"
  )
})

test_that("cat() output emitted internally by the accessors never leaks into the report", {
  mock_pacha_accessors(
    heading = function(species) {
      cat("internal noise\n")
      paste0("# ", species)
    },
    names_md = function(...) {
      cat("more noise\n")
      ""
    }
  )

  expect_output(report <- pacha_report("Cinchona officinalis", print = FALSE), NA)
  expect_equal(report, "# Cinchona officinalis")
})

test_that("CANARY: mocks are actually intercepting the three accessors (no real I/O possible)", {
  mock_pacha_accessors(
    heading = function(species) stop("REAL pacha_sc_full_name_md() WAS REACHED"),
    names_md = function(...) stop("REAL common_names_pacha_md() WAS REACHED"),
    uses_md = function(...) stop("REAL sustainable_uses_pacha_md() WAS REACHED")
  )

  expect_error(pacha_report("Cinchona officinalis", print = FALSE), "REAL pacha_sc_full_name_md")
})
