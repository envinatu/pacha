# Contributing to pacha

This outlines how to propose a change to pacha. For a detailed discussion on contributing to this and other tidyverse-style packages, please see the [tidyverse contributing guide](https://rstd.io/tidy-contrib) and the [tidyverse code review principles](https://code-review.tidyverse.org/).

## Project direction

pacha currently ships with examples and defaults built around the "Listado de plantas de uso y aprovechamiento sostenible en Ecuador" checklist, but **the goal of the package is not to be Ecuador-specific**. The query and reporting layer is built on the Catalogue of Life Data Package (ColDP) schema so it can be pointed at any ChecklistBank-compatible dataset or local ColDP archive. Contributions are especially welcome in two directions:

- **Database/schema portability** — generalizing assumptions that currently bake in the Ecuador checklist (field names, dataset IDs, default endpoints, taxonomic ranks/categories specific to that source) so the same functions work against other national or thematic checklists without special-casing. If you're adding support for a new dataset, prefer adding configuration (e.g. via `pacha_configure()`) over hardcoding new conditionals in the query functions.
- **Standardization** — keeping outputs, argument names, and error handling consistent across query functions regardless of the underlying dataset, so results from different databases are comparable and interchangeable.

### Language dictionaries

Every module resolves `language` dynamically from `inst/lang/*.yml` — adding a language should never require touching R code. Contributions expanding language coverage are very welcome:

- Copy an existing file under `inst/lang/` (e.g. `es.yml`) as a starting point and translate all keys — don't drop or rename keys, since query functions and `pacha_report()` look them up by key.
- Keep the same key structure/nesting as the reference file so partial translations still fall back correctly.
- Test your new file by setting `language = "<your-code>"` in `pacha_configure()` and running `pacha_report()` against a sample query.
- Open the PR with only the new/changed `.yml` file(s) plus any test fixtures — no R code changes should be needed to add a language.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the *source* file (usually `.R`, not `.Rd`).

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it's needed. If you've found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

- Fork the package and clone onto your computer. If you haven't done this before, use `usethis::create_from_github("envinatu/pacha", fork = TRUE)`.
- Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes `R CMD check` by running `devtools::check()`. If `R CMD check` doesn't pass cleanly, it's a good idea to ask for help before continuing.
- Create a Git branch for your pull request (PR). A good name for a branch describes what it does, e.g. `add-gbif-schema-mapping`.
- Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser. The title of your PR should briefly describe the change. The body of your PR should contain `Fixes #issue-number`.
- For user-facing changes, add a bullet to `NEWS.md` (if it exists) that concisely describes the change. Follow the style described in <https://style.tidyverse.org/news.html>.

### Documentation conventions (roxygen2)

The full rationale for these rules lives in `R/pacha-package.R` (`?pacha` after loading the package) — read it once before writing new accessors. These rules apply to `common_names_pacha()`, `establishment_pacha()`, `sustainable_uses_pacha()`, `threat_status_pacha()`, `reference_pacha()`, `compare_pacha()`, `is_listed()`, `pacha_sc_full_name()`, `indexation_urls_pacha()`, `pacha_configure()`, `pacha_clear_cache()`, and `pacha_report()`.

**Simple query functions vs. Markdown reporting**

Unlike packages that ship a plain/`_md` twin for every accessor, `pacha` centralizes Markdown formatting in `pacha_report()`:

- Query functions (`common_names_pacha()`, `establishment_pacha()`, `sustainable_uses_pacha()`, `threat_status_pacha()`, `reference_pacha()`, `is_listed()`, `pacha_sc_full_name()`, `compare_pacha()`) return plain data/text — no embedded Markdown, no `_md` suffix. `is_listed()`'s `detailed` argument follows the same rule: even in detailed mode, its output stays plain text.
- `pacha_report()` consumes the output of those query functions and is the only place Markdown formatting happens. New query functions should wire their output into `pacha_report()` rather than growing their own `_md` twin.
- If a genuine need for a per-function plain/`_md` pair does arise later (identical signature, output-format-only difference → share one help page with `@rdname`; different parameters or error-handling contract → separate pages), state which bucket it falls into in your PR description.

**One-line wrapper families**

`indexation_urls_pacha()` is the shared entry point for the indexation-URL wrapper family (one function per external index/database). New wrappers in this family get one shared `@rdname` page anchored on `indexation_urls_pacha()`, with a `@details` table mapping each wrapper to the database/endpoint it targets. Keep `@export` on every wrapper — only the documentation is consolidated. This is also the natural place to add wrappers for non-Ecuador databases as portability work lands.

**Shared parameters between non-twin functions**

Several query functions share most parameters (e.g. taxon/species identifier, dataset source) without being a plain/`_md` pair. Don't repeat `@param` blocks — use `@inheritParams` pointing at the function that documents them fully (e.g. `@inheritParams common_names_pacha`), and document only what's specific to the new function.

**Language argument**

`pacha_configure()` resolves `language` dynamically from `inst/lang/*.yml`. Never hardcode a closed set like `language = c("es", "en")` with `match.arg()` — that breaks the "drop a new `.yml` file to add a language" contract the rest of the package relies on.

## Before opening a PR

- Run `devtools::document()` and commit the resulting `man/*.Rd` changes.
- Run `devtools::check()` and make sure it's clean (or that any existing NOTEs are unrelated to your change).
- Run `devtools::test()` — `Config/testthat/edition: 3` is required for new tests.
- If your change touches caching (`pacha_clear_cache()`) or configuration (`pacha_configure()`), confirm existing `testthat` coverage still passes.
- If you added a new query function intended for use in `pacha_report()`, say in your PR description how it plugs into the reporting layer.
- If your change generalizes something that was Ecuador-specific, or adds support for a new database/schema, call that out explicitly so reviewers can check it doesn't regress the existing checklist.
- If you added or changed language dictionaries, list which `.yml` file(s) changed and confirm you tested against `pacha_report()`.

## Code of Conduct

Please note that pacha is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
