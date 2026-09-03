# pacha

`pacha` is an R package to query and report on taxonomic and ethnobotanical checklist data. It was built around the "Listado de plantas de uso y aprovechamiento sostenible en Ecuador" checklist (<doi:10.48580/dgvrn>), retrieved via [ChecklistBank](https://www.checklistbank.org/) or from a local Catalogue of Life Data Package (ColDP) archive — but the query and reporting layer is built on the ColDP schema, so it can be pointed at any ChecklistBank-compatible dataset, not just the Ecuador checklist.

All user-facing results default to Spanish; language dictionaries are read dynamically from `inst/lang/*.yml`, so adding a language is a matter of dropping in a new file rather than changing code — set your language with `pacha_configure()`.

Query functions (`common_names_pacha()`, `establishment_pacha()`, `sustainable_uses_pacha()`, `threat_status_pacha()`, `reference_pacha()`, `is_listed()`, `pacha_sc_full_name()`, `compare_pacha()`) return plain data/text. Markdown-formatted reports for direct inclusion in documents are produced by `pacha_report()`.

## Installation

``` r
install.packages("pak")
pak::pkg_install("envinatu/pacha")
```

For a conventional development installation:

``` r
devtools::install_github("envinatu/pacha")
```

## Quick start

``` r
library(pacha)

pacha_configure(language = "es")

is_listed("Bidens andicola")
common_names_pacha("Bidens andicola")
sustainable_uses_pacha("Bidens andicola", detailed = TRUE)
pacha_report("Bidens andicola")
```

## Pointing pacha at another database

Because the underlying layer speaks ColDP, `pacha` is not tied to the Ecuador checklist. To query a different ChecklistBank-compatible dataset, configure the dataset source before calling the query functions:

``` r
pacha_configure(
  dataset    = "<your-checklistbank-dataset-id>",
  base_url   = "<checklistbank-instance-url>",  # only if not the default instance
  source_name = "<display name for this source>"
)
```

Or, to query a local ColDP archive instead of the live API:

``` r
pacha_configure(
  coldp_zip_file = "path/to/archive.zip",
  coldp_country  = "EC"  # ISO country code, if the archive needs scoping
)
```

`pacha_configure()` also exposes `language` (see above), `fetcher` (swap the underlying HTTP client, e.g. for testing or caching), `use_exclude_pattern` and `labels` (control which taxa/fields are filtered or how they're labelled), `source` and `timeout` (network request timeout). See `?pacha_configure` for the full argument reference.

Contributions that generalize dataset-specific assumptions, add wrappers to the `indexation_urls_pacha()` family for other indices, or expand `inst/lang/*.yml` with new languages are very welcome — see [CONTRIBUTING.md](CONTRIBUTING.md).

## Data sources and attribution

The package is a query client; it does not redistribute the underlying datasets. Users are responsible for observing source licences, terms of use, and citation requirements — including citing the "Listado de plantas de uso y aprovechamiento sostenible en Ecuador" checklist (<doi:10.48580/dgvrn>) when using its data, or the equivalent citation for any other dataset queried through `pacha`. Results can change when the remote services change.

## Code of Conduct

Please note that the pacha project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
