## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Windows 11, R 4.5.2
<!-- antes de enviar: rodar devtools::check_win_devel(), devtools::check_win_release() e rhub::rhub_check()
     e listar aqui os ambientes em que passaram -->
* win-builder (devel and release): A FAZER
* R-hub (linux, macos, windows): A FAZER

## Notes for CRAN

* The package gives access to data from the Multi-Year Plans ('Planos
  Plurianuais') of the Brazilian states. The data (about 85 MB) and the
  original documents are too large for CRAN, so they are hosted as release
  assets of the public GitHub repository and downloaded on demand into
  `tools::R_user_dir("ppabr", "cache")`. Nothing is written to the user's
  home directory or working directory.
* Functions that need internet fail gracefully: when the download is not
  possible they give an informative message and return `NULL`, without error.
* Examples that download data are wrapped in `\donttest{}` and handle a
  `NULL` result; tests that download data are skipped on CRAN. The catalog,
  data dictionary and summary functions work offline from small internal
  data.
* The documentation and the vignette are in Portuguese (`Language: pt-BR`),
  as the intended users are researchers of Brazilian public policy; the
  Title and Description are in English.
