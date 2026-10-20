## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local Windows 11, R 4.5.2
* win-builder: R-devel (2026-10-18 r90707 ucrt): 1 NOTE (new submission)
* R-hub v2 (GitHub Actions): linux (R-devel), macos-arm64 (R-devel),
  windows (R-devel) and donttest (R-devel, with `--run-donttest`): OK.
  On the nosuggests platform the only error is the re-building of the
  vignette, which needs 'rmarkdown' (listed in Suggests, as the vignette
  builder); examples and tests pass there.

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
