test_that("resumo dos indicadores funciona sem internet", {
  res <- ppa_indicadores(resumo = TRUE)
  expect_s3_class(res, "data.frame")
  expect_true(all(res$versao == "original"))
  expect_gt(length(unique(paste(res$uf, res$ciclo))), 80)
  expect_true(all(res$n_indicadores > 0))
  expect_error(ppa_indicadores(uf = "XX", resumo = TRUE), "UF")
})

test_that("ppa_indicadores baixa a camada harmonizada", {
  skip_on_cran()
  ind <- ppa_indicadores(uf = "PR", ciclo = 2024)
  skip_if(is.null(ind), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_equal(nrow(ind), ppa_indicadores(uf = "PR", ciclo = 2024, resumo = TRUE)$n_indicadores)
  expect_true(all(c("indicador", "indice_referencia", "meta_final", "metas_anuais") %in% names(ind)))
  expect_false(any(is.na(ind$indicador)))
  ok <- !is.na(ind$metas_anuais)
  expect_true(all(grepl("^\\d{4}: ", ind$metas_anuais[ok])))
})
