test_that("resumo das acoes funciona sem internet", {
  res <- ppa_acoes(resumo = TRUE)
  expect_s3_class(res, "data.frame")
  expect_true(all(res$versao == "original"))
  expect_gt(length(unique(paste(res$uf, res$ciclo))), 90)
  expect_true(all(res$n_acoes > 0))
  expect_true(any(grepl("suba", res$nivel)))
  expect_error(ppa_acoes(uf = "XX", resumo = TRUE), "UF")
})

test_that("ppa_acoes baixa a camada harmonizada", {
  skip_on_cran()
  a <- ppa_acoes(uf = "MG", ciclo = 2024)
  skip_if(is.null(a), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_equal(nrow(a), ppa_acoes(uf = "MG", ciclo = 2024, resumo = TRUE)$n_acoes)
  expect_true(all(c("programa", "acao", "produto", "pagina_pdf") %in% names(a)))
  expect_false(any(is.na(a$acao) & is.na(a$acao_codigo)))
})
