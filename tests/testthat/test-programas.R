test_that("resumo dos programas funciona sem internet", {
  res <- ppa_programas(resumo = TRUE)
  expect_s3_class(res, "data.frame")
  expect_true(all(res$versao == "original"))
  expect_gt(length(unique(paste(res$uf, res$ciclo))), 120)
  expect_true(all(res$n_programas > 0))

  ce <- ppa_programas(uf = "ce", ciclo = 2024, resumo = TRUE)
  expect_equal(nrow(ce), 1)
  expect_equal(ce$uf, "CE")
})

test_that("versoes do resumo", {
  todas <- ppa_programas(resumo = TRUE, versao = "todas")
  rec <- ppa_programas(resumo = TRUE, versao = "mais_recente")
  orig <- ppa_programas(resumo = TRUE)
  expect_gt(nrow(todas), nrow(orig))
  expect_equal(nrow(rec), nrow(orig))
  expect_true(any(rec$versao == "revisada"))
  expect_error(ppa_programas(uf = "XX", resumo = TRUE), "UF")
})

test_that("ppa_programas baixa a camada harmonizada", {
  skip_on_cran()
  prog <- ppa_programas(uf = "CE", ciclo = 2024)
  skip_if(is.null(prog), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_equal(nrow(prog), ppa_programas(uf = "CE", ciclo = 2024, resumo = TRUE)$n_programas)
  expect_true(all(c("programa_codigo", "programa", "objetivo", "pagina_pdf") %in% names(prog)))
})
