test_that("resumo dos valores funciona sem internet", {
  res <- ppa_valores(resumo = TRUE)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("uf", "ciclo", "versao", "n_programas", "total_bilhoes", "alerta") %in% names(res)))
  expect_true(all(res$versao == "original"))
  expect_gt(nrow(res), 110)
  expect_true(all(res$total_bilhoes > 0, na.rm = TRUE))
  ap <- res[res$uf == "AP" & res$ciclo == "2024-2027", ]
  expect_equal(nrow(ap), 1)
  expect_false(is.na(ap$alerta))

  pe <- ppa_valores(uf = "pe", ciclo = 2024, resumo = TRUE)
  expect_equal(nrow(pe), 1)
  expect_error(ppa_valores(uf = "XX", resumo = TRUE), "UF")
})

test_that("ppa_valores baixa a camada harmonizada", {
  skip_on_cran()
  v <- ppa_valores(uf = "PE", ciclo = 2024)
  skip_if(is.null(v), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_true(all(v$valor >= 0))
  tot <- sum(v$valor[v$periodo_completo]) / 1e9
  expect_equal(round(tot, 2), ppa_valores(uf = "PE", ciclo = 2024, resumo = TRUE)$total_bilhoes)
})
