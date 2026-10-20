test_that("dicionario bate com o catalogo", {
  dic <- ppa_dicionario()
  cat_ppa <- ppa_catalogo()
  expect_equal(nrow(dic), sum(cat_ppa$n_colunas))
  expect_true(all(c("uf", "ciclo", "tabela", "coluna", "tipo", "preenchimento",
                    "exemplo", "descricao", "origem_descricao") %in% names(dic)))
  expect_true(all(dic$origem_descricao %in% c("manual", "regra", NA)))
  expect_true(all(dic$preenchimento >= 0 & dic$preenchimento <= 1))
})

test_that("filtros do dicionario funcionam", {
  ce <- ppa_dicionario("^programas$", uf = "ce", ciclo = 2024)
  expect_true(all(ce$uf == "CE" & ce$ciclo == "2024-2027" & ce$tabela == "programas"))
  expect_equal(
    ce$coluna,
    strsplit(ppa_catalogo(uf = "CE", ciclo = 2024, tabela = "^programas$")$colunas, ", ")[[1]]
  )
  expect_true(all(grepl("publico", ppa_dicionario(coluna = "publico")$coluna)))
  expect_error(ppa_dicionario(uf = "XX"), "UF")
})

test_that("colunas comuns tem descricao", {
  res <- ppa_dicionario(resumo = TRUE)
  expect_equal(anyDuplicated(res$coluna), 0)
  expect_equal(res$n_ppas[res$coluna == "uf"], nrow(unique(ppa_catalogo()[, c("uf", "ciclo")])))
  comuns <- res[res$n_ppas >= 10, ]
  expect_true(all(!is.na(comuns$descricao)))
})
