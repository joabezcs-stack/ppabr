test_that("cat\u00e1logo cobre as 27 UFs e os 5 ciclos", {
  cat_ppa <- ppa_catalogo()
  expect_s3_class(cat_ppa, "data.frame")
  expect_length(unique(cat_ppa$uf), 27)
  expect_setequal(unique(cat_ppa$ciclo),
                  c("2008-2011", "2012-2015", "2016-2019", "2020-2023", "2024-2027"))
})

test_that("filtros do cat\u00e1logo funcionam", {
  ce <- ppa_catalogo(uf = "ce", ciclo = 2024)
  expect_true(all(ce$uf == "CE"))
  expect_true(all(ce$ciclo == "2024-2027"))
  expect_identical(ppa_catalogo(uf = "CE", ciclo = "2024-2027"), ce)

  prog <- ppa_catalogo(tabela = "programa")
  expect_true(all(grepl("programa", prog$tabela)))
})

test_that("entradas inv\u00e1lidas d\u00e3o erro claro", {
  expect_error(ppa_catalogo(uf = "XX"), "UF")
  expect_error(ppa_catalogo(ciclo = "abc"), "ciclo")
  expect_error(ppa_tabela(c("CE", "BA"), 2024), "uma UF")
  expect_error(ppa_tabela("CE", 2024, "nao_existe"), "n\u00e3o existe")
})
