test_that("lista de documentos funciona sem internet", {
  d <- ppa_documentos()
  expect_s3_class(d, "data.frame")
  expect_true(all(c("documento", "arquivo_original", "paginas", "paginas_texto", "paginas_ocr",
                    "paginas_sem_texto") %in% names(d)))
  expect_equal(d$paginas, d$paginas_texto + d$paginas_ocr + d$paginas_sem_texto)
  expect_true(all(grepl("^[A-Z]{2}_\\d{4}-\\d{4}_\\d{2}\\.pdf$", d$documento)))
  sp <- ppa_documentos(uf = "sp", ciclo = 2024)
  expect_true(all(sp$uf == "SP" & sp$ciclo == "2024-2027"))
})

test_that("tabelas apontam para documentos existentes", {
  cat_ppa <- ppa_catalogo()
  doc <- cat_ppa$documento[!is.na(cat_ppa$documento)]
  expect_true(all(doc %in% ppa_documentos()$documento))
})

test_that("ppa_documento valida entradas", {
  expect_error(ppa_documento(c("CE", "SP"), 2024), "uma UF")
  expect_error(ppa_documento("CE", 2024, documento = "nao_existe.pdf"), "n\u00e3o encontrado")
})

test_that("ppa_documento baixa o texto por p\u00e1gina", {
  skip_on_cran()
  txt <- ppa_documento("CE", 2024, paginas = 1:3)
  skip_if(is.null(txt), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_true(all(c("documento", "pagina", "texto", "origem_texto") %in% names(txt)))
  expect_true(all(txt$pagina %in% 1:3))
})
