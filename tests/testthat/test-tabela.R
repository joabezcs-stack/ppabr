test_that("ppa_tabela l\u00ea um PPA do reposit\u00f3rio", {
  skip_on_cran()

  ce <- ppa_tabela("CE", 2024)
  skip_if(is.null(ce), "reposit\u00f3rio de dados indispon\u00edvel")
  expect_type(ce, "list")
  expect_setequal(names(ce), ppa_catalogo(uf = "CE", ciclo = 2024)$tabela)

  prog <- ppa_tabela("CE", 2024, "programas")
  expect_s3_class(prog, "data.frame")
  expect_true(all(c("uf", "ppa", "pagina_pdf") %in% names(prog)))
})

test_that("sem conex\u00e3o, ppa_tabela avisa e devolve NULL", {
  withr_local <- options(ppabr.url_dados = "http://127.0.0.1:1/inexistente")
  on.exit(options(withr_local))
  cache <- file.path(tempdir(), "ppabr_cache_teste")
  local_mocked_bindings(pasta_cache = function() cache)

  expect_message(res <- ppa_tabela("CE", 2024, atualizar = TRUE), "N\u00e3o foi poss\u00edvel")
  expect_null(res)
})
