# Inventário das tabelas com ações em cada PPA (apoio para espec_acoes.R): grava data-raw/inv_acoes.txt
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
re_acao <- "^(acao|acao_nome|nome_acao|titulo_acao|acao_orcamentaria|acao_programatica|acoes)$"
out <- c()
for (p in unique(paste0(catalogo_ppa$uf, "_", catalogo_ppa$ciclo))) {
  tb <- readRDS(file.path(rel, paste0(p, ".rds")))
  linhas <- c()
  for (nm in names(tb)) {
    t <- tb[[nm]]
    ac <- grep(re_acao, names(t), value = TRUE)
    if (!length(ac)) next
    cod <- intersect(c("acao_codigo", "codigo_acao", "cod_acao"), names(t))[1]
    nd <- length(unique(if (!is.na(cod)) paste(t[[cod]], t[[ac[1]]]) else t[[ac[1]]]))
    linhas <- c(linhas, sprintf("  %s [%d linhas, %d acoes via %s/%s]: %s", nm, nrow(t), nd, ac[1], cod,
                                paste(setdiff(names(t), c("uf", "ppa", "pagina_pdf")), collapse = ", ")))
  }
  out <- c(out, paste0("### ", p, if (!length(linhas)) "  (SEM ACOES)"), linhas)
}
writeLines(out, file.path("data-raw", "inv_acoes.txt"), useBytes = TRUE)
cat(sum(grepl("SEM ACOES", out)), "PPAs sem tabela de acoes\n")
cat(grep("SEM ACOES", out, value = TRUE), sep = "\n")
