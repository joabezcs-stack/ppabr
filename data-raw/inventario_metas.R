# Inventário das tabelas de metas, iniciativas, entregas, compromissos, prioridades e produtos (apoio para
# espec_metas.R): grava data-raw/inv_metas.txt
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
re_tab <- "meta|iniciativ|entrega|compromisso|prioridad|produto|desafio|objetivos?(_|$)|resultados_pactuados|propostas"
out <- c()
for (p in unique(paste0(catalogo_ppa$uf, "_", catalogo_ppa$ciclo))) {
  tb <- readRDS(file.path(rel, paste0(p, ".rds")))
  nm <- names(tb)[grepl(re_tab, names(tb)) & !grepl("indicador", names(tb))]
  out <- c(out, paste0("### ", p, if (!length(nm)) "  (NADA)"))
  for (n in nm) {
    t <- tb[[n]]
    out <- c(out, sprintf("  %s [%d]: %s", n, nrow(t), paste(setdiff(names(t), c("uf", "ppa", "pagina_pdf")), collapse = ", ")))
  }
}
writeLines(out, file.path("data-raw", "inv_metas.txt"), useBytes = TRUE)
cat(sum(grepl("\\(NADA\\)", out)), "PPAs sem tabelas candidatas\n")
