# Inventário das tabelas de indicadores (apoio para espec_indicadores.R): grava data-raw/inv_indicadores.txt
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
out <- c()
todas_cols <- c()
for (p in unique(paste0(catalogo_ppa$uf, "_", catalogo_ppa$ciclo))) {
  tb <- readRDS(file.path(rel, paste0(p, ".rds")))
  nm <- names(tb)[grepl("indicador|resultados_pactuados", names(tb)) |
                    vapply(tb, function(t) any(c("indicador", "nome_indicador") %in% names(t)), logical(1))]
  out <- c(out, paste0("### ", p, if (!length(nm)) "  (SEM INDICADORES)"))
  for (n in nm) {
    t <- tb[[n]]
    todas_cols <- c(todas_cols, names(t))
    out <- c(out, sprintf("  %s [%d]: %s", n, nrow(t), paste(setdiff(names(t), c("uf", "ppa", "pagina_pdf")), collapse = ", ")))
  }
}
writeLines(out, file.path("data-raw", "inv_indicadores.txt"), useBytes = TRUE)
cat(sum(grepl("SEM INDICADORES", out)), "PPAs sem indicadores\n")
tc <- sort(table(todas_cols), decreasing = TRUE)
cat(paste(names(tc), tc, sep = "=", collapse = "  "), "\n")
