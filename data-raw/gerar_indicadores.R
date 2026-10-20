# Gera a camada harmonizada de indicadores (rodar depois de gerar_programas.R, com o diretório de trabalho
# em ppabr/):
#   - data-raw/release/indicadores.rds : uma linha por indicador (por programa, nível e recorte regional)
#   - R/sysdata.rda                    : acrescenta cobertura_indicadores (resumo por PPA)
# A especificação está em espec_indicadores.R. Os valores (índice de referência, metas) ficam como texto:
# os documentos misturam números, percentuais e expressões ("ND", "a apurar", "Aumentar").

source(file.path("data-raw", "espec_programas.R"), encoding = "UTF-8")
source(file.path("data-raw", "funcoes_harmonizar.R"), encoding = "UTF-8")
source(file.path("data-raw", "espec_indicadores.R"), encoding = "UTF-8")
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
programas <- readRDS(file.path(rel, "programas.rds"))

re_meta_ano <- "^(meta|indice_esperado|esperado|indice_desejado|desejado|indice|indice_previsto)_(\\d{4})$"
txt <- function(v) {
  if (is.numeric(v)) v <- format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE, big.mark = "", decimal.mark = ",")
  v <- trimws(as.character(v))
  v[v %in% c("", "NA")] <- NA
  v
}

ler_parte <- function(t, pt, inicio_ciclo) {
  mapa <- if (is.null(pt$mapa)) list() else pt$mapa
  out <- data.frame(row.names = seq_len(nrow(t)))
  usadas <- character(0)
  for (cp in campos_ind) {
    cands <- if (cp %in% names(mapa)) mapa[[cp]] else setdiff(intersect(sinonimos_ind[[cp]], names(t)), usadas)
    col <- cands[1]
    out[[cp]] <- if (is.na(col) || !col %in% names(t)) NA_character_ else txt(t[[col]])
    if (!is.na(col)) usadas <- c(usadas, col)
  }
  out$pagina_pdf <- if ("pagina_pdf" %in% names(t)) as.integer(t$pagina_pdf) else NA_integer_
  out$nivel <- if (is.null(pt$nivel)) "programa" else if (startsWith(pt$nivel, "col:")) txt(t[[sub("^col:", "", pt$nivel)]]) else pt$nivel
  # metas por ano
  cols <- grep(re_meta_ano, names(t), value = TRUE)
  anos <- sub(re_meta_ano, "\\2", cols)
  cols_n <- grep("^desejado_ano_\\d$", names(t), value = TRUE)
  cols <- c(cols, cols_n)
  anos <- c(anos, inicio_ciclo + as.integer(sub("^desejado_ano_", "", cols_n)) - 1L)
  if (!is.null(pt$longo)) {
    out$metas <- ifelse(is.na(t[[pt$longo[["valor"]]]]), NA,
                        paste0(txt(t[[pt$longo[["ano"]]]]), ": ", txt(t[[pt$longo[["valor"]]]])))
  } else if (length(cols)) {
    m <- vapply(seq_along(cols), function(j) ifelse(is.na(txt(t[[cols[j]]])), NA, paste0(anos[j], ": ", txt(t[[cols[j]]]))),
                character(nrow(t)))
    m <- matrix(m, nrow = nrow(t))
    out$metas <- apply(m, 1, function(r) if (all(is.na(r))) NA_character_ else paste(r[!is.na(r)], collapse = " | "))
  } else {
    out$metas <- NA_character_
  }
  out$indicador <- trimws(sub("\\s*Unidade (de )?Medida:?\\s*$", "", out$indicador))
  p <- separa_codigo(out$programa_codigo, out$programa)
  out$programa_codigo <- p$cod
  out$programa <- p$nome
  out[!is.na(out$indicador) & grepl("\\p{L}", out$indicador, perl = TRUE), , drop = FALSE]
}

agregar_ind <- function(d) {
  kp <- ifelse(!is.na(norm_codigo(d$programa_codigo)), paste0("c:", norm_codigo(d$programa_codigo)),
               paste0("n:", norm(d$programa)))
  k <- paste(d$nivel, kp, norm(d$indicador), norm(d$recorte), sep = "|")
  grupos <- unname(split(seq_len(nrow(d)), factor(k, levels = unique(k))))
  do.call(rbind, lapply(grupos, function(i) {
    linha <- data.frame(
      nivel = d$nivel[i[1]],
      programa_codigo = d$programa_codigo[i][!is.na(d$programa_codigo[i])][1],
      programa = escolher_nome(d$programa[i]),
      indicador = escolher_nome(d$indicador[i])
    )
    for (cp in setdiff(campos_ind, c("programa_codigo", "programa", "indicador"))) linha[[cp]] <- juntar(d[[cp]][i])
    # metas por ano: pares "ano: valor" únicos, em ordem de ano
    pares <- unique(trimws(unlist(strsplit(d$metas[i][!is.na(d$metas[i])], " | ", fixed = TRUE))))
    pares <- pares[order(substr(pares, 1, 4))]
    linha$metas_anuais <- if (length(pares)) paste(pares, collapse = " | ") else NA_character_
    # sem meta final no documento: a meta do último ano
    if (is.na(linha$meta_final) && length(pares)) linha$meta_final <- sub("^[^:]*:\\s*", "", pares[length(pares)])
    linha$pagina_pdf <- suppressWarnings(min(d$pagina_pdf[i], na.rm = TRUE))
    if (!is.finite(linha$pagina_pdf)) linha$pagina_pdf <- NA_integer_
    linha$tabela_origem <- juntar(d$tabela[i])
    linha
  }))
}

saida <- list()
ppas <- unique(paste0(catalogo_ppa$uf, "_", catalogo_ppa$ciclo))
for (p in ppas) {
  tabelas <- readRDS(file.path(rel, paste0(p, ".rds")))
  if (p %in% names(espec_indicadores)) {
    es <- espec_indicadores[[p]]
    if (is.null(es)) next
  } else if ("indicadores" %in% names(tabelas)) {
    es <- list(list(fonte = "indicadores"))
  } else next
  uf <- substr(p, 1, 2); ciclo <- substr(p, 4, 12); inicio <- as.integer(substr(ciclo, 1, 4))
  nomes <- if (is.null(names(es))) rep("", length(es)) else names(es)
  versoes <- list(original = es[nomes == ""])
  if ("revisada" %in% nomes) versoes$revisada <- es[["revisada"]]
  for (v in names(versoes)) {
    d <- do.call(rbind, lapply(versoes[[v]], function(pt) {
      t <- tabelas[[pt$fonte]]
      if (is.null(t)) stop(p, ": tabela ", pt$fonte, " não existe")
      x <- ler_parte(t, pt, inicio)
      x$tabela <- rep(pt$fonte, nrow(x))
      x
    }))
    if (is.null(d) || !nrow(d)) next
    p_ppa <- programas[programas$uf == uf & programas$ciclo == ciclo & programas$versao %in% c(v, "original"), ]
    d <- agregar_ind(completar_programa(d, p_ppa))
    saida[[paste(p, v)]] <- data.frame(uf = uf, ciclo = ciclo, versao = v, d)
  }
  message(p)
}

indicadores <- do.call(rbind, saida)
rownames(indicadores) <- NULL
indicadores$regiao <- catalogo_ppa$regiao[match(indicadores$uf, catalogo_ppa$uf)]
indicadores <- indicadores[, c("uf", "regiao", "ciclo", "versao", "nivel", "programa_codigo", "programa", "indicador",
                               "tipo_indicador", "unidade_medida", "fonte", "periodicidade", "polaridade", "recorte",
                               "indice_referencia", "data_referencia", "meta_final", "metas_anuais", "pagina_pdf",
                               "tabela_origem")]
saveRDS(indicadores, file.path(rel, "indicadores.rds"), compress = "xz")

k <- paste(indicadores$uf, indicadores$ciclo, indicadores$versao)
cobertura_indicadores <- do.call(rbind, lapply(unname(split(indicadores, factor(k, levels = unique(k)))), function(d) {
  pre <- vapply(c("programa", "unidade_medida", "fonte", "indice_referencia", "data_referencia", "meta_final",
                  "metas_anuais", "recorte"), function(cp) round(mean(!is.na(d[[cp]])), 2), numeric(1))
  data.frame(uf = d$uf[1], ciclo = d$ciclo[1], versao = d$versao[1], niveis = juntar(d$nivel),
             n_indicadores = nrow(d), t(pre),
             tabela_origem = juntar(unlist(strsplit(d$tabela_origem, " | ", fixed = TRUE))))
}))
rownames(cobertura_indicadores) <- NULL

save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes",
                        "cobertura_indicadores", "cobertura_metas", "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")
message(nrow(indicadores), " indicadores em ", length(unique(paste(indicadores$uf, indicadores$ciclo))), " PPAs. ",
        "indicadores.rds: ", round(file.size(file.path(rel, "indicadores.rds")) / 1024^2, 2), " MB.")
