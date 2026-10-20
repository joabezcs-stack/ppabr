# Gera a camada harmonizada de metas, iniciativas e entregas (rodar depois de gerar_programas.R, com o
# diretório de trabalho em ppabr/):
#   - data-raw/release/metas.rds : uma linha por item (meta, iniciativa, entrega, compromisso, produto...)
#   - R/sysdata.rda              : acrescenta cobertura_metas (resumo por PPA)
# A especificação está em espec_metas.R. Quantidades e metas por ano ficam como texto (como no documento).

source(file.path("data-raw", "espec_programas.R"), encoding = "UTF-8")
source(file.path("data-raw", "funcoes_harmonizar.R"), encoding = "UTF-8")
source(file.path("data-raw", "espec_metas.R"), encoding = "UTF-8")
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
programas <- readRDS(file.path(rel, "programas.rds"))

txt <- function(v) {
  if (is.numeric(v)) v <- format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE, big.mark = "", decimal.mark = ",")
  v <- trimws(as.character(v))
  v[v %in% c("", "NA")] <- NA
  v
}
re_ano <- "^(meta|qtd)_(\\d{4})$"

ler_parte <- function(t, pt, inicio_ciclo) {
  if (!is.null(pt$filtro)) t <- t[with(t, eval(parse(text = pt$filtro))) %in% TRUE, , drop = FALSE]
  mapa <- pt$mapa
  out <- data.frame(row.names = seq_len(nrow(t)))
  usadas <- unlist(mapa[!is.na(unlist(mapa))])
  for (cp in campos_meta) {
    col <- if (cp %in% names(mapa)) mapa[[cp]] else setdiff(intersect(sinonimos_meta[[cp]], names(t)), usadas)[1]
    out[[cp]] <- if (is.na(col) || !col %in% names(t)) NA_character_ else txt(t[[col]])
    if (!is.na(col) && !cp %in% names(mapa)) usadas <- c(usadas, col)
  }
  out$pagina_pdf <- if ("pagina_pdf" %in% names(t)) as.integer(t$pagina_pdf) else NA_integer_
  out$tipo <- if (startsWith(pt$tipo, "col:")) tolower(txt(t[[sub("^col:", "", pt$tipo)]])) else pt$tipo
  cols <- grep(re_ano, names(t), value = TRUE)
  anos <- sub(re_ano, "\\2", cols)
  cols_n <- grep("^meta_ano_\\d$", names(t), value = TRUE)
  cols <- c(cols, cols_n)
  anos <- c(anos, inicio_ciclo + as.integer(sub("^meta_ano_", "", cols_n)) - 1L)
  if (length(cols)) {
    mm <- matrix(vapply(seq_along(cols), function(j) ifelse(is.na(txt(t[[cols[j]]])), NA, paste0(anos[j], ": ", txt(t[[cols[j]]]))),
                        character(nrow(t))), nrow = nrow(t))
    out$metas <- apply(mm, 1, function(r) if (all(is.na(r))) NA_character_ else paste(r[!is.na(r)], collapse = " | "))
  } else {
    out$metas <- NA_character_
  }
  p <- separa_codigo(out$programa_codigo, out$programa)
  out$programa_codigo <- p$cod
  out$programa <- p$nome
  out[!is.na(out$descricao) & grepl("\\p{L}", out$descricao, perl = TRUE), , drop = FALSE]
}

agregar_metas <- function(d) {
  kp <- ifelse(!is.na(norm_codigo(d$programa_codigo)), paste0("c:", norm_codigo(d$programa_codigo)),
               paste0("n:", norm(d$programa)))
  ki <- ifelse(!is.na(norm_codigo(d$item_codigo)), paste0("c:", norm_codigo(d$item_codigo)), paste0("n:", norm(d$descricao)))
  k <- paste(d$tipo, kp, norm(d$vinculo_codigo), ki, norm(d$recorte), sep = "|")
  grupos <- unname(split(seq_len(nrow(d)), factor(k, levels = unique(k))))
  do.call(rbind, lapply(grupos, function(i) {
    linha <- data.frame(
      tipo = d$tipo[i[1]],
      programa_codigo = d$programa_codigo[i][!is.na(d$programa_codigo[i])][1],
      programa = escolher_nome(d$programa[i]),
      vinculo_codigo = d$vinculo_codigo[i][!is.na(d$vinculo_codigo[i])][1],
      vinculo = escolher_nome(d$vinculo[i]),
      item_codigo = d$item_codigo[i][!is.na(d$item_codigo[i])][1],
      descricao = escolher_nome(d$descricao[i])
    )
    for (cp in c("orgao_responsavel", "unidade_medida", "quantidade", "recorte")) linha[[cp]] <- juntar(d[[cp]][i])
    pares <- unique(trimws(unlist(strsplit(d$metas[i][!is.na(d$metas[i])], " | ", fixed = TRUE))))
    pares <- pares[order(substr(pares, 1, 4))]
    linha$metas_anuais <- if (length(pares)) paste(pares, collapse = " | ") else NA_character_
    linha$pagina_pdf <- suppressWarnings(min(d$pagina_pdf[i], na.rm = TRUE))
    if (!is.finite(linha$pagina_pdf)) linha$pagina_pdf <- NA_integer_
    linha$tabela_origem <- juntar(d$tabela[i])
    linha
  }))
}

saida <- list()
for (p in names(espec_metas)) {
  tabelas <- readRDS(file.path(rel, paste0(p, ".rds")))
  uf <- substr(p, 1, 2); ciclo <- substr(p, 4, 12); inicio <- as.integer(substr(ciclo, 1, 4))
  es <- espec_metas[[p]]
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
    d <- agregar_metas(completar_programa(d, p_ppa))
    saida[[paste(p, v)]] <- data.frame(uf = uf, ciclo = ciclo, versao = v, d)
  }
  message(p)
}

metas <- do.call(rbind, saida)
rownames(metas) <- NULL
metas$regiao <- catalogo_ppa$regiao[match(metas$uf, catalogo_ppa$uf)]
metas <- metas[, c("uf", "regiao", "ciclo", "versao", "tipo", "programa_codigo", "programa", "vinculo_codigo", "vinculo",
                   "item_codigo", "descricao", "orgao_responsavel", "unidade_medida", "quantidade", "metas_anuais",
                   "recorte", "pagina_pdf", "tabela_origem")]
saveRDS(metas, file.path(rel, "metas.rds"), compress = "xz")

k <- paste(metas$uf, metas$ciclo, metas$versao)
cobertura_metas <- do.call(rbind, lapply(unname(split(metas, factor(k, levels = unique(k)))), function(d) {
  pre <- vapply(c("programa", "vinculo", "item_codigo", "orgao_responsavel", "unidade_medida", "quantidade",
                  "metas_anuais", "recorte"), function(cp) round(mean(!is.na(d[[cp]])), 2), numeric(1))
  data.frame(uf = d$uf[1], ciclo = d$ciclo[1], versao = d$versao[1], tipos = juntar(d$tipo), n_itens = nrow(d), t(pre),
             tabela_origem = juntar(unlist(strsplit(d$tabela_origem, " | ", fixed = TRUE))))
}))
rownames(cobertura_metas) <- NULL

save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes",
                        "cobertura_indicadores", "cobertura_metas", "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")
message(nrow(metas), " itens em ", length(unique(paste(metas$uf, metas$ciclo))), " PPAs. metas.rds: ",
        round(file.size(file.path(rel, "metas.rds")) / 1024^2, 2), " MB.")
