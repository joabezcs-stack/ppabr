# Gera a camada harmonizada de ações (rodar depois de gerar_programas.R, com o diretório de trabalho em ppabr/):
#   - data-raw/release/acoes.rds : uma linha por ação (ou subação) de cada programa de cada PPA
#   - R/sysdata.rda              : acrescenta cobertura_acoes (resumo por PPA)
# A especificação (de onde vêm as ações de cada PPA) está em espec_acoes.R.

source(file.path("data-raw", "espec_programas.R"), encoding = "UTF-8")   # campos/sinonimos dos programas
source(file.path("data-raw", "funcoes_harmonizar.R"), encoding = "UTF-8") # norm, norm_codigo, juntar, escolher_nome
source(file.path("data-raw", "espec_acoes.R"), encoding = "UTF-8")
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")
programas <- readRDS(file.path(rel, "programas.rds"))

mapear_acao <- function(t, mapa = list()) {
  out <- data.frame(row.names = seq_len(nrow(t)))
  for (cp in campos_acao) {
    col <- if (cp %in% names(mapa)) mapa[[cp]] else intersect(sinonimos_acao[[cp]], names(t))[1]
    out[[cp]] <- if (is.na(col) || !col %in% names(t)) NA_character_ else trimws(as.character(t[[col]]))
  }
  out$pagina_pdf <- if ("pagina_pdf" %in% names(t)) as.integer(t$pagina_pdf) else NA_integer_
  p <- separa_codigo(out$programa_codigo, out$programa); out$programa_codigo <- p$cod; out$programa <- p$nome
  a <- separa_codigo(out$acao_codigo, out$acao); out$acao_codigo <- a$cod; out$acao <- a$nome
  total <- !is.na(out$acao) & grepl("^(SUB)?TOTA(L|IS)\\b", norm(out$acao))
  out[(!is.na(out$acao) | !is.na(out$acao_codigo)) & !total, , drop = FALSE]
}


agregar_acoes <- function(d) {
  kp <- ifelse(!is.na(norm_codigo(d$programa_codigo)), paste0("c:", norm_codigo(d$programa_codigo)),
               paste0("n:", norm(d$programa)))
  ka <- ifelse(!is.na(norm_codigo(d$acao_codigo)), paste0("c:", norm_codigo(d$acao_codigo)), paste0("n:", norm(d$acao)))
  k <- paste(kp, ka, sep = "|")
  grupos <- split(seq_len(nrow(d)), factor(k, levels = unique(k)))
  do.call(rbind, lapply(grupos, function(i) {
    linha <- data.frame(
      nivel = d$nivel[i[1]],
      programa_codigo = d$programa_codigo[i][!is.na(d$programa_codigo[i])][1],
      programa = escolher_nome(d$programa[i]),
      acao_codigo = d$acao_codigo[i][!is.na(d$acao_codigo[i])][1],
      acao = escolher_nome(d$acao[i])
    )
    for (cp in c("tipo_acao", "orgao_responsavel", "finalidade", "produto", "unidade_medida")) linha[[cp]] <- juntar(d[[cp]][i])
    linha$pagina_pdf <- suppressWarnings(min(d$pagina_pdf[i], na.rm = TRUE))
    if (!is.finite(linha$pagina_pdf)) linha$pagina_pdf <- NA_integer_
    linha$tabela_origem <- juntar(d$tabela[i])
    linha
  }))
}

montar_acoes <- function(tabelas, partes, p_ppa, ppa) {
  d <- do.call(rbind, lapply(partes, function(pt) {
    t <- tabelas[[pt$fonte]]
    if (is.null(t)) stop(ppa, ": tabela ", pt$fonte, " não existe")
    if (!is.null(pt$filtro)) t <- t[with(t, eval(parse(text = pt$filtro))) %in% TRUE, , drop = FALSE]
    m <- mapear_acao(t, if (is.null(pt$mapa)) list() else pt$mapa)
    m$tabela <- rep(pt$fonte, nrow(m))
    m$nivel <- rep(if (is.null(pt$nivel)) "ação" else pt$nivel, nrow(m))
    m
  }))
  if (is.null(d) || nrow(d) == 0) return(NULL)
  agregar_acoes(completar_programa(d, p_ppa))
}

saida <- list()
for (p in names(espec_acoes)) {
  tabelas <- readRDS(file.path(rel, paste0(p, ".rds")))
  uf <- substr(p, 1, 2); ciclo <- substr(p, 4, 12)
  es <- espec_acoes[[p]]
  nomes <- if (is.null(names(es))) rep("", length(es)) else names(es)
  versoes <- list(original = es[nomes == ""])
  if ("revisada" %in% nomes) versoes$revisada <- es[["revisada"]]
  for (v in names(versoes)) {
    p_ppa <- programas[programas$uf == uf & programas$ciclo == ciclo & programas$versao %in% c(v, "original"), ]
    p_ppa <- p_ppa[!duplicated(p_ppa$programa_codigo) | is.na(p_ppa$programa_codigo), ]
    d <- montar_acoes(tabelas, versoes[[v]], p_ppa, p)
    if (is.null(d)) { warning(p, " ", v, ": nenhuma ação", call. = FALSE); next }
    saida[[paste(p, v)]] <- data.frame(uf = uf, ciclo = ciclo, versao = v, d)
  }
  message(p)
}

acoes <- do.call(rbind, saida)
rownames(acoes) <- NULL
acoes$regiao <- catalogo_ppa$regiao[match(acoes$uf, catalogo_ppa$uf)]
acoes <- acoes[, c("uf", "regiao", "ciclo", "versao", "nivel", campos_acao, "pagina_pdf", "tabela_origem")]
saveRDS(acoes, file.path(rel, "acoes.rds"), compress = "xz")

k <- paste(acoes$uf, acoes$ciclo, acoes$versao)
cobertura_acoes <- do.call(rbind, lapply(unname(split(acoes, factor(k, levels = unique(k)))), function(d) {
  pre <- vapply(c("programa_codigo", "programa", "acao_codigo", "tipo_acao", "orgao_responsavel", "finalidade",
                  "produto", "unidade_medida"), function(cp) round(mean(!is.na(d[[cp]])), 2), numeric(1))
  data.frame(uf = d$uf[1], ciclo = d$ciclo[1], versao = d$versao[1], nivel = juntar(d$nivel),
             n_acoes = nrow(d), n_programas = length(unique(paste(d$programa_codigo, d$programa))), t(pre),
             tabela_origem = juntar(unlist(strsplit(d$tabela_origem, " | ", fixed = TRUE))))
}))
names(cobertura_acoes)[names(cobertura_acoes) == "programa_codigo"] <- "com_codigo_programa"
names(cobertura_acoes)[names(cobertura_acoes) == "acao_codigo"] <- "com_codigo_acao"
rownames(cobertura_acoes) <- NULL

save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes", "cobertura_indicadores", "cobertura_metas",
                        "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")
message(nrow(acoes), " ações em ", length(unique(paste(acoes$uf, acoes$ciclo))), " PPAs. acoes.rds: ",
        round(file.size(file.path(rel, "acoes.rds")) / 1024^2, 2), " MB.")
