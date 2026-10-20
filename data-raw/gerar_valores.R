# Gera a camada harmonizada de valores a partir dos arquivos de data-raw/release
# (rodar depois de gerar_dados.R e gerar_programas.R, com o diretório de trabalho em ppabr/):
#   - data-raw/release/valores.rds : valor total de cada programa por período, em reais
#   - R/sysdata.rda                : acrescenta cobertura_valores (resumo por PPA)
# A especificação (de onde vêm os valores de cada PPA) está em espec_valores.R.

source(file.path("data-raw", "espec_programas.R"), encoding = "UTF-8")
source(file.path("data-raw", "espec_valores.R"), encoding = "UTF-8")
source(file.path("data-raw", "funcoes_harmonizar.R"), encoding = "UTF-8")
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")

# período do plano quando difere do nome do ciclo (o arquivo MA 2008-2011 é o PPA 2010-2013;
# o MG 2024-2027 é a revisão de 2024, com anos 2025-2028)
periodo_ppa <- c("MA_2008-2011" = "2010-2013", "MG_2024-2027" = "2025-2028")

# "2024", "2025-2027", "2025 a 2027" -> c(inicio, fim)
anos <- function(x) {
  a <- regmatches(x, gregexpr("\\d{4}", x))
  t(vapply(a, function(v) if (length(v) == 0) c(NA, NA) else as.integer(c(v[1], v[length(v)])), integer(2)))
}

ler_parte <- function(tabelas, pt, p) {
  t <- tabelas[[pt$fonte]]
  if (is.null(t)) stop(p, ": tabela ", pt$fonte, " não existe")
  if (!is.null(pt$filtro)) t <- t[with(t, eval(parse(text = pt$filtro))) %in% TRUE, , drop = FALSE]
  if (nrow(t) == 0) {
    warning(p, ": o filtro não deixou linhas em ", pt$fonte, call. = FALSE)
    return(NULL)
  }
  m <- mapear(t, if (is.null(pt$mapa)) list() else pt$mapa, exige_nome = FALSE)
  m <- m[!is.na(m$programa_codigo) | !is.na(m$programa), , drop = FALSE]
  linhas <- as.integer(rownames(m))
  m$chave <- chave(m)
  ppa_txt <- if (p %in% names(periodo_ppa)) periodo_ppa[[p]] else substr(p, 4, 12)

  if (!is.null(pt$ano)) {
    longo <- data.frame(m, periodo = as.character(t[[pt$ano]][linhas]), coluna_origem = pt$valor,
                        valor = as.numeric(t[[pt$valor]][linhas]))
  } else {
    longo <- do.call(rbind, lapply(names(pt$valores), function(cl) {
      per <- pt$valores[[cl]]
      data.frame(m, periodo = if (per == "ppa") ppa_txt else per, coluna_origem = cl,
                 valor = as.numeric(t[[cl]][linhas]))
    }))
  }
  longo <- longo[!is.na(longo$valor), , drop = FALSE]
  if (nrow(longo) == 0) {
    warning(p, ": nenhuma linha de valor em ", pt$fonte, call. = FALSE)
    return(NULL)
  }
  if (isTRUE(pt$dedup)) longo <- longo[!duplicated(longo[, c("chave", "periodo", "valor")]), , drop = FALSE]
  longo$valor <- longo$valor * (if (is.null(pt$escala)) 1 else pt$escala)
  longo$unidade_original <- if (identical(pt$escala, 1000)) "R$ mil" else "R$"
  longo$calculo <- if (identical(pt$nivel, "acao")) "soma das ações" else "quadro do programa"
  longo$parcial <- isTRUE(pt$parcial)
  longo$tabela_origem <- pt$fonte
  longo
}

# soma por programa e período
somar <- function(d) {
  g <- split(seq_len(nrow(d)), factor(paste(d$chave, d$periodo), levels = unique(paste(d$chave, d$periodo))))
  do.call(rbind, lapply(g, function(i) {
    data.frame(
      programa_codigo = d$programa_codigo[i][!is.na(d$programa_codigo[i])][1],
      programa = escolher_nome(d$programa[i]),
      chave = d$chave[i[1]], periodo = d$periodo[i[1]], valor = sum(d$valor[i]),
      unidade_original = d$unidade_original[i[1]], calculo = d$calculo[i[1]], parcial = d$parcial[i[1]],
      pagina_pdf = suppressWarnings(as.integer(min(d$pagina_pdf[i], na.rm = TRUE))),
      tabela_origem = d$tabela_origem[i[1]], coluna_origem = juntar(d$coluna_origem[i])
    )
  }))
}

montar <- function(tabelas, partes, p) {
  lidas <- do.call(rbind, lapply(partes, ler_parte, tabelas = tabelas, p = p))
  if (is.null(lidas)) return(NULL)
  d <- somar(lidas)
  a <- anos(d$periodo)
  d$ano_inicio <- a[, 1]
  d$ano_fim <- a[, 2]
  d$periodo <- ifelse(d$ano_inicio == d$ano_fim, as.character(d$ano_inicio), paste0(d$ano_inicio, "-", d$ano_fim))
  ap <- anos(if (p %in% names(periodo_ppa)) periodo_ppa[[p]] else substr(p, 4, 12))
  d$periodo_completo <- d$ano_inicio == ap[1, 1] & d$ano_fim == ap[1, 2]

  # total do plano somando os períodos, quando o documento não traz o total e os
  # períodos cobrem o plano inteiro sem sobreposição
  extra <- lapply(split(d, d$chave), function(x) {
    if (any(x$periodo_completo)) return(NULL)
    x <- x[order(x$ano_inicio), ]
    contiguo <- all(x$ano_inicio[-1] == x$ano_fim[-nrow(x)] + 1)
    if (!contiguo || x$ano_inicio[1] != ap[1, 1] || x$ano_fim[nrow(x)] != ap[1, 2]) return(NULL)
    y <- x[1, ]
    y$periodo <- paste0(ap[1, 1], "-", ap[1, 2])
    y$ano_inicio <- ap[1, 1]
    y$ano_fim <- ap[1, 2]
    y$valor <- sum(x$valor)
    y$periodo_completo <- TRUE
    y$calculo <- "soma dos períodos"
    y$coluna_origem <- juntar(x$coluna_origem)
    y
  })
  rbind(d, do.call(rbind, extra))
}

saida <- list()
for (p in names(espec_valores)) {
  tabelas <- readRDS(file.path(rel, paste0(p, ".rds")))
  es <- espec_valores[[p]]
  nomes <- if (is.null(names(es))) rep("", length(es)) else names(es)
  versoes <- list(original = es[nomes == ""])
  if ("revisada" %in% nomes) versoes$revisada <- es[["revisada"]]
  for (v in names(versoes)) {
    d <- montar(tabelas, versoes[[v]], p)
    if (is.null(d)) next
    saida[[paste(p, v)]] <- data.frame(uf = substr(p, 1, 2), ciclo = substr(p, 4, 12), versao = v, d)
  }
}

valores <- do.call(rbind, saida)
rownames(valores) <- NULL
valores$regiao <- catalogo_ppa$regiao[match(valores$uf, catalogo_ppa$uf)]
valores <- valores[order(valores$uf, valores$ciclo, valores$versao, valores$chave, valores$ano_inicio, -valores$ano_fim), ]
valores <- valores[, c("uf", "regiao", "ciclo", "versao", "programa_codigo", "programa", "periodo", "ano_inicio",
                       "ano_fim", "periodo_completo", "valor", "unidade_original", "calculo", "parcial",
                       "pagina_pdf", "tabela_origem", "coluna_origem")]
rownames(valores) <- NULL
saveRDS(valores, file.path(rel, "valores.rds"), compress = "xz")

# resumo por PPA: número de programas com valor e total do plano (em R$ bilhões)
k <- paste(valores$uf, valores$ciclo, valores$versao)
cobertura_valores <- do.call(rbind, lapply(split(valores, factor(k, levels = unique(k))), function(d) {
  tot <- d[d$periodo_completo, ]
  data.frame(uf = d$uf[1], ciclo = d$ciclo[1], versao = d$versao[1],
             n_programas = length(unique(paste(d$programa_codigo, d$programa))),
             periodos = paste(unique(d$periodo[!d$periodo_completo]), collapse = ", "),
             total_bilhoes = if (nrow(tot)) round(sum(tot$valor) / 1e9, 2) else NA_real_,
             calculo = juntar(d$calculo), parcial = any(d$parcial))
}))
k_ppa <- paste0(cobertura_valores$uf, "_", cobertura_valores$ciclo)
cobertura_valores$alerta <- unname(ifelse(k_ppa %in% names(alertas_valores), alertas_valores[k_ppa], NA_character_))
rownames(cobertura_valores) <- NULL
# mantém o que já estiver no sysdata (ex.: documentos_ppa, de gerar_documentos.R)
save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes", "cobertura_indicadores", "cobertura_metas", "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")

message(nrow(valores), " linhas de valor em ", length(unique(paste(valores$uf, valores$ciclo))), " PPAs. ",
        "valores.rds: ", round(file.size(file.path(rel, "valores.rds")) / 1024^2, 2), " MB.")
