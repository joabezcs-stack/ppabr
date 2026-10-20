# Gera a camada harmonizada de programas a partir dos arquivos de data-raw/release
# (rodar depois de gerar_dados.R, com o diretório de trabalho em ppabr/):
#   - data-raw/release/programas.rds : uma linha por programa de cada PPA (sobe no release)
#   - R/sysdata.rda                  : acrescenta cobertura_programas (resumo por PPA)
# A especificação (de onde vêm os programas de cada PPA) está em espec_programas.R.

source(file.path("data-raw", "espec_programas.R"), encoding = "UTF-8")
load(file.path("R", "sysdata.rda"))
rel <- file.path("data-raw", "release")

source(file.path("data-raw", "funcoes_harmonizar.R"), encoding = "UTF-8")


# uma linha por programa: textos diferentes do mesmo programa ficam juntos, separados por " | "
agregar <- function(d) {
  grupos <- split(seq_len(nrow(d)), factor(d$chave, levels = unique(d$chave)))
  do.call(rbind, lapply(grupos, function(i) {
    linha <- data.frame(
      programa_codigo = d$programa_codigo[i][!is.na(d$programa_codigo[i])][1],
      programa = escolher_nome(d$programa[i])
    )
    for (cp in setdiff(campos, c("programa_codigo", "programa"))) linha[[cp]] <- juntar(d[[cp]][i])
    linha$pagina_pdf <- suppressWarnings(min(d$pagina_pdf[i], na.rm = TRUE))
    if (!is.finite(linha$pagina_pdf)) linha$pagina_pdf <- NA_integer_
    linha$tabela_origem <- juntar(d$tabela[i])
    linha
  }))
}

montar <- function(tabelas, es) {
  partes <- lapply(es$fonte, function(f) {
    t <- tabelas[[f]]
    if (is.null(t)) stop("tabela ", f, " não existe")
    if (!is.null(es$filtro)) t <- t[with(t, eval(parse(text = es$filtro))) %in% TRUE, , drop = FALSE]
    d <- mapear(t, if (is.null(es$mapa)) list() else es$mapa)
    d$tabela <- rep(f, nrow(d))
    d
  })
  d <- do.call(rbind, partes)
  if (nrow(d) == 0) return(NULL)
  d$chave <- chave(d)
  res <- agregar(d)

  for (cp in es$compl) {
    t <- tabelas[[cp$tabela]]
    m <- mapear(t, exige_nome = cp$por == "nome")
    for (h in names(cp$campos)) m[[h]] <- trimws(as.character(t[[cp$campos[[h]]]]))[as.integer(rownames(m))]
    # sep = " ": as linhas são pedaços de um mesmo texto, não itens diferentes
    junta <- if (identical(cp$sep, " ")) function(v) {
      v <- v[!is.na(v) & v != ""]
      if (length(v) == 0) NA_character_ else paste(v, collapse = " ")
    } else juntar
    if (cp$por == "codigo") {
      k_base <- norm_codigo(res$programa_codigo)
      k_comp <- norm_codigo(m$programa_codigo)
    } else {
      k_base <- norm(res$programa)
      k_comp <- norm(m$programa)
    }
    for (h in names(cp$campos)) {
      valores <- tapply(m[[h]], k_comp, junta)
      vazio <- is.na(res[[h]]) & !is.na(k_base) & k_base %in% names(valores)
      res[[h]][vazio] <- valores[k_base[vazio]]
      if (any(vazio)) res$tabela_origem[vazio] <- paste(res$tabela_origem[vazio], cp$tabela, sep = " | ")
    }
  }
  res
}

ppas <- unique(paste0(catalogo_ppa$uf, "_", catalogo_ppa$ciclo))
saida <- list()
sem_fonte <- character()
for (p in ppas) {
  tabelas <- readRDS(file.path(rel, paste0(p, ".rds")))
  es <- espec[[p]]
  if (is.null(es)) {
    if (!"programas" %in% names(tabelas)) {
      sem_fonte <- c(sem_fonte, p)
      next
    }
    es <- list(fonte = "programas")
  }
  versoes <- list(original = es)
  if (!is.null(es$revisada)) versoes$revisada <- es$revisada
  for (v in names(versoes)) {
    res <- montar(tabelas, versoes[[v]])
    if (is.null(res)) next
    saida[[paste(p, v)]] <- data.frame(uf = substr(p, 1, 2), ciclo = substr(p, 4, 12), versao = v, res)
  }
}

programas <- do.call(rbind, saida)
rownames(programas) <- NULL
programas$regiao <- catalogo_ppa$regiao[match(programas$uf, catalogo_ppa$uf)]
programas <- programas[, c("uf", "regiao", "ciclo", "versao", campos, "pagina_pdf", "tabela_origem")]
saveRDS(programas, file.path(rel, "programas.rds"), compress = "xz")

# resumo por PPA (fica no pacote: o que existe sem baixar nada)
chave_ppa <- paste(programas$uf, programas$ciclo, programas$versao)
cobertura_programas <- do.call(rbind, lapply(split(programas, factor(chave_ppa, levels = unique(chave_ppa))), function(d) {
  preench <- vapply(campos[-(1:2)], function(cp) round(mean(!is.na(d[[cp]])), 2), numeric(1))
  data.frame(uf = d$uf[1], ciclo = d$ciclo[1], versao = d$versao[1], n_programas = nrow(d),
             com_codigo = round(mean(!is.na(d$programa_codigo)), 2), t(preench),
             tabela_origem = juntar(unlist(strsplit(d$tabela_origem, " | ", fixed = TRUE))))
}))
rownames(cobertura_programas) <- NULL

# mantém o que já estiver no sysdata (ex.: cobertura_valores, de gerar_valores.R)
save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes", "cobertura_indicadores", "cobertura_metas", "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")

message(nrow(programas), " programas em ", length(unique(paste(programas$uf, programas$ciclo))), " PPAs (",
        sum(programas$versao == "revisada"), " linhas de versões revisadas). ",
        "Sem fonte de programas: ", paste(sem_fonte, collapse = ", "), ". ",
        "programas.rds: ", round(file.size(file.path(rel, "programas.rds")) / 1024^2, 2), " MB.")
