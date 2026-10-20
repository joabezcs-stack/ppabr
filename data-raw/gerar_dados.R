# Gera os arquivos de dados do ppabr a partir de ../saidas:
#   - data-raw/release/UF_ciclo.rds  : lista com todas as tabelas do PPA (sobe no release)
#   - data-raw/release/UF_ciclo.xlsx : a mesma lista em planilha, uma aba por tabela
#   - R/sysdata.rda                  : catálogo (catalogo_ppa), embutido no pacote
# Rodar com o diretório de trabalho na pasta do pacote (ppabr/).

library(readr)
library(writexl)

origem <- file.path("..", "saidas")
destino <- file.path("data-raw", "release")
dir.create(destino, showWarnings = FALSE, recursive = TRUE)

regioes <- c(
  AC = "Norte", AM = "Norte", AP = "Norte", PA = "Norte", RO = "Norte", RR = "Norte", TO = "Norte",
  AL = "Nordeste", BA = "Nordeste", CE = "Nordeste", MA = "Nordeste", PB = "Nordeste",
  PE = "Nordeste", PI = "Nordeste", RN = "Nordeste", SE = "Nordeste",
  ES = "Sudeste", MG = "Sudeste", RJ = "Sudeste", SP = "Sudeste",
  PR = "Sul", RS = "Sul", SC = "Sul",
  DF = "Centro-Oeste", GO = "Centro-Oeste", MS = "Centro-Oeste", MT = "Centro-Oeste"
)

# Os csv foram gravados com write_csv2 (";" e vírgula decimal). Tudo é lido como
# texto e só vira número a coluna em que todos os valores são números sem zero à
# esquerda e cujo nome não indica código (códigos ficam como texto: "0101").
para_numero <- function(t) {
  for (col in names(t)) {
    v <- t[[col]]
    if (grepl("codigo|^cod_|_cod$", col)) next
    preenchidos <- v[!is.na(v)]
    if (length(preenchidos) == 0) next
    if (all(grepl("^-?\\d+(,\\d+)?$", preenchidos)) && !any(grepl("^-?0\\d", preenchidos))) {
      t[[col]] <- as.numeric(sub(",", ".", v, fixed = TRUE))
    }
  }
  if ("pagina_pdf" %in% names(t) && is.numeric(t$pagina_pdf)) t$pagina_pdf <- as.integer(t$pagina_pdf)
  t
}

csvs <- list.files(origem, pattern = "^[A-Z]{2}_\\d{4}-\\d{4}_.+\\.csv$", recursive = TRUE, full.names = TRUE)
ppa <- sub("^([A-Z]{2}_\\d{4}-\\d{4})_.*$", "\\1", basename(csvs))

catalogo <- list()
colunas <- list()
for (p in unique(ppa)) {
  arquivos <- csvs[ppa == p]
  nomes <- sub(paste0("^", p, "_(.+)\\.csv$"), "\\1", basename(arquivos))
  tabelas <- lapply(arquivos, function(a) {
    t <- read_delim(a, delim = ";", col_types = cols(.default = col_character()),
                    na = "", locale = locale(encoding = "UTF-8"), progress = FALSE)
    para_numero(as.data.frame(t))
  })
  names(tabelas) <- nomes

  saveRDS(tabelas, file.path(destino, paste0(p, ".rds")), compress = "xz")

  # xlsx: o Excel limita a 31 caracteres o nome da aba e a 32.767 os da célula
  para_xlsx <- lapply(tabelas, function(t) {
    for (col in names(t)) if (is.character(t[[col]])) t[[col]] <- substr(t[[col]], 1, 32000)
    t
  })
  names(para_xlsx) <- substr(names(para_xlsx), 1, 31)
  write_xlsx(para_xlsx, file.path(destino, paste0(p, ".xlsx")))

  uf <- substr(p, 1, 2)
  catalogo[[p]] <- data.frame(
    uf = uf,
    regiao = unname(regioes[uf]),
    ciclo = substr(p, 4, 12),
    tabela = nomes,
    linhas = vapply(tabelas, nrow, integer(1)),
    n_colunas = vapply(tabelas, ncol, integer(1)),
    colunas = vapply(tabelas, function(t) paste(names(t), collapse = ", "), character(1)),
    arquivo = paste0(p, ".rds")
  )
  colunas[[p]] <- do.call(rbind, lapply(nomes, function(nm) {
    t <- tabelas[[nm]]
    data.frame(
      uf = uf, ciclo = substr(p, 4, 12), tabela = nm, coluna = names(t),
      tipo = vapply(t, function(v) if (is.numeric(v)) "numérico" else "texto", character(1)),
      preenchimento = vapply(t, function(v) if (length(v) == 0) 0 else round(mean(!is.na(v) & v != ""), 3),
                             numeric(1)),
      exemplo = vapply(t, function(v) {
        v <- v[!is.na(v) & v != ""]
        if (length(v) == 0) NA_character_ else substr(as.character(v[1]), 1, 80)
      }, character(1))
    )
  }))
  message(p, ": ", length(tabelas), " tabelas")
}

catalogo_ppa <- do.call(rbind, catalogo)
catalogo_ppa <- catalogo_ppa[order(catalogo_ppa$uf, catalogo_ppa$ciclo, catalogo_ppa$tabela), ]
rownames(catalogo_ppa) <- NULL

# notas de qualidade (data-raw/notas_qualidade.csv): uf/ciclo/tabela vazios valem para todos
# (a nota pode conter ";": só os 4 primeiros ";" de cada linha separam campos)
linhas_notas <- readLines(file.path("data-raw", "notas_qualidade.csv"), encoding = "UTF-8")[-1]
linhas_notas <- linhas_notas[linhas_notas != ""]
partes <- regmatches(linhas_notas, regexec("^([^;]*);([^;]*);([^;]*);([^;]*);(.*)$", linhas_notas))
notas <- as.data.frame(do.call(rbind, lapply(partes, `[`, 2:6)), stringsAsFactors = FALSE)
names(notas) <- c("uf", "ciclo", "tabela", "tipo", "nota")
notas[notas == ""] <- NA
catalogo_ppa$notas <- vapply(seq_len(nrow(catalogo_ppa)), function(i) {
  x <- catalogo_ppa[i, ]
  ok <- (is.na(notas$uf) | notas$uf == x$uf) & (is.na(notas$ciclo) | notas$ciclo == x$ciclo) &
    (is.na(notas$tabela) | notas$tabela == x$tabela)
  if (!any(ok)) NA_character_ else paste0("[", notas$tipo[ok], "] ", notas$nota[ok], collapse = " | ")
}, character(1))
sem_tabela <- notas[!is.na(notas$tabela) & !paste(notas$uf, notas$ciclo, notas$tabela) %in%
                      paste(catalogo_ppa$uf, catalogo_ppa$ciclo, catalogo_ppa$tabela) & !is.na(notas$ciclo), ]
if (nrow(sem_tabela)) warning("Notas sem tabela correspondente: ", paste(sem_tabela$uf, sem_tabela$ciclo, sem_tabela$tabela, collapse = ", "))

# dicionário: uma linha por coluna de cada tabela, com a descrição (manual ou por regra)
source(file.path("data-raw", "regras_dicionario.R"), encoding = "UTF-8")
colunas_ppa <- do.call(rbind, colunas)
desc <- t(vapply(unique(colunas_ppa$coluna), descrever, character(2)))
colunas_ppa$descricao <- desc[colunas_ppa$coluna, 1]
colunas_ppa$origem_descricao <- desc[colunas_ppa$coluna, 2]
colunas_ppa <- colunas_ppa[order(colunas_ppa$uf, colunas_ppa$ciclo, colunas_ppa$tabela), ]
rownames(colunas_ppa) <- NULL
message("Colunas com descrição: ", round(100 * mean(!is.na(colunas_ppa$descricao)), 1),
        "% das ocorrências; ", sum(is.na(desc[, 1])), " de ", nrow(desc), " nomes distintos sem descrição.")

# mantém o que os outros geradores já gravaram no sysdata (coberturas, documentos) e a coluna `documento`
# do catálogo (de gerar_documentos.R); rodar de novo aqueles geradores só quando os dados mudarem
antigo <- new.env()
if (file.exists(file.path("R", "sysdata.rda"))) load(file.path("R", "sysdata.rda"), envir = antigo)
if (exists("catalogo_ppa", envir = antigo) && "documento" %in% names(antigo$catalogo_ppa)) {
  ca <- antigo$catalogo_ppa
  catalogo_ppa$documento <- ca$documento[match(paste(catalogo_ppa$uf, catalogo_ppa$ciclo, catalogo_ppa$tabela),
                                               paste(ca$uf, ca$ciclo, ca$tabela))]
}
for (obj in setdiff(ls(antigo), c("catalogo_ppa", "colunas_ppa"))) assign(obj, get(obj, envir = antigo))
save(list = c("catalogo_ppa", "colunas_ppa", setdiff(ls(antigo), c("catalogo_ppa", "colunas_ppa"))),
     file = file.path("R", "sysdata.rda"), compress = "xz")

tam <- file.size(list.files(destino, full.names = TRUE))
message("\n", length(unique(ppa)), " PPAs, ", nrow(catalogo_ppa), " tabelas. ",
        "Release: ", round(sum(tam) / 1024^2, 1), " MB. ",
        "sysdata.rda: ", round(file.size(file.path("R", "sysdata.rda")) / 1024, 1), " KB.")
