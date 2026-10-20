# Gera os documentos (PDFs) e o texto por página de cada PPA, para ppa_documento()/ppa_documentos():
#   - data-raw/release/UF_ciclo_texto.rds : uma linha por página de cada PDF do PPA (documento, pagina, texto,
#                                           origem_texto = "camada de texto" / "OCR" / NA quando não há texto)
#   - data-raw/documentos_publicar.csv    : PDF original -> nome publicado (UF_ciclo_NN.pdf), para subir no release
#   - R/sysdata.rda                       : acrescenta documentos_ppa (lista dos documentos) e a coluna
#                                           `documento` no catalogo_ppa (de qual PDF cada tabela foi extraída)
# Rodar depois de gerar_dados.R, gerar_programas.R e gerar_valores.R, com o diretório de trabalho em ppabr/.
# O texto vem do pdftotext (camada de texto do PDF); para as páginas que passaram por OCR na extração
# (dados_ocr/, mapa em scripts/ocr_ppa.R) vale o texto do OCR.

projeto <- ".."
rel <- file.path("data-raw", "release")
pdftotext <- "C:/Users/joabz/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdftotext.exe"
load(file.path("R", "sysdata.rda"))

inv <- read.csv2(file.path(projeto, "saidas", "01_inventario_arquivos.csv"), encoding = "UTF-8",
                 colClasses = "character")
inv <- inv[inv$uf != "BR" & tolower(inv$extensao) == "pdf", ]
inv <- inv[order(inv$uf, inv$ciclo, inv$arquivo), ]

# OCR: nome -> PDF de origem (lista "pdfs" de scripts/ocr_ppa.R)
ocr_src <- readLines(file.path(projeto, "scripts", "ocr_ppa.R"), encoding = "UTF-8")
m <- regmatches(ocr_src, regexec("^\\s*([A-Z]{2}_\\d{4}_\\d{4}\\w*)\\s*=\\s*\"([^\"]+\\.pdf)\"", ocr_src))
m <- m[lengths(m) == 3]
mapa_ocr <- setNames(vapply(m, `[`, "", 3), vapply(m, `[`, "", 2))

ler_ocr <- function(nome) {
  arq <- file.path(projeto, "dados_ocr", paste0(nome, ".txt"))
  if (!file.exists(arq)) return(NULL)
  l <- readLines(arq, encoding = "UTF-8")
  pag <- as.integer(sub("^<<<PAG (\\d+)>>>.*$", "\\1", l))
  marca <- grepl("^<<<PAG \\d+>>>", l)
  g <- cumsum(marca)
  pags <- pag[marca]
  txt <- vapply(split(l[!marca], g[!marca]), paste, "", collapse = "\n")
  setNames(txt, pags[as.integer(names(txt))])
}

texto_pdf <- function(caminho) {
  tmp <- tempfile(fileext = ".txt")
  system2(pdftotext, c("-enc", "UTF-8", shQuote(caminho), shQuote(tmp)), stdout = FALSE, stderr = FALSE)
  if (!file.exists(tmp)) return(character(0))
  x <- readChar(tmp, file.size(tmp), useBytes = TRUE)
  Encoding(x) <- "UTF-8"
  unlink(tmp)
  p <- strsplit(x, "\f", fixed = TRUE)[[1]]
  if (length(p) && !grepl("\\S", p[length(p)])) p <- p[-length(p)]
  p
}

docs <- list()
publicar <- list()
for (k in unique(paste(inv$uf, inv$ciclo))) {
  d <- inv[paste(inv$uf, inv$ciclo) == k, ]
  uf <- d$uf[1]; ciclo <- d$ciclo[1]
  paginas <- list()
  for (j in seq_len(nrow(d))) {
    nome_pub <- sprintf("%s_%s_%02d.pdf", uf, ciclo, j)
    caminho <- file.path(projeto, d$caminho[j])
    p <- texto_pdf(caminho)
    if (length(p) == 0) {
      # o pdftotext não abriu o arquivo: tenta o pdftools; se falhar, as páginas ficam sem texto
      p <- tryCatch(pdftools::pdf_text(caminho), error = function(e) character(0))
      if (length(p) == 0) {
        n <- suppressWarnings(as.integer(d$n_paginas[j]))
        n <- tryCatch(pdftools::pdf_info(caminho)$pages, error = function(e) n)
        p <- rep("", max(1L, n, na.rm = TRUE))
      }
    }
    txt <- trimws(gsub("[ \t]+\n", "\n", p))
    origem <- ifelse(grepl("\\p{L}{3,}", txt, perl = TRUE), "camada de texto", NA_character_)
    # OCR feito na extração vale para as páginas que ele cobre
    nomes_ocr <- names(mapa_ocr)[normalizePath(file.path(projeto, mapa_ocr), mustWork = FALSE) ==
                                   normalizePath(caminho, mustWork = FALSE)]
    for (no in nomes_ocr) {
      o <- ler_ocr(no)
      if (is.null(o)) next
      i <- as.integer(names(o))
      i <- i[i >= 1 & i <= length(txt)]
      txt[i] <- trimws(o[as.character(i)])
      origem[i] <- "OCR"
    }
    txt[is.na(origem)] <- NA_character_
    paginas[[j]] <- data.frame(documento = nome_pub, pagina = seq_along(txt), texto = txt, origem_texto = origem)
    publicar[[length(publicar) + 1]] <- data.frame(origem = d$caminho[j], publicado = nome_pub)
    docs[[length(docs) + 1]] <- data.frame(
      uf = uf, ciclo = ciclo, documento = nome_pub, arquivo_original = d$arquivo[j],
      paginas = length(txt), tamanho_mb = round(file.size(caminho) / 1024^2, 1),
      paginas_texto = sum(origem %in% "camada de texto"), paginas_ocr = sum(origem %in% "OCR"),
      paginas_sem_texto = sum(is.na(origem)))
  }
  saveRDS(do.call(rbind, paginas), file.path(rel, sprintf("%s_%s_texto.rds", uf, ciclo)), compress = "xz")
  message(k, ": ", nrow(d), " PDF(s)")
}
documentos_ppa <- do.call(rbind, docs)
rownames(documentos_ppa) <- NULL
write.csv2(do.call(rbind, publicar), file.path("data-raw", "documentos_publicar.csv"), row.names = FALSE,
           fileEncoding = "UTF-8")

# --- de qual documento veio cada tabela: palavras de algumas linhas procuradas no texto da página indicada
norm <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT"))
catalogo_ppa$documento <- NA_character_
for (k in unique(paste(catalogo_ppa$uf, catalogo_ppa$ciclo))) {
  uf <- substr(k, 1, 2); ciclo <- substr(k, 4, 12)
  arq_txt <- file.path(rel, sprintf("%s_%s_texto.rds", uf, ciclo))
  if (!file.exists(arq_txt)) next
  pg <- readRDS(arq_txt)
  docs_k <- unique(pg$documento)
  if (length(docs_k) == 1) {
    catalogo_ppa$documento[paste(catalogo_ppa$uf, catalogo_ppa$ciclo) == k] <- docs_k
    next
  }
  pg$tn <- norm(pg$texto)
  tabs <- readRDS(file.path(rel, sprintf("%s_%s.rds", uf, ciclo)))
  for (nm in names(tabs)) {
    t <- tabs[[nm]]
    if (!"pagina_pdf" %in% names(t) || nrow(t) == 0) next
    txtcols <- names(t)[vapply(t, is.character, logical(1)) & !names(t) %in% c("uf", "ppa")]
    amostra <- t[unique(round(seq(1, nrow(t), length.out = min(25, nrow(t))))), , drop = FALSE]
    votos <- setNames(numeric(length(docs_k)), docs_k)
    for (r in seq_len(nrow(amostra))) {
      pgn <- amostra$pagina_pdf[r]
      if (is.na(pgn)) next
      cand <- unlist(lapply(txtcols, function(cn) amostra[[cn]][r]))
      cand <- cand[!is.na(cand) & nchar(cand) >= 8]
      if (!length(cand)) next
      palavras <- unique(unlist(strsplit(norm(cand), "[^a-z0-9]+")))
      palavras <- palavras[nchar(palavras) >= 5]
      if (!length(palavras)) next
      for (dd in docs_k) {
        tx <- pg$tn[pg$documento == dd & pg$pagina == pgn]
        if (length(tx) && !is.na(tx)) votos[dd] <- votos[dd] + mean(vapply(palavras, grepl, logical(1), x = tx, fixed = TRUE))
      }
    }
    if (max(votos) > 0) {
      catalogo_ppa$documento[catalogo_ppa$uf == uf & catalogo_ppa$ciclo == ciclo & catalogo_ppa$tabela == nm] <-
        names(votos)[which.max(votos)]
    }
  }
}

save(list = intersect(c("catalogo_ppa", "colunas_ppa", "cobertura_programas", "cobertura_valores", "cobertura_acoes", "cobertura_indicadores", "cobertura_metas",
                        "documentos_ppa"), ls()),
     file = file.path("R", "sysdata.rda"), compress = "xz")
tam <- sum(file.size(list.files(rel, pattern = "_texto\\.rds$", full.names = TRUE)))
message("\n", nrow(documentos_ppa), " PDFs; páginas com texto: ", sum(documentos_ppa$paginas_texto),
        ", por OCR: ", sum(documentos_ppa$paginas_ocr), ", sem texto: ", sum(documentos_ppa$paginas_sem_texto),
        ". Textos: ", round(tam / 1024^2, 1), " MB. Tabelas com documento identificado: ",
        sum(!is.na(catalogo_ppa$documento)), " de ", nrow(catalogo_ppa), ".")
