#' Documentos (PDFs) de cada PPA
#'
#' Lista os documentos originais de cada PPA, sem baixar nada: o nome com que
#' cada PDF é publicado, o nome do arquivo original, o número de páginas, o
#' tamanho e quantas páginas têm texto (da camada de texto do PDF ou de OCR).
#'
#' @param uf Sigla(s) da(s) UF(s). `NULL` (padrão) traz todas.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`). Aceita mais de um. `NULL` traz todos.
#'
#' @return Um `data.frame` com `uf`, `ciclo`, `documento`, `arquivo_original`,
#'   `paginas`, `tamanho_mb`, `paginas_texto`, `paginas_ocr` e
#'   `paginas_sem_texto`.
#'
#' @seealso [ppa_documento()] para ler o texto ou baixar os PDFs; a coluna
#'   `documento` de [ppa_catalogo()] diz de qual PDF cada tabela foi extraída.
#'
#' @examples
#' ppa_documentos(uf = "SP", ciclo = 2024)
#'
#' # Quantas páginas digitalizadas ficaram sem texto
#' d <- ppa_documentos()
#' sum(d$paginas_sem_texto)
#' @export
ppa_documentos <- function(uf = NULL, ciclo = NULL) {
  d <- documentos_ppa
  d$versao <- "original"
  d <- filtrar_ppa(d, uf, ciclo, "todas")
  d$versao <- NULL
  d
}

#' Texto e PDF dos documentos de um PPA
#'
#' Para trabalhar com o texto do PPA como dado: devolve uma linha por página
#' de cada documento do PPA, com o texto da página. O texto vem da camada de
#' texto do PDF ou, nas páginas digitalizadas que passaram por OCR na
#' extração, do OCR (`origem_texto`). Páginas digitalizadas sem OCR ficam com
#' `texto = NA`. Com `formato = "pdf"`, baixa os PDFs originais.
#'
#' A coluna `pagina` corresponde à `pagina_pdf` das tabelas de [ppa_tabela()];
#' a coluna `documento` de [ppa_catalogo()] diz de qual PDF cada tabela veio.
#'
#' @param uf Sigla da UF, por exemplo `"CE"`.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`).
#' @param formato `"texto"` (padrão) devolve o texto por página; `"pdf"` baixa
#'   os PDFs e devolve o caminho de cada um.
#' @param documento Nome de um ou mais documentos (veja [ppa_documentos()]).
#'   `NULL` (padrão) traz todos os documentos do PPA.
#' @param paginas Números das páginas a manter (só para `formato = "texto"`).
#'   `NULL` traz todas.
#' @param atualizar Se `TRUE`, baixa de novo mesmo que esteja no cache.
#'
#' @return Com `formato = "texto"`, um `data.frame` com `uf`, `ciclo`,
#'   `documento`, `pagina`, `texto` e `origem_texto`. Com `formato = "pdf"`, o
#'   caminho dos PDFs baixados (invisível). Sem conexão, avisa e devolve
#'   `NULL`.
#'
#' @examples
#' \donttest{
#' # Texto do PPA 2024-2027 do Ceará, uma linha por página
#' txt <- ppa_documento("CE", 2024)
#' if (!is.null(txt)) {
#'   # páginas que falam de primeira infância
#'   head(txt[grepl("primeira infância", txt$texto, ignore.case = TRUE), c("documento", "pagina")])
#' }
#' }
#' @export
ppa_documento <- function(uf, ciclo, formato = c("texto", "pdf"), documento = NULL,
                          paginas = NULL, atualizar = FALSE) {
  formato <- match.arg(formato)
  if (length(uf) != 1 || length(ciclo) != 1) {
    stop("Informe uma UF e um ciclo por vez.", call. = FALSE)
  }
  docs <- ppa_documentos(uf = uf, ciclo = ciclo)
  if (nrow(docs) == 0) {
    stop("N\u00e3o h\u00e1 documentos do PPA ", normalizar_ciclo(ciclo), " de ", toupper(uf), ".", call. = FALSE)
  }
  if (!is.null(documento)) {
    inval <- setdiff(documento, docs$documento)
    if (length(inval) > 0) {
      stop("Documento n\u00e3o encontrado: ", paste(inval, collapse = ", "), ". Dispon\u00edveis: ",
           paste(docs$documento, collapse = ", "), call. = FALSE)
    }
    docs <- docs[docs$documento %in% documento, ]
  }

  if (formato == "pdf") {
    caminhos <- character(0)
    for (d in docs$documento) {
      cam <- baixar(d, atualizar = atualizar, base = url_documentos(), pasta = pasta_cache(versao_documentos))
      if (is.null(cam)) return(invisible(NULL))
      caminhos <- c(caminhos, cam)
    }
    message("PDF(s) em ", dirname(caminhos[1]))
    return(invisible(caminhos))
  }

  arq <- sprintf("%s_%s_texto.rds", docs$uf[1], docs$ciclo[1])
  caminho <- baixar(arq, atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  txt <- readRDS(caminho)
  txt <- txt[txt$documento %in% docs$documento, ]
  if (!is.null(paginas)) txt <- txt[txt$pagina %in% paginas, ]
  res <- data.frame(uf = docs$uf[1], ciclo = docs$ciclo[1], txt)
  rownames(res) <- NULL
  res
}
