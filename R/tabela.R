#' Lê as tabelas de um PPA
#'
#' Baixa o arquivo do PPA (só na primeira vez; depois usa o cache) e devolve
#' as tabelas como foram extraídas do documento original. As tabelas variam
#' de um PPA para outro: consulte [ppa_catalogo()] para ver quais existem.
#'
#' Toda tabela traz as colunas `uf`, `ppa` (ciclo) e `pagina_pdf`, a página
#' do documento original de onde a linha foi extraída.
#'
#' @param uf Sigla da UF, por exemplo `"CE"`.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`).
#' @param tabela Nome de uma tabela do PPA (veja [ppa_catalogo()]). Se `NULL`
#'   (padrão), devolve todas as tabelas em uma lista.
#' @param formato `"data.frame"` (padrão) lê as tabelas no R; `"xlsx"` baixa a
#'   planilha do PPA (uma aba por tabela) e devolve o caminho do arquivo. No
#'   xlsx, textos com mais de 32.000 caracteres são cortados (limite do Excel).
#' @param atualizar Se `TRUE`, baixa o arquivo de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` (quando `tabela` é informada), uma lista nomeada de
#'   `data.frame`s (quando `tabela = NULL`) ou, com `formato = "xlsx"`, o
#'   caminho da planilha. Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' \donttest{
#' # Todas as tabelas do PPA 2024-2027 do Ceará
#' ce <- ppa_tabela("CE", 2024)
#' if (!is.null(ce)) names(ce)
#'
#' # Só os programas
#' prog <- ppa_tabela("CE", 2024, "programas")
#' }
#' @export
ppa_tabela <- function(uf, ciclo, tabela = NULL,
                       formato = c("data.frame", "xlsx"), atualizar = FALSE) {
  formato <- match.arg(formato)
  if (length(uf) != 1 || length(ciclo) != 1) {
    stop("Informe uma UF e um ciclo por vez.", call. = FALSE)
  }
  disponiveis <- ppa_catalogo(uf = uf, ciclo = ciclo)
  if (nrow(disponiveis) == 0) {
    stop("N\u00e3o h\u00e1 tabelas extra\u00eddas do PPA ", normalizar_ciclo(ciclo),
         " de ", toupper(uf), ".", call. = FALSE)
  }
  if (!is.null(tabela) && !tabela %in% disponiveis$tabela) {
    stop("Tabela \"", tabela, "\" n\u00e3o existe neste PPA. Dispon\u00edveis: ",
         paste(disponiveis$tabela, collapse = ", "), call. = FALSE)
  }

  arquivo <- disponiveis$arquivo[1]
  if (formato == "xlsx") {
    arquivo <- sub("\\.rds$", ".xlsx", arquivo)
  }
  caminho <- baixar(arquivo, atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  if (formato == "xlsx") {
    message("Planilha salva em ", caminho)
    return(invisible(caminho))
  }

  tabelas <- readRDS(caminho)
  if (is.null(tabela)) tabelas else tabelas[[tabela]]
}
