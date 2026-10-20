#' Catálogo dos PPAs e tabelas disponíveis
#'
#' Lista as tabelas extraídas de cada PPA, sem baixar nada. Use para saber o
#' que existe antes de chamar [ppa_tabela()].
#'
#' @param uf Sigla(s) da(s) UF(s), por exemplo `"CE"` ou `c("BA", "PE")`.
#'   `NULL` (padrão) traz todas.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`). Aceita mais de um. `NULL` traz todos.
#' @param tabela Texto procurado no nome da tabela (expressão regular), por
#'   exemplo `"programa"` ou `"indicador"`. `NULL` traz todas.
#'
#' @return Um `data.frame` com uma linha por tabela e as colunas `uf`,
#'   `regiao`, `ciclo`, `tabela`, `linhas`, `n_colunas`, `colunas` (nomes
#'   separados por vírgula), `arquivo` (arquivo do PPA no repositório),
#'   `documento` (PDF de onde a tabela foi extraída, veja [ppa_documentos()];
#'   `NA` quando a tabela veio de planilha ou não foi possível identificar) e
#'   `notas` (notas de qualidade: correções aplicadas na extração, erros do
#'   próprio documento e limitações conhecidas, separadas por `" | "`).
#'
#' @examples
#' # Tabelas do PPA 2024-2027 do Ceará
#' ppa_catalogo(uf = "CE", ciclo = 2024)
#'
#' # Em quais PPAs há tabelas de indicadores
#' ind <- ppa_catalogo(tabela = "indicador")
#' table(ind$ciclo)
#' @export
ppa_catalogo <- function(uf = NULL, ciclo = NULL, tabela = NULL) {
  res <- catalogo_ppa
  if (!is.null(uf)) {
    uf <- toupper(uf)
    invalidas <- setdiff(uf, res$uf)
    if (length(invalidas) > 0) {
      stop("UF n\u00e3o encontrada: ", paste(invalidas, collapse = ", "), call. = FALSE)
    }
    res <- res[res$uf %in% uf, ]
  }
  if (!is.null(ciclo)) {
    res <- res[res$ciclo %in% normalizar_ciclo(ciclo), ]
  }
  if (!is.null(tabela)) {
    res <- res[grepl(tabela, res$tabela, ignore.case = TRUE), ]
  }
  rownames(res) <- NULL
  res
}
