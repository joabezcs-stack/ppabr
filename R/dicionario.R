#' Dicionário das colunas
#'
#' Descreve as colunas das tabelas dos PPAs, sem baixar nada: o que cada coluna
#' significa, o tipo, quanto está preenchida e um exemplo de valor.
#'
#' Como os PPAs não seguem um padrão, a mesma coluna pode ter sentidos um pouco
#' diferentes entre estados (por exemplo, valores em reais ou em milhares de
#' reais). As descrições são gerais: na dúvida, confira o documento original
#' pela coluna `pagina_pdf`. A coluna `origem_descricao` indica se a descrição
#' foi escrita à mão (`"manual"`) ou montada a partir do nome da coluna
#' (`"regra"`, por exemplo `valor_2024` = valor do ano 2024). Colunas sem
#' descrição ficam com `NA`.
#'
#' @param tabela Texto procurado no nome da tabela (expressão regular), por
#'   exemplo `"programas"`. Use `"^programas$"` para o nome exato. `NULL`
#'   (padrão) traz todas.
#' @param uf Sigla(s) da(s) UF(s). `NULL` traz todas.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`). Aceita mais de um. `NULL` traz todos.
#' @param coluna Texto procurado no nome da coluna (expressão regular), por
#'   exemplo `"publico"`. `NULL` traz todas.
#' @param resumo Se `TRUE`, devolve uma linha por nome de coluna, com em quantos
#'   PPAs ela aparece, em vez de uma linha por coluna de cada tabela.
#'
#' @return Um `data.frame`. Com `resumo = FALSE` (padrão), uma linha por coluna
#'   de cada tabela: `uf`, `ciclo`, `tabela`, `coluna`, `tipo`
#'   (`"numérico"` ou `"texto"`), `preenchimento` (proporção de valores
#'   não vazios), `exemplo`, `descricao` e `origem_descricao`. Com
#'   `resumo = TRUE`: `coluna`, `descricao`, `origem_descricao`, `n_ppas`,
#'   `n_tabelas` e `ufs`.
#'
#' @examples
#' # Colunas da tabela de programas do PPA 2024-2027 do Ceará
#' ppa_dicionario("^programas$", uf = "CE", ciclo = 2024)
#'
#' # Em quantos PPAs aparece alguma coluna sobre público-alvo
#' ppa_dicionario(coluna = "publico", resumo = TRUE)
#' @export
ppa_dicionario <- function(tabela = NULL, uf = NULL, ciclo = NULL, coluna = NULL,
                           resumo = FALSE) {
  res <- colunas_ppa
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
  if (!is.null(coluna)) {
    res <- res[grepl(coluna, res$coluna, ignore.case = TRUE), ]
  }

  if (resumo) {
    ppa <- paste(res$uf, res$ciclo)
    por_coluna <- split(seq_len(nrow(res)), res$coluna)
    res <- data.frame(
      coluna = names(por_coluna),
      descricao = vapply(por_coluna, function(i) res$descricao[i[1]], character(1)),
      origem_descricao = vapply(por_coluna, function(i) res$origem_descricao[i[1]], character(1)),
      n_ppas = vapply(por_coluna, function(i) length(unique(ppa[i])), integer(1)),
      n_tabelas = lengths(por_coluna),
      ufs = vapply(por_coluna, function(i) paste(sort(unique(res$uf[i])), collapse = ", "),
                   character(1))
    )
    res <- res[order(-res$n_ppas, res$coluna), ]
  }
  rownames(res) <- NULL
  res
}
