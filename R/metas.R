#' Metas, iniciativas e entregas dos PPAs, harmonizadas
#'
#' Uma linha por item do plano ligado a um programa (meta, iniciativa,
#' entrega, compromisso, produto ou meta prioritária, conforme o PPA), com as
#' mesmas colunas para todos os estados e ciclos. A camada é baixada uma vez e
#' fica em cache.
#'
#' Os PPAs usam nomes e hierarquias diferentes: `tipo` diz o que é o item
#' (como o documento o chama) e `vinculo` diz a que ele está ligado
#' (objetivo, objetivo específico, compromisso ou iniciativa). Quantidades e
#' metas por ano ficam como texto, como no documento; `metas_anuais` segue o
#' formato `"2024: 10 | 2025: 12"`.
#'
#' Não entram aqui as metas físicas das ações por região e ano (quantidade de
#' produto de cada ação, como em MG, RJ, RO, RR, PE e PI), que são
#' detalhamento das ações e continuam em [ppa_tabela()]. PPAs que não trazem
#' metas, iniciativas nem entregas também ficam de fora (veja
#' `resumo = TRUE`).
#'
#' @inheritParams ppa_programas
#' @param tipo Texto procurado no tipo do item (expressão regular), por
#'   exemplo `"meta"`, `"iniciativa"` ou `"entrega"`. `NULL` traz todos.
#' @param resumo Se `TRUE`, não baixa nada e devolve, para cada PPA, o número
#'   de itens, os tipos e a proporção preenchida de cada campo.
#' @param atualizar Se `TRUE`, baixa a camada de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` com as colunas `uf`, `regiao`, `ciclo`, `versao`,
#'   `tipo`, `programa_codigo`, `programa`, `vinculo_codigo`, `vinculo`,
#'   `item_codigo`, `descricao`, `orgao_responsavel`, `unidade_medida`,
#'   `quantidade`, `metas_anuais`, `recorte`, `pagina_pdf` e `tabela_origem`.
#'   Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' # O que cada PPA traz, sem baixar nada
#' head(ppa_metas(resumo = TRUE))
#'
#' \donttest{
#' ba <- ppa_metas(uf = "BA", ciclo = 2024, tipo = "iniciativa")
#' if (!is.null(ba)) head(ba[, c("programa", "vinculo", "descricao")])
#' }
#' @export
ppa_metas <- function(uf = NULL, ciclo = NULL, tipo = NULL,
                      versao = c("original", "mais_recente", "todas"),
                      resumo = FALSE, atualizar = FALSE) {
  versao <- match.arg(versao)
  if (resumo) {
    res <- filtrar_ppa(cobertura_metas, uf, ciclo, versao)
    if (!is.null(tipo)) res <- res[grepl(tipo, res$tipos, ignore.case = TRUE), ]
    rownames(res) <- NULL
    return(res)
  }
  caminho <- baixar("metas.rds", atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  res <- filtrar_ppa(readRDS(caminho), uf, ciclo, versao)
  if (!is.null(tipo)) res <- res[grepl(tipo, res$tipo, ignore.case = TRUE), ]
  rownames(res) <- NULL
  res
}
