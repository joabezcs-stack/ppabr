#' Valores financeiros dos programas, harmonizados
#'
#' Valor previsto para cada programa de cada PPA, por período, em reais. Uma
#' linha por programa e período: os anos ou períodos que o documento traz (ex.:
#' `"2024"` e `"2025-2027"`) e o total do plano (`periodo_completo = TRUE`). A
#' camada é baixada uma vez e fica em cache.
#'
#' Cuidados na leitura:
#' * Os valores foram convertidos para reais quando o documento estava em
#'   milhares de reais (`unidade_original = "R$ mil"`).
#' * `calculo` indica como o valor foi obtido: `"quadro do programa"` (lido
#'   no quadro do programa), `"soma das ações"` (somado das ações, produtos,
#'   regiões ou fontes do programa) ou `"soma dos períodos"` (total do plano
#'   calculado a partir dos anos, quando o documento não traz o total).
#' * Os PPAs não são homogêneos: alguns trazem só parte dos programas, outros
#'   só alguns anos. `ppa_valores(resumo = TRUE)` mostra o total de cada PPA e
#'   a coluna `alerta` aponta os casos que pedem conferência no documento
#'   (use `pagina_pdf` e [ppa_tabela()]).
#' * Para somar o valor de um PPA, use só as linhas com
#'   `periodo_completo = TRUE`, ou só as dos períodos parciais, nunca as duas.
#'
#' Ficam de fora os PPAs sem quadro de valores por programa (CE 2016-2019,
#' CE 2020-2023, AP 2016-2019, AP 2020-2023, TO 2012-2015).
#'
#' @inheritParams ppa_programas
#' @param resumo Se `TRUE`, não baixa nada e devolve, para cada PPA, o número
#'   de programas com valor, os períodos disponíveis, o total do plano em
#'   bilhões de reais e os alertas.
#' @param atualizar Se `TRUE`, baixa a camada de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` com as colunas `uf`, `regiao`, `ciclo`, `versao`,
#'   `programa_codigo`, `programa`, `periodo`, `ano_inicio`, `ano_fim`,
#'   `periodo_completo`, `valor` (em reais), `unidade_original`, `calculo`,
#'   `parcial` (documento com valores de só parte dos programas), `pagina_pdf`,
#'   `tabela_origem` e `coluna_origem`. Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' # Total de cada PPA, sem baixar nada
#' res <- ppa_valores(resumo = TRUE)
#' head(res)
#'
#' \donttest{
#' # Valor total dos programas do PPA 2024-2027 de Pernambuco
#' pe <- ppa_valores(uf = "PE", ciclo = 2024)
#' if (!is.null(pe)) {
#'   tot <- pe[pe$periodo_completo, ]
#'   head(tot[order(-tot$valor), c("programa", "valor")])
#' }
#' }
#' @export
ppa_valores <- function(uf = NULL, ciclo = NULL,
                        versao = c("original", "mais_recente", "todas"),
                        resumo = FALSE, atualizar = FALSE) {
  versao <- match.arg(versao)
  if (resumo) {
    return(filtrar_ppa(cobertura_valores, uf, ciclo, versao))
  }
  caminho <- baixar("valores.rds", atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  filtrar_ppa(readRDS(caminho), uf, ciclo, versao)
}
