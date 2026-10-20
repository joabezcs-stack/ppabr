#' Indicadores dos PPAs, harmonizados
#'
#' Uma linha por indicador de cada PPA (por programa, nível e recorte
#' regional), com as mesmas colunas para todos os estados e ciclos: índice de
#' referência, meta ao final do plano e metas por ano. A camada é baixada uma
#' vez e fica em cache.
#'
#' Os valores (`indice_referencia`, `meta_final`, `metas_anuais`) ficam como
#' texto, do jeito que estão no documento: os PPAs misturam números (com
#' separadores diferentes), percentuais e expressões ("ND", "a apurar",
#' "Aumentar"). `metas_anuais` junta as metas de cada ano no formato
#' `"2024: 21 | 2025: 22"`; quando o documento não traz a meta final,
#' `meta_final` é a meta do último ano.
#'
#' O `nivel` diz a que o indicador se refere: programa (o mais comum),
#' objetivo, impacto, iniciativa, ação, produto, entrega ou resultado
#' estratégico, conforme o PPA. `recorte` é a região quando o indicador é
#' regionalizado (por exemplo, nas Regiões de Integração do Pará).
#'
#' @inheritParams ppa_programas
#' @param resumo Se `TRUE`, não baixa nada e devolve, para cada PPA, o número
#'   de indicadores, os níveis e a proporção preenchida de cada campo.
#' @param atualizar Se `TRUE`, baixa a camada de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` com as colunas `uf`, `regiao`, `ciclo`, `versao`,
#'   `nivel`, `programa_codigo`, `programa`, `indicador`, `tipo_indicador`,
#'   `unidade_medida`, `fonte`, `periodicidade`, `polaridade`, `recorte`,
#'   `indice_referencia`, `data_referencia`, `meta_final`, `metas_anuais`,
#'   `pagina_pdf` e `tabela_origem`. Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' # O que cada PPA traz, sem baixar nada
#' head(ppa_indicadores(resumo = TRUE))
#'
#' \donttest{
#' pr <- ppa_indicadores(uf = "PR", ciclo = 2024)
#' if (!is.null(pr)) head(pr[, c("indicador", "indice_referencia", "meta_final")])
#' }
#' @export
ppa_indicadores <- function(uf = NULL, ciclo = NULL,
                            versao = c("original", "mais_recente", "todas"),
                            resumo = FALSE, atualizar = FALSE) {
  versao <- match.arg(versao)
  if (resumo) {
    return(filtrar_ppa(cobertura_indicadores, uf, ciclo, versao))
  }
  caminho <- baixar("indicadores.rds", atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  filtrar_ppa(readRDS(caminho), uf, ciclo, versao)
}
