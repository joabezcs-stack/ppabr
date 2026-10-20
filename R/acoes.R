#' Ações dos PPAs, harmonizadas
#'
#' Uma linha por ação de cada programa de cada PPA, com as mesmas colunas para
#' todos os estados e ciclos. A camada é baixada uma vez e fica em cache.
#'
#' Cada PPA organiza as ações de um jeito; a harmonização leva as colunas
#' originais para um conjunto comum de campos. Campo que não existe no
#' documento fica `NA` (veja `resumo = TRUE`). Quando o documento repete a ação
#' (por produto, região ou unidade orçamentária), as linhas são juntadas e os
#' textos diferentes ficam separados por `" | "` (por exemplo, os vários
#' produtos de uma ação).
#'
#' Em Santa Catarina (2012-2015 em diante) a unidade do plano é a subação, e
#' as linhas trazem `nivel = "subação"`. Ficam de fora os PPAs que não
#' organizam o plano em ações, mas em iniciativas, entregas ou produtos (por
#' exemplo BA 2012-2015 em diante, PR 2012-2015 a 2020-2023, RN 2016-2019 e
#' 2020-2023, SE 2012-2015 em diante, SP 2016-2019 em diante); essas tabelas
#' continuam disponíveis em [ppa_tabela()]. Valores financeiros não estão nesta
#' camada.
#'
#' @inheritParams ppa_programas
#' @param resumo Se `TRUE`, não baixa nada e devolve, para cada PPA, o número
#'   de ações e de programas e a proporção preenchida de cada campo.
#' @param atualizar Se `TRUE`, baixa a camada de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` com as colunas `uf`, `regiao`, `ciclo`, `versao`,
#'   `nivel` (`"ação"` ou `"subação"`), `programa_codigo`, `programa`,
#'   `acao_codigo`, `acao`, `tipo_acao` (projeto, atividade, operação
#'   especial...), `orgao_responsavel` (órgão, unidade orçamentária ou
#'   executora), `finalidade`, `produto`, `unidade_medida`, `pagina_pdf` e
#'   `tabela_origem`. Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' # O que cada PPA traz, sem baixar nada
#' head(ppa_acoes(resumo = TRUE))
#'
#' \donttest{
#' mg <- ppa_acoes(uf = "MG", ciclo = 2024)
#' if (!is.null(mg)) head(mg[, c("programa", "acao", "produto")])
#' }
#' @export
ppa_acoes <- function(uf = NULL, ciclo = NULL,
                      versao = c("original", "mais_recente", "todas"),
                      resumo = FALSE, atualizar = FALSE) {
  versao <- match.arg(versao)
  if (resumo) {
    return(filtrar_ppa(cobertura_acoes, uf, ciclo, versao))
  }
  caminho <- baixar("acoes.rds", atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  filtrar_ppa(readRDS(caminho), uf, ciclo, versao)
}
