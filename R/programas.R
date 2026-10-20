#' Programas dos PPAs, harmonizados
#'
#' Uma linha por programa de cada PPA, com as mesmas colunas para todos os
#' estados e ciclos, o que permite comparar PPAs diretamente. A camada é
#' baixada uma vez (cerca de 1 MB) e fica em cache.
#'
#' Cada PPA organiza os programas de um jeito; a harmonização leva as colunas
#' originais para um conjunto comum de campos. Campo que não existe no
#' documento fica `NA` (veja `resumo = TRUE` para saber o que cada PPA traz).
#' Quando o documento repete o programa (por órgão, unidade orçamentária ou
#' região), as linhas são juntadas e os textos diferentes ficam separados por
#' `" | "`. A coluna `tabela_origem` indica de quais tabelas de
#' [ppa_tabela()] os dados vieram, e `pagina_pdf` a primeira página do
#' documento original em que o programa aparece.
#'
#' Valores financeiros não estão nesta camada: as unidades (reais ou milhares
#' de reais) e os recortes (ano, período, fonte) variam muito entre os PPAs.
#'
#' @param uf Sigla(s) da(s) UF(s). `NULL` (padrão) traz todas.
#' @param ciclo Ano inicial do PPA (ex.: `2024`) ou período (ex.:
#'   `"2024-2027"`). Aceita mais de um. `NULL` traz todos.
#' @param versao Alguns PPAs têm uma versão revisada ou atualizada além da lei
#'   original. `"original"` (padrão) traz a lei original; `"mais_recente"`, a
#'   revisada quando existe (e a original nos demais); `"todas"`, as duas.
#' @param resumo Se `TRUE`, não baixa nada e devolve, para cada PPA, o número
#'   de programas e a proporção preenchida de cada campo.
#' @param atualizar Se `TRUE`, baixa a camada de novo mesmo que esteja no
#'   cache.
#'
#' @return Um `data.frame` com as colunas `uf`, `regiao`, `ciclo`, `versao`,
#'   `programa_codigo`, `programa`, `tipo_programa`, `eixo` (agrupamento
#'   estratégico de maior nível: eixo, área temática, área de resultado,
#'   diretriz etc.), `orgao_responsavel`, `objetivo`, `justificativa`
#'   (justificativa, contextualização ou diagnóstico), `publico_alvo`,
#'   `pagina_pdf` e `tabela_origem`. Sem conexão, avisa e devolve `NULL`.
#'
#' @examples
#' # O que cada PPA traz, sem baixar nada
#' res <- ppa_programas(resumo = TRUE)
#' head(res)
#'
#' \donttest{
#' # Programas do Nordeste no ciclo 2024-2027
#' ne <- ppa_programas(uf = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
#'                     ciclo = 2024)
#' if (!is.null(ne)) table(ne$uf)
#' }
#' @export
ppa_programas <- function(uf = NULL, ciclo = NULL,
                          versao = c("original", "mais_recente", "todas"),
                          resumo = FALSE, atualizar = FALSE) {
  versao <- match.arg(versao)
  if (resumo) {
    return(filtrar_ppa(cobertura_programas, uf, ciclo, versao))
  }
  caminho <- baixar("programas.rds", atualizar = atualizar)
  if (is.null(caminho)) {
    return(invisible(NULL))
  }
  filtrar_ppa(readRDS(caminho), uf, ciclo, versao)
}
