#' Arquivos baixados (cache)
#'
#' Mostra os arquivos já baixados (tabelas, camadas harmonizadas, textos e
#' PDFs dos PPAs) ou apaga o cache.
#'
#' @param limpar Se `TRUE`, apaga todos os arquivos do cache.
#'
#' @return Um `data.frame` com `arquivo`, `versao` (versão dos dados ou dos
#'   documentos) e `tamanho_mb` dos arquivos em cache (vazio se não houver
#'   nenhum). Com `limpar = TRUE`, devolve invisivelmente o caminho da pasta
#'   apagada.
#'
#' @examples
#' ppa_cache()
#' @export
ppa_cache <- function(limpar = FALSE) {
  raiz <- tools::R_user_dir("ppabr", which = "cache")
  if (limpar) {
    unlink(raiz, recursive = TRUE)
    message("Cache apagado: ", raiz)
    return(invisible(raiz))
  }
  arquivos <- list.files(raiz, full.names = TRUE, recursive = TRUE)
  data.frame(
    arquivo = basename(arquivos),
    versao = basename(dirname(arquivos)),
    tamanho_mb = round(file.size(arquivos) / 1024^2, 2)
  )
}
