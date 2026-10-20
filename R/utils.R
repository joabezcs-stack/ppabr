# Versão dos dados publicada no repositório (tag do release)
versao_dados <- "dados-v0.1"

# Endereço base dos arquivos. Pode ser trocado com
# options(ppabr.url_dados = "...") (útil para testar com arquivos locais).
url_dados <- function() {
  getOption(
    "ppabr.url_dados",
    paste0("https://github.com/joabezcs-stack/ppabr/releases/download/", versao_dados)
  )
}

# Versão dos PDFs originais (release próprio: não muda quando os dados são corrigidos)
versao_documentos <- "documentos-v1"
url_documentos <- function() {
  getOption(
    "ppabr.url_documentos",
    paste0("https://github.com/joabezcs-stack/ppabr/releases/download/", versao_documentos)
  )
}

# Pasta de cache do pacote, separada por versão dos dados
pasta_cache <- function(versao = versao_dados) {
  file.path(tools::R_user_dir("ppabr", which = "cache"), versao)
}

# Aceita 2024, "2024" ou "2024-2027" e devolve "2024-2027"
normalizar_ciclo <- function(ciclo) {
  ciclo <- as.character(ciclo)
  inicio <- substr(ciclo, 1, 4)
  if (any(!grepl("^\\d{4}$", inicio))) {
    stop("`ciclo` deve ser o ano inicial do PPA (ex.: 2024) ou o per\u00edodo (ex.: \"2024-2027\").",
         call. = FALSE)
  }
  paste0(inicio, "-", as.integer(inicio) + 3L)
}

# Filtra uma camada harmonizada por UF, ciclo e versão. "mais_recente" troca a
# versão original pela revisada nos PPAs que têm revisão.
filtrar_ppa <- function(d, uf = NULL, ciclo = NULL, versao = "original") {
  if (!is.null(uf)) {
    uf <- toupper(uf)
    invalidas <- setdiff(uf, catalogo_ppa$uf)
    if (length(invalidas) > 0) {
      stop("UF n\u00e3o encontrada: ", paste(invalidas, collapse = ", "), call. = FALSE)
    }
    d <- d[d$uf %in% uf, ]
  }
  if (!is.null(ciclo)) {
    d <- d[d$ciclo %in% normalizar_ciclo(ciclo), ]
  }
  if (versao == "original") {
    d <- d[d$versao == "original", ]
  } else if (versao == "mais_recente") {
    revisados <- unique(paste(d$uf, d$ciclo)[d$versao == "revisada"])
    d <- d[d$versao == "revisada" | !paste(d$uf, d$ciclo) %in% revisados, ]
  }
  rownames(d) <- NULL
  d
}

# Baixa um arquivo do repositório para o cache. Se não houver conexão ou o
# arquivo não existir, avisa e devolve NULL, sem erro (exigência do CRAN).
baixar <- function(arquivo, atualizar = FALSE, base = url_dados(), pasta = pasta_cache()) {
  destino <- file.path(pasta, arquivo)
  if (file.exists(destino) && !atualizar) {
    return(destino)
  }
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  url <- paste0(base, "/", arquivo)
  temp <- tempfile(fileext = paste0(".", tools::file_ext(arquivo)))
  ok <- tryCatch(
    {
      utils::download.file(url, temp, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok || !file.exists(temp) || file.size(temp) == 0) {
    message("N\u00e3o foi poss\u00edvel baixar ", arquivo, " de ", base, ".\n",
            "Verifique a conex\u00e3o com a internet e tente de novo.")
    return(NULL)
  }
  file.copy(temp, destino, overwrite = TRUE)
  unlink(temp)
  destino
}
