# Funções comuns às camadas harmonizadas (programas e valores): identificar o programa
# de cada linha, normalizar nomes e códigos. Usa campos e sinonimos de espec_programas.R.

# nome sem acento, maiúsculo e só com letras e números: para casar programas pelo nome
norm <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  x <- trimws(gsub("\\s+", " ", toupper(gsub("[^A-Za-z0-9]+", " ", x))))
  sub("^PROGRAMA ", "", x)  # "Programa Industrialização" = "Industrialização"
}
# nome do programa quando as linhas trazem variações: se todas são "NOME - órgão",
# fica a parte comum; senão, o nome mais frequente (corrige variações de extração)
escolher_nome <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_character_)
  if (length(unique(v)) == 1) return(v[1])
  hifen <- grepl("-", v)
  prefixo <- trimws(sub("\\s*-.*$", "", v[hifen]))
  if (mean(hifen) >= 0.5 && length(unique(norm(prefixo))) == 1 && nchar(prefixo[1]) > 0) return(prefixo[1])
  names(sort(table(factor(v, levels = unique(v))), decreasing = TRUE))[1]
}
# códigos "0220" e "220" são o mesmo programa
norm_codigo <- function(x) {
  x <- gsub("[^0-9A-Za-z]", "", x)
  ifelse(is.na(x) | x == "", NA, sub("^0+(?=.)", "", x, perl = TRUE))
}
juntar <- function(v) {
  v <- unique(trimws(v[!is.na(v)]))
  v <- v[v != ""]
  if (length(v) == 0) NA_character_ else paste(v, collapse = " | ")
}

# leva as colunas de uma tabela para os campos harmonizados
mapear <- function(t, mapa = list(), exige_nome = TRUE) {
  out <- data.frame(row.names = seq_len(nrow(t)))
  for (cp in campos) {
    col <- if (cp %in% names(mapa)) mapa[[cp]] else intersect(sinonimos[[cp]], names(t))[1]
    out[[cp]] <- if (is.na(col) || !col %in% names(t)) NA_character_ else trimws(as.character(t[[col]]))
  }
  out$pagina_pdf <- if ("pagina_pdf" %in% names(t)) as.integer(t$pagina_pdf) else NA_integer_
  # "1008 - EDUCAÇÃO QUE QUEREMOS": separa o código embutido no nome
  re <- "^(\\d[\\d.]*)\\s*[-–|]\\s*(.+)$"   # "1008 - NOME" ou "1100 | NOME" (TO 2020+)
  emb <- !is.na(out$programa) & grepl(re, out$programa, perl = TRUE)
  out$programa_codigo[emb & is.na(out$programa_codigo)] <-
    sub(re, "\\1", out$programa[emb & is.na(out$programa_codigo)], perl = TRUE)
  out$programa[emb] <- sub(re, "\\2", out$programa[emb], perl = TRUE)
  # fora linhas sem programa e linhas de total
  total <- !is.na(out$programa) & grepl("^(SUB)?TOTA(L|IS)\\b", norm(out$programa))
  ok <- if (exige_nome) !is.na(out$programa) & out$programa != "" & !total else !total
  out[ok, , drop = FALSE]
}

chave <- function(d) {
  cod <- norm_codigo(d$programa_codigo)
  ifelse(!is.na(cod), paste0("c:", cod), paste0("n:", norm(d$programa)))
}

# "1008 - NOME" ou "1100 | NOME" (TO 2020+): separa o código embutido no nome
re_cod <- "^(\\d[\\d.]*)\\s*[-–|]\\s*(.+)$"
separa_codigo <- function(cod, nome) {
  emb <- !is.na(nome) & grepl(re_cod, nome, perl = TRUE)
  cod[emb & is.na(cod)] <- sub(re_cod, "\\1", nome[emb & is.na(cod)], perl = TRUE)
  nome[emb] <- sub(re_cod, "\\2", nome[emb], perl = TRUE)
  list(cod = cod, nome = trimws(nome))
}

# completa código/nome do programa pela camada de programas do mesmo PPA (p_ppa)
completar_programa <- function(d, p_ppa) {
  if (!nrow(p_ppa)) return(d)
  kc <- norm_codigo(p_ppa$programa_codigo)
  kn <- norm(p_ppa$programa)
  sem_nome <- is.na(d$programa) & !is.na(d$programa_codigo)
  i <- match(norm_codigo(d$programa_codigo[sem_nome]), kc)
  d$programa[sem_nome] <- p_ppa$programa[i]
  sem_cod <- is.na(d$programa_codigo) & !is.na(d$programa)
  i <- match(norm(d$programa[sem_cod]), kn)
  d$programa_codigo[sem_cod] <- p_ppa$programa_codigo[i]
  d
}
