# Descrição das colunas para o ppa_dicionario().
# 1) descrições escritas à mão em data-raw/dicionario_colunas.csv;
# 2) para os demais nomes, regras por padrão (ano no fim, códigos, R$ mil, fontes etc.),
#    que reaproveitam a descrição manual da parte "base" do nome;
# 3) o que não casa com nada fica sem descrição (NA): consultar o documento original.

manual <- read.csv2(file.path("data-raw", "dicionario_colunas.csv"), encoding = "UTF-8",
                    stringsAsFactors = FALSE)
manual <- setNames(manual$descricao, manual$coluna)

# nome da coluna -> texto legível, com os acentos das palavras mais comuns
acentos <- c(
  acao = "ação", acoes = "ações", orgao = "órgão", orgaos = "órgãos", regiao = "região",
  regioes = "regiões", codigo = "código", orcamentaria = "orçamentária", orcamentario = "orçamentário",
  orcamentarios = "orçamentários", indice = "índice", publico = "público", descricao = "descrição",
  classificacao = "classificação", totalizacao = "totalização", fisico = "físico", fisica = "física",
  especifico = "específico", tematico = "temático", estrategico = "estratégico", estrategica = "estratégica",
  estrategia = "estratégia", periodo = "período", implementacao = "implementação", situacao = "situação",
  apuracao = "apuração", referencia = "referência", municipio = "município", territorio = "território",
  sintese = "síntese", numero = "número", area = "área", credito = "crédito", debito = "débito",
  transferencia = "transferência", execucao = "execução", gestao = "gestão", ultima = "última",
  previsao = "previsão", titulo = "título", subacao = "subação", subacoes = "subações", nao = "não",
  extraorcamentario = "extraorçamentário", financeira = "financeira", desenvolvimento = "desenvolvimento"
)
humano <- function(x) {
  palavras <- strsplit(x, "_")[[1]]
  troca <- palavras %in% names(acentos)
  palavras[troca] <- acentos[palavras[troca]]
  paste(palavras, collapse = " ")
}

qualificadores <- c(
  tesouro = "recursos do Tesouro", outras_fontes = "outras fontes", capital = "despesas de capital",
  corrente = "despesas correntes", credito = "acréscimo (emenda)", debito = "redução (emenda)",
  acrescida = "acréscimo (emenda)", deduzida = "redução (emenda)"
)
prefixos <- c(prog = "Programa", acao = "Ação", prod = "Produto", indicador = "Indicador",
              objetivo = "Objetivo", meta = "Meta", valor = "Valor", entrega = "Entrega")
regioes_nomes <- c("norte", "sul", "leste", "oeste", "noroeste", "nordeste", "sudoeste", "sudeste",
                   "centro_ocidental", "centro_oriental", "centro_sul", "centro_expandido",
                   "norte_central", "norte_pioneiro", "metropolitana_de_curitiba")

# devolve c(descricao, origem); origem = "manual", "regra" ou NA
descrever <- function(col) {
  if (!is.na(manual[col])) return(c(unname(manual[col]), "manual"))
  base <- function(x) {
    d <- descrever(x)[1]
    if (is.na(d)) humano(x) else d
  }
  regra <- function(txt) c(txt, "regra")
  m <- function(re) regmatches(col, regexec(re, col))[[1]]

  if (col %in% regioes_nomes) return(regra(paste0("Valor ou meta para a região ", humano(col))))
  if (length(x <- m("^meta_rd_(\\d+)$"))) return(regra(paste0("Meta para a região de desenvolvimento ", x[2])))
  if (length(x <- m("^rgint_\\d+_(.+)$"))) return(regra(paste0("Valor para a região geográfica intermediária de ", humano(x[2]))))
  if (length(x <- m("^ano_(\\d{4})$"))) return(regra(paste0("Valor ou meta do ano ", x[2])))
  if (length(x <- m("^(.+)_(\\d{4})_(\\d{4})$"))) return(regra(paste0(base(x[2]), " — período ", x[3], "-", x[4])))
  if (length(x <- m("^(.+)_(\\d{4})_mil$"))) return(regra(paste0(base(x[2]), " — ano ", x[3], " (em R$ mil)")))
  if (length(x <- m("^(.+)_(\\d{4})$"))) return(regra(paste0(base(x[2]), " — ano ", x[3])))
  if (length(x <- m("^(.+)_ano_(\\d)$"))) return(regra(paste0(base(x[2]), " — ", x[3], "º ano do plano")))
  if (length(x <- m("^(.+)_mil$"))) return(regra(paste0(base(x[2]), " (em R$ mil)")))
  if (length(x <- m("^(.+)_pct$"))) return(regra(paste0(base(x[2]), " (percentual)")))
  if (length(x <- m("^(.+)_num$"))) return(regra(paste0(base(x[2]), " (número extraído do texto)")))
  if (length(x <- m("^(codigo|cod)_(.+)$"))) return(regra(paste0("Código (", humano(x[3]), ")")))
  if (length(x <- m("^(.+)_(codigo|cod)$"))) return(regra(paste0("Código (", humano(x[2]), ")")))
  if (length(x <- m("^(.+)_total$"))) return(regra(paste0(base(x[2]), " — total")))
  if (length(x <- m("^total_(.+)$"))) return(regra(paste0("Total — ", base(x[2]))))
  re_q <- paste0("^(.+)_(", paste(names(qualificadores), collapse = "|"), ")$")
  if (length(x <- m(re_q))) return(regra(paste0(base(x[2]), " — ", qualificadores[[x[3]]])))
  re_p <- paste0("^(", paste(names(prefixos), collapse = "|"), ")_(.+)$")
  if (length(x <- m(re_p))) return(regra(paste0(prefixos[[x[2]]], " — ", base(x[3]))))
  c(NA_character_, NA_character_)
}
