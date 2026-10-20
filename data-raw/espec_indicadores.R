# Especificação da camada harmonizada de indicadores (ppa_indicadores()).
#
# Uma linha por indicador (por programa, nível e recorte regional). As colunas de cada tabela de origem vão
# para os campos harmonizados pelos sinônimos abaixo. Padrão: a tabela "indicadores" do PPA, nível
# "programa". `espec_indicadores` lista os PPAs que fogem disso. Campos de cada parte:
#   fonte : tabela    nivel : texto fixo ou nome de coluna da tabela ("col:nivel")    mapa : troca sinônimos
#   longo : c(ano = coluna, valor = coluna) quando cada linha é um ano da meta (PA)
#   revisada : partes da versão revisada do PPA
# As metas por ano saem das colunas meta_AAAA, indice_esperado_AAAA, esperado_AAAA, desejado_AAAA,
# indice_desejado_AAAA, indice_AAAA e desejado_ano_N (N-ésimo ano do plano).

campos_ind <- c("programa_codigo", "programa", "indicador", "tipo_indicador", "unidade_medida", "fonte",
                "periodicidade", "polaridade", "recorte", "indice_referencia", "data_referencia", "meta_final")

sinonimos_ind <- list(
  programa_codigo = c("programa_codigo", "codigo_programa", "cod_programa", "programa_numero"),
  programa = c("programa", "programa_nome", "nome_programa", "descricao_do_programa", "titulo_programa"),
  indicador = c("indicador", "nome_indicador"),
  tipo_indicador = c("tipo_indicador"),
  unidade_medida = c("unidade_medida", "unidade_de_medida", "unidade", "medida"),
  fonte = c("fonte", "fonte_da_informacao", "fonte_de_apuracao", "fonte_dos_dados", "descricao_fonte_informacao"),
  periodicidade = c("periodicidade"),
  polaridade = c("polaridade", "tendencia", "valor_desejado_ou_polaridade"),
  recorte = c("regiao_de_integracao", "regiao", "territorializacao", "base_geografica"),
  indice_referencia = c("indice_referencia", "indice_de_referencia", "referencia_indice", "valor_referencia",
                        "indice_mais_recente", "indice_recente", "indice_atual", "mais_recente", "valor_mais_recente",
                        "qtde_valor_mais_recente", "linha_de_base_valor", "linha_de_base", "referencia",
                        "indice_inicial", "ultima_apuracao", "inicio_ppa", "situacao_atual", "indice",
                        "referencia_indice_e_meta_2011"),
  data_referencia = c("data_referencia", "data_de_referencia", "ano_referencia", "ano_de_referencia", "referencia_ano",
                      "referencia_data", "data_indice_referencia", "apurado_em", "data_apuracao", "data_de_apuracao",
                      "ano_apuracao", "ano_de_apuracao", "ano_base", "linha_de_base_ano", "periodo_referencia",
                      "periodo_de_referencia", "ultima_apuracao_ano", "data", "ano"),
  meta_final = c("meta_final", "meta_final_valor", "indice_final", "final_ppa", "indice_ao_final_do_ppa",
                 "ao_final_do_plano", "meta_ao_final_do_ppa", "meta_ppa", "valor_final_ppa", "indice_esperado_ppa",
                 "indice_proposto", "indice_previsto", "indice_desejado", "qtde_indicador_termino_ppa",
                 "meta_2024_2027", "valor_2024_2027", "indice_esperado_2008_a_2011", "indice_esperado_2012_a_2015",
                 "indice_esperado_ate_2015", "resultado_esperado_2019", "resultado_esperado_ate_2015",
                 "indice_previsto_2019", "indice_previsto_2023", "meta_indicador", "meta_melhoria_2012_2015")
)

espec_indicadores <- list(
  "AC_2016-2019" = NULL, "AC_2008-2011" = NULL,
  "AC_2024-2027" = list(list(fonte = "objetivos_especificos", nivel = "objetivo", mapa = list(recorte = "regionalizacao"))),
  "AP_2024-2027" = list(list(fonte = "indicadores_resultado", mapa = list(indice_referencia = "referencia"))),
  "BA_2024-2027" = list(list(fonte = "indicadores_programa")),
  "CE_2012-2015" = list(list(fonte = "resultados_indicadores", nivel = "resultado estratégico")),
  "CE_2024-2027" = list(list(fonte = "indicadores", nivel = "col:nivel")),
  "DF_2020-2023" = list(list(fonte = "indicadores_impacto", nivel = "impacto"),
                        list(fonte = "indicadores_objetivos", nivel = "objetivo"),
                        revisada = list(list(fonte = "atualizada_indicadores_impacto", nivel = "impacto"),
                                        list(fonte = "atualizada_indicadores_objetivos", nivel = "objetivo"))),
  "DF_2024-2027" = list(list(fonte = "indicadores_impacto", nivel = "impacto"),
                        list(fonte = "indicadores_objetivos", nivel = "objetivo"),
                        revisada = list(list(fonte = "atualizada_indicadores_impacto", nivel = "impacto"),
                                        list(fonte = "atualizada_indicadores_objetivos", nivel = "objetivo"))),
  "ES_2016-2019" = list(list(fonte = "indicadores_resultado", nivel = "resultado (desafio)")),
  "ES_2020-2023" = list(list(fonte = "indicadores_resultado")),
  "ES_2024-2027" = list(list(fonte = "indicadores_resultado")),
  "MA_2020-2023" = list(list(fonte = "programas")),
  "MA_2024-2027" = list(list(fonte = "programas")),
  "MT_2016-2019" = list(list(fonte = "resultados_pactuados")),
  "PA_2012-2015" = list(list(fonte = "indicadores"),
                        list(fonte = "metas_mapa_exclusao", nivel = "mapa da exclusão social")),
  "PA_2016-2019" = list(list(fonte = "indicadores_processo"),
                        list(fonte = "indicadores_resultado", nivel = "resultado (região)")),
  "PA_2020-2023" = list(list(fonte = "indicadores", longo = c(ano = "ano_esperado", valor = "indice_esperado"))),
  "PA_2024-2027" = list(list(fonte = "indicadores", longo = c(ano = "ano_esperado", valor = "indice_esperado"))),
  "RJ_2020-2023" = list(list(fonte = "indicadores", nivel = "ação")),
  "RJ_2024-2027" = list(list(fonte = "indicadores_programa"),
                        list(fonte = "indicadores_iniciativa", nivel = "iniciativa"),
                        revisada = list(list(fonte = "rev2025_indicadores_programa"),
                                        list(fonte = "rev2025_indicadores_iniciativa", nivel = "iniciativa"))),
  "RN_2024-2027" = list(list(fonte = "programa_indicadores"),
                        list(fonte = "metas", nivel = "col:nivel")),   # "Entrega" ou "Objetivo Específico"
  "RS_2020-2023" = list(list(fonte = "indicadores", nivel = "col:nivel")),
  "RS_2024-2027" = list(list(fonte = "indicadores", nivel = "col:nivel")),
  "SC_2012-2015" = list(list(fonte = "indicadores"), revisada = list(list(fonte = "rev2013_indicadores"))),
  "SE_2024-2027" = list(list(fonte = "objetivos_especificos", nivel = "objetivo")),
  "SP_2020-2023" = list(list(fonte = "indicadores_programa"),
                        list(fonte = "indicadores_produto", nivel = "produto")),
  "TO_2008-2011" = list(list(fonte = "indicadores")),
  "TO_2016-2019" = list(list(fonte = "indicadores", nivel = "objetivo")),
  "TO_2020-2023" = list(list(fonte = "indicadores", nivel = "objetivo")),
  "TO_2024-2027" = list(list(fonte = "indicadores", nivel = "objetivo"))
)
# PPAs sem indicador utilizável (só texto corrido de quadros, ou lista de ODS): fora da camada
fora_indicadores <- c("AC_2016-2019", "AC_2020-2023_iniciativas_metas_indicadores")
