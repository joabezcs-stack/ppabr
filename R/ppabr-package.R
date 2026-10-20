#' ppabr: dados dos Planos Plurianuais dos estados brasileiros
#'
#' O pacote dá acesso às tabelas extraídas dos Planos Plurianuais (PPAs) dos
#' 26 estados e do Distrito Federal, dos ciclos 2008-2011 a 2024-2027.
#'
#' Cada PPA é um arquivo único, com todas as suas tabelas, guardado em um
#' repositório público. O arquivo é baixado na primeira vez que é pedido e
#' fica em cache no computador (veja [ppa_cache()]).
#'
#' Funções principais:
#' * [ppa_catalogo()]: lista os PPAs e as tabelas disponíveis;
#' * [ppa_tabela()]: lê as tabelas de um PPA;
#' * [ppa_dicionario()]: descreve as colunas das tabelas;
#' * [ppa_programas()]: programas de todos os PPAs com colunas padronizadas;
#' * [ppa_acoes()]: ações de todos os PPAs com colunas padronizadas;
#' * [ppa_indicadores()]: indicadores com índice de referência e metas;
#' * [ppa_metas()]: metas, iniciativas e entregas de cada programa;
#' * [ppa_valores()]: valor previsto de cada programa por período, em reais;
#' * [ppa_documentos()] e [ppa_documento()]: os PDFs originais e o texto de
#'   cada página, para usar o texto como dado.
#'
#' @keywords internal
"_PACKAGE"
