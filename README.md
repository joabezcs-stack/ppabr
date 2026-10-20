# ppabr

Dados dos **Planos Plurianuais (PPAs)** dos 26 estados brasileiros e do Distrito
Federal, ciclos 2008-2011 a 2024-2027, extraídos dos documentos oficiais e
organizados em tabelas.

São 133 PPAs e 736 tabelas (programas, ações, metas, indicadores, valores,
regionalização etc.). Cada PPA é um arquivo único, baixado sob demanda e guardado
em cache; o pacote em si é leve.

## Instalação

```r
# versão em desenvolvimento
# install.packages("remotes")
remotes::install_github("joabezcs-stack/ppabr")
```

## Uso

```r
library(ppabr)

# O que existe, sem baixar nada
ppa_catalogo(uf = "CE", ciclo = 2024)
ppa_catalogo(tabela = "indicador")

# Todas as tabelas de um PPA (lista de data.frames)
ce <- ppa_tabela("CE", 2024)
names(ce)

# Uma tabela
prog <- ppa_tabela("CE", 2024, "programas")

# A planilha do PPA, uma aba por tabela
ppa_tabela("CE", 2024, formato = "xlsx")

# O que significa cada coluna (sem baixar nada)
ppa_dicionario("^programas$", uf = "CE", ciclo = 2024)
ppa_dicionario(coluna = "publico", resumo = TRUE)

# Programas de todos os PPAs, com as mesmas colunas (comparáveis entre estados)
ppa_programas(resumo = TRUE)                 # o que cada PPA traz, sem baixar
ne <- ppa_programas(uf = c("BA", "CE", "PE"), ciclo = 2024)

# Ações de todos os PPAs, com as mesmas colunas
ppa_acoes(resumo = TRUE)
mg <- ppa_acoes(uf = "MG", ciclo = 2024)

# Indicadores: referência, meta final e metas por ano
ppa_indicadores(resumo = TRUE)
pr <- ppa_indicadores(uf = "PR", ciclo = 2024)

# Metas, iniciativas e entregas de cada programa
ppa_metas(resumo = TRUE)
ba <- ppa_metas(uf = "BA", ciclo = 2024, tipo = "iniciativa")

# Valor de cada programa por período, em reais
ppa_valores(resumo = TRUE)                   # total de cada PPA e alertas
pe <- ppa_valores(uf = "PE", ciclo = 2024)

# Documentos originais: o texto de cada página (texto como dado) e os PDFs
ppa_documentos(uf = "CE", ciclo = 2024)    # sem baixar nada
txt <- ppa_documento("CE", 2024)            # uma linha por página
ppa_documento("CE", 2024, formato = "pdf")   # baixa os PDFs

# Arquivos baixados
ppa_cache()
```

Toda tabela tem as colunas `uf`, `ppa` e `pagina_pdf`, a página do documento
original de onde a linha veio; a coluna `documento` de `ppa_catalogo()` diz de qual
PDF. O catálogo também traz `notas` de qualidade: correções feitas na extração,
erros do próprio documento e limitações conhecidas.

## Próximos passos

- `ppa_metas_fisicas()`: metas físicas das ações por região e ano.

## Licença

Código: MIT. Dados: CC BY 4.0. Os dados vêm de documentos públicos dos governos
estaduais.
