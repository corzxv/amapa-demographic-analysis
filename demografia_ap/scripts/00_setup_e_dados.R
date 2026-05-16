################################################################################
# TRABALHO 1 — DEMOGRAFIA — UnB
# Unidade Federativa: Amapá (UF AP, código IBGE 16)
# Passo 0: configuração do ambiente e download de todas as bases brutas
#
# Referência metodológica principal:
#   materiais da disciplina de Demografia e documentação das fontes oficiais.
#
# Cuidados metodológicos (cf. _contexto_demografia.md §3.4):
#   - Local de ocorrência ≠ residência habitual: sempre filtrar CODMUNRES
#     começando com "16" para garantir residentes no Amapá.
#   - Sub-registro SIM: ~20% nacional, mais alto no Norte/Nordeste.
#   - Cobertura SINASC: ~95% nacional, possivelmente menor no Norte.
#   - TBM e TBN devem ser padronizadas antes de comparações inter-regionais.
################################################################################

# ==============================================================================
# 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES
# ==============================================================================

# Lista de pacotes necessários
pkgs <- c(
  "microdatasus",   # download de microdados SIM e SINASC via DATASUS
  "sidrar",         # acesso a tabelas SIDRA/IBGE (projeções, censos)
  "censobr",        # microdados dos Censos Demográficos (Censo 2022)
  "read.dbc",       # leitura de arquivos .dbc (método manual, backup)
  "tidyverse",      # manipulação e visualização (dplyr, ggplot2, tidyr, etc.)
  "lubridate",      # manipulação de datas
  "ggplot2",        # gráficos (já incluso no tidyverse, mas carregado explicitamente)
  "knitr",          # geração de tabelas/relatórios
  "kableExtra",     # formatação avançada de tabelas knitr
  "scales",         # formatação de eixos em ggplot2
  "patchwork",      # composição de múltiplos gráficos
  "janitor",        # limpeza de nomes de colunas e tabelas de frequência
  "writexl"         # exportação para Excel (.xlsx)
)

# Verificar quais ainda não estão instalados e instalar
pkgs_faltando <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(pkgs_faltando) > 0) {
  message("Instalando pacotes ausentes: ", paste(pkgs_faltando, collapse = ", "))
  install.packages(pkgs_faltando, dependencies = TRUE)
} else {
  message("Todos os pacotes já estão instalados.")
}

# microdatasus pode estar desatualizado no CRAN; preferir versão do GitHub:
# devtools::install_github("rfsaldanha/microdatasus")

# Carregar todos os pacotes e registrar sucesso/falha
resultados_pkg <- sapply(pkgs, function(p) {
  tryCatch({
    library(p, character.only = TRUE)
    TRUE
  }, error = function(e) {
    warning("Falha ao carregar pacote: ", p, " — ", conditionMessage(e))
    FALSE
  })
})

# Exibir status de carregamento
cat("\n--- Status dos pacotes ---\n")
for (p in names(resultados_pkg)) {
  status <- if (resultados_pkg[[p]]) "OK" else "FALHA"
  cat(sprintf("  %-20s %s\n", p, status))
}

# ==============================================================================
# 2. CONFIGURAÇÃO DE CAMINHOS
# ==============================================================================

# Raiz do projeto — ajustar se necessário
RAIZ <- file.path(
  "C:/Users/Concursos Felipe/Documents/UnB/Trabalho Demografia/demografia_ap"
)

DIR_BRUTOS     <- file.path(RAIZ, "dados", "brutos")
DIR_PROC       <- file.path(RAIZ, "dados", "processados")
DIR_SCRIPTS    <- file.path(RAIZ, "scripts")
DIR_TABELAS    <- file.path(RAIZ, "outputs", "tabelas")
DIR_GRAFICOS   <- file.path(RAIZ, "outputs", "graficos")
DIR_RELATORIO  <- file.path(RAIZ, "relatorio")

# Criar diretórios caso não existam (segurança)
for (d in c(DIR_BRUTOS, DIR_PROC, DIR_SCRIPTS, DIR_TABELAS,
            DIR_GRAFICOS, DIR_RELATORIO)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Constantes do projeto
UF_SIGLA  <- "AP"          # sigla usada pelo microdatasus
UF_IBGE   <- "16"          # código IBGE da UF (prefixo do CODMUNRES)
ANO_INI   <- 2000
ANO_FIM   <- 2024

cat(sprintf("\nProjeto configurado: UF=%s (código IBGE=%s), período %d–%d\n",
            UF_SIGLA, UF_IBGE, ANO_INI, ANO_FIM))

# ==============================================================================
# FUNÇÕES AUXILIARES
# ==============================================================================

# Baixa dados do DATASUS ano a ano para evitar o erro de tipo em bind_rows().
# Raiz do problema: o DATASUS muda o tipo da coluna CONTADOR entre anos
# (double em alguns, character em outros). fetch_datasus() com múltiplos anos
# chama bind_rows() internamente e falha na incompatibilidade de tipos.
# Solução: baixar um ano por vez, converter tudo para character antes de juntar,
# e deixar process_sim()/process_sinasc() fazerem a tipagem correta depois.
fetch_datasus_seguro <- function(ano_ini, ano_fim, uf, sistema) {
  anos <- ano_ini:ano_fim
  lista <- vector("list", length(anos))
  for (i in seq_along(anos)) {
    cat(sprintf("    baixando %s %s %d...\n", sistema, uf, anos[i]))
    lista[[i]] <- tryCatch({
      df <- fetch_datasus(
        year_start         = anos[i],
        year_end           = anos[i],
        uf                 = uf,
        information_system = sistema
      )
      # Coerce tudo para character: elimina conflitos de tipo entre anos
      dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
    }, error = function(e) {
      warning(sprintf("    [AVISO] %s %d falhou: %s", sistema, anos[i], conditionMessage(e)))
      NULL
    })
  }
  dplyr::bind_rows(lista)
}

# Exibe resumo de qualidade de um data.frame
resumo_qualidade <- function(df, nome_base, vars_chave = NULL) {
  cat(sprintf("\n=== RESUMO: %s ===\n", nome_base))
  cat(sprintf("  Linhas: %d | Colunas: %d\n", nrow(df), ncol(df)))

  if (!is.null(vars_chave)) {
    cat("  Missing nas variáveis-chave:\n")
    for (v in vars_chave) {
      if (v %in% names(df)) {
        pct <- mean(is.na(df[[v]]) | df[[v]] %in% c("", "99", "9999", "99999")) * 100
        cat(sprintf("    %-25s %6.2f%% ausente/ignorado\n", v, pct))
      } else {
        cat(sprintf("    %-25s [coluna ausente na base]\n", v))
      }
    }
  }
}

# Verifica código da UF nas bases DATASUS (CODMUNRES começa com UF_IBGE)
verifica_uf <- function(df, col_codmun = "CODMUNRES") {
  if (!col_codmun %in% names(df)) {
    cat(sprintf("  [AVISO] Coluna '%s' não encontrada.\n", col_codmun))
    return(invisible(NULL))
  }
  codigo_uf_obs <- unique(substr(as.character(df[[col_codmun]]), 1, 2))
  tem_16 <- "16" %in% codigo_uf_obs
  cat(sprintf("  Verificação UF IBGE 16: %s\n", if (tem_16) "PRESENTE nos dados" else "AUSENTE — rever filtro"))
  cat(sprintf("  Códigos UF observados: %s\n", paste(sort(codigo_uf_obs), collapse = ", ")))
  invisible(tem_16)
}

# ==============================================================================
# 3. BASE 1 — SINASC: NASCIDOS VIVOS DO AMAPÁ (2000–2024)
# ==============================================================================
# Fonte: DATASUS / SINASC via pacote microdatasus
# Nota sobre 2024: dados potencialmente preliminares (defasagem típica de
#   12-18 meses no SINASC; cf. Szwarcwald et al. 2019 apud _contexto_demografia.md §2.17)
# Decisão metodológica: filtrar por CODMUNRES == "16..." para capturar apenas
#   nascimentos cujas mães residiam no Amapá (local de residência ≠ ocorrência).
# ==============================================================================

cat("\n\n--- Baixando SINASC AP (2000–2024) ---\n")

arq_sinasc <- file.path(DIR_BRUTOS, "sinasc_ap_2000_2024.rds")

if (!file.exists(arq_sinasc)) {
  sinasc_bruto <- fetch_datasus_seguro(ANO_INI, ANO_FIM, UF_SIGLA, "SINASC")
  saveRDS(sinasc_bruto, arq_sinasc)
  cat(sprintf("  SINASC salvo em: %s\n", arq_sinasc))
} else {
  sinasc_bruto <- readRDS(arq_sinasc)
  cat("  SINASC carregado do cache local.\n")
}

# Processar labels (process_sinasc converte códigos em fatores descritivos)
sinasc <- process_sinasc(sinasc_bruto)

# Período coberto
cat(sprintf("  Período: %s a %s\n",
            min(sinasc$DTNASC, na.rm = TRUE),
            max(sinasc$DTNASC, na.rm = TRUE)))

# Nota sobre 2024
n_2024_sinasc <- sum(substr(sinasc$DTNASC, 1, 4) == "2024", na.rm = TRUE)
cat(sprintf("  Registros com ano 2024: %d ", n_2024_sinasc))
cat("  [ATENÇÃO: dados de 2024 são PRELIMINARES — defasagem histórica de 12-18 meses no SINASC]\n")

# Verificar código UF
verifica_uf(sinasc_bruto, "CODMUNRES")

# Resumo de qualidade — variáveis-chave definidas pelas análises a fazer
vars_chave_sinasc <- c("DTNASC", "IDADEMAE", "SEXO", "RACACORMAE",
                       "ESCMAE", "CODMUNRES", "GESTACAO", "PESO",
                       "CONSULTAS", "MUNRESID")
resumo_qualidade(sinasc, "SINASC AP 2000–2024", vars_chave_sinasc)

# Totais anuais de nascimentos (para comparação com TabNet)
nascimentos_ano <- sinasc_bruto |>
  mutate(ANO = substr(DTNASC, 1, 4)) |>
  filter(!is.na(ANO)) |>
  count(ANO, name = "n_nascimentos") |>
  arrange(ANO)

cat("\n  Totais anuais de nascimentos (SINASC):\n")
print(as.data.frame(nascimentos_ano))

# ==============================================================================
# 4. BASE 2 — SIM-DO: ÓBITOS GERAIS DO AMAPÁ (2000–2024)
# ==============================================================================
# Fonte: DATASUS / SIM via microdatasus (information_system = "SIM-DO")
# O parâmetro uf="AP" recupera óbitos OCORRIDOS no Amapá.
# Para obter óbitos de RESIDENTES no Amapá (incluindo os que morreram em
#   outras UFs), a abordagem correta seria baixar todas as UFs e filtrar por
#   CODMUNRES, o que é computacionalmente proibitivo.
# Decisão metodológica adotada:
#   (a) Baixar SIM-DO "AP" (ocorridos no AP) e filtrar por CODMUNRES=="16..."
#       → captura residentes do AP que morreram no próprio estado (maioria);
#   (b) Documentar como limitação a exclusão de residentes do AP que morreram
#       em outros estados (tipicamente transferidos para hospitais de referência
#       em Belém/PA ou Manaus/AM).
# Sub-registro estimado: 20-40% no Norte (RIPSA/DATASUS).
# ==============================================================================

cat("\n\n--- Baixando SIM-DO AP (2000–2024) ---\n")

arq_sim_do <- file.path(DIR_BRUTOS, "sim_do_ap_2000_2024.rds")

if (!file.exists(arq_sim_do)) {
  sim_do_bruto <- fetch_datasus_seguro(ANO_INI, ANO_FIM, UF_SIGLA, "SIM-DO")
  saveRDS(sim_do_bruto, arq_sim_do)
  cat(sprintf("  SIM-DO salvo em: %s\n", arq_sim_do))
} else {
  sim_do_bruto <- readRDS(arq_sim_do)
  cat("  SIM-DO carregado do cache local.\n")
}

sim_do <- process_sim(sim_do_bruto)

# Período coberto
cat(sprintf("  Período: %s a %s\n",
            min(sim_do$DTOBITO, na.rm = TRUE),
            max(sim_do$DTOBITO, na.rm = TRUE)))

# Nota sobre 2024
n_2024_sim <- sum(substr(sim_do$DTOBITO, 1, 4) == "2024", na.rm = TRUE)
cat(sprintf("  Registros com ano 2024: %d ", n_2024_sim))
cat("  [ATENÇÃO: dados de 2024 podem ser PRELIMINARES/PARCIAIS]\n")

# Verificar código UF
verifica_uf(sim_do_bruto, "CODMUNRES")

# Resumo de qualidade
vars_chave_sim <- c("DTOBITO", "CAUSABAS", "SEXO", "IDADE",
                    "RACACOR", "ESC", "CODMUNRES", "LOCOCOR")
resumo_qualidade(sim_do, "SIM-DO AP 2000–2024", vars_chave_sim)

# Totais anuais de óbitos
obitos_ano <- sim_do_bruto |>
  mutate(ANO = substr(DTOBITO, 1, 4)) |>
  filter(!is.na(ANO)) |>
  count(ANO, name = "n_obitos") |>
  arrange(ANO)

cat("\n  Totais anuais de óbitos (SIM-DO):\n")
print(as.data.frame(obitos_ano))

# Proporção com causa básica ignorada (capítulo XVIII = R00-R99 = mal definidas)
pct_mal_def <- mean(
  substr(sim_do_bruto$CAUSABAS, 1, 1) == "R",
  na.rm = TRUE
) * 100
cat(sprintf("\n  Óbitos por causas mal definidas (cap. XVIII CID-10): %.1f%%\n",
            pct_mal_def))
cat("  [Referência nacional: ~15%; Norte pode ser maior — cf. RIPSA]\n")

# ==============================================================================
# 5. BASE 3 — SIM-DOFET: ÓBITOS FETAIS DO AMAPÁ (2000–2024)
# ==============================================================================
# Fonte: DATASUS / SIM via microdatasus (information_system = "SIM-DOFET")
# Óbito fetal no Brasil (definição legal): feto sem vida ao nascer com
#   ≥ 20 semanas de gestação OU ≥ 500 g OU ≥ 25 cm.
# Será usado para calcular TMPerinatal e TMFetal_tardia.
# ==============================================================================

cat("\n\n--- Baixando SIM-DOFET AP (2000–2024) ---\n")

arq_sim_fet <- file.path(DIR_BRUTOS, "sim_dofet_ap_2000_2024.rds")

if (!file.exists(arq_sim_fet)) {
  sim_fet_bruto <- fetch_datasus_seguro(ANO_INI, ANO_FIM, UF_SIGLA, "SIM-DOFET")
  saveRDS(sim_fet_bruto, arq_sim_fet)
  cat(sprintf("  SIM-DOFET salvo em: %s\n", arq_sim_fet))
} else {
  sim_fet_bruto <- readRDS(arq_sim_fet)
  cat("  SIM-DOFET carregado do cache local.\n")
}

sim_fet <- process_sim(sim_fet_bruto)

cat(sprintf("  Registros fetais: %d\n", nrow(sim_fet)))
verifica_uf(sim_fet_bruto, "CODMUNRES")

vars_chave_fet <- c("DTOBITO", "SEXO", "GESTACAO", "CODMUNRES")
resumo_qualidade(sim_fet, "SIM-DOFET AP 2000–2024", vars_chave_fet)

obitos_fet_ano <- sim_fet_bruto |>
  mutate(ANO = substr(DTOBITO, 1, 4)) |>
  filter(!is.na(ANO)) |>
  count(ANO, name = "n_obitos_fetais") |>
  arrange(ANO)

cat("\n  Totais anuais de óbitos fetais:\n")
print(as.data.frame(obitos_fet_ano))

# ==============================================================================
# 6. BASE 4 — PROJEÇÕES POPULACIONAIS IBGE REVISÃO 2024 (SIDRA)
# ==============================================================================
# Fonte: SIDRA/IBGE via pacote sidrar
#
# Tabelas relevantes no SIDRA para projeções estaduais:
#   - Tabela 7358: "Projeção da população, por sexo e grupos de idade"
#     (Revisão 2018 — publicada com Censo 2010 como base)
#   - Tabela 9697: Revisão 2024 — confirmada junto ao IBGE em 2024/2025
#     (pode não estar disponível via sidrar; verificar sidrar::search_sidra())
#
# Decisão metodológica: tentar tabela 9697 (revisão 2024) e, em caso de falha,
#   usar tabela 7358 (revisão 2018) documentando a diferença. A revisão 2024
#   incorpora o Censo 2022 como nova base e é a versão metodologicamente
#   preferível para trabalhos com dados a partir de 2023.
#
# Variável de interesse: população por sexo e IDADE SIMPLES (0, 1, 2, ..., 99+)
#   para o Amapá (código IBGE de UF = 16).
# ==============================================================================

cat("\n\n--- Baixando projeções populacionais IBGE (SIDRA) ---\n")

arq_proj <- file.path(DIR_BRUTOS, "projecoes_ibge_ap.rds")

# Função auxiliar: inspeciona a tabela SIDRA via info_sidra() para descobrir
# os IDs reais de variáveis antes de chamar get_sidra().
# Razão: passar variable = "allAvailableVar" falha em tabelas de projeção porque
# o sidrar não consegue enumerar as variáveis quando a tabela usa classificações
# complexas — o erro é "does not contain the allAvailableVar variable".
# Solução: chamar info_sidra() primeiro, extrair os IDs, depois get_sidra().
tenta_sidra <- function(tabela_id,
                        geo        = "State",
                        geo_filter = list(State = "16"),
                        variable   = NULL,    # NULL = descobrir via info_sidra
                        period     = "all") {

  # Passo 1: inspecionar a tabela
  info <- tryCatch(
    info_sidra(tabela_id),
    error = function(e) {
      message(sprintf("  [AVISO] info_sidra(%d) falhou: %s", tabela_id, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(info)) return(NULL)

  # Passo 2: descobrir IDs de variável se não fornecidos.
  # info_sidra() retorna $variable (singular) com colunas "cod" e "desc.
  if (is.null(variable)) {
    vars_df <- tryCatch(info[["variable"]], error = function(e) NULL)
    if (!is.null(vars_df) && "cod" %in% names(vars_df)) {
      variable <- vars_df$cod
      cat(sprintf("    Tabela %d — variáveis encontradas: %s\n",
                  tabela_id, paste(variable, collapse = ", ")))
    } else {
      variable <- 93  # fallback: código padrão de "Pessoas" no SIDRA/IBGE
      cat(sprintf("    Tabela %d — variáveis não detectadas; usando variable=93\n",
                  tabela_id))
    }
  }

  # Passo 3: tentar o download com os IDs descobertos
  tryCatch(
    get_sidra(x = tabela_id, variable = variable, geo = geo,
              geo.filter = geo_filter, period = period, format = 3),
    error = function(e) {
      message(sprintf("  [AVISO] get_sidra(%d) falhou: %s",
                      tabela_id, conditionMessage(e)))
      NULL
    }
  )
}

if (!file.exists(arq_proj)) {

  # Parâmetros confirmados via info_sidra(7358):
  #   variable = 606  ("População")
  #   c2    = Sexo    (Total, Homens, Mulheres)
  #   c287  = Idade   (idades simples 0–90+, 111 categorias)
  #   c1933 = Ano     (2000–2060, 61 categorias)
  #   period = "2018" (ano de publicação da revisão — fixo na tabela)
  #   geo = "State" + geo.filter = list(State = "16") → apenas Amapá
  #
  # Tabela 9697 (Revisão 2024): descartada.
  # info_sidra(9697) confirmou que a tabela existe mas só está disponível em
  # nível nacional (N1) — não suporta nível estadual (N3). Inútil para o AP.
  cat("  Tabela 9697 (Revisão 2024) descartada: sem nível estadual no SIDRA.\n")
  proj <- NULL

  # Tabela 7358 (Revisão 2018) — parâmetros confirmados via diagnóstico:
  #   variable = 606  (População)
  #   Não passar 'classific': o padrão "allClassific" inclui automaticamente
  #   c2 (Sexo), c287 (Idades simples 0–90+) e c1933 (Anos 2000–2060).
  #   Passar classific = c("c2","c287","c1933") causava o erro
  #   "'length = 3' em coerção a 'logical(1)'" dentro do sidrar.
  #   period = "2018" é o ano de publicação (fixo); os anos de projeção
  #   ficam na classificação c1933, não no parâmetro period.
  cat("  Baixando SIDRA tabela 7358 (Revisão 2018)...\n")
  proj <- tryCatch(
    get_sidra(
      x          = 7358,
      variable   = 606,
      geo        = "State",
      geo.filter = list(State = "16"),
      period     = "2018",
      format     = 3
      # classific omitido → "allClassific" (padrão): inclui c2, c287 e c1933
    ),
    error = function(e) {
      message("  [AVISO] get_sidra(7358) falhou: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(proj)) {
    saveRDS(proj, arq_proj)
    cat(sprintf("  Projeções salvas em: %s\n", arq_proj))
  } else {
    cat("\n  [FALLBACK — AÇÃO NECESSÁRIA]\n")
    cat("  Nenhuma tabela SIDRA de projeções funcionou.\n")
    cat("  Download manual da Revisão 2024:\n")
    cat("  https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html\n")
    cat("  Após baixar o Excel, carregar com:\n")
    cat("    library(readxl); proj <- read_excel('arquivo.xlsx')\n")
    cat("    saveRDS(proj, '", arq_proj, "')\n", sep = "")
  }

} else {
  proj <- readRDS(arq_proj)
  cat("  Projeções carregadas do cache local.\n")
}

if (!is.null(proj)) {
  cat(sprintf("  Dimensões: %d linhas × %d colunas\n", nrow(proj), ncol(proj)))

  # Verificar se cobre o Amapá (código 16)
  col_uf <- grep("(Unidade|Estado|UF).*(dig|igo)", names(proj),
                 value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(col_uf)) {
    ufs_obs <- unique(proj[[col_uf]])
    cat(sprintf("  UFs presentes: %s\n", paste(sort(ufs_obs), collapse = ", ")))
    cat(sprintf("  Amapá (16) presente: %s\n",
                if ("16" %in% ufs_obs) "SIM" else "NÃO"))
  }

  # Verificar cobertura temporal (anos na classificação c1933)
  col_ano <- grep("Ano", names(proj), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(col_ano)) {
    anos_proj <- sort(unique(proj[[col_ano]]))
    cat(sprintf("  Anos cobertos: %s a %s (%d anos)\n",
                min(anos_proj), max(anos_proj), length(anos_proj)))
  }
}

# ==============================================================================
# 7. BASE 5 — CENSO 2022: POPULAÇÃO DO AMAPÁ POR SEXO E IDADE (censobr / SIDRA)
# ==============================================================================
# Fonte preferencial: censobr (microdados do Censo 2022)
#   - Variável v0601 = sexo (1=Masc, 2=Fem)
#   - Variável v0602 = idade
#   - Filtrar por code_state == 16 (Amapá)
# Fonte alternativa: SIDRA tabela 9514 (Censo 2022, população por sexo e idade)
#
# Decisão metodológica: censobr permite análises com pesos amostrais corretos;
#   para denominadores populacionais simples (taxas), SIDRA é suficiente e mais
#   leve. Usar SIDRA como primeira opção para economia de memória/tempo.
# ==============================================================================

cat("\n\n--- Baixando Censo 2022 — Amapá por sexo e idade ---\n")

arq_censo <- file.path(DIR_BRUTOS, "censo2022_pop_ap.rds")

if (!file.exists(arq_censo)) {

  # Parâmetros confirmados via info_sidra(9514):
  #   variable = 93  ("População residente")
  #   c2   = Sexo (Total, Homens, Mulheres)
  #   c287 = Idade (idades simples < 1 mês até 100+, 134 categorias)
  #   c286 = Forma de declaração da idade (Total=113635, Data nascimento, Presumida)
  #
  # Decisão: baixar allCategories e filtrar depois por:
  #   Sexo != "Total"  (para não duplicar totais)
  #   Forma == "Total" (código 113635 — inclui ambas as formas de declaração)
  #   Idade = idades simples (excluir grupos como "0 a 4 anos")
  # Parâmetros confirmados via info_sidra(9514):
  #   variable = 93  (População residente)
  #   Não passar 'classific': mesmo bug da tabela 7358 — passar
  #   classific = c("c2","c287","c286") causa "'length = 3' em coerção a
  #   'logical(1)'" dentro do sidrar. Sem classific, o padrão "allClassific"
  #   inclui c2 (Sexo), c287 (Idades simples) e c286 (Forma de declaração).
  #   Filtrar c286 == "Total" na etapa de limpeza (script 03_populacao.R).
  cat("  Baixando SIDRA tabela 9514 (Censo 2022 — pop por sexo e idade)...\n")
  censo_sidra <- tryCatch(
    get_sidra(
      x          = 9514,
      variable   = 93,
      geo        = "State",
      geo.filter = list(State = "16"),
      period     = "2022",
      format     = 3
      # classific omitido → "allClassific": inclui c2, c287 e c286
    ),
    error = function(e) {
      message("  [AVISO] get_sidra(9514) falhou: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(censo_sidra)) {
    saveRDS(censo_sidra, arq_censo)
    cat(sprintf("  Censo 2022 (SIDRA) salvo em: %s\n", arq_censo))
    censo_pop <- censo_sidra
    attr(censo_pop, "fonte") <- "SIDRA tabela 9514"
  } else {
    # Tentativa 2: censobr
    cat("  Tentando censobr::read_population(year = 2022)...\n")
    censo_cb <- tryCatch({
      censobr::read_population(
        year    = 2022,
        columns = c("code_state", "v0601", "v0602", "v0010"),
        add_labels = "pt"
      ) |>
        filter(code_state == 16)
    }, error = function(e) {
      message("  [AVISO] censobr falhou: ", conditionMessage(e))
      NULL
    })

    if (!is.null(censo_cb)) {
      saveRDS(censo_cb, arq_censo)
      cat(sprintf("  Censo 2022 (censobr) salvo em: %s\n", arq_censo))
      censo_pop <- censo_cb
      attr(censo_pop, "fonte") <- "censobr::read_population(2022)"
    } else {
      cat("  [AÇÃO NECESSÁRIA] Baixar manualmente:\n")
      cat("  SIDRA 9514: https://sidra.ibge.gov.br/tabela/9514\n")
      censo_pop <- NULL
    }
  }
} else {
  censo_pop <- readRDS(arq_censo)
  cat("  Censo 2022 carregado do cache local.\n")
}

if (!is.null(censo_pop)) {
  cat(sprintf("  Dimensões: %d linhas × %d colunas\n", nrow(censo_pop), ncol(censo_pop)))
  cat("  Colunas disponíveis:\n")
  print(names(censo_pop))
}

# ==============================================================================
# 8. PAINEL CONSOLIDADO DE QUALIDADE — IMPRESSÃO FINAL
# ==============================================================================

cat("\n\n")
cat(strrep("=", 72), "\n")
cat("PAINEL CONSOLIDADO — BASES BAIXADAS\n")
cat(strrep("=", 72), "\n")

bases_info <- list(
  list(nome = "SINASC AP 2000-2024",
       arq  = arq_sinasc,
       obj  = if (exists("sinasc"))      sinasc      else NULL),
  list(nome = "SIM-DO AP 2000-2024",
       arq  = arq_sim_do,
       obj  = if (exists("sim_do"))      sim_do      else NULL),
  list(nome = "SIM-DOFET AP 2000-2024",
       arq  = arq_sim_fet,
       obj  = if (exists("sim_fet"))     sim_fet     else NULL),
  list(nome = "Projeções IBGE AP",
       arq  = arq_proj,
       obj  = if (exists("proj"))        proj        else NULL),
  list(nome = "Censo 2022 AP",
       arq  = arq_censo,
       obj  = if (exists("censo_pop"))   censo_pop   else NULL)
)

for (b in bases_info) {
  arq_existe <- file.exists(b$arq)
  tam_kb <- if (arq_existe) round(file.info(b$arq)$size / 1024) else NA
  n_linhas <- if (!is.null(b$obj) && is.data.frame(b$obj)) nrow(b$obj) else NA
  cat(sprintf(
    "  %-30s | Arquivo: %-3s | Tam: %6s KB | Linhas: %s\n",
    b$nome,
    if (arq_existe) "OK" else "NÃO",
    if (!is.na(tam_kb)) tam_kb else "—",
    if (!is.na(n_linhas)) n_linhas else "—"
  ))
}

cat(strrep("=", 72), "\n")
cat(sprintf("Script concluído em: %s\n", Sys.time()))

################################################################################
# PRÓXIMOS PASSOS (executados nos scripts seguintes):
#   01_limpeza_sinasc.R    — filtrar residentes AP, recodificar, salvar processado
#   02_limpeza_sim.R       — idem para SIM-DO e SIM-DOFET
#   03_populacao.R         — organizar projeções e Censo 2022 por idade simples
#   04_indicadores_fecund.R — TBN, TFG, TEF (por idade e ano), TFT
#   05_indicadores_mortal.R — TBM, TEM, TMI, TMPerinatal, padronização
#   06_tabua_vida.R        — construção da tábua de vida abreviada (Reed-Merrell)
#   07_graficos.R          — pirâmide etária, curvas TEF, tendências
#   08_relatorio.R         — geração do relatório final em LaTeX/RMarkdown
################################################################################
