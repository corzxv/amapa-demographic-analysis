################################################################################
# TRABALHO 1 — DEMOGRAFIA — UnB
# Questão 3: Indicadores de Mortalidade Geral e Infantil/Perinatal
# Amapá (AP, código IBGE 16) | Anos: 2010, 2019, 2021, 2022, 2024
#
# Referência metodológica: materiais da disciplina e documentação das fontes oficiais.
################################################################################

# ==============================================================================
# 0. CONFIGURAÇÃO
# ==============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(writexl)

RAIZ       <- "C:/Users/Concursos Felipe/Documents/UnB/Trabalho Demografia/demografia_ap"
DIR_BRUTOS <- file.path(RAIZ, "dados", "brutos")
DIR_PROC   <- file.path(RAIZ, "dados", "processados")
DIR_TAB    <- file.path(RAIZ, "outputs", "tabelas")
DIR_GRF    <- file.path(RAIZ, "outputs", "graficos")

UF_IBGE  <- "16"
ANOS_ANA <- c(2010, 2019, 2021, 2022, 2024)
ANOS_TMI <- c(2022, 2023, 2024)   # média para TMI

# Grupos etários para nMx (18 grupos)
GRUPOS_MX <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80+")

# ==============================================================================
# 1. FUNÇÕES AUXILIARES
# ==============================================================================

# Decodifica o campo IDADE do SIM (raw) → idade em anos completos.
# Codificação DATASUS: 1º dígito = unidade (0=min,1=h,2=d,3=mês,4=ano,5=100+ano)
#   dígitos 2-3 = quantidade.
# Retorna 0 para todas as mortes com menos de 1 ano (códigos 0,1,2,3) — exceto
#   "400" (0 anos completos, ambíguo dentro do 1º ano) que também retorna 0.
decode_idade_anos <- function(x) {
  x <- as.character(x)
  x[is.na(x) | nchar(trimws(x)) < 3] <- NA_character_
  u <- substr(x, 1, 1)
  v <- suppressWarnings(as.integer(substr(x, 2, 3)))
  case_when(
    u %in% c("0","1","2","3")          ~ 0,
    u == "4"                            ~ as.double(v),
    u == "5"                            ~ 100 + as.double(v),
    TRUE                                ~ NA_real_
  )
}

# Decodifica o campo IDADE do SIM → idade em dias (para componentização TMI).
# Retorna NA para "400" (0 anos — não é possível determinar dias exatos).
decode_idade_dias <- function(x) {
  x <- as.character(x)
  x[is.na(x) | nchar(trimws(x)) < 3] <- NA_character_
  u <- substr(x, 1, 1)
  v <- suppressWarnings(as.double(substr(x, 2, 3)))
  case_when(
    u == "0" ~ v / (60 * 24),            # minutos → dias
    u == "1" ~ v / 24,                   # horas → dias
    u == "2" ~ v,                        # dias
    u == "3" ~ v * 30.44,               # meses → dias (aprox.)
    TRUE     ~ NA_real_                  # "400" e resto: indeterminado
  )
}

# Classifica idade em anos → grupo etário para nMx
classificar_grupo_mx <- function(idade_anos) {
  case_when(
    idade_anos == 0                          ~ "<1",
    idade_anos >= 1  & idade_anos <= 4       ~ "1-4",
    idade_anos >= 5  & idade_anos <= 9       ~ "5-9",
    idade_anos >= 10 & idade_anos <= 14      ~ "10-14",
    idade_anos >= 15 & idade_anos <= 19      ~ "15-19",
    idade_anos >= 20 & idade_anos <= 24      ~ "20-24",
    idade_anos >= 25 & idade_anos <= 29      ~ "25-29",
    idade_anos >= 30 & idade_anos <= 34      ~ "30-34",
    idade_anos >= 35 & idade_anos <= 39      ~ "35-39",
    idade_anos >= 40 & idade_anos <= 44      ~ "40-44",
    idade_anos >= 45 & idade_anos <= 49      ~ "45-49",
    idade_anos >= 50 & idade_anos <= 54      ~ "50-54",
    idade_anos >= 55 & idade_anos <= 59      ~ "55-59",
    idade_anos >= 60 & idade_anos <= 64      ~ "60-64",
    idade_anos >= 65 & idade_anos <= 69      ~ "65-69",
    idade_anos >= 70 & idade_anos <= 74      ~ "70-74",
    idade_anos >= 75 & idade_anos <= 79      ~ "75-79",
    idade_anos >= 80                         ~ "80+",
    TRUE                                     ~ NA_character_
  )
}

# Extrai ano de DTOBITO robustamente (suporta YYYYMMDD, DDMMYYYY, YYYY-MM-DD,
# DD/MM/YYYY). Prioriza chars 1-4 se formarem ano válido (1990-2030); caso
# contrário tenta chars 5-8 (formato DDMMYYYY do SIM-AP confirmado no diagnóstico).
extrair_ano_obito <- function(x) {
  x  <- as.character(trimws(x))
  a1 <- suppressWarnings(as.integer(substr(x, 1, 4)))
  a2 <- suppressWarnings(as.integer(substr(x, 5, 8)))
  if_else(!is.na(a1) & a1 >= 1990 & a1 <= 2030, a1,
    if_else(!is.na(a2) & a2 >= 1990 & a2 <= 2030, a2, NA_integer_))
}

# Converte coluna SIDRA para numérico (separa milhar PT-BR)
parse_sidra_num <- function(x) {
  x <- as.character(x)
  x[trimws(x) %in% c("-","..","...","X","x","")] <- NA_character_
  x <- gsub("\\.", "", x)
  x <- gsub(",",  ".", x)
  suppressWarnings(as.numeric(x))
}

# ==============================================================================
# 2. CARREGAR DADOS
# ==============================================================================

cat("Carregando bases...\n")
sim_bruto     <- readRDS(file.path(DIR_BRUTOS, "sim_do_ap_2000_2024.rds"))
sim_fet_bruto <- readRDS(file.path(DIR_BRUTOS, "sim_dofet_ap_2000_2024.rds"))
sinasc_proc   <- readRDS(file.path(DIR_PROC,   "sinasc_ap_proc.rds"))
proj_raw      <- readRDS(file.path(DIR_BRUTOS, "projecoes_ibge_ap.rds"))
cat("Bases carregadas.\n")

# ==============================================================================
# 3. PROCESSAR PROJEÇÕES POPULACIONAIS
# (mesma lógica do script 02, aqui replicada para independência do script)
# ==============================================================================

names(proj_raw) <- make.unique(names(proj_raw), sep = ".")

proj_base <- proj_raw |>
  rename(sexo = Sexo, idade = Idade, ano = `Ano.1`, pop = Valor) |>
  mutate(
    ano = suppressWarnings(as.integer(as.character(ano))),
    pop = parse_sidra_num(pop)
  ) |>
  filter(!is.na(pop), pop > 0, !is.na(ano), ano >= 2000)

# Pop. total por ano (linha Sexo="Total", Idade="Total")
pop_total_proj <- proj_base |>
  filter(grepl("^Total$", sexo,  ignore.case = TRUE),
         grepl("^Total$", idade, ignore.case = TRUE),
         ano %in% c(ANOS_ANA, ANOS_TMI)) |>
  group_by(ano) |>
  summarise(pop_total = sum(pop, na.rm = TRUE), .groups = "drop")

# Pop. por sexo e grupo etário para nMx — apenas idades simples (sem "X a Y anos")
# NOTA: SIDRA tabela 7358 usa "Homens"/"Mulheres" (não "Homem"/"Mulher").
#   grepl("^Hom", ...) captura ambos.
pop_mx_proj <- proj_base |>
  filter(
    !grepl("^Total$", sexo,  ignore.case = TRUE),
    !grepl("^Total$", idade, ignore.case = TRUE),
    !grepl(" a ", as.character(idade), fixed = TRUE),
    ano %in% ANOS_ANA
  ) |>
  mutate(
    idade_num   = suppressWarnings(as.integer(gsub("[^0-9].*", "", as.character(idade)))),
    sexo_padrao = case_when(
      grepl("^Hom|Masc", sexo, ignore.case = TRUE) ~ "Masculino",
      grepl("Mulher|^Fem", sexo, ignore.case = TRUE) ~ "Feminino",
      TRUE ~ NA_character_
    ),
    grupo_mx = classificar_grupo_mx(idade_num)
  ) |>
  filter(!is.na(sexo_padrao), !is.na(grupo_mx), !is.na(idade_num)) |>
  group_by(ano, sexo_padrao, grupo_mx) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

cat("Sexos em pop_mx_proj:", paste(unique(pop_mx_proj$sexo_padrao), collapse = " | "), "\n")

cat(sprintf("pop_mx_proj: %d linhas | anos: %s\n",
            nrow(pop_mx_proj),
            paste(sort(unique(pop_mx_proj$ano)), collapse = " ")))

# ==============================================================================
# 4. PROCESSAR SIM — ÓBITOS GERAIS
# ==============================================================================
# Usa dados BRUTOS (não process_sim) para preservar a codificação IDADE intacta,
# necessária para decode_idade_anos() e decode_idade_dias().

# --- Diagnóstico do formato de DTOBITO (executar uma vez para validar) ---
dtobito_amostra <- head(unique(as.character(sim_bruto$DTOBITO[!is.na(sim_bruto$DTOBITO)])), 8)
cat("\nAmostra DTOBITO (raw):", paste(dtobito_amostra, collapse = " | "), "\n")
cat("Classe DTOBITO:", class(sim_bruto$DTOBITO), "\n")
# Se os valores começarem com dia (ex.: "15032010"), chars 1-4 = "1503" ≠ ano
# → extrair_ano_obito() detecta automaticamente e usa chars 5-8 como ano.

sim_ap <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr     = as.character(IDADE),
    DTOBITO_chr   = as.character(DTOBITO),
    SEXO_chr      = as.character(SEXO)
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE
  ) |>
  mutate(
    ano_obito  = extrair_ano_obito(DTOBITO_chr),
    sexo       = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE            ~ "Ignorado"
    ),
    idade_anos = decode_idade_anos(IDADE_chr),
    grupo_mx   = classificar_grupo_mx(idade_anos)
  ) |>
  filter(!is.na(ano_obito))

cat(sprintf("\nSIM AP residentes filtrados: %d registros\n", nrow(sim_ap)))
cat(sprintf("  Período: %d–%d\n", min(sim_ap$ano_obito), max(sim_ap$ano_obito)))
cat(sprintf("  Sexo ignorado: %.1f%%\n",
            mean(sim_ap$sexo == "Ignorado") * 100))
cat(sprintf("  IDADE indeterminada (NA): %.1f%%\n",
            mean(is.na(sim_ap$idade_anos)) * 100))
cat("  Distribuição de óbitos por ano (ANOS_ANA):\n")
print(sim_ap |> filter(ano_obito %in% ANOS_ANA) |> count(ano_obito))

# Dados de 2024 são preliminares
n_2024 <- sum(sim_ap$ano_obito == 2024)
cat(sprintf("  Óbitos 2024: %d [ATENÇÃO: dados PRELIMINARES]\n", n_2024))

# ==============================================================================
# 5. QUESTÃO 3a — TAXA BRUTA DE MORTALIDADE (TBM)
# ==============================================================================

obitos_ano <- sim_ap |>
  filter(ano_obito %in% ANOS_ANA) |>
  count(ano_obito, name = "n_obitos") |>
  rename(ano = ano_obito)

tab_tbm <- obitos_ano |>
  left_join(pop_total_proj, by = "ano") |>
  mutate(TBM = round(n_obitos / pop_total * 1000, 2))

cat("\n--- TBM por ano ---\n")
print(tab_tbm)

# ==============================================================================
# 6. QUESTÃO 3a — TAXAS ESPECÍFICAS DE MORTALIDADE (nMx)
# ==============================================================================

# Contagem de óbitos por ano, sexo e grupo etário
obitos_mx <- sim_ap |>
  filter(ano_obito %in% ANOS_ANA,
         sexo %in% c("Masculino","Feminino"),
         !is.na(grupo_mx)) |>
  count(ano_obito, sexo, grupo_mx, name = "n_obitos") |>
  rename(ano = ano_obito)

# Completar com zeros (alguns grupos podem ter 0 óbitos em certos anos)
grade_completa <- expand_grid(
  ano      = ANOS_ANA,
  sexo     = c("Masculino","Feminino"),
  grupo_mx = GRUPOS_MX
)

nMx_long <- grade_completa |>
  left_join(obitos_mx, by = c("ano","sexo","grupo_mx")) |>
  replace_na(list(n_obitos = 0L)) |>
  left_join(pop_mx_proj,
            by = c("ano", "sexo" = "sexo_padrao", "grupo_mx")) |>
  mutate(
    nMx      = if_else(is.na(pop) | pop == 0, NA_real_,
                       n_obitos / pop * 1000),
    grupo_mx = factor(grupo_mx, levels = GRUPOS_MX)
  )

cat("\n--- nMx (amostra — Masculino 2022) ---\n")
print(nMx_long |> filter(sexo == "Masculino", ano == 2022) |>
        select(grupo_mx, n_obitos, pop, nMx))

# Tabelas wide
nMx_masc_wide <- nMx_long |>
  filter(sexo == "Masculino") |>
  select(grupo_mx, ano, nMx) |>
  mutate(nMx = round(nMx, 4)) |>
  pivot_wider(names_from = ano, values_from = nMx, names_prefix = "nMx_") |>
  rename(`Grupo etário` = grupo_mx)

nMx_fem_wide <- nMx_long |>
  filter(sexo == "Feminino") |>
  select(grupo_mx, ano, nMx) |>
  mutate(nMx = round(nMx, 4)) |>
  pivot_wider(names_from = ano, values_from = nMx, names_prefix = "nMx_") |>
  rename(`Grupo etário` = grupo_mx)

cat("\nnMx Masculino:\n"); print(nMx_masc_wide)
cat("\nnMx Feminino:\n");  print(nMx_fem_wide)

# ==============================================================================
# 7. GRÁFICOS nMx — escala logarítmica, homens e mulheres por ano
# ==============================================================================

cores_sexo <- c("Masculino" = "steelblue4", "Feminino" = "#8b1a1a")

p_nmx_list <- map(ANOS_ANA, function(a) {
  df <- nMx_long |>
    filter(ano == a, !is.na(nMx), nMx > 0)

  ggplot(df, aes(x = grupo_mx, y = nMx, color = sexo, group = sexo)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
    scale_y_log10(
      name   = "nMx (por mil hab., esc. log)",
      breaks = c(0.1, 0.5, 1, 5, 10, 50, 100, 200),
      labels = c("0,10","0,50","1,0","5,0","10","50","100","200")
    ) +
    scale_color_manual(values = cores_sexo, name = "Sexo") +
    labs(
      title    = sprintf("Taxas Específicas de Mortalidade — %d", a),
      subtitle = "Amapá — homens e mulheres (escala logarítmica)",
      x        = "Grupo etário"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(size = 9, color = "gray30"),
      axis.text.x     = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )
})
names(p_nmx_list) <- as.character(ANOS_ANA)

# Salvar gráficos individuais
walk2(p_nmx_list, as.character(ANOS_ANA), function(p, a) {
  ggsave(
    file.path(DIR_GRF, sprintf("nmx_%s_ap.png", a)),
    plot = p, width = 22, height = 13, units = "cm", dpi = 300
  )
})

# Painel combinado (3 + 2 layout)
p_nmx_painel <- (
  (p_nmx_list[["2010"]] + p_nmx_list[["2019"]] + p_nmx_list[["2021"]]) /
  (p_nmx_list[["2022"]] + p_nmx_list[["2024"]] + plot_spacer())
) +
  plot_annotation(
    title    = "Taxas Específicas de Mortalidade (nMx) por sexo e grupo etário",
    subtitle = "Amapá — anos selecionados — escala logarítmica",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "gray30")
    )
  )

ggsave(
  file.path(DIR_GRF, "nmx_painel_ap.png"),
  plot = p_nmx_painel, width = 40, height = 28, units = "cm", dpi = 300
)
cat("Gráficos nMx salvos.\n")

# ==============================================================================
# 8. QUESTÃO 3b — MORTALIDADE INFANTIL: IDENTIFICAR ÓBITOS < 1 ANO
# ==============================================================================
# Inclui mortes codificadas como: minutos (0xx), horas (1xx), dias (2xx),
# meses (3xx), OU "400" (0 anos completos — inclusas na TMI total mas com
# idade em dias indeterminada para os componentes).

obitos_inf <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr     = as.character(IDADE),
    DTOBITO_chr   = as.character(DTOBITO),
    SEXO_chr      = as.character(SEXO)
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE,
    substr(IDADE_chr, 1, 1) %in% c("0","1","2","3") |
      (substr(IDADE_chr, 1, 1) == "4" & substr(IDADE_chr, 2, 3) == "00")
  ) |>
  mutate(
    ano_obito  = extrair_ano_obito(DTOBITO_chr),
    sexo       = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE            ~ "Ignorado"
    ),
    idade_dias = decode_idade_dias(IDADE_chr),
    componente = case_when(
      is.na(idade_dias)                  ~ "Indeterminado",
      idade_dias <  7                    ~ "Neonatal precoce (0-6d)",
      idade_dias <  28                   ~ "Neonatal tardio (7-27d)",
      TRUE                               ~ "Pós-neonatal (28d-<1a)"
    )
  ) |>
  filter(!is.na(ano_obito))

# Qualidade: % com idade em dias indeterminada
pct_indet <- mean(obitos_inf$componente == "Indeterminado") * 100
cat(sprintf("\nÓbitos infantis AP total: %d\n", nrow(obitos_inf)))
cat(sprintf("  Idade em dias indeterminada: %.1f%%\n", pct_indet))
cat("  (causa: código '400' = 0 anos completos sem precisão diária)\n")
cat("  Distribuição por componente:\n")
print(table(obitos_inf$componente, useNA = "ifany"))

# ==============================================================================
# 9. QUESTÃO 3b — TMI E COMPONENTES
# TMI = média(óbitos infantis 2022-2024) / NV_2023 * 1000
# ==============================================================================

# Nascidos vivos 2023 (total e por sexo)
nv_2023_total <- sinasc_proc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
         substr(as.character(DTNASC), 1, 4) == "2023") |>
  nrow()

nv_2023_sexo <- sinasc_proc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
         substr(as.character(DTNASC), 1, 4) == "2023") |>
  mutate(sexo_rn = case_when(
    as.character(SEXO) %in% c("1","Masculino") ~ "Masculino",
    as.character(SEXO) %in% c("2","Feminino")  ~ "Feminino",
    TRUE ~ "Ignorado"
  )) |>
  count(sexo_rn, name = "NV")

cat(sprintf("\nNascidos vivos 2023: %d\n", nv_2023_total))
cat("  Por sexo:\n"); print(nv_2023_sexo)

# Óbitos infantis por ano e sexo (inclui Ignorado no total)
inf_por_ano_sexo <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI)

# Helper: média de óbitos e TMI dado subconjunto e NV denominador
calcular_tmi <- function(df_inf, nv) {
  df_inf |>
    count(ano_obito, name = "n") |>
    summarise(media = mean(n)) |>
    mutate(NV = nv, TMI = round(media / NV * 1000, 2))
}

# TMI total
tmi_total <- calcular_tmi(inf_por_ano_sexo, nv_2023_total) |>
  mutate(sexo = "Total")

# TMI por sexo (só sexos identificados; denominador = NV do mesmo sexo)
tmi_sexo <- map_dfr(c("Masculino","Feminino"), function(s) {
  nv_s <- nv_2023_sexo |> filter(sexo_rn == s) |> pull(NV)
  if (length(nv_s) == 0) return(NULL)
  calcular_tmi(inf_por_ano_sexo |> filter(sexo == s), nv_s) |>
    mutate(sexo = s)
})

tab_tmi <- bind_rows(tmi_total, tmi_sexo) |>
  select(sexo, media, NV, TMI) |>
  rename(`Sexo` = sexo,
         `Média óbitos infantis 2022-24` = media,
         `NV 2023` = NV,
         `TMI (‰)` = TMI)

cat("\n--- TMI ---\n"); print(tab_tmi)

# ----- Componentes da TMI -----
# Apenas óbitos com componente determinado

comp_raw <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI,
         componente != "Indeterminado")

# Função: tabela de componentes por sexo dado nv denominador
tab_comp_sexo <- function(sexo_filtro, nv) {
  df <- if (sexo_filtro == "Total") comp_raw else
    comp_raw |> filter(sexo == sexo_filtro)

  df |>
    count(ano_obito, componente, name = "n") |>
    group_by(componente) |>
    summarise(media = mean(n), .groups = "drop") |>
    complete(componente = c("Neonatal precoce (0-6d)",
                            "Neonatal tardio (7-27d)",
                            "Pós-neonatal (28d-<1a)"),
             fill = list(media = 0)) |>
    mutate(taxa = round(media / nv * 1000, 2),
           sexo = sexo_filtro)
}

nv_masc <- nv_2023_sexo |> filter(sexo_rn == "Masculino") |> pull(NV)
nv_fem  <- nv_2023_sexo |> filter(sexo_rn == "Feminino")  |> pull(NV)

tab_componentes <- bind_rows(
  tab_comp_sexo("Total",     nv_2023_total),
  tab_comp_sexo("Masculino", nv_masc),
  tab_comp_sexo("Feminino",  nv_fem)
) |>
  select(sexo, componente, media, taxa) |>
  rename(Sexo = sexo, Componente = componente,
         `Média óbitos 2022-24` = media, `Taxa (‰)` = taxa)

cat("\n--- Componentes da TMI ---\n"); print(tab_componentes)

# ==============================================================================
# 10. QUESTÃO 3b — MORTALIDADE PERINATAL
# TMP = (fetais ≥22sem + neonatais precoces 0-6d) / (NV + fetais) × 1000
# Numerador e denominador: médias/referência 2022-2024 / 2023
# ==============================================================================

# Processar SIM-DOFET (óbitos fetais)
sim_fet_ap <- sim_fet_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    DTOBITO_chr   = as.character(DTOBITO),
    GESTACAO_chr  = as.character(GESTACAO)
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE
  ) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    # Codificação raw: "1"=<22sem,"2"=22-27,"3"=28-31,"4"=32-36,"5"=37-41,"6"=42+,"9"=ign.
    # Também aceita rótulos se process_sim() tiver sido aplicado.
    gest_code = suppressWarnings(as.integer(GESTACAO_chr)),
    gest_ge22 = case_when(
      gest_code %in% 2:6             ~ TRUE,
      grepl("22|28|32|37|42", GESTACAO_chr) ~ TRUE,
      TRUE                           ~ FALSE
    ),
    gest_ignorada = is.na(gest_code) | GESTACAO_chr %in% c("9","99")
  ) |>
  filter(!is.na(ano_obito))

pct_gest_ig <- mean(sim_fet_ap$gest_ignorada) * 100
cat(sprintf("\nÓbitos fetais AP filtrados: %d\n", nrow(sim_fet_ap)))
cat(sprintf("  GESTACAO ignorada: %.1f%%\n", pct_gest_ig))
cat("  [Alta incompletude esperada — ver log_qualidade_dados.md]\n")

# Contagem por ano (2022-2024): fetais ≥22 sem e todos (proxy)
fet_ano <- sim_fet_ap |>
  filter(ano_obito %in% ANOS_TMI) |>
  group_by(ano_obito) |>
  summarise(
    n_fet_ge22  = sum(gest_ge22),
    n_fet_todos = n(),
    .groups = "drop"
  )

cat("\nFetais por ano (2022-2024):\n"); print(fet_ano)

# Óbitos neonatais precoces (0-6 dias) por ano
neo_prec_ano <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI, componente == "Neonatal precoce (0-6d)") |>
  count(ano_obito, name = "n_neo_prec")

# Juntar e calcular médias
perinatal_df <- fet_ano |>
  left_join(neo_prec_ano, by = "ano_obito") |>
  replace_na(list(n_neo_prec = 0L))

media_fet_ge22  <- mean(perinatal_df$n_fet_ge22)
media_fet_todos <- mean(perinatal_df$n_fet_todos)
media_neo_prec  <- mean(perinatal_df$n_neo_prec)

# Denominadores referência 2023
n_fet_2023_ge22  <- sim_fet_ap |> filter(ano_obito == 2023, gest_ge22) |> nrow()
n_fet_2023_todos <- sim_fet_ap |> filter(ano_obito == 2023) |> nrow()

den_ge22  <- nv_2023_total + n_fet_2023_ge22
den_todos <- nv_2023_total + n_fet_2023_todos

TMP_ge22  <- (media_fet_ge22  + media_neo_prec) / den_ge22  * 1000
TMP_todos <- (media_fet_todos + media_neo_prec) / den_todos * 1000

tab_tmp <- tibble(
  Critério = c(
    "Óbitos fetais ≥22 semanas de gestação (critério da tarefa)",
    "Todos os óbitos fetais (proxy conservador — alta incompletude da GESTACAO)"
  ),
  `Fetais (média 2022-24)` = round(c(media_fet_ge22, media_fet_todos), 1),
  `Neo. precoces (média)`  = round(media_neo_prec, 1),
  `Denominador (NV+fet 2023)` = c(den_ge22, den_todos),
  `TMP (‰)` = round(c(TMP_ge22, TMP_todos), 2)
)

cat("\n--- Taxa de Mortalidade Perinatal ---\n"); print(tab_tmp)
cat(sprintf("\n[NOTA] %.1f%% dos óbitos fetais têm GESTACAO ignorada.\n", pct_gest_ig))
cat("Recomenda-se usar a linha 'todos os fetais' como estimativa mais robusta.\n")

# ==============================================================================
# 11. EXPORTAR TABELAS
# ==============================================================================

# 3a — TBM
tab_3a_tbm_exp <- tab_tbm |>
  rename(Ano=ano, `Óbitos totais`=n_obitos,
         `Pop. projetada`=pop_total, `TBM (‰)`=TBM)

# 3a — nMx
tab_3a_nmx_masc_exp <- nMx_masc_wide
tab_3a_nmx_fem_exp  <- nMx_fem_wide

# 3b — TMI e componentes
tab_3b_tmi_exp   <- tab_tmi
tab_3b_comp_exp  <- tab_componentes
tab_3b_tmp_exp   <- tab_tmp

# CSV
write_csv(tab_3a_tbm_exp,      file.path(DIR_TAB, "tab_3a_tbm.csv"))
write_csv(tab_3a_nmx_masc_exp, file.path(DIR_TAB, "tab_3a_nmx_masculino.csv"))
write_csv(tab_3a_nmx_fem_exp,  file.path(DIR_TAB, "tab_3a_nmx_feminino.csv"))
write_csv(tab_3b_tmi_exp,      file.path(DIR_TAB, "tab_3b_tmi.csv"))
write_csv(tab_3b_comp_exp,     file.path(DIR_TAB, "tab_3b_tmi_componentes.csv"))
write_csv(tab_3b_tmp_exp,      file.path(DIR_TAB, "tab_3b_mortalidade_perinatal.csv"))

# Excel (todas as abas)
write_xlsx(
  list(
    `3a_TBM`           = as.data.frame(tab_3a_tbm_exp),
    `3a_nMx_Masculino` = as.data.frame(tab_3a_nmx_masc_exp),
    `3a_nMx_Feminino`  = as.data.frame(tab_3a_nmx_fem_exp),
    `3b_TMI`           = as.data.frame(tab_3b_tmi_exp),
    `3b_TMI_componentes` = as.data.frame(tab_3b_comp_exp),
    `3b_TMP`           = as.data.frame(tab_3b_tmp_exp)
  ),
  path = file.path(DIR_TAB, "questao3_tabelas.xlsx")
)
cat("Tabelas exportadas.\n")

# ==============================================================================
# 12. SALVAR OBJETOS PARA ETAPA SEGUINTE (tábua de vida)
# ==============================================================================
# nMx_masculino e nMx_feminino: tibbles com todas as colunas para a tábua
# TMI_masculino e TMI_feminino: escalares (por mil nascidos vivos)

saveRDS(
  nMx_long |>
    filter(sexo == "Masculino") |>
    select(ano, grupo_mx, n_obitos, pop, nMx) |>
    arrange(ano, grupo_mx),
  file.path(DIR_PROC, "nMx_masculino.rds")
)

saveRDS(
  nMx_long |>
    filter(sexo == "Feminino") |>
    select(ano, grupo_mx, n_obitos, pop, nMx) |>
    arrange(ano, grupo_mx),
  file.path(DIR_PROC, "nMx_feminino.rds")
)

saveRDS(
  tmi_sexo |> filter(sexo == "Masculino") |> pull(TMI),
  file.path(DIR_PROC, "TMI_masculino.rds")
)

saveRDS(
  tmi_sexo |> filter(sexo == "Feminino") |> pull(TMI),
  file.path(DIR_PROC, "TMI_feminino.rds")
)

cat("\nRDS salvos em dados/processados/:\n")
cat("  nMx_masculino.rds\n  nMx_feminino.rds\n")
cat("  TMI_masculino.rds\n  TMI_feminino.rds\n")

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("QUESTÃO 3 CONCLUÍDA\n")
cat(strrep("=", 60), "\n")
cat("Gráficos em outputs/graficos/:\n")
cat("  nmx_2010_ap.png  nmx_2019_ap.png  nmx_2021_ap.png\n")
cat("  nmx_2022_ap.png  nmx_2024_ap.png\n")
cat("  nmx_painel_ap.png (painel combinado 40×28 cm)\n")
cat("Tabelas em outputs/tabelas/:\n")
cat("  tab_3a_tbm.csv\n  tab_3a_nmx_masculino.csv\n  tab_3a_nmx_feminino.csv\n")
cat("  tab_3b_tmi.csv\n  tab_3b_tmi_componentes.csv\n  tab_3b_mortalidade_perinatal.csv\n")
cat("  questao3_tabelas.xlsx\n")
cat("RDS para tábua de vida: dados/processados/nMx_{masculino,feminino}.rds\n")
cat("RDS para tábua de vida: dados/processados/TMI_{masculino,feminino}.rds\n")
