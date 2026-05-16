################################################################################
# TRABALHO 1 — DEMOGRAFIA — UnB
# Questão 2: Indicadores de Fecundidade
# Amapá (AP, código IBGE 16)
################################################################################

library(tidyverse)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)
library(writexl)
library(janitor)

RAIZ       <- "C:/Users/Concursos Felipe/Documents/UnB/Trabalho Demografia/demografia_ap"
DIR_BRUTOS <- file.path(RAIZ, "dados", "brutos")
DIR_PROC   <- file.path(RAIZ, "dados", "processados")
DIR_TAB    <- file.path(RAIZ, "outputs", "tabelas")
DIR_GRF    <- file.path(RAIZ, "outputs", "graficos")

UF_IBGE  <- "16"
ANOS_ANA <- c(2010, 2019, 2021, 2022, 2024)
GRUPOS   <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49")

# ==============================================================================
# 1. CARREGAR DADOS
# ==============================================================================

sinasc    <- readRDS(file.path(DIR_PROC, "sinasc_ap_proc.rds"))
censo_raw <- readRDS(file.path(DIR_BRUTOS, "censo2022_pop_ap.rds"))

# Projeção IBGE Revisão 2024 — AP
arq_proj_2024_rds <- file.path(DIR_BRUTOS, "projecoes_ibge_rev2024_ap.rds")
arq_proj_2024_csv <- file.path(DIR_BRUTOS, "projecoes_ibge_rev2024_ap.csv")

if (file.exists(arq_proj_2024_rds)) {
  proj_2024_ap <- readRDS(arq_proj_2024_rds)
} else if (file.exists(arq_proj_2024_csv)) {
  proj_2024_ap <- read.csv(arq_proj_2024_csv, fileEncoding = "UTF-8")
  saveRDS(proj_2024_ap, arq_proj_2024_rds)
} else {
  stop("Arquivo da Projeção IBGE Revisão 2024 não encontrado em dados/brutos.")
}

# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================

achar_col <- function(df, padroes, excluir = "\\(") {
  for (p in padroes) {
    candidatos <- grep(p, names(df), value = TRUE, ignore.case = TRUE)
    candidatos <- candidatos[!grepl(excluir, candidatos)]
    if (length(candidatos) >= 1) return(candidatos[1])
  }
  stop(sprintf("Coluna não encontrada: %s", paste(padroes, collapse = ", ")))
}

parse_sidra_num <- function(x) {
  x <- as.character(x)
  x[trimws(x) %in% c("-", "..", "...", "X", "x", "")] <- NA_character_
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x)
  suppressWarnings(as.numeric(x))
}

cramers_v <- function(tab) {
  teste <- suppressWarnings(chisq.test(tab))
  n <- sum(tab)
  k <- min(nrow(tab) - 1, ncol(tab) - 1)
  sqrt(as.numeric(teste$statistic) / (n * k))
}

# ==============================================================================
# 3. PROJEÇÃO POPULACIONAL — IBGE REVISÃO 2024
# ==============================================================================

proj_2024_ap <- proj_2024_ap |>
  mutate(
    ano       = as.integer(ano),
    idade_num = as.integer(idade_num),
    pop       = as.numeric(pop)
  ) |>
  filter(sigla == "AP", !is.na(pop), pop > 0)

pop_total_proj <- proj_2024_ap |>
  filter(sexo == "Ambos", ano %in% ANOS_ANA) |>
  group_by(ano) |>
  summarise(pop_total = sum(pop, na.rm = TRUE), .groups = "drop")

pop_fem_proj <- proj_2024_ap |>
  filter(
    sexo == "Mulheres",
    idade_num >= 15,
    idade_num <= 49,
    ano %in% ANOS_ANA
  ) |>
  mutate(grupo = GRUPOS[findInterval(idade_num, c(15,20,25,30,35,40,45))]) |>
  filter(!is.na(grupo)) |>
  group_by(ano, grupo) |>
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

cat("\nPopulação total projetada — IBGE Revisão 2024:\n")
print(pop_total_proj)

# ==============================================================================
# 4. CENSO 2022
# ==============================================================================

col_v_c <- achar_col(censo_raw, c("^Valor$", "valor"))
col_s_c <- achar_col(censo_raw, c("^Sexo$", "sexo"))
col_i_c <- achar_col(censo_raw, c("^Grupos de idade$", "grupos de idade",
                                  "^Grupo de idade$", "^Idade$"))

col_f_c <- tryCatch(
  achar_col(censo_raw, c("forma|declar")),
  error = function(e) NA_character_
)

censo_base <- censo_raw

if (!is.na(col_f_c) && col_f_c %in% names(censo_base)) {
  censo_base <- censo_base |>
    filter(grepl("^Total$", .data[[col_f_c]], ignore.case = TRUE))
}

censo_base <- censo_base |>
  rename(sexo = all_of(col_s_c), idade = all_of(col_i_c), pop = all_of(col_v_c)) |>
  mutate(pop = parse_sidra_num(pop)) |>
  filter(!is.na(pop), pop > 0)

pop_total_censo <- censo_base |>
  filter(
    grepl("^Total$", sexo, ignore.case = TRUE),
    grepl("^Total$", idade, ignore.case = TRUE)
  ) |>
  pull(pop) |>
  sum(na.rm = TRUE)

pop_fem_censo <- censo_base |>
  filter(
    grepl("Mulher|Fem", sexo, ignore.case = TRUE),
    !grepl("^Total$", idade, ignore.case = TRUE),
    !grepl(" a ", as.character(idade), fixed = TRUE)
  ) |>
  mutate(
    idade_num = suppressWarnings(as.integer(gsub("[^0-9].*", "", as.character(idade))))
  ) |>
  filter(idade_num >= 15, idade_num <= 49, !is.na(idade_num)) |>
  mutate(grupo = GRUPOS[findInterval(idade_num, c(15,20,25,30,35,40,45))]) |>
  filter(!is.na(grupo)) |>
  group_by(grupo) |>
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 5. SINASC
# ==============================================================================

sinasc_ap <- sinasc |>
  filter(
    !is.na(CODMUNRES),
    substr(as.character(CODMUNRES), 1, 2) == UF_IBGE
  ) |>
  mutate(
    ano_nasc  = as.integer(substr(as.character(DTNASC), 1, 4)),
    idade_mae = suppressWarnings(as.integer(as.character(IDADEMAE))),
    sexo_rn   = as.character(SEXO)
  ) |>
  filter(!is.na(ano_nasc), !is.na(idade_mae), idade_mae >= 10, idade_mae <= 55) |>
  mutate(
    eh_feminino = grepl("^2$|^Fem|^F$", sexo_rn, ignore.case = TRUE),
    grupo = case_when(
      idade_mae >= 15 & idade_mae <= 19 ~ "15-19",
      idade_mae >= 20 & idade_mae <= 24 ~ "20-24",
      idade_mae >= 25 & idade_mae <= 29 ~ "25-29",
      idade_mae >= 30 & idade_mae <= 34 ~ "30-34",
      idade_mae >= 35 & idade_mae <= 39 ~ "35-39",
      idade_mae >= 40 & idade_mae <= 44 ~ "40-44",
      idade_mae >= 45 & idade_mae <= 49 ~ "45-49",
      TRUE ~ NA_character_
    )
  )

nasc_total_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA) |>
  count(ano_nasc, name = "nascimentos")

nasc_grupo_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA, !is.na(grupo)) |>
  count(ano_nasc, grupo, name = "nascimentos")

nasc_fem_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA, !is.na(grupo), eh_feminino) |>
  count(ano_nasc, grupo, name = "nasc_fem")

# ==============================================================================
# 6. TÁBUA FEMININA — AP, IBGE REVISÃO 2024
# ==============================================================================

arq_tabua_lx <- file.path(DIR_BRUTOS, "tabua_mortalidade_rev2024_ap_mulheres.csv")

if (!file.exists(arq_tabua_lx)) {
  stop("Arquivo tabua_mortalidade_rev2024_ap_mulheres.csv não encontrado em dados/brutos.")
}

tabua_lx_fem <- read.csv(arq_tabua_lx, fileEncoding = "UTF-8") |>
  mutate(
    ano = as.integer(ano),
    grupo = as.character(grupo),
    Lx_sobre_l0 = as.numeric(Lx_sobre_l0)
  ) |>
  filter(ano %in% ANOS_ANA, grupo %in% GRUPOS)

# ==============================================================================
# 7. CÁLCULO DOS INDICADORES
# ==============================================================================

calcular_indicadores <- function(ano_ref, pop_fem_q, pop_tot) {
  
  df <- tibble(grupo = GRUPOS) |>
    left_join(pop_fem_q |> select(grupo, pop_fem), by = "grupo") |>
    left_join(nasc_grupo_ano |> filter(ano_nasc == ano_ref), by = "grupo") |>
    left_join(nasc_fem_ano |> filter(ano_nasc == ano_ref), by = "grupo") |>
    left_join(
      tabua_lx_fem |>
        filter(ano == ano_ref) |>
        select(grupo, Lx_sobre_l0),
      by = "grupo"
    ) |>
    replace_na(list(nascimentos = 0L, nasc_fem = 0L)) |>
    mutate(
      nfx = ifelse(is.na(pop_fem) | pop_fem == 0, NA_real_,
                   nascimentos / pop_fem * 1000),
      nfx_fem = ifelse(is.na(pop_fem) | pop_fem == 0, NA_real_,
                       nasc_fem / pop_fem * 1000),
      ano = ano_ref
    )
  
  n_nasc <- nasc_total_ano |>
    filter(ano_nasc == ano_ref) |>
    pull(nascimentos)
  
  if (length(n_nasc) == 0) n_nasc <- 0L
  
  pop_f_15_49 <- sum(df$pop_fem, na.rm = TRUE)
  
  TBN <- n_nasc / pop_tot * 1000
  TFG <- n_nasc / pop_f_15_49 * 1000
  TFT <- 5 * sum(df$nfx / 1000, na.rm = TRUE)
  TBR <- 5 * sum(df$nfx_fem / 1000, na.rm = TRUE)
  TLR <- sum(df$nfx_fem / 1000 * df$Lx_sobre_l0, na.rm = TRUE)
  
  list(
    resumo = tibble(
      ano = ano_ref,
      TBN = round(TBN, 2),
      TFG = round(TFG, 2),
      TFT = round(TFT, 3),
      TBR = round(TBR, 3),
      TLR = round(TLR, 3),
      nascimentos = n_nasc,
      pop_total = round(pop_tot),
      pop_fem_15_49 = round(pop_f_15_49)
    ),
    curva = df
  )
}

resultados <- map(set_names(ANOS_ANA, ANOS_ANA), function(a) {
  pf <- pop_fem_proj |>
    filter(ano == a) |>
    select(grupo, pop_fem)
  
  pt <- pop_total_proj |>
    filter(ano == a) |>
    pull(pop_total)
  
  calcular_indicadores(a, pf, pt)
})

tab_resumo <- map_dfr(resultados, "resumo")

curvas_todas <- map_dfr(resultados, "curva") |>
  mutate(
    grupo = factor(grupo, levels = GRUPOS),
    ano = as.character(ano)
  )

cat("\n--- INDICADORES DE FECUNDIDADE — PROJEÇÃO IBGE REVISÃO 2024 ---\n")
print(tab_resumo)

# ==============================================================================
# 8. COMPARAÇÃO 2022 — PROJEÇÃO REV. 2024 VS CENSO 2022
# ==============================================================================

res_censo_2022 <- calcular_indicadores(2022, pop_fem_censo, pop_total_censo)

tab_comp_2022 <- bind_rows(
  tab_resumo |>
    filter(ano == 2022) |>
    mutate(fonte = "Projeção IBGE (Rev. 2024)"),
  res_censo_2022$resumo |>
    mutate(fonte = "Censo 2022")
) |>
  select(fonte, TBN, TFG, TFT, TBR, TLR, nascimentos, pop_total, pop_fem_15_49)

cat("\n--- COMPARAÇÃO 2022: PROJEÇÃO REV. 2024 VS CENSO ---\n")
print(tab_comp_2022)

# ==============================================================================
# 9. GRÁFICOS — QUESTÃO 2A E 2B
# ==============================================================================

cores_anos <- c(
  "2010" = "#2166ac",
  "2019" = "#4dac26",
  "2021" = "#d6604d",
  "2022" = "#f59322",
  "2024" = "#313695"
)

p_tef <- ggplot(curvas_todas |> filter(!is.na(nfx)),
                aes(x = grupo, y = nfx, color = ano, group = ano)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = cores_anos, name = "Ano") +
  scale_y_continuous(
    name = "nfx (nascimentos por mil mulheres)",
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Taxas Específicas de Fecundidade por grupo etário",
    subtitle = "Amapá — 2010, 2019, 2021, 2022 e 2024",
    x = "Grupo etário da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "right"
  )

ggsave(file.path(DIR_GRF, "tef_ap.png"),
       plot = p_tef, width = 22, height = 13, units = "cm", dpi = 300)

p_tft <- tab_resumo |>
  mutate(ano = factor(ano, levels = ANOS_ANA)) |>
  ggplot(aes(x = ano, y = TFT, fill = as.character(ano))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "gray45") +
  geom_text(aes(label = sprintf("%.2f", TFT)),
            vjust = -0.45, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.55, y = 2.17,
           label = "Nível de reposição (2,1)",
           hjust = 0, size = 2.8, color = "gray45") +
  scale_fill_manual(values = cores_anos) +
  scale_y_continuous(
    name = "TFT (filhos por mulher)",
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Taxa de Fecundidade Total (TFT)",
    subtitle = "Amapá — anos selecionados",
    x = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "tft_ap.png"),
       plot = p_tft, width = 22, height = 13, units = "cm", dpi = 300)

p_tbn <- tab_resumo |>
  mutate(ano = factor(ano, levels = ANOS_ANA)) |>
  ggplot(aes(x = ano, y = TBN, fill = as.character(ano))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", TBN)),
            vjust = -0.45, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = cores_anos) +
  scale_y_continuous(
    name = "TBN (por mil habitantes)",
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Taxa Bruta de Natalidade (TBN)",
    subtitle = "Amapá — anos selecionados",
    x = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "tbn_ap.png"),
       plot = p_tbn, width = 22, height = 13, units = "cm", dpi = 300)

curva_comp_2022 <- bind_rows(
  resultados[["2022"]]$curva |>
    mutate(fonte = "Projeção IBGE (Rev. 2024)"),
  res_censo_2022$curva |>
    mutate(fonte = "Censo 2022")
) |>
  filter(!is.na(nfx)) |>
  mutate(grupo = factor(grupo, levels = GRUPOS))

p_comp <- ggplot(curva_comp_2022,
                 aes(x = grupo, y = nfx, color = fonte, group = fonte)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(
    values = c("Projeção IBGE (Rev. 2024)" = "steelblue4",
               "Censo 2022" = "#8b1a1a"),
    name = "Denominador"
  ) +
  scale_y_continuous(name = "nfx (nascimentos por mil mulheres)") +
  labs(
    title = "TEF 2022 — denominador: projeção vs. Censo",
    subtitle = "Amapá — nascimentos SINASC 2022",
    x = "Grupo etário da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_GRF, "tef_comp_2022_ap.png"),
       plot = p_comp, width = 22, height = 13, units = "cm", dpi = 300)

tab_reprod <- tab_resumo |>
  select(ano, TBR, TLR) |>
  pivot_longer(c(TBR, TLR), names_to = "indicador", values_to = "valor") |>
  mutate(ano = factor(ano, levels = ANOS_ANA))

p_reprod <- ggplot(tab_reprod, aes(x = ano, y = valor, fill = indicador)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray45") +
  geom_text(aes(label = sprintf("%.3f", valor)),
            position = position_dodge(0.6),
            vjust = -0.45, size = 3.2, fontface = "bold") +
  annotate("text", x = 0.55, y = 1.03,
           label = "Nível de reposição (1,0)",
           hjust = 0, size = 2.8, color = "gray45") +
  scale_fill_manual(values = c("TBR" = "steelblue3", "TLR" = "#8b1a1a"),
                    name = "Indicador") +
  scale_y_continuous(
    name = "Filhas por mulher",
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Taxa Bruta e Líquida de Reprodução",
    subtitle = "Amapá — anos selecionados",
    x = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "tbr_tlr_ap.png"),
       plot = p_reprod, width = 22, height = 13, units = "cm", dpi = 300)

# ==============================================================================
# 10. QUESTÃO 2D — ASSOCIAÇÕES NO SINASC 2024
# ==============================================================================

sinasc_2024 <- sinasc |>
  filter(
    !is.na(CODMUNRES),
    substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
    substr(as.character(DTNASC), 1, 4) == "2024"
  ) |>
  mutate(
    idade_mae = suppressWarnings(as.integer(as.character(IDADEMAE))),
    grupo_mae = case_when(
      is.na(idade_mae) ~ NA_character_,
      idade_mae < 20 ~ "< 20",
      idade_mae >= 20 & idade_mae <= 24 ~ "20-24",
      idade_mae >= 25 & idade_mae <= 29 ~ "25-29",
      idade_mae >= 30 & idade_mae <= 34 ~ "30-34",
      idade_mae >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    grupo_mae = factor(grupo_mae, levels = c("< 20","20-24","25-29","30-34","35+")),
    
    escmae_raw = as.character(ESCMAE),
    escolaridade = case_when(
      is.na(ESCMAE) | escmae_raw %in% c("9","Ignorado","Ignorada") ~ "Ignorada",
      escmae_raw %in% c("1","Nenhuma","Nenhum") ~ "Nenhuma",
      escmae_raw %in% c("2","1 a 3 anos","3","4 a 7 anos") ~ "Fund. incompleto",
      escmae_raw %in% c("4","8 a 11 anos") ~ "Médio/Fund. completo",
      escmae_raw %in% c("5","12 e mais","12 anos ou mais") |
        grepl("12", escmae_raw) ~ "Superior",
      grepl("^1$|1 a 3|^2$|4 a 7|^3$", escmae_raw) ~ "Fund. incompleto",
      grepl("^4$|8 a 11", escmae_raw) ~ "Médio/Fund. completo",
      grepl("^5$|12 e mais", escmae_raw) ~ "Superior",
      TRUE ~ "Ignorada"
    ),
    escolaridade = factor(
      escolaridade,
      levels = c("Nenhuma","Fund. incompleto","Médio/Fund. completo",
                 "Superior","Ignorada")
    ),
    
    parto_raw = as.character(PARTO),
    tipo_parto = case_when(
      is.na(PARTO) | parto_raw %in% c("9","Ignorado") ~ NA_character_,
      parto_raw %in% c("1","Vaginal","vaginal") ~ "Vaginal",
      parto_raw %in% c("2","Cesáreo","cesareo","Cesario","Cesária","Cesárea") ~ "Cesárea",
      TRUE ~ NA_character_
    )
  )

df_id_esc <- sinasc_2024 |>
  filter(!is.na(grupo_mae), !is.na(escolaridade)) |>
  count(grupo_mae, escolaridade, name = "n") |>
  group_by(grupo_mae) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

p_heat <- df_id_esc |>
  ggplot(aes(x = escolaridade, y = grupo_mae, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), size = 3.0) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "% (linha)",
    labels = \(x) paste0(x, "%")
  ) +
  labs(
    title = "Idade da mãe × Escolaridade da mãe — SINASC 2024",
    subtitle = "Amapá — distribuição percentual por grupo de idade (% de linha)",
    x = "Escolaridade da mãe",
    y = "Grupo de idade da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(file.path(DIR_GRF, "heat_idade_esc_2024.png"),
       plot = p_heat, width = 22, height = 13, units = "cm", dpi = 300)

df_parto_esc <- sinasc_2024 |>
  filter(!is.na(tipo_parto), !is.na(escolaridade)) |>
  count(tipo_parto, escolaridade, name = "n") |>
  group_by(tipo_parto) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

p_parto <- df_parto_esc |>
  ggplot(aes(x = tipo_parto, y = pct, fill = escolaridade)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = ifelse(pct >= 5, sprintf("%.1f%%", pct), "")),
    position = position_stack(vjust = 0.5),
    size = 3.2,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Blues", name = "Escolaridade") +
  scale_y_continuous(
    name = "Proporção (%)",
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Tipo de parto × Escolaridade da mãe — SINASC 2024",
    subtitle = "Amapá — distribuição percentual por tipo de parto (% de linha)",
    x = "Tipo de parto"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "parto_esc_2024.png"),
       plot = p_parto, width = 22, height = 13, units = "cm", dpi = 300)

# Medidas de associação — V de Cramer
tab_id_esc_abs <- sinasc_2024 |>
  filter(!is.na(grupo_mae), !is.na(escolaridade)) |>
  count(grupo_mae, escolaridade) |>
  pivot_wider(names_from = escolaridade, values_from = n, values_fill = 0) |>
  column_to_rownames("grupo_mae") |>
  as.matrix()

tab_parto_esc_abs <- sinasc_2024 |>
  filter(!is.na(tipo_parto), !is.na(escolaridade)) |>
  count(tipo_parto, escolaridade) |>
  pivot_wider(names_from = escolaridade, values_from = n, values_fill = 0) |>
  column_to_rownames("tipo_parto") |>
  as.matrix()

assoc_2d <- tibble(
  associacao = c("Idade da mãe × Escolaridade", "Tipo de parto × Escolaridade"),
  medida = "V de Cramer",
  valor = c(cramers_v(tab_id_esc_abs), cramers_v(tab_parto_esc_abs))
) |>
  mutate(valor = round(valor, 3))

cat("\n--- MEDIDAS DE ASSOCIAÇÃO — QUESTÃO 2D ---\n")
print(assoc_2d)

# ==============================================================================
# 11. EXPORTAR TABELAS
# ==============================================================================

tab_2a_exp <- tab_resumo |>
  rename(
    `Ano` = ano,
    `TBN (‰)` = TBN,
    `TFG (‰)` = TFG,
    `TFT` = TFT,
    `TBR` = TBR,
    `TLR` = TLR,
    `Nascimentos` = nascimentos,
    `Pop. total` = pop_total,
    `Mulheres 15-49` = pop_fem_15_49
  )

tab_tef_exp <- curvas_todas |>
  select(ano, grupo, pop_fem, nascimentos, nasc_fem, nfx, nfx_fem) |>
  mutate(across(c(nfx, nfx_fem), \(x) round(x, 2))) |>
  rename(
    `Ano` = ano,
    `Grupo etário` = grupo,
    `Pop. feminina` = pop_fem,
    `Nascimentos` = nascimentos,
    `Nasc. femininos` = nasc_fem,
    `nfx (‰)` = nfx,
    `nfx feminina (‰)` = nfx_fem
  )

tab_2b_exp <- tab_comp_2022 |>
  rename(
    `Denominador` = fonte,
    `TBN (‰)` = TBN,
    `TFG (‰)` = TFG,
    `TFT` = TFT,
    `TBR` = TBR,
    `TLR` = TLR,
    `Nascimentos` = nascimentos,
    `Pop. total` = pop_total,
    `Mulheres 15-49` = pop_fem_15_49
  )

tab_2d_id_exp <- df_id_esc |>
  mutate(pct = round(pct, 1)) |>
  rename(
    `Grupo de idade` = grupo_mae,
    `Escolaridade` = escolaridade,
    `n` = n,
    `% (linha)` = pct
  )

tab_2d_parto_exp <- df_parto_esc |>
  mutate(pct = round(pct, 1)) |>
  rename(
    `Tipo de parto` = tipo_parto,
    `Escolaridade` = escolaridade,
    `n` = n,
    `% (linha)` = pct
  )

write_csv(tab_2a_exp, file.path(DIR_TAB, "tab_2a_indicadores_fecundidade.csv"))
write_csv(tab_tef_exp, file.path(DIR_TAB, "tab_2a_tef_completa.csv"))
write_csv(tab_2b_exp, file.path(DIR_TAB, "tab_2b_comp_2022_proj_censo.csv"))
write_csv(tab_2d_id_exp, file.path(DIR_TAB, "tab_2d_idade_escolaridade_2024.csv"))
write_csv(tab_2d_parto_exp, file.path(DIR_TAB, "tab_2d_parto_escolaridade_2024.csv"))
write_csv(assoc_2d, file.path(DIR_TAB, "tab_2d_medidas_associacao.csv"))

write_xlsx(
  list(
    `2a_indicadores` = as.data.frame(tab_2a_exp),
    `2a_TEF_completa` = as.data.frame(tab_tef_exp),
    `2b_comp_2022` = as.data.frame(tab_2b_exp),
    `2d_idade_esc` = as.data.frame(tab_2d_id_exp),
    `2d_parto_esc` = as.data.frame(tab_2d_parto_exp),
    `2d_associacao` = as.data.frame(assoc_2d)
  ),
  path = file.path(DIR_TAB, "questao2_tabelas.xlsx")
)

cat("\nQuestão 2 concluída com Projeção IBGE Revisão 2024.\n")
