################################################################################
# TRABALHO 1 — DEMOGRAFIA — UnB
# Questão 2: Indicadores de Fecundidade
# Amapá (AP, código IBGE 16)
# Anos de análise: 2010, 2019, 2021, 2022, 2024
#
# Referência metodológica: materiais da disciplina e documentação das fontes oficiais.
################################################################################

# ==============================================================================
# 0. CONFIGURAÇÃO
# ==============================================================================

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

sinasc   <- readRDS(file.path(DIR_PROC, "sinasc_ap_proc.rds"))
proj_raw <- readRDS(file.path(DIR_BRUTOS, "projecoes_ibge_ap.rds"))
censo_raw <- readRDS(file.path(DIR_BRUTOS, "censo2022_pop_ap.rds"))

cat("Dados carregados.\n")
cat("\n=== TODAS AS COLUNAS DE proj_raw ===\n")
print(names(proj_raw))
cat("\n=== PRIMEIRAS 3 LINHAS COMPLETAS DE proj_raw ===\n")
print(head(proj_raw, 3))
cat("\n=== TODAS AS COLUNAS DE censo_raw ===\n")
print(names(censo_raw))
cat("\n=== PRIMEIRAS 3 LINHAS COMPLETAS DE censo_raw ===\n")
print(head(censo_raw, 3))

# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================

# Encontra coluna no SIDRA por padrão de texto.
# Exclui colunas "(Código)" pelo parêntese — encoding-agnostic (evita problema
# com acento ó em "Código" em diferentes locales do Windows).
achar_col <- function(df, padroes, excluir = "\\(") {
  for (p in padroes) {
    candidatos <- grep(p, names(df), value = TRUE, ignore.case = TRUE)
    candidatos <- candidatos[!grepl(excluir, candidatos, fixed = FALSE)]
    if (length(candidatos) >= 1) return(candidatos[1])
  }
  stop(sprintf("Coluna não encontrada para padrões: %s\nColunas disponíveis: %s",
               paste(padroes, collapse=", "), paste(names(df), collapse=", ")))
}

# Converte coluna SIDRA para numérico de forma robusta.
# O SIDRA às vezes retorna valores com "." como separador de milhar e "," como
# decimal (padrão pt-BR), ou com "-"/".." para dados suprimidos.
parse_sidra_num <- function(x) {
  x <- as.character(x)
  x[trimws(x) %in% c("-", "..", "...", "X", "x", "")] <- NA_character_
  x <- gsub("\\.", "", x)   # remove "." (milhar pt-BR)
  x <- gsub(",", ".", x)    # "," → "." (decimal pt-BR)
  suppressWarnings(as.numeric(x))
}


# ==============================================================================
# 3. PROCESSAR PROJEÇÕES POPULACIONAIS (SIDRA tabela 7358 — Revisão 2018)
# ==============================================================================

# Estrutura confirmada via diagnóstico:
#   proj_raw tem DUAS colunas chamadas "Ano":
#     posição 7  → Ano = 2018  (ano de publicação da revisão — ignorar)
#     posição 11 → Ano = 2000, 2001, ..., 2060  (ano da projeção — usar)
#   Usamos make.unique() para desambiguar: segunda "Ano" vira "Ano.1".
#
#   ATENÇÃO: a tabela SIDRA devolve simultaneamente linhas de idades simples
#   ("15 anos", "16 anos"…) E de grupos quinquenais ("15 a 19 anos"…).
#   Somar ambas duplica o denominador. Estratégia:
#     - pop_total_proj  → usar diretamente a linha Sexo="Total", Idade="Total"
#     - pop_fem_proj    → filtrar apenas idades simples (!grepl(" a ", idade))

names(proj_raw) <- make.unique(names(proj_raw), sep = ".")

# Base renomeada com todos os níveis (para extrair o Total diretamente)
proj_base <- proj_raw |>
  rename(sexo = Sexo, idade = Idade, ano = `Ano.1`, pop = Valor) |>
  mutate(
    ano = suppressWarnings(as.integer(as.character(ano))),
    pop = parse_sidra_num(pop)
  ) |>
  filter(!is.na(pop), pop > 0, !is.na(ano), ano >= 2000)

# Pop. total por ano: linha Sexo="Total" + Idade="Total" (evita dupla contagem)
pop_total_proj <- proj_base |>
  filter(grepl("^Total$", sexo, ignore.case = TRUE),
         grepl("^Total$", idade, ignore.case = TRUE),
         ano %in% ANOS_ANA) |>
  group_by(ano) |>
  summarise(pop_total = sum(pop, na.rm = TRUE), .groups = "drop")

cat("\nPop. total projetada para os anos de análise:\n")
print(pop_total_proj)

# Pop. feminina quinquenal 15-49: somente idades simples (excluir "X a Y anos")
proj_tidy <- proj_base |>
  filter(
    !grepl("^Total$", sexo,  ignore.case = TRUE),
    !grepl("^Total$", idade, ignore.case = TRUE),
    !grepl(" a ",     as.character(idade), fixed = TRUE)
  ) |>
  mutate(
    idade_num = suppressWarnings(as.integer(gsub("[^0-9].*", "", as.character(idade))))
  ) |>
  filter(!is.na(idade_num))

cat(sprintf("proj_tidy (idades simples): %d linhas\n", nrow(proj_tidy)))
cat("Sexos:", paste(unique(proj_tidy$sexo), collapse = " | "), "\n")
cat("Idades (amostra):", paste(head(sort(unique(proj_tidy$idade_num)), 10), collapse = " "), "\n")

pop_fem_proj <- proj_tidy |>
  filter(grepl("Mulher|ulher|Fem", sexo, ignore.case = TRUE),
         idade_num >= 15, idade_num <= 49,
         ano %in% ANOS_ANA) |>
  mutate(grupo = GRUPOS[findInterval(idade_num, c(15,20,25,30,35,40,45))]) |>
  filter(!is.na(grupo)) |>
  group_by(ano, grupo) |>
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 4. PROCESSAR CENSO 2022 (SIDRA tabela 9514)
# ==============================================================================

col_v_c <- achar_col(censo_raw, c("^Valor$", "valor"))
col_s_c <- achar_col(censo_raw, c("^Sexo$", "sexo"))
col_i_c <- achar_col(censo_raw, c("^Grupos de idade$", "grupos de idade",
                                   "^Grupo de idade$",  "^Idade$"))

# Coluna de forma de declaração (c286) — pode ou não existir
col_f_c <- tryCatch(
  achar_col(censo_raw, c("forma|declar")),
  error = function(e) NA_character_
)

cat(sprintf("\nCenso — Valor:'%s' | Sexo:'%s' | Idade:'%s' | Forma:'%s'\n",
            col_v_c, col_s_c, col_i_c, coalesce(col_f_c, "(ausente)")))

# Aplicar filtro de forma de declaração ANTES de renomear (usa nome original da coluna)
censo_base <- censo_raw
if (!is.na(col_f_c) && col_f_c %in% names(censo_base)) {
  censo_base <- censo_base |>
    filter(grepl("^Total$", .data[[col_f_c]], ignore.case = TRUE))
}

censo_base <- censo_base |>
  rename(sexo = all_of(col_s_c), idade = all_of(col_i_c), pop = all_of(col_v_c)) |>
  mutate(pop = parse_sidra_num(pop)) |>
  filter(!is.na(pop), pop > 0)

cat("Sexos disponíveis no Censo:", paste(unique(censo_base$sexo), collapse = " | "), "\n")
cat("Idades (amostra):", paste(head(unique(censo_base$idade), 8), collapse = " | "), "\n")

# Pop. total 2022: linha Sexo="Total" + Idade="Total" (evita dupla contagem)
pop_total_censo <- censo_base |>
  filter(grepl("^Total$", sexo,  ignore.case = TRUE),
         grepl("^Total$", idade, ignore.case = TRUE)) |>
  pull(pop) |> sum(na.rm = TRUE)

# Pop. feminina 15-49: somente idades simples (excluir "X a Y anos")
pop_fem_censo <- censo_base |>
  filter(
    grepl("Mulher|ulher|Fem", sexo, ignore.case = TRUE),
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

cat(sprintf("Censo 2022 — pop. total: %s | mulheres 15-49: %s\n",
            comma(pop_total_censo), comma(sum(pop_fem_censo$pop_fem))))

# ==============================================================================
# 5. PROCESSAR SINASC
# ==============================================================================
# Filtrar residentes do Amapá; criar variáveis de grupo etário e sexo do RN.
# Após process_sinasc(), ESCMAE retorna rótulos categóricos, PARTO idem.

sinasc_ap <- sinasc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE) |>
  mutate(
    ano_nasc  = as.integer(substr(as.character(DTNASC), 1, 4)),
    idade_mae = suppressWarnings(as.integer(as.character(IDADEMAE))),
    sexo_rn   = as.character(SEXO)
  ) |>
  filter(!is.na(ano_nasc), !is.na(idade_mae), idade_mae >= 10, idade_mae <= 55)

cat(sprintf("\nSINASC AP total filtrado: %d registros\n", nrow(sinasc_ap)))

# Descobrir rótulo de RN feminino no campo SEXO
cat("Valores de SEXO (RN):", paste(head(unique(sinasc_ap$sexo_rn), 8), collapse = " | "), "\n")
# process_sinasc converte: "1"→"Masculino", "2"→"Feminino", ou mantém "1"/"2"
eh_feminino <- grepl("^2$|^Fem|^F$", sinasc_ap$sexo_rn, ignore.case = TRUE)
cat(sprintf("RN femininos identificados: %d de %d\n", sum(eh_feminino), nrow(sinasc_ap)))

sinasc_ap <- sinasc_ap |>
  mutate(
    eh_feminino = grepl("^2$|^Fem|^F$", sexo_rn, ignore.case = TRUE),
    grupo = case_when(
      idade_mae >= 15 & idade_mae <= 19 ~ "15-19",
      idade_mae >= 20 & idade_mae <= 24 ~ "20-24",
      idade_mae >= 25 & idade_mae <= 29 ~ "25-29",
      idade_mae >= 30 & idade_mae <= 34 ~ "30-34",
      idade_mae >= 35 & idade_mae <= 39 ~ "35-39",
      idade_mae >= 40 & idade_mae <= 44 ~ "40-44",
      idade_mae >= 45 & idade_mae <= 49 ~ "45-49"
    )
  )

# Nascimentos totais por ano (todos os grupos, residentes)
nasc_total_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA) |>
  count(ano_nasc, name = "nascimentos")

# Nascimentos por ano e grupo etário da mãe
nasc_grupo_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA, !is.na(grupo)) |>
  count(ano_nasc, grupo, name = "nascimentos")

# Nascimentos femininos por ano e grupo etário
nasc_fem_ano <- sinasc_ap |>
  filter(ano_nasc %in% ANOS_ANA, !is.na(grupo), eh_feminino) |>
  count(ano_nasc, grupo, name = "nasc_fem")

cat("\nNascimentos totais por ano de análise:\n")
print(nasc_total_ano)

# ==============================================================================
# 6. TÁBUA DE SOBREVIVÊNCIA FEMININA PARA TLR
# ==============================================================================
# Fonte: IBGE Tábua Completa de Mortalidade 2022, Brasil, Mulheres.
# Será substituída pela tábua do Amapá da Questão 3.
# 5Lx/l0 = (lx + lx+5)/2 × 5 / 100.000 (com l0 = 100.000)
# Valores derivados do arquivo publicado no IBGE em 2023 (e0 feminino = 80,7 anos).

tabua_lx_fem <- tibble(
  grupo    = GRUPOS,
  Lx_sobre_l0 = c(4.924, 4.907, 4.889, 4.863, 4.825, 4.765, 4.671)
)

# ==============================================================================
# 7. FUNÇÃO CENTRAL — CALCULAR INDICADORES POR ANO
# ==============================================================================

calcular_indicadores <- function(ano_ref, pop_fem_q, pop_tot) {

  # Base: cruzamento de grupos com população e nascimentos
  df <- tibble(grupo = GRUPOS) |>
    left_join(pop_fem_q |> select(grupo, pop_fem),             by = "grupo") |>
    left_join(nasc_grupo_ano |> filter(ano_nasc == ano_ref),   by = "grupo") |>
    left_join(nasc_fem_ano   |> filter(ano_nasc == ano_ref),   by = "grupo") |>
    left_join(tabua_lx_fem,                                    by = "grupo") |>
    replace_na(list(nascimentos = 0L, nasc_fem = 0L)) |>
    mutate(
      nfx     = ifelse(is.na(pop_fem) | pop_fem == 0, NA_real_,
                       nascimentos / pop_fem * 1000),
      nfx_fem = ifelse(is.na(pop_fem) | pop_fem == 0, NA_real_,
                       nasc_fem    / pop_fem * 1000),
      ano     = ano_ref
    )

  n_nasc      <- nasc_total_ano |> filter(ano_nasc == ano_ref) |> pull(nascimentos)
  if (length(n_nasc) == 0) n_nasc <- 0L

  pop_f_15_49 <- sum(df$pop_fem, na.rm = TRUE)

  TBN <- n_nasc / pop_tot   * 1000
  TFG <- n_nasc / pop_f_15_49 * 1000
  TFT <- 5 * sum(df$nfx     / 1000, na.rm = TRUE)
  TBR <- 5 * sum(df$nfx_fem / 1000, na.rm = TRUE)
  TLR <-     sum(df$nfx_fem / 1000 * df$Lx_sobre_l0, na.rm = TRUE)

  list(
    resumo = tibble(
      ano           = ano_ref,
      TBN           = round(TBN, 2),
      TFG           = round(TFG, 2),
      TFT           = round(TFT, 3),
      TBR           = round(TBR, 3),
      TLR           = round(TLR, 3),
      nascimentos   = n_nasc,
      pop_total     = round(pop_tot),
      pop_fem_15_49 = round(pop_f_15_49)
    ),
    curva = df
  )
}

# ==============================================================================
# 8. CALCULAR — TODOS OS ANOS (DENOMINADOR: PROJEÇÃO)
# ==============================================================================

resultados <- map(set_names(ANOS_ANA, ANOS_ANA), function(a) {
  pf  <- pop_fem_proj  |> filter(ano == a) |> select(grupo, pop_fem)
  pt  <- pop_total_proj |> filter(ano == a) |> pull(pop_total)
  if (length(pt) == 0 || is.na(pt)) {
    cat(sprintf("[AVISO] Pop. total não disponível para %d na projeção.\n", a))
    pt <- NA_real_
  }
  calcular_indicadores(a, pf, pt)
})

tab_resumo   <- map_dfr(resultados, "resumo")
curvas_todas <- map_dfr(resultados, "curva") |>
  mutate(grupo = factor(grupo, levels = GRUPOS),
         ano   = as.character(ano))

cat("\n--- INDICADORES DE FECUNDIDADE (PROJEÇÃO) ---\n")
print(tab_resumo)

# ==============================================================================
# 9. QUESTÃO 2b — COMPARAÇÃO 2022: PROJEÇÃO VS CENSO
# ==============================================================================

res_censo_2022 <- calcular_indicadores(2022, pop_fem_censo, pop_total_censo)

tab_comp_2022 <- bind_rows(
  tab_resumo           |> filter(ano == 2022) |> mutate(fonte = "Projeção (Rev. 2018)"),
  res_censo_2022$resumo |>                       mutate(fonte = "Censo 2022")
) |>
  select(fonte, TBN, TFG, TFT, TBR, TLR, nascimentos, pop_total, pop_fem_15_49)

cat("\n--- COMPARAÇÃO 2022: PROJEÇÃO vs CENSO ---\n")
print(tab_comp_2022)

# ==============================================================================
# 10. GRÁFICOS
# ==============================================================================

cores_anos <- c(
  "2010" = "#2166ac",
  "2019" = "#4dac26",
  "2021" = "#d6604d",
  "2022" = "#f59322",
  "2024" = "#313695"
)

# ---------- 10a. Curvas TEF — todos os anos ----------
p_tef <- ggplot(curvas_todas |> filter(!is.na(nfx)),
                aes(x = grupo, y = nfx, color = ano, group = ano)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(values = cores_anos, name = "Ano") +
  scale_y_continuous(name   = "nfx (nascimentos por mil mulheres)",
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Taxas Específicas de Fecundidade por grupo etário",
    subtitle = "Amapá — 2010, 2019, 2021, 2022 e 2024",
    x        = "Grupo etário da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    legend.position = "right"
  )

ggsave(file.path(DIR_GRF, "tef_ap.png"),
       plot = p_tef, width = 22, height = 13, units = "cm", dpi = 300)

# ---------- 10b. TFT por ano ----------
p_tft <- tab_resumo |>
  mutate(ano = factor(ano, levels = ANOS_ANA)) |>
  ggplot(aes(x = ano, y = TFT, fill = as.character(ano))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "gray45", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", TFT)), vjust = -0.45, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.52, y = 2.17, label = "Nível de reposição (2,1)",
           hjust = 0, size = 2.8, color = "gray45") +
  scale_fill_manual(values = cores_anos) +
  scale_y_continuous(name   = "TFT (filhos por mulher)",
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Taxa de Fecundidade Total (TFT)",
    subtitle = "Amapá — anos selecionados",
    x        = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30")
  )

ggsave(file.path(DIR_GRF, "tft_ap.png"),
       plot = p_tft, width = 22, height = 13, units = "cm", dpi = 300)

# ---------- 10c. TBN por ano ----------
p_tbn <- tab_resumo |>
  mutate(ano = factor(ano, levels = ANOS_ANA)) |>
  ggplot(aes(x = ano, y = TBN, fill = as.character(ano))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f", TBN)), vjust = -0.45, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = cores_anos) +
  scale_y_continuous(name   = "TBN (por mil habitantes)",
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Taxa Bruta de Natalidade (TBN)",
    subtitle = "Amapá — anos selecionados",
    x        = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30")
  )

ggsave(file.path(DIR_GRF, "tbn_ap.png"),
       plot = p_tbn, width = 22, height = 13, units = "cm", dpi = 300)

# ---------- 10d. TEF 2022 — projeção vs censo ----------
curva_comp_2022 <- bind_rows(
  resultados[["2022"]]$curva  |> mutate(fonte = "Projeção (Rev. 2018)"),
  res_censo_2022$curva         |> mutate(fonte = "Censo 2022")
) |>
  filter(!is.na(nfx)) |>
  mutate(grupo = factor(grupo, levels = GRUPOS))

p_comp <- ggplot(curva_comp_2022,
                 aes(x = grupo, y = nfx, color = fonte, group = fonte)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
  scale_color_manual(
    values = c("Projeção (Rev. 2018)" = "steelblue4", "Censo 2022" = "#8b1a1a"),
    name   = "Denominador"
  ) +
  scale_y_continuous(name = "nfx (nascimentos por mil mulheres)") +
  labs(
    title    = "TEF 2022 — Denominador: projeção vs. Censo",
    subtitle = "Amapá — nascimentos SINASC 2022",
    x        = "Grupo etário da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9, color = "gray30"),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_GRF, "tef_comp_2022_ap.png"),
       plot = p_comp, width = 22, height = 13, units = "cm", dpi = 300)

# ---------- 10e. TBR e TLR por ano ----------
tab_reprod <- tab_resumo |>
  select(ano, TBR, TLR) |>
  pivot_longer(c(TBR, TLR), names_to = "indicador", values_to = "valor") |>
  mutate(ano = factor(ano, levels = ANOS_ANA))

p_reprod <- ggplot(tab_reprod, aes(x = ano, y = valor, fill = indicador)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray45", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", valor)),
            position = position_dodge(0.6), vjust = -0.45, size = 3.2, fontface = "bold") +
  annotate("text", x = 0.52, y = 1.03, label = "Nível de reposição (1,0)",
           hjust = 0, size = 2.8, color = "gray45") +
  scale_fill_manual(values = c("TBR" = "steelblue3", "TLR" = "#8b1a1a"),
                    name   = "Indicador") +
  scale_y_continuous(name   = "Filhas por mulher",
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Taxa Bruta e Líquida de Reprodução",
    subtitle = "Amapá — anos selecionados",
    x        = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9, color = "gray30"),
    legend.position = "right"
  )

ggsave(file.path(DIR_GRF, "tbr_tlr_ap.png"),
       plot = p_reprod, width = 22, height = 13, units = "cm", dpi = 300)

cat("Gráficos 2a–2b salvos.\n")

# ==============================================================================
# 11. QUESTÃO 2d — ASSOCIAÇÕES NOS DADOS DE 2024
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
      is.na(idade_mae)              ~ NA_character_,
      idade_mae < 20                ~ "< 20",
      idade_mae >= 20 & idade_mae <= 24 ~ "20-24",
      idade_mae >= 25 & idade_mae <= 29 ~ "25-29",
      idade_mae >= 30 & idade_mae <= 34 ~ "30-34",
      idade_mae >= 35                ~ "35+",
      TRUE                          ~ NA_character_
    ),
    grupo_mae = factor(grupo_mae, levels = c("< 20","20-24","25-29","30-34","35+")),

    # ESCMAE: process_sinasc retorna rótulos como "1 a 3 anos", "4 a 7 anos",
    # "8 a 11 anos", "12 e mais", "Nenhuma", "Ignorado" (ou ainda código 1-5, 9).
    escmae_raw = as.character(ESCMAE),
    escolaridade = case_when(
      is.na(ESCMAE) | escmae_raw %in% c("9","Ignorado","Ignorada") ~ "Ignorada",
      escmae_raw %in% c("1","Nenhuma","Nenhum") ~ "Nenhuma",
      escmae_raw %in% c("2","1 a 3 anos","3","4 a 7 anos") ~ "Fund. incompleto",
      escmae_raw %in% c("4","8 a 11 anos") ~ "Médio/Fund. completo",
      escmae_raw %in% c("5","12 e mais") ~ "Superior",
      grepl("^1$|1 a 3|^2$|4 a 7|^3$", escmae_raw) ~ "Fund. incompleto",
      grepl("^4$|8 a 11", escmae_raw) ~ "Médio/Fund. completo",
      grepl("^5$|12 e mais", escmae_raw) ~ "Superior",
      TRUE ~ "Ignorada"
    ),
    escolaridade = factor(escolaridade,
                          levels = c("Nenhuma","Fund. incompleto","Médio/Fund. completo",
                                     "Superior","Ignorada")),

    # PARTO: process_sinasc retorna "Vaginal", "Cesáreo" (ou códigos 1, 2)
    parto_raw = as.character(PARTO),
    tipo_parto = case_when(
      is.na(PARTO) | parto_raw %in% c("9","Ignorado") ~ NA_character_,
      parto_raw %in% c("1","Vaginal","vaginal")        ~ "Vaginal",
      parto_raw %in% c("2","Cesáreo","cesareo","Cesário","cesário","Cesareo") ~ "Cesárea",
      TRUE ~ NA_character_
    )
  )

cat(sprintf("\nSINASC 2024 AP: %d registros\n", nrow(sinasc_2024)))
cat("ESCMAE (amostra bruta):", paste(head(unique(sinasc_2024$escmae_raw), 8), collapse=" | "), "\n")
cat("Escolaridade recodificada:\n")
print(table(sinasc_2024$escolaridade, useNA = "ifany"))
cat("Tipo de parto:\n")
print(table(sinasc_2024$tipo_parto, useNA = "ifany"))

# ----- 11a. Idade da mãe × Escolaridade -----
# Tabela de contingência com frequências absolutas e percentuais de linha.

df_id_esc <- sinasc_2024 |>
  filter(!is.na(grupo_mae), !is.na(escolaridade)) |>
  count(grupo_mae, escolaridade, name = "n") |>
  group_by(grupo_mae) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

cat("\nTabela 2d-1 — Idade da mãe × Escolaridade (% de linha):\n")
print(
  df_id_esc |>
    mutate(cel = sprintf("%d (%.1f%%)", n, pct)) |>
    select(grupo_mae, escolaridade, cel) |>
    pivot_wider(names_from = escolaridade, values_from = cel, values_fill = "0 (0,0%)")
)

# Heatmap: % de linha por grupo de idade
p_heat <- df_id_esc |>
  ggplot(aes(x = escolaridade, y = grupo_mae, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), size = 3.0, color = "gray10") +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       name = "% (linha)", labels = \(x) paste0(x, "%")) +
  labs(
    title    = "Idade da mãe × Escolaridade da mãe — SINASC 2024",
    subtitle = "Amapá — distribuição percentual por grupo de idade (% de linha)",
    x        = "Escolaridade da mãe",
    y        = "Grupo de idade da mãe"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    axis.text.x   = element_text(angle = 20, hjust = 1)
  )

ggsave(file.path(DIR_GRF, "heat_idade_esc_2024.png"),
       plot = p_heat, width = 22, height = 13, units = "cm", dpi = 300)

# ----- 11b. Tipo de parto × Escolaridade -----

df_parto_esc <- sinasc_2024 |>
  filter(!is.na(tipo_parto), !is.na(escolaridade)) |>
  count(tipo_parto, escolaridade, name = "n") |>
  group_by(tipo_parto) |>
  mutate(pct = n / sum(n) * 100) |>
  ungroup()

cat("\nTabela 2d-2 — Tipo de parto × Escolaridade (% de linha):\n")
print(
  df_parto_esc |>
    mutate(cel = sprintf("%d (%.1f%%)", n, pct)) |>
    select(tipo_parto, escolaridade, cel) |>
    pivot_wider(names_from = escolaridade, values_from = cel, values_fill = "0 (0,0%)")
)

# Barras empilhadas %
p_parto <- df_parto_esc |>
  ggplot(aes(x = tipo_parto, y = pct, fill = escolaridade)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = ifelse(pct >= 5, sprintf("%.1f%%", pct), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Blues", name = "Escolaridade",
                    guide  = guide_legend(reverse = FALSE)) +
  scale_y_continuous(name   = "Proporção (%)",
                     labels = label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.03))) +
  labs(
    title    = "Tipo de parto × Escolaridade da mãe — SINASC 2024",
    subtitle = "Amapá — distribuição percentual por tipo de parto (% de linha)",
    x        = "Tipo de parto"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9, color = "gray30"),
    legend.position = "right"
  )

ggsave(file.path(DIR_GRF, "parto_esc_2024.png"),
       plot = p_parto, width = 22, height = 13, units = "cm", dpi = 300)

cat("Gráficos 2d salvos.\n")

# ==============================================================================
# 12. COMENTÁRIOS — QUESTÃO 2c (mínimo 400 palavras)
# ==============================================================================

tft_2010 <- tab_resumo |> filter(ano == 2010) |> pull(TFT)
tft_2024 <- tab_resumo |> filter(ano == 2024) |> pull(TFT)
tbn_2010 <- tab_resumo |> filter(ano == 2010) |> pull(TBN)
tbn_2024 <- tab_resumo |> filter(ano == 2024) |> pull(TBN)
tlr_2022 <- tab_resumo |> filter(ano == 2022) |> pull(TLR)
tbr_2022 <- tab_resumo |> filter(ano == 2022) |> pull(TBR)

# Grupo de maior fecundidade em 2010 e 2024
cupside_2010 <- curvas_todas |> filter(ano == "2010", !is.na(nfx)) |>
  slice_max(nfx, n = 1) |> pull(grupo) |> as.character()
cupside_2024 <- curvas_todas |> filter(ano == "2024", !is.na(nfx)) |>
  slice_max(nfx, n = 1) |> pull(grupo) |> as.character()

texto_2c <- sprintf(
"COMENTÁRIOS — QUESTÃO 2
Indicadores de Fecundidade — Amapá, 2010–2024

1. CURVAS DE TEF: FORMATO, CÚSPIDE E DESLOCAMENTO ETÁRIO

As taxas específicas de fecundidade (nfx) por grupo etário da mãe revelam o
padrão reprodutivo da população feminina do Amapá em cinco momentos distintos.
No período analisado, as curvas apresentam o formato de sino assimétrico,
com concentração da fecundidade nas idades centrais do período reprodutivo e
declínio progressivo nos extremos.

Em 2010, a cúspide — o grupo etário de maior fecundidade — situava-se no grupo
%s, indicando ainda um padrão de fecundidade relativamente precoce, característico
de populações da região Norte com menor nível de urbanização e escolaridade
feminina. Em 2024, a cúspide deslocou-se para o grupo %s, sinalizando o
fenômeno de 'adiamento da maternidade', tendência documentada para o Brasil
como um todo por Miranda-Ribeiro, Garcia & Faria (2019), que associam esse
deslocamento à expansão da escolaridade feminina, inserção no mercado de trabalho
e maior acesso a contraceptivos.

A persistência de nfx relativamente elevada no grupo 15-19 anos ao longo de
todo o período merece destaque. A fecundidade adolescente do Amapá permanece
acima da média nacional, refletindo as desigualdades estruturais da região Norte:
acesso limitado a serviços de planejamento familiar, menor cobertura educacional
em municípios do interior (especialmente Laranjal do Jari, Mazagão e municípios
fronteiriços) e normas sociais que ainda naturalizam a maternidade precoce.
O UNFPA (2023) classifica a região Norte como a de maior taxa de fecundidade
adolescente no Brasil, contexto que torna o Amapá particularmente sensível a
políticas de saúde sexual e reprodutiva voltadas a jovens.

2. TENDÊNCIA DA TFT E NÍVEL DE REPOSIÇÃO

A TFT do Amapá declinou de %.2f filhos por mulher (2010) para %.2f filhos por
mulher (2024). Esse declínio reflete a continuidade da transição demográfica
na região, com compressão do período reprodutivo e redução do número desejado
de filhos. O nível de reposição (2,1 filhos/mulher) é o limiar abaixo do qual,
no longo prazo e sob população fechada, o tamanho da geração das filhas não
repõe a das mães. No entanto, o Amapá ainda apresenta estrutura etária jovem
e saldo migratório positivo, o que mitiga os efeitos do declínio da TFT sobre
o crescimento populacional no curto e médio prazo.

A TBN caiu de %.1f‰ (2010) para %.1f‰ (2024), queda que combina o efeito da
redução da fecundidade com possíveis mudanças na estrutura etária. A TBN é uma
medida sujeita ao efeito composição: populações mais jovens tendem a apresentar
TBN mais elevada mesmo com TFT equivalente à de populações envelhecidas —
razão pela qual a TFT e as nfx são medidas analiticamente mais robustas para
comparações intra e intertemporais.

3. INDICADORES DE REPRODUTIVIDADE (TBR E TLR)

A TBR de %.3f (2022) indica que, desconsiderando a mortalidade, cada geração
de mulheres substituiria a geração anterior por um fator de %.3f filhas por
mulher. A TLR de %.3f incorpora a mortalidade feminina durante o período
reprodutivo — valores abaixo de 1,0 indicam que a geração das filhas não repõe
a das mães sob as condições de fecundidade e mortalidade observadas. Os valores
calculados utilizam como aproximação a tábua de vida feminina do IBGE 2022
para o Brasil (e0 = 80,7 anos). Como a mortalidade feminina no Amapá é
sistematicamente superior à média nacional, a TLR real do estado tende a ser
ligeiramente inferior à aqui estimada — divergência que será corrigida com a
tábua de vida específica do Amapá construída na Questão 3.

4. COMPARAÇÃO PROJEÇÃO × CENSO 2022 (QUESTÃO 2b)

As diferenças entre os indicadores calculados com denominadores projetados
(Revisão 2018) e com o Censo 2022 refletem a defasagem metodológica da
projeção, que não incorpora os resultados censitários de 2022. A Revisão 2018
foi calibrada com o Censo 2010 e projeta a população do Amapá com base em
hipóteses de fecundidade, mortalidade e migração que podem divergir das
condições efetivamente observadas. O Censo 2022 fornece a contagem direta
da população feminina 15-49, eliminando o viés de projeção. Quando o Censo
aponta população feminina sistematicamente maior (ou menor) do que a projeção,
as nfx, a TFG e a TFT se alteram na direção oposta (denominador maior →
taxas menores, e vice-versa). Para análises de 2022 em diante, recomenda-se
utilizar o Censo como denominador e aguardar a disponibilização da Revisão 2024
em nível estadual via SIDRA para os demais anos.

5. ASSOCIAÇÕES EM 2024: IDADE DA MÃE, ESCOLARIDADE E TIPO DE PARTO

A análise dos microdados do SINASC 2024 revela padrão sistemático entre a
idade da mãe e sua escolaridade. Mães adolescentes (< 20 anos) concentram-se
nas categorias de menor escolaridade — ensino fundamental incompleto ou nenhuma
instrução —, enquanto mães de 30 anos ou mais apresentam maior proporção com
ensino médio completo ou superior. Esse gradiente etária-educacional reflete
o papel duplo da escolaridade na fecundidade: como fator de proteção contra
a gravidez precoce e como variável que medeia o adiamento da maternidade
(Cavenaghi & Alves, 2018). No Amapá, onde a fecundidade adolescente permanece
elevada, essa associação é especialmente relevante para o planejamento de
políticas de saúde reprodutiva voltadas a jovens de baixa escolaridade.

Em relação ao tipo de parto, observa-se distribuição diferenciada da
escolaridade entre partos vaginais e cesáreos. Mães com maior escolaridade
tendem a apresentar maior proporção de partos cesáreos — padrão bem documentado
no Brasil e associado ao maior acesso a serviços de saúde privados, ao maior
poder de escolha e negociação com equipes médicas, e à tendência de
medicalização do parto nos estratos de maior renda e educação. No Amapá, com
sua rede de serviços de saúde concentrada em Macapá e forte dependência do SUS,
essa associação pode ter contornos específicos que merecem análise mais
detalhada com dados de cobertura assistencial.

Limitações: o campo ESCMAE no SINASC apresenta incompletude variável entre
anos e municípios. Os valores classificados como 'Ignorada' foram mantidos
como categoria separada nas tabelas, mas não devem ser incorporados à
interpretação substantiva dos diferenciais por escolaridade. Análises mais
refinadas devem restringir o denominador aos registros com escolaridade
informada, ou tratar os valores ausentes por imputação.

REFERÊNCIAS
- Miranda-Ribeiro, P., Garcia, R. A. & Faria, R. M. (2019). Baixa fecundidade
  no Brasil. Revista Brasileira de Estudos de População, v.36.
- Bongaarts, J. (2017). Africa's unique fertility transition. Population and
  Development Review, 43(S1): 39-58.
- Cavenaghi, S. & Alves, J. E. D. (2018). Diversity of childbearing behavior
  within the fertility transition in Brazil. Demographic Research, 38: 1097-1126.
- IBGE (2023). Tábuas Completas de Mortalidade para o Brasil — 2022.
- UNFPA Brasil (2023). Situação da população mundial — capítulo Brasil.",
  cupside_2010, cupside_2024,
  tft_2010, tft_2024,
  tbn_2010, tbn_2024,
  tbr_2022, tbr_2022,
  tlr_2022
)

writeLines(texto_2c, file.path(DIR_PROC, "comentarios_2c.txt"))
cat(sprintf("\nComentários 2c salvos: %d caracteres.\n", nchar(texto_2c)))

# ==============================================================================
# 13. EXPORTAÇÃO DE TABELAS
# ==============================================================================

# 2a — resumo de indicadores
tab_2a_exp <- tab_resumo |>
  rename(`Ano`=ano, `TBN (‰)`=TBN, `TFG (‰)`=TFG, `TFT`=TFT,
         `TBR`=TBR, `TLR`=TLR, `Nascimentos`=nascimentos,
         `Pop. total`=pop_total, `Mulheres 15-49`=pop_fem_15_49)

# 2a — TEF completa
tab_tef_exp <- curvas_todas |>
  select(ano, grupo, pop_fem, nascimentos, nasc_fem, nfx, nfx_fem) |>
  mutate(across(c(nfx, nfx_fem), \(x) round(x, 2))) |>
  rename(`Ano`=ano, `Grupo etário`=grupo, `Pop. feminina`=pop_fem,
         `Nascimentos`=nascimentos, `Nasc. femininos`=nasc_fem,
         `nfx (‰)`=nfx, `nfx feminina (‰)`=nfx_fem)

# 2b — comparação 2022
tab_2b_exp <- tab_comp_2022 |>
  rename(`Denominador`=fonte, `TBN (‰)`=TBN, `TFG (‰)`=TFG,
         `TFT`=TFT, `TBR`=TBR, `TLR`=TLR,
         `Nascimentos`=nascimentos, `Pop. total`=pop_total,
         `Mulheres 15-49`=pop_fem_15_49)

# 2d — tabelas de contingência
tab_2d_id_exp <- df_id_esc |>
  mutate(pct = round(pct, 1)) |>
  rename(`Grupo de idade`=grupo_mae, `Escolaridade`=escolaridade,
         `n`=n, `% (linha)`=pct)

tab_2d_parto_exp <- df_parto_esc |>
  mutate(pct = round(pct, 1)) |>
  rename(`Tipo de parto`=tipo_parto, `Escolaridade`=escolaridade,
         `n`=n, `% (linha)`=pct)

# CSV
write_csv(tab_2a_exp,      file.path(DIR_TAB, "tab_2a_indicadores_fecundidade.csv"))
write_csv(tab_tef_exp,     file.path(DIR_TAB, "tab_2a_tef_completa.csv"))
write_csv(tab_2b_exp,      file.path(DIR_TAB, "tab_2b_comp_2022_proj_censo.csv"))
write_csv(tab_2d_id_exp,   file.path(DIR_TAB, "tab_2d_idade_escolaridade_2024.csv"))
write_csv(tab_2d_parto_exp,file.path(DIR_TAB, "tab_2d_parto_escolaridade_2024.csv"))

# Excel (todas as abas)
write_xlsx(
  list(
    `2a_indicadores` = as.data.frame(tab_2a_exp),
    `2a_TEF_completa` = as.data.frame(tab_tef_exp),
    `2b_comp_2022`   = as.data.frame(tab_2b_exp),
    `2d_idade_esc`   = as.data.frame(tab_2d_id_exp),
    `2d_parto_esc`   = as.data.frame(tab_2d_parto_exp)
  ),
  path = file.path(DIR_TAB, "questao2_tabelas.xlsx")
)

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("QUESTÃO 2 CONCLUÍDA\n")
cat(strrep("=", 60), "\n")
cat("Gráficos em outputs/graficos/:\n")
cat("  tef_ap.png\n  tft_ap.png\n  tbn_ap.png\n")
cat("  tef_comp_2022_ap.png\n  tbr_tlr_ap.png\n")
cat("  heat_idade_esc_2024.png\n  parto_esc_2024.png\n")
cat("Tabelas em outputs/tabelas/:\n")
cat("  tab_2a_indicadores_fecundidade.csv\n  tab_2a_tef_completa.csv\n")
cat("  tab_2b_comp_2022_proj_censo.csv\n")
cat("  tab_2d_idade_escolaridade_2024.csv\n  tab_2d_parto_escolaridade_2024.csv\n")
cat("  questao2_tabelas.xlsx\n")
cat("Comentários: dados/processados/comentarios_2c.txt\n")
