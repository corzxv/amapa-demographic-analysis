################################################################################
# QUESTÃO 3 — Mortalidade geral, específica, infantil e perinatal
################################################################################

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
ANOS_TMI <- c(2022, 2023, 2024)

GRUPOS_MX <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80+")

decode_idade_anos <- function(x) {
  x <- as.character(x)
  x[is.na(x) | nchar(trimws(x)) < 3] <- NA_character_
  u <- substr(x, 1, 1)
  v <- suppressWarnings(as.integer(substr(x, 2, 3)))
  case_when(
    u %in% c("0","1","2","3") ~ 0,
    u == "4" ~ as.double(v),
    u == "5" ~ 100 + as.double(v),
    TRUE ~ NA_real_
  )
}

decode_idade_dias <- function(x) {
  x <- as.character(x)
  x[is.na(x) | nchar(trimws(x)) < 3] <- NA_character_
  u <- substr(x, 1, 1)
  v <- suppressWarnings(as.double(substr(x, 2, 3)))
  case_when(
    u == "0" ~ v / (60 * 24),
    u == "1" ~ v / 24,
    u == "2" ~ v,
    u == "3" ~ v * 30.44,
    TRUE ~ NA_real_
  )
}

classificar_grupo_mx <- function(idade_anos) {
  case_when(
    idade_anos == 0 ~ "<1",
    idade_anos >= 1  & idade_anos <= 4  ~ "1-4",
    idade_anos >= 5  & idade_anos <= 9  ~ "5-9",
    idade_anos >= 10 & idade_anos <= 14 ~ "10-14",
    idade_anos >= 15 & idade_anos <= 19 ~ "15-19",
    idade_anos >= 20 & idade_anos <= 24 ~ "20-24",
    idade_anos >= 25 & idade_anos <= 29 ~ "25-29",
    idade_anos >= 30 & idade_anos <= 34 ~ "30-34",
    idade_anos >= 35 & idade_anos <= 39 ~ "35-39",
    idade_anos >= 40 & idade_anos <= 44 ~ "40-44",
    idade_anos >= 45 & idade_anos <= 49 ~ "45-49",
    idade_anos >= 50 & idade_anos <= 54 ~ "50-54",
    idade_anos >= 55 & idade_anos <= 59 ~ "55-59",
    idade_anos >= 60 & idade_anos <= 64 ~ "60-64",
    idade_anos >= 65 & idade_anos <= 69 ~ "65-69",
    idade_anos >= 70 & idade_anos <= 74 ~ "70-74",
    idade_anos >= 75 & idade_anos <= 79 ~ "75-79",
    idade_anos >= 80 ~ "80+",
    TRUE ~ NA_character_
  )
}

extrair_ano_obito <- function(x) {
  x  <- as.character(trimws(x))
  a1 <- suppressWarnings(as.integer(substr(x, 1, 4)))
  a2 <- suppressWarnings(as.integer(substr(x, 5, 8)))
  if_else(!is.na(a1) & a1 >= 1990 & a1 <= 2030, a1,
          if_else(!is.na(a2) & a2 >= 1990 & a2 <= 2030, a2, NA_integer_))
}

# Bases
sim_bruto     <- readRDS(file.path(DIR_BRUTOS, "sim_do_ap_2000_2024.rds"))
sim_fet_bruto <- readRDS(file.path(DIR_BRUTOS, "sim_dofet_ap_2000_2024.rds"))
sinasc_proc   <- readRDS(file.path(DIR_PROC, "sinasc_ap_proc.rds"))
proj_2024_ap  <- readRDS(file.path(DIR_BRUTOS, "projecoes_ibge_rev2024_ap.rds"))

# População — Revisão 2024
proj_2024_ap <- proj_2024_ap |>
  mutate(
    ano = as.integer(ano),
    idade_num = as.integer(idade_num),
    pop = as.numeric(pop)
  ) |>
  filter(sigla == "AP", !is.na(pop), pop > 0)

pop_total_proj <- proj_2024_ap |>
  filter(sexo == "Ambos", ano %in% ANOS_ANA) |>
  group_by(ano) |>
  summarise(pop_total = sum(pop), .groups = "drop")

pop_mx_proj <- proj_2024_ap |>
  filter(sexo %in% c("Homens","Mulheres"), ano %in% ANOS_ANA) |>
  mutate(
    sexo_padrao = if_else(sexo == "Homens", "Masculino", "Feminino"),
    grupo_mx = classificar_grupo_mx(idade_num)
  ) |>
  filter(!is.na(grupo_mx)) |>
  group_by(ano, sexo_padrao, grupo_mx) |>
  summarise(pop = sum(pop), .groups = "drop")

# SIM — óbitos gerais
sim_ap <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr = as.character(IDADE),
    DTOBITO_chr = as.character(DTOBITO),
    SEXO_chr = as.character(SEXO)
  ) |>
  filter(!is.na(CODMUNRES_chr),
         substr(CODMUNRES_chr, 1, 2) == UF_IBGE) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    sexo = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    idade_anos = decode_idade_anos(IDADE_chr),
    grupo_mx = classificar_grupo_mx(idade_anos)
  ) |>
  filter(!is.na(ano_obito))

# TBM
tab_tbm <- sim_ap |>
  filter(ano_obito %in% ANOS_ANA) |>
  count(ano = ano_obito, name = "n_obitos") |>
  left_join(pop_total_proj, by = "ano") |>
  mutate(TBM = round(n_obitos / pop_total * 1000, 2))

# nMx
obitos_mx <- sim_ap |>
  filter(ano_obito %in% ANOS_ANA,
         sexo %in% c("Masculino","Feminino"),
         !is.na(grupo_mx)) |>
  count(ano = ano_obito, sexo, grupo_mx, name = "n_obitos")

grade_completa <- expand_grid(
  ano = ANOS_ANA,
  sexo = c("Masculino","Feminino"),
  grupo_mx = GRUPOS_MX
)

nMx_long <- grade_completa |>
  left_join(obitos_mx, by = c("ano","sexo","grupo_mx")) |>
  replace_na(list(n_obitos = 0L)) |>
  left_join(pop_mx_proj,
            by = c("ano", "sexo" = "sexo_padrao", "grupo_mx")) |>
  mutate(
    nMx = if_else(is.na(pop) | pop == 0, NA_real_, n_obitos / pop * 1000),
    grupo_mx = factor(grupo_mx, levels = GRUPOS_MX)
  )

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

# Gráficos nMx
cores_sexo <- c("Masculino" = "steelblue4", "Feminino" = "#8b1a1a")

p_nmx_list <- map(ANOS_ANA, function(a) {
  df <- nMx_long |>
    filter(ano == a, !is.na(nMx), nMx > 0)
  
  ggplot(df, aes(x = grupo_mx, y = nMx, color = sexo, group = sexo)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.8, shape = 21, fill = "white", stroke = 1.5) +
    scale_y_log10(name = "nMx (por mil hab., escala log)") +
    scale_color_manual(values = cores_sexo, name = "Sexo") +
    labs(
      title = sprintf("Taxas Específicas de Mortalidade — %d", a),
      subtitle = "Amapá — homens e mulheres",
      x = "Grupo etário"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "bottom"
    )
})
names(p_nmx_list) <- as.character(ANOS_ANA)

walk2(p_nmx_list, as.character(ANOS_ANA), function(p, a) {
  ggsave(file.path(DIR_GRF, sprintf("nmx_%s_ap.png", a)),
         plot = p, width = 22, height = 13, units = "cm", dpi = 300)
})

p_nmx_painel <- (
  (p_nmx_list[["2010"]] + p_nmx_list[["2019"]] + p_nmx_list[["2021"]]) /
    (p_nmx_list[["2022"]] + p_nmx_list[["2024"]] + plot_spacer())
) +
  plot_annotation(
    title = "Taxas Específicas de Mortalidade (nMx) por sexo e grupo etário",
    subtitle = "Amapá — anos selecionados — escala logarítmica"
  )

ggsave(file.path(DIR_GRF, "nmx_painel_ap.png"),
       plot = p_nmx_painel, width = 40, height = 28, units = "cm", dpi = 300)

# Mortalidade infantil
obitos_inf <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr = as.character(IDADE),
    DTOBITO_chr = as.character(DTOBITO),
    SEXO_chr = as.character(SEXO)
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE,
    substr(IDADE_chr, 1, 1) %in% c("0","1","2","3") |
      (substr(IDADE_chr, 1, 1) == "4" & substr(IDADE_chr, 2, 3) == "00")
  ) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    sexo = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    idade_dias = decode_idade_dias(IDADE_chr),
    componente = case_when(
      is.na(idade_dias) ~ "Indeterminado",
      idade_dias < 7 ~ "Neonatal precoce (0-6d)",
      idade_dias < 28 ~ "Neonatal tardio (7-27d)",
      TRUE ~ "Pós-neonatal (28d-<1a)"
    )
  ) |>
  filter(!is.na(ano_obito))

nv_2023 <- sinasc_proc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
         substr(as.character(DTNASC), 1, 4) == "2023")

nv_2023_total <- nrow(nv_2023)

nv_2023_sexo <- nv_2023 |>
  mutate(sexo_rn = case_when(
    as.character(SEXO) %in% c("1","Masculino") ~ "Masculino",
    as.character(SEXO) %in% c("2","Feminino") ~ "Feminino",
    TRUE ~ "Ignorado"
  )) |>
  count(sexo_rn, name = "NV")

calcular_tmi <- function(df_inf, nv) {
  df_inf |>
    count(ano_obito, name = "n") |>
    complete(ano_obito = ANOS_TMI, fill = list(n = 0)) |>
    summarise(media = mean(n)) |>
    mutate(NV = nv, TMI = round(media / NV * 1000, 2))
}

inf_anos <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI)

tmi_total <- calcular_tmi(inf_anos, nv_2023_total) |>
  mutate(sexo = "Total")

tmi_sexo <- map_dfr(c("Masculino","Feminino"), function(s) {
  nv_s <- nv_2023_sexo |> filter(sexo_rn == s) |> pull(NV)
  calcular_tmi(inf_anos |> filter(sexo == s), nv_s) |>
    mutate(sexo = s)
})

tab_tmi <- bind_rows(tmi_total, tmi_sexo) |>
  select(sexo, media, NV, TMI) |>
  rename(
    `Sexo` = sexo,
    `Média óbitos infantis 2022-24` = media,
    `NV 2023` = NV,
    `TMI (‰)` = TMI
  )

# Componentes da TMI
tab_componentes <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI,
         componente != "Indeterminado") |>
  count(ano_obito, componente, name = "n") |>
  complete(
    ano_obito = ANOS_TMI,
    componente = c("Neonatal precoce (0-6d)",
                   "Neonatal tardio (7-27d)",
                   "Pós-neonatal (28d-<1a)"),
    fill = list(n = 0)
  ) |>
  group_by(componente) |>
  summarise(media = mean(n), .groups = "drop") |>
  mutate(taxa = round(media / nv_2023_total * 1000, 2)) |>
  rename(
    Componente = componente,
    `Média óbitos 2022-24` = media,
    `Taxa (‰)` = taxa
  )

# Mortalidade perinatal — critério do material: fetais 28+ semanas + neonatal precoce
sim_fet_ap <- sim_fet_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    DTOBITO_chr = as.character(DTOBITO),
    GESTACAO_chr = as.character(GESTACAO)
  ) |>
  filter(!is.na(CODMUNRES_chr),
         substr(CODMUNRES_chr, 1, 2) == UF_IBGE) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    gest_code = suppressWarnings(as.integer(GESTACAO_chr)),
    gest_ge28 = case_when(
      gest_code %in% 3:6 ~ TRUE,
      grepl("28|32|37|42", GESTACAO_chr) ~ TRUE,
      TRUE ~ FALSE
    ),
    gest_ignorada = is.na(gest_code) | GESTACAO_chr %in% c("9","99")
  ) |>
  filter(!is.na(ano_obito))

fet_ano <- sim_fet_ap |>
  filter(ano_obito %in% ANOS_TMI) |>
  group_by(ano_obito) |>
  summarise(
    n_fet_ge28 = sum(gest_ge28),
    n_fet_todos = n(),
    .groups = "drop"
  )

neo_prec_ano <- obitos_inf |>
  filter(ano_obito %in% ANOS_TMI,
         componente == "Neonatal precoce (0-6d)") |>
  count(ano_obito, name = "n_neo_prec")

perinatal_df <- fet_ano |>
  left_join(neo_prec_ano, by = "ano_obito") |>
  replace_na(list(n_neo_prec = 0L))

media_fet_ge28 <- mean(perinatal_df$n_fet_ge28)
media_fet_todos <- mean(perinatal_df$n_fet_todos)
media_neo_prec <- mean(perinatal_df$n_neo_prec)

fet_2023_ge28 <- sim_fet_ap |>
  filter(ano_obito == 2023, gest_ge28) |>
  nrow()

fet_2023_todos <- sim_fet_ap |>
  filter(ano_obito == 2023) |>
  nrow()

tab_tmp <- tibble(
  Critério = c("Fetais 28+ semanas",
               "Todos os fetais — sensibilidade"),
  `Fetais (média 2022-24)` = round(c(media_fet_ge28, media_fet_todos), 1),
  `Neo. precoces (média)` = round(media_neo_prec, 1),
  `Denominador (NV+fet 2023)` = c(nv_2023_total + fet_2023_ge28,
                                  nv_2023_total + fet_2023_todos),
  `TMP (‰)` = round(c(
    (media_fet_ge28 + media_neo_prec) / (nv_2023_total + fet_2023_ge28) * 1000,
    (media_fet_todos + media_neo_prec) / (nv_2023_total + fet_2023_todos) * 1000
  ), 2)
)

# Exportar
write_csv(tab_tbm, file.path(DIR_TAB, "tab_3a_tbm.csv"))
write_csv(nMx_masc_wide, file.path(DIR_TAB, "tab_3a_nmx_masculino.csv"))
write_csv(nMx_fem_wide, file.path(DIR_TAB, "tab_3a_nmx_feminino.csv"))
write_csv(tab_tmi, file.path(DIR_TAB, "tab_3b_tmi.csv"))
write_csv(tab_componentes, file.path(DIR_TAB, "tab_3b_tmi_componentes.csv"))
write_csv(tab_tmp, file.path(DIR_TAB, "tab_3b_mortalidade_perinatal.csv"))

write_xlsx(
  list(
    `3a_TBM` = as.data.frame(tab_tbm),
    `3a_nMx_Masculino` = as.data.frame(nMx_masc_wide),
    `3a_nMx_Feminino` = as.data.frame(nMx_fem_wide),
    `3b_TMI` = as.data.frame(tab_tmi),
    `3b_TMI_componentes` = as.data.frame(tab_componentes),
    `3b_TMP` = as.data.frame(tab_tmp)
  ),
  path = file.path(DIR_TAB, "questao3_tabelas.xlsx")
)

saveRDS(nMx_long, file.path(DIR_PROC, "nMx_long_q3_rev2024.rds"))

cat("\nQuestão 3 concluída com Projeção IBGE Revisão 2024.\n")
