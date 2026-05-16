################################################################################
# TRABALHO 1 — DEMOGRAFIA — UnB
# Questão 3c e 3d: Causas de morte e Tábuas de Vida
# Amapá (AP, código IBGE 16)
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

UF_IBGE <- "16"

ANOS_CAUSAS <- c(2010, 2021, 2024)
ANOS_TV     <- c(2010, 2024)
ANOS_TMI    <- c(2022, 2023, 2024)

GRUPOS_MX <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29",
               "30-34","35-39","40-44","45-49","50-54","55-59",
               "60-64","65-69","70-74","75-79","80+")

# ==============================================================================
# 1. FUNÇÕES AUXILIARES
# ==============================================================================

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

extrair_ano_obito <- function(x) {
  x  <- as.character(trimws(x))
  a1 <- suppressWarnings(as.integer(substr(x, 1, 4)))
  a2 <- suppressWarnings(as.integer(substr(x, 5, 8)))
  if_else(!is.na(a1) & a1 >= 1990 & a1 <= 2030, a1,
          if_else(!is.na(a2) & a2 >= 1990 & a2 <= 2030, a2, NA_integer_))
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

classificar_grupo_idade_causas <- function(idade_anos) {
  case_when(
    idade_anos < 5 ~ "<5",
    idade_anos >= 5  & idade_anos <= 14 ~ "5-14",
    idade_anos >= 15 & idade_anos <= 39 ~ "15-39",
    idade_anos >= 40 & idade_anos <= 59 ~ "40-59",
    idade_anos >= 60 ~ "60+",
    TRUE ~ NA_character_
  )
}

classificar_cid10_grupo <- function(causa) {
  cid <- toupper(gsub("[^A-Z0-9]", "", as.character(causa)))
  letra <- substr(cid, 1, 1)
  num <- suppressWarnings(as.integer(substr(cid, 2, 3)))
  
  case_when(
    str_detect(cid, "^B342|^B342|^B34.2|^U071|^U072") ~ "COVID-19 (B34.2/U07)",
    
    letra %in% c("A","B") ~ "I. Infecciosas e parasitárias",
    letra == "C" | (letra == "D" & !is.na(num) & num <= 48) ~ "II. Neoplasias",
    letra == "D" & !is.na(num) & num >= 50 ~ "III. Sangue e imunitárias",
    letra == "E" ~ "IV. Endócrinas, nutricionais e metabólicas",
    letra == "F" ~ "V. Transtornos mentais e comportamentais",
    letra == "G" ~ "VI. Sistema nervoso",
    letra == "H" & !is.na(num) & num <= 59 ~ "VII. Olho e anexos",
    letra == "H" & !is.na(num) & num >= 60 ~ "VIII. Ouvido e apófise mastoide",
    letra == "I" ~ "IX. Aparelho circulatório",
    letra == "J" ~ "X. Aparelho respiratório",
    letra == "K" ~ "XI. Aparelho digestivo",
    letra == "L" ~ "XII. Pele e tecido subcutâneo",
    letra == "M" ~ "XIII. Osteomuscular e conjuntivo",
    letra == "N" ~ "XIV. Aparelho geniturinário",
    letra == "O" ~ "XV. Gravidez, parto e puerpério",
    letra == "P" ~ "XVI. Período perinatal",
    letra == "Q" ~ "XVII. Malformações congênitas",
    letra == "R" ~ "XVIII. Sintomas e achados anormais",
    letra %in% c("V","W","X","Y") ~ "XX. Causas externas",
    letra == "Z" ~ "XXI. Fatores de contato com serviços",
    letra == "U" ~ "XXII. Códigos especiais",
    TRUE ~ "Ignorado ou mal definido"
  )
}

# ==============================================================================
# 2. CARREGAR BASES
# ==============================================================================

sim_bruto   <- readRDS(file.path(DIR_BRUTOS, "sim_do_ap_2000_2024.rds"))
sinasc_proc <- readRDS(file.path(DIR_PROC, "sinasc_ap_proc.rds"))
proj_2024   <- readRDS(file.path(DIR_BRUTOS, "projecoes_ibge_rev2024_ap.rds"))

# ==============================================================================
# 3c. ESTRUTURA DE MORTALIDADE POR CAUSAS
# ==============================================================================

sim_causas <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr     = as.character(IDADE),
    DTOBITO_chr   = as.character(DTOBITO),
    SEXO_chr      = as.character(SEXO),
    causa_basica  = as.character(CAUSABAS)
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE
  ) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    idade_anos = decode_idade_anos(IDADE_chr),
    sexo = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    grupo_idade_causa = classificar_grupo_idade_causas(idade_anos),
    grupo_causa = classificar_cid10_grupo(causa_basica),
    covid = str_detect(toupper(gsub("[^A-Z0-9]", "", causa_basica)),
                       "^B342|^U071|^U072")
  ) |>
  filter(ano_obito %in% ANOS_CAUSAS)

# Top 20 grupos de causas no conjunto dos anos selecionados
top20_causas <- sim_causas |>
  count(grupo_causa, name = "obitos_total") |>
  arrange(desc(obitos_total)) |>
  slice_head(n = 20)

sim_causas_top20 <- sim_causas |>
  mutate(
    grupo_causa_top20 = if_else(
      grupo_causa %in% top20_causas$grupo_causa,
      grupo_causa,
      "Demais causas"
    )
  )

tab_causas_ano <- sim_causas_top20 |>
  count(ano_obito, grupo_causa_top20, name = "obitos") |>
  group_by(ano_obito) |>
  mutate(
    total_ano = sum(obitos),
    pct = obitos / total_ano * 100
  ) |>
  ungroup() |>
  arrange(ano_obito, desc(obitos))

tab_causas_sexo <- sim_causas_top20 |>
  filter(sexo %in% c("Masculino","Feminino")) |>
  count(ano_obito, sexo, grupo_causa_top20, name = "obitos") |>
  group_by(ano_obito, sexo) |>
  mutate(
    total = sum(obitos),
    pct = obitos / total * 100
  ) |>
  ungroup()

tab_causas_idade <- sim_causas_top20 |>
  filter(!is.na(grupo_idade_causa)) |>
  count(ano_obito, grupo_idade_causa, grupo_causa_top20, name = "obitos") |>
  group_by(ano_obito, grupo_idade_causa) |>
  mutate(
    total = sum(obitos),
    pct = obitos / total * 100
  ) |>
  ungroup()

tab_covid <- sim_causas |>
  filter(covid) |>
  count(ano_obito, sexo, grupo_idade_causa, name = "obitos_covid") |>
  arrange(ano_obito, sexo, grupo_idade_causa)

cat("\nTop 20 grupos de causas:\n")
print(top20_causas)

cat("\nÓbitos por COVID-19 detectados:\n")
print(tab_covid)

# Gráfico 1: estrutura geral por ano
ordem_causas <- tab_causas_ano |>
  group_by(grupo_causa_top20) |>
  summarise(total = sum(obitos), .groups = "drop") |>
  arrange(total) |>
  pull(grupo_causa_top20)

p_causas_ano <- tab_causas_ano |>
  mutate(grupo_causa_top20 = factor(grupo_causa_top20, levels = ordem_causas)) |>
  ggplot(aes(x = pct, y = grupo_causa_top20, fill = as.factor(ano_obito))) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Estrutura de mortalidade por grupos de causas",
    subtitle = "Amapá — 2010, 2021 e 2024",
    x = "Participação no total de óbitos do ano",
    y = "Grupo de causa",
    fill = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_GRF, "causas_top20_ano_ap.png"),
       p_causas_ano, width = 28, height = 18, units = "cm", dpi = 300)

# Gráfico 2: causas por sexo
p_causas_sexo <- tab_causas_sexo |>
  filter(grupo_causa_top20 != "Demais causas") |>
  mutate(grupo_causa_top20 = factor(grupo_causa_top20, levels = ordem_causas)) |>
  ggplot(aes(x = pct, y = grupo_causa_top20, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ ano_obito) +
  scale_x_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Estrutura de causas de morte por sexo",
    subtitle = "Amapá — anos selecionados",
    x = "Participação nos óbitos do sexo",
    y = "Grupo de causa",
    fill = "Sexo"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_GRF, "causas_top20_sexo_ap.png"),
       p_causas_sexo, width = 32, height = 20, units = "cm", dpi = 300)

# Gráfico 3: COVID-19 por sexo e idade
p_covid <- tab_covid |>
  filter(!is.na(grupo_idade_causa), sexo %in% c("Masculino","Feminino")) |>
  ggplot(aes(x = grupo_idade_causa, y = obitos_covid, fill = sexo)) +
  geom_col(position = "dodge") +
  facet_wrap(~ ano_obito) +
  labs(
    title = "Óbitos por COVID-19 segundo sexo e grupo de idade",
    subtitle = "CID-10 B34.2/U07 — Amapá",
    x = "Grupo de idade",
    y = "Óbitos",
    fill = "Sexo"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(file.path(DIR_GRF, "covid_sexo_idade_ap.png"),
       p_covid, width = 24, height = 14, units = "cm", dpi = 300)

# ==============================================================================
# 3d. TÁBUAS DE VIDA
# ==============================================================================

# Recalcular nMx por sexo, idade e ano usando a Projeção IBGE Rev. 2024
sim_tv <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr     = as.character(IDADE),
    DTOBITO_chr   = as.character(DTOBITO),
    SEXO_chr      = as.character(SEXO)
  ) |>
  filter(!is.na(CODMUNRES_chr),
         substr(CODMUNRES_chr, 1, 2) == UF_IBGE) |>
  mutate(
    ano_obito = extrair_ano_obito(DTOBITO_chr),
    idade_anos = decode_idade_anos(IDADE_chr),
    idade_dias = decode_idade_dias(IDADE_chr),
    sexo = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    grupo_mx = classificar_grupo_mx(idade_anos)
  ) |>
  filter(ano_obito %in% ANOS_TV,
         sexo %in% c("Masculino","Feminino"),
         !is.na(grupo_mx))

proj_tv <- proj_2024 |>
  mutate(
    ano = as.integer(ano),
    idade_num = as.integer(idade_num),
    pop = as.numeric(pop),
    sexo = case_when(
      sexo == "Homens" ~ "Masculino",
      sexo == "Mulheres" ~ "Feminino",
      TRUE ~ NA_character_
    ),
    grupo_mx = classificar_grupo_mx(idade_num)
  ) |>
  filter(ano %in% ANOS_TV,
         sexo %in% c("Masculino","Feminino"),
         !is.na(grupo_mx)) |>
  group_by(ano, sexo, grupo_mx) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_tv <- sim_tv |>
  count(ano = ano_obito, sexo, grupo_mx, name = "n_obitos")

grade_tv <- expand_grid(
  ano = ANOS_TV,
  sexo = c("Masculino","Feminino"),
  grupo_mx = GRUPOS_MX
)

nMx_tv <- grade_tv |>
  left_join(obitos_tv, by = c("ano","sexo","grupo_mx")) |>
  replace_na(list(n_obitos = 0L)) |>
  left_join(proj_tv, by = c("ano","sexo","grupo_mx")) |>
  mutate(
    nMx = n_obitos / pop * 1000,
    mx = nMx / 1000,
    grupo_mx = factor(grupo_mx, levels = GRUPOS_MX)
  )

# TMI por sexo do item b: média 2022-2024 / NV 2023
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
    )
  ) |>
  filter(ano_obito %in% ANOS_TMI)

nv_2023_sexo <- sinasc_proc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
         substr(as.character(DTNASC), 1, 4) == "2023") |>
  mutate(sexo = case_when(
    as.character(SEXO) %in% c("1","Masculino") ~ "Masculino",
    as.character(SEXO) %in% c("2","Feminino") ~ "Feminino",
    TRUE ~ "Ignorado"
  )) |>
  count(sexo, name = "NV")

tmi_item_b_sexo <- map_dfr(c("Masculino","Feminino"), function(s) {
  media_obitos <- obitos_inf |>
    filter(sexo == s) |>
    count(ano_obito, name = "n") |>
    complete(ano_obito = ANOS_TMI, fill = list(n = 0)) |>
    summarise(media = mean(n)) |>
    pull(media)
  
  nv_s <- nv_2023_sexo |>
    filter(sexo == s) |>
    pull(NV)
  
  tibble(
    sexo = s,
    TMI_item_b = media_obitos / nv_s,
    TMI_item_b_pmil = TMI_item_b * 1000
  )
})

# TMI específica de 2010, para evitar usar TMI 2022-24 numa tábua de 2010.
# Se o professor pedir estritamente a TMI do item b também para 2010, mude a opção abaixo para TRUE.
USAR_TMI_ITEM_B_TAMBEM_2010 <- FALSE

tmi_2010_sexo <- map_dfr(c("Masculino","Feminino"), function(s) {
  ob_2010 <- sim_bruto |>
    mutate(
      CODMUNRES_chr = as.character(CODMUNRES),
      IDADE_chr = as.character(IDADE),
      DTOBITO_chr = as.character(DTOBITO),
      SEXO_chr = as.character(SEXO)
    ) |>
    filter(
      !is.na(CODMUNRES_chr),
      substr(CODMUNRES_chr, 1, 2) == UF_IBGE,
      extrair_ano_obito(DTOBITO_chr) == 2010,
      case_when(SEXO_chr == "1" ~ "Masculino",
                SEXO_chr == "2" ~ "Feminino",
                TRUE ~ "Ignorado") == s,
      substr(IDADE_chr, 1, 1) %in% c("0","1","2","3") |
        (substr(IDADE_chr, 1, 1) == "4" & substr(IDADE_chr, 2, 3) == "00")
    ) |>
    nrow()
  
  nv_2010 <- sinasc_proc |>
    filter(
      !is.na(CODMUNRES),
      substr(as.character(CODMUNRES), 1, 2) == UF_IBGE,
      substr(as.character(DTNASC), 1, 4) == "2010",
      case_when(as.character(SEXO) %in% c("1","Masculino") ~ "Masculino",
                as.character(SEXO) %in% c("2","Feminino") ~ "Feminino",
                TRUE ~ "Ignorado") == s
    ) |>
    nrow()
  
  tibble(sexo = s, TMI_2010 = ob_2010 / nv_2010)
})

q0_usado <- expand_grid(
  ano = ANOS_TV,
  sexo = c("Masculino","Feminino")
) |>
  left_join(tmi_item_b_sexo, by = "sexo") |>
  left_join(tmi_2010_sexo, by = "sexo") |>
  mutate(
    q0 = case_when(
      ano == 2024 ~ TMI_item_b,
      ano == 2010 & USAR_TMI_ITEM_B_TAMBEM_2010 ~ TMI_item_b,
      ano == 2010 & !USAR_TMI_ITEM_B_TAMBEM_2010 ~ TMI_2010
    ),
    q0_pmil = q0 * 1000
  )

# Fatores de separação a0 e a1_4 estimados com o SIM
# a0: idade média ao óbito infantil em anos.
# a1_4: anos médios vividos no intervalo 1-4 entre óbitos de 1 a 4 anos.
fatores_sep <- sim_bruto |>
  mutate(
    CODMUNRES_chr = as.character(CODMUNRES),
    IDADE_chr = as.character(IDADE),
    DTOBITO_chr = as.character(DTOBITO),
    SEXO_chr = as.character(SEXO),
    ano = extrair_ano_obito(DTOBITO_chr),
    sexo = case_when(
      SEXO_chr == "1" ~ "Masculino",
      SEXO_chr == "2" ~ "Feminino",
      TRUE ~ "Ignorado"
    ),
    idade_anos = decode_idade_anos(IDADE_chr),
    idade_dias = decode_idade_dias(IDADE_chr),
    idade_exata_aprox = case_when(
      !is.na(idade_dias) ~ idade_dias / 365.25,
      !is.na(idade_anos) ~ idade_anos + 0.5,
      TRUE ~ NA_real_
    )
  ) |>
  filter(
    !is.na(CODMUNRES_chr),
    substr(CODMUNRES_chr, 1, 2) == UF_IBGE,
    ano %in% ANOS_TV,
    sexo %in% c("Masculino","Feminino")
  ) |>
  group_by(ano, sexo) |>
  summarise(
    a0 = mean(idade_exata_aprox[idade_anos == 0], na.rm = TRUE),
    a1_4 = mean(idade_exata_aprox[idade_anos >= 1 & idade_anos <= 4] - 1,
                na.rm = TRUE),
    n_obitos_0 = sum(idade_anos == 0, na.rm = TRUE),
    n_obitos_1_4 = sum(idade_anos >= 1 & idade_anos <= 4, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    a0 = if_else(is.nan(a0), 0.15, a0),
    a1_4 = if_else(is.nan(a1_4), 2.0, a1_4)
  )

# Função para construir tábua abreviada
construir_tabua <- function(ano_ref, sexo_ref) {
  
  df <- nMx_tv |>
    filter(ano == ano_ref, sexo == sexo_ref) |>
    arrange(grupo_mx) |>
    mutate(
      x = c(0, 1, seq(5, 80, by = 5)),
      n = c(1, 4, rep(5, 15), NA_real_)
    ) |>
    left_join(
      fatores_sep |> filter(ano == ano_ref, sexo == sexo_ref) |>
        select(ano, sexo, a0, a1_4),
      by = c("ano","sexo")
    ) |>
    left_join(
      q0_usado |> filter(ano == ano_ref, sexo == sexo_ref) |>
        select(ano, sexo, q0),
      by = c("ano","sexo")
    ) |>
    mutate(
      ax = case_when(
        grupo_mx == "<1" ~ a0,
        grupo_mx == "1-4" ~ a1_4,
        grupo_mx == "80+" ~ NA_real_,
        TRUE ~ n / 2
      ),
      nqx = case_when(
        grupo_mx == "<1" ~ q0,
        grupo_mx == "80+" ~ 1,
        TRUE ~ (n * mx) / (1 + (n - ax) * mx)
      ),
      nqx = pmin(nqx, 1)
    )
  
  lx <- numeric(nrow(df))
  ndx <- numeric(nrow(df))
  nLx <- numeric(nrow(df))
  
  lx[1] <- 100000
  
  for (i in seq_len(nrow(df))) {
    ndx[i] <- lx[i] * df$nqx[i]
    
    if (df$grupo_mx[i] == "80+") {
      nLx[i] <- lx[i] / df$mx[i]
    } else {
      nLx[i] <- df$n[i] * lx[i] - (df$n[i] - df$ax[i]) * ndx[i]
    }
    
    if (i < nrow(df)) {
      lx[i + 1] <- lx[i] - ndx[i]
    }
  }
  
  df$lx <- lx
  df$ndx <- ndx
  df$nLx <- nLx
  df$Tx <- rev(cumsum(rev(df$nLx)))
  df$ex <- df$Tx / df$lx
  
  df |>
    select(ano, sexo, grupo_mx, x, n, n_obitos, pop, nMx, mx, ax, nqx,
           lx, ndx, nLx, Tx, ex)
}

tabuas_vida <- map_dfr(ANOS_TV, function(a) {
  map_dfr(c("Masculino","Feminino"), function(s) {
    construir_tabua(a, s)
  })
})

resumo_tv <- tabuas_vida |>
  filter(x %in% c(0, 60)) |>
  select(ano, sexo, idade_exata = x, ex) |>
  pivot_wider(names_from = idade_exata, values_from = ex, names_prefix = "e") |>
  mutate(
    e0 = round(e0, 2),
    e60 = round(e60, 2)
  )

# Comparação IBGE — tenta ler a planilha oficial se o pacote readxl estiver disponível.
arq_tabua_ibge <- file.path(DIR_BRUTOS, "projecoes_2024_tab5_tabuas_mortalidade.xlsx")

if (requireNamespace("readxl", quietly = TRUE) && file.exists(arq_tabua_ibge)) {
  tabua_ibge <- readxl::read_excel(arq_tabua_ibge, skip = 5) |>
    janitor::clean_names() |>
    filter(sigla == "AP",
           ano %in% ANOS_TV,
           sexo %in% c("Homens","Mulheres"),
           idade %in% c(0, 60)) |>
    transmute(
      ano = as.integer(ano),
      sexo = if_else(sexo == "Homens", "Masculino", "Feminino"),
      idade_exata = as.integer(idade),
      ex_ibge = as.numeric(ex)
    ) |>
    pivot_wider(names_from = idade_exata, values_from = ex_ibge,
                names_prefix = "e") |>
    rename(e0_ibge = e0, e60_ibge = e60)
} else {
  warning("Pacote readxl ausente ou planilha IBGE não encontrada. Comparação IBGE ficará como NA.")
  tabua_ibge <- expand_grid(
    ano = ANOS_TV,
    sexo = c("Masculino","Feminino")
  ) |>
    mutate(e0_ibge = NA_real_, e60_ibge = NA_real_)
}

comparacao_e0_e60 <- resumo_tv |>
  left_join(tabua_ibge, by = c("ano","sexo")) |>
  mutate(
    dif_e0_ibge = round(e0 - e0_ibge, 2),
    dif_e60_ibge = round(e60 - e60_ibge, 2)
  )

# Gráficos lx e nqx
p_lx <- tabuas_vida |>
  ggplot(aes(x = x, y = lx, color = sexo, linetype = as.factor(ano))) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Masculino" = "steelblue4", "Feminino" = "#8b1a1a")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Função lx da Tábua de Vida",
    subtitle = "Amapá — 2010 e 2024",
    x = "Idade exata",
    y = expression(l[x]),
    color = "Sexo",
    linetype = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "tv_lx_ap.png"),
       p_lx, width = 22, height = 13, units = "cm", dpi = 300)

p_nqx <- tabuas_vida |>
  filter(nqx > 0) |>
  ggplot(aes(x = x, y = nqx, color = sexo, linetype = as.factor(ano))) +
  geom_line(linewidth = 1.1) +
  scale_y_log10(labels = label_number()) +
  scale_color_manual(values = c("Masculino" = "steelblue4", "Feminino" = "#8b1a1a")) +
  labs(
    title = "Função nqx da Tábua de Vida",
    subtitle = "Amapá — 2010 e 2024 — escala logarítmica",
    x = "Idade exata",
    y = expression(n*q[x]),
    color = "Sexo",
    linetype = "Ano"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_GRF, "tv_nqx_ap.png"),
       p_nqx, width = 22, height = 13, units = "cm", dpi = 300)

# ==============================================================================
# EXPORTAÇÃO
# ==============================================================================

write_csv(top20_causas, file.path(DIR_TAB, "tab_3c_top20_causas.csv"))
write_csv(tab_causas_ano, file.path(DIR_TAB, "tab_3c_causas_ano.csv"))
write_csv(tab_causas_sexo, file.path(DIR_TAB, "tab_3c_causas_sexo.csv"))
write_csv(tab_causas_idade, file.path(DIR_TAB, "tab_3c_causas_idade.csv"))
write_csv(tab_covid, file.path(DIR_TAB, "tab_3c_covid_sexo_idade.csv"))

write_csv(fatores_sep, file.path(DIR_TAB, "tab_3d_fatores_separacao.csv"))
write_csv(q0_usado, file.path(DIR_TAB, "tab_3d_q0_usado.csv"))
write_csv(tabuas_vida, file.path(DIR_TAB, "tab_3d_tabuas_vida.csv"))
write_csv(resumo_tv, file.path(DIR_TAB, "tab_3d_resumo_e0_e60.csv"))
write_csv(comparacao_e0_e60, file.path(DIR_TAB, "tab_3d_comparacao_ibge.csv"))

write_xlsx(
  list(
    `3c_top20_causas` = as.data.frame(top20_causas),
    `3c_causas_ano` = as.data.frame(tab_causas_ano),
    `3c_causas_sexo` = as.data.frame(tab_causas_sexo),
    `3c_causas_idade` = as.data.frame(tab_causas_idade),
    `3c_covid` = as.data.frame(tab_covid),
    `3d_fatores_sep` = as.data.frame(fatores_sep),
    `3d_q0_usado` = as.data.frame(q0_usado),
    `3d_tabuas` = as.data.frame(tabuas_vida),
    `3d_resumo_e0_e60` = as.data.frame(resumo_tv),
    `3d_comparacao` = as.data.frame(comparacao_e0_e60)
  ),
  path = file.path(DIR_TAB, "questao3_cd_tabelas.xlsx")
)

cat("\nQuestão 3c e 3d concluídas.\n")
cat("Gráficos salvos em outputs/graficos/:\n")
cat("  causas_top20_ano_ap.png\n")
cat("  causas_top20_sexo_ap.png\n")
cat("  covid_sexo_idade_ap.png\n")
cat("  tv_lx_ap.png\n")
cat("  tv_nqx_ap.png\n")
cat("\nTabelas salvas em outputs/tabelas/.\n")
